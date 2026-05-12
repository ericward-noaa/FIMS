#' Augment a FIMSFit object for use with yardstick and tidymodels
#'
#' Returns a tidy tibble of observed-vs-expected pairs drawn from the output of
#' [get_estimates()].  Only rows that have both an observed value and a
#' expected value are included, so parameter
#' rows without data observations are automatically dropped.
#'
#' The returned tibble follows the conventions expected by every
#' [yardstick] metric function:
#'
#' | Column        | Role                                                      |
#' |---------------|-----------------------------------------------------------|
#' | `.truth`      | Observed data value (maps from `observed`)                |
#' | `.pred`       | Model-expected value (maps from `expected`)               |
#' | `.weight`     | Inverse-variance weight from `uncertainty` (optional)     |
#' | `label`       | Parameter / quantity label, e.g. `"landings_expected"`    |
#' | `fleet`       | Fleet identifier (integer)                                |
#' | `module_id`   | Unique module identifier                                  |
#' | `distribution`| Likelihood distribution used for this data stream         |
#' | `year_i`      | Year index (present when available in the estimates)      |
#' | `age_i`       | Age index  (present when available in the estimates)      |
#'
#' @param x A `FIMSFit` object returned from [fit_fims()].
#' @param include_weights Logical (default `TRUE`).  When `TRUE` and
#'   `uncertainty` is available, a `.weight` column is added equal to
#'   `1 / uncertainty^2` (inverse-variance weights).  Rows where
#'   `uncertainty` is `NA` or zero receive `NA` weights, which yardstick
#'   silently drops when calling weighted metrics.
#' @param ... Unused; present for S3 method compatibility.
#'
#' @return A [tibble::tibble()] with at least the columns `.truth`, `.pred`,
#'   and optional `.weight`, plus grouping-metadata columns.
#'
#' @seealso [get_fit_metrics()], [get_estimates()], [yardstick::metrics()]
#' @export
augment.FIMSFit <- function(x, include_weights = TRUE, ...) {
  estimates <- get_estimates(x)

  # Keep only rows that are data-fit rows: both observed and expected must be
  # present.  Parameter rows (fixed_effects, random_effects) that have no
  # observed data counterpart are excluded.
  fit_rows <- estimates |>
    dplyr::filter(
      !is.na(observed),
      !is.na(expected)
    )

  if (nrow(fit_rows) == 0) {
    cli::cli_warn(c(
      "!" = "No observed/expected pairs found in the FIMSFit estimates.",
      "i" = "Returning an empty tibble. Check that the model has data-likelihood components."
    ))
    return(tibble::tibble(
      .truth  = numeric(),
      .pred   = numeric(),
      label   = character(),
      fleet   = integer(),
      module_id = integer(),
      distribution = character()
    ))
  }

  # Determine which optional index columns are present in the output
  # (year_i, age_i, length_i, season_i, ...).  These are carried through so
  # users can group metrics by, e.g., year.
  index_cols <- names(fit_rows)[grepl("_i$", names(fit_rows))]

  # Core metadata columns to retain for grouping / filtering
  meta_cols <- intersect(
    c("label", "module_id", "module_type", "fleet", "distribution",
      "estimation_type", index_cols),
    names(fit_rows)
  )

  out <- fit_rows |>
    dplyr::select(
      dplyr::all_of(meta_cols),
      ".truth"  = observed,
      ".pred"   = expected,
      dplyr::any_of("uncertainty")
    ) |>
    dplyr::mutate(
      .truth = as.numeric(.truth),
      .pred  = as.numeric(.pred)
    )

  if (include_weights && "uncertainty" %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        .weight = dplyr::if_else(
          !is.na(uncertainty) & uncertainty > 0,
          1 / uncertainty^2,
          NA_real_
        )
      ) |>
      dplyr::select(-uncertainty)
  } else if ("uncertainty" %in% names(out)) {
    out <- dplyr::select(out, -uncertainty)
  }

  out
}

#' Compute yardstick metrics for a fitted FIMS model
#'
#' Extracts observed-vs-expected pairs from a `FIMSFit` object via
#' `augment.FIMSFit()` and evaluates a [yardstick::metric_set()] over them.
#'
#' By default the metrics are computed over **all** data streams combined.
#' Pass one or more column names to `group_by` to get per-stream breakdowns
#' (e.g., `group_by = "label"` gives one row per data-stream label such as
#' `"landings_expected"`, `"age_comp_expected"`, etc.).
#'
#' @param x A `FIMSFit` object returned from [fit_fims()].
#' @param metrics A [yardstick::metric_set()].  Defaults to
#'   `yardstick::metric_set(yardstick::rmse, yardstick::mae, yardstick::rsq)`.
#'   Any regression metric that accepts `truth` / `estimate` (and optionally
#'   `case_weights`) can be included.
#' @param group_by A character vector of column names in the augmented tibble
#'   to group by before computing metrics.  Common choices:
#'   * `"label"`        – one row-set per data-stream label
#'   * `"fleet"`        – one row-set per fleet
#'   * `"distribution"` – one row-set per likelihood distribution
#'   * `c("label", "fleet")` – per label × fleet combination
#'   Defaults to `NULL` (no grouping).
#' @param weighted Logical (default `FALSE`).  When `TRUE`, inverse-variance
#'   weights from `uncertainty` are passed to the metric functions via the
#'   `case_weights` argument.  Only metrics that accept `case_weights` will
#'   use them; others silently ignore the column.
#' @param ... Additional arguments forwarded to `augment.FIMSFit()`.
#'
#' @return A [tibble::tibble()] in the standard yardstick result format:
#'   columns `.metric`, `.estimator`, `.estimate`, plus any grouping columns.
#'
#' @examples
#' \dontrun{
#' fit <- fit_fims(input)
#'
#' # Overall RMSE / MAE / R²
#' get_fit_metrics(fit)
#'
#' # Per-data-stream breakdown
#' get_fit_metrics(fit, group_by = "label")
#'
#' # Per-fleet, custom metric set
#' get_fit_metrics(
#'   fit,
#'   metrics  = yardstick::metric_set(yardstick::rmse, yardstick::mape),
#'   group_by = "fleet"
#' )
#'
#' # Inverse-variance weighted metrics
#' get_fit_metrics(fit, weighted = TRUE)
#' }
#'
#' @seealso `augment.FIMSFit()`, [yardstick::metric_set()]
#' @export
get_fit_metrics <- function(
    x,
    metrics   = yardstick::metric_set(
      yardstick::rmse,
      yardstick::mae,
      yardstick::rsq
    ),
    group_by  = NULL,
    weighted  = FALSE,
    ...
) {
  if (!is.FIMSFit(x)) {
    cli::cli_abort("{.arg x} must be a {.cls FIMSFit} object.")
  }
  if (!requireNamespace("yardstick", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg yardstick} is required. Install it with
       {.code install.packages('yardstick')}."
    )
  }

  aug <- augment.FIMSFit(x, include_weights = weighted, ...)

  if (nrow(aug) == 0) {
    cli::cli_warn("Augmented data is empty; returning empty metrics tibble.")
    return(tibble::tibble(
      .metric    = character(),
      .estimator = character(),
      .estimate  = numeric()
    ))
  }

  # Validate grouping columns
  if (!is.null(group_by)) {
    bad <- setdiff(group_by, names(aug))
    if (length(bad) > 0) {
      cli::cli_abort(
        "Column{?s} {.val {bad}} not found in the augmented tibble.
         Available columns: {.val {names(aug)}}."
      )
    }
    aug <- dplyr::group_by(aug, dplyr::across(dplyr::all_of(group_by)))
  }

  # Build the metric call: with or without case_weights
  if (weighted && ".weight" %in% names(aug)) {
    result <- metrics(
      data          = aug,
      truth         = .truth,
      estimate      = .pred,
      case_weights  = .weight
    )
  } else {
    result <- metrics(
      data     = aug,
      truth    = .truth,
      estimate = .pred
    )
  }

  result
}

#' Extract a single data stream from a FIMSFit augmented tibble
#'
#' Convenience filter to pull out one specific data stream (e.g. the landings
#' for fleet 1) so you can pass it directly to any yardstick metric or plot it.
#'
#' @param x A `FIMSFit` object **or** an already-augmented tibble from
#'   `augment.FIMSFit()`.
#' @param stream_label Character scalar.  The value of the `label` column to
#'   retain, e.g. `"landings_expected"` or `"age_comp_expected"`.  If `NULL`
#'   (default), no filtering on label is done.
#' @param fleet_id Integer scalar.  Fleet to retain.  If `NULL` (default), all
#'   fleets are included.
#' @param ... Forwarded to `augment.FIMSFit()` when `x` is a `FIMSFit`.
#'
#' @return A [tibble::tibble()] subset of the augmented data.
#' @export
get_fit_stream <- function(x, stream_label = NULL, fleet_id = NULL, ...) {
  aug <- if (is.FIMSFit(x)) augment.FIMSFit(x, ...) else x

  if (!is.null(stream_label)) {
    aug <- dplyr::filter(aug, label == stream_label)
  }
  if (!is.null(fleet_id)) {
    aug <- dplyr::filter(aug, fleet == fleet_id)
  }
  aug
}

#' Tidy a FIMSFit object into a parameter-level tibble
#'
#' Returns one row per estimated parameter following the
#' [broom::tidy()] convention.  Standard columns (`term`, `estimate`,
#' `std.error`, `statistic`, `p.value`) are always present; FIMS-specific
#' columns (`module_name`, `module_id`, `estimation_type`, `gradient`) are
#' appended so the full context is available for filtering and plotting.
#'
#' @section Parameter types:
#' FIMS distinguishes three `estimation_type` values:
#' \describe{
#'   \item{`"fixed_effects"`}{Directly optimised parameters (selectivity,
#'     log_Fmort, log_q, …).}
#'   \item{`"random_effects"`}{Integrated-out random effects (log_devs, …).}
#'   \item{`"derived_quantity"`}{Model outputs that are not parameters
#'     (spawning biomass, expected catches, …). Uncertainty here comes from
#'     the delta method via [TMB::sdreport()].}
#' }
#' Pass any subset of these strings to `parameters` to control which rows are
#' returned.
#'
#' @section Inference:
#' `statistic` and `p.value` are computed as a two-sided Wald z-test:
#' `z = estimate / std.error`,
#' `p = 2 * pnorm(-|z|)`.
#' These are asymptotically valid for fixed effects under regularity
#' conditions; treat them as approximate for random effects and derived
#' quantities.
#'
#' @param x A `FIMSFit` object returned from [fit_fims()].
#' @param parameters Character vector controlling which `estimation_type`
#'   values to include.  Defaults to `c("fixed_effects", "random_effects")`.
#'   Pass `"derived_quantity"` to include derived quantities such as spawning
#'   biomass and expected data values, or pass all three to get every row.
#' @param conf.int Logical (default `FALSE`).  When `TRUE`, `conf.low` and
#'   `conf.high` columns are added using a normal approximation:
#'   `estimate ± qnorm((1 + conf.level) / 2) * std.error`.
#' @param conf.level Numeric (default `0.95`).  The confidence level used when
#'   `conf.int = TRUE`.
#' @param ... Unused; present for S3 method compatibility.
#'
#' @return A [tibble::tibble()] with columns:
#' \describe{
#'   \item{`term`}{Parameter label (from `label` in [get_estimates()]).}
#'   \item{`estimate`}{Point estimate at the MLE.}
#'   \item{`std.error`}{Standard error from [TMB::sdreport()].}
#'   \item{`statistic`}{Wald z-statistic (`estimate / std.error`).}
#'   \item{`p.value`}{Two-sided p-value for the z-test.}
#'   \item{`conf.low`, `conf.high`}{Confidence bounds (only when
#'     `conf.int = TRUE`).}
#'   \item{`module_name`}{Name of the FIMS module (e.g. `"Selectivity"`).}
#'   \item{`module_id`}{Integer module identifier.}
#'   \item{`estimation_type`}{One of `"fixed_effects"`, `"random_effects"`,
#'     or `"derived_quantity"`.}
#'   \item{`gradient`}{Gradient of the log-likelihood at the MLE. Values
#'     close to zero indicate a well-converged parameter.}
#' }
#'
#' @examples
#' \dontrun{
#' data("data_big")
#' data_4_model <- FIMSFrame(data_big)
#'
#' fit <- create_default_parameters(
#'   configurations = create_default_configurations(data = data_4_model),
#'   data = data_4_model
#' ) |>
#'   initialize_fims(data = data_4_model) |>
#'   fit_fims(optimize = TRUE)
#'
#' # Fixed and random effects (default)
#' tidy(fit)
#'
#' # Fixed effects only, with 95% confidence intervals
#' tidy(fit, parameters = "fixed_effects", conf.int = TRUE)
#'
#' # All rows including derived quantities
#' tidy(fit, parameters = c("fixed_effects", "random_effects", "derived_quantity"))
#' }
#'
#' @seealso [glance.FIMSFit()], [get_estimates()], [fit_fims()]
#' @export
tidy.FIMSFit <- function(
    x,
    parameters = c("fixed_effects", "random_effects"),
    conf.int   = FALSE,
    conf.level = 0.95,
    ...
) {
  valid_types <- c("fixed_effects", "random_effects", "derived_quantity")
  bad <- setdiff(parameters, valid_types)
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "{.arg parameters} contains unknown estimation type{?s}: {.val {bad}}.",
      "i" = "Valid values are: {.val {valid_types}}."
    ))
  }

  estimates <- get_estimates(x)

  # Metadata columns to carry through (drop index columns and data columns
  # that belong in augment(), not tidy())
  meta_cols <- intersect(
    c("module_name", "module_id", "module_type", "fleet_name",
      "estimation_type", "gradient"),
    names(estimates)
  )

  out <- estimates |>
    dplyr::filter(estimation_type %in% parameters) |>
    dplyr::select(
      term           = label,
      estimate       = estimated,
      std.error      = uncertainty,
      dplyr::all_of(meta_cols)
    ) |>
    dplyr::mutate(
      estimate  = as.numeric(estimate),
      std.error = as.numeric(std.error),
      statistic = estimate / std.error,
      p.value   = 2 * stats::pnorm(-abs(statistic))
    ) |>
    dplyr::relocate(statistic, p.value, .after = std.error)

  if (conf.int) {
    z <- stats::qnorm((1 + conf.level) / 2)
    out <- out |>
      dplyr::mutate(
        conf.low  = estimate - z * std.error,
        conf.high = estimate + z * std.error
      )
  }

  out
}

#' Glance at a FIMSFit object — one-row model summary
#'
#' Returns a single-row tibble of model-level diagnostics following the
#' [broom::glance()] convention.  Standard information-criterion columns
#' (`logLik`, `AIC`, `BIC`, `nobs`) are included alongside FIMS-specific
#' diagnostics (`max_gradient`, `marginal_nll`, `total_nll`, `converged`,
#' `terminal_ssb`).
#'
#' @section Information criteria:
#' AIC and BIC are computed from the marginal log-likelihood (i.e. after
#' integrating out random effects), using the number of fixed-effect
#' parameters as \eqn{k}:
#' \deqn{AIC = 2k - 2 \ell_m}
#' \deqn{BIC = k \log(n) - 2 \ell_m}
#' where \eqn{\ell_m} is the marginal log-likelihood and \eqn{n} is the total
#' number of data observations.  These values will be `NA` when
#' `optimize = FALSE` was passed to [fit_fims()].
#'
#' @param x A `FIMSFit` object returned from [fit_fims()].
#' @param ... Unused; present for S3 method compatibility.
#'
#' @return A [tibble::tibble()] with one row and the following columns:
#' \describe{
#'   \item{`logLik`}{Marginal log-likelihood at the MLE
#'     (`-opt$objective`).}
#'   \item{`AIC`}{Akaike information criterion (marginal, fixed effects only).}
#'   \item{`BIC`}{Bayesian information criterion (marginal, fixed effects only).}
#'   \item{`nobs`}{Total number of data observations (rows with both an
#'     observed and an expected value in [get_estimates()]).}
#'   \item{`npar_fixed`}{Number of fixed-effect parameters.}
#'   \item{`npar_random`}{Number of random-effect parameters.}
#'   \item{`marginal_nll`}{Marginal negative log-likelihood
#'     (`opt$objective`); `NA` if the model was not optimised.}
#'   \item{`total_nll`}{Total (joint) negative log-likelihood from the TMB
#'     report (`report$jnll`).}
#'   \item{`max_gradient`}{Maximum absolute gradient at the MLE. Values
#'     below 0.001 are generally considered well-converged.}
#'   \item{`converged`}{Logical; `TRUE` when `opt$convergence == 0` **and**
#'     `max_gradient < 0.001`.}
#'   \item{`terminal_ssb`}{Spawning stock biomass at the terminal year,
#'     extracted from the TMB report. Returns a list-column when multiple
#'     populations are present.}
#'   \item{`fims_version`}{The version of FIMS used to fit the model.}
#'   \item{`runtime_secs`}{Total wall-clock time of the fit in seconds.}
#' }
#'
#' @examples
#' \dontrun{
#' data("data_big")
#' data_4_model <- FIMSFrame(data_big)
#'
#' fit <- create_default_parameters(
#'   configurations = create_default_configurations(data = data_4_model),
#'   data = data_4_model
#' ) |>
#'   initialize_fims(data = data_4_model) |>
#'   fit_fims(optimize = TRUE)
#'
#' glance(fit)
#'
#' # Compare sensitivity runs in one table
#' dplyr::bind_rows(
#'   glance(age_only_fit)    |> dplyr::mutate(model = "age_only"),
#'   glance(length_only_fit) |> dplyr::mutate(model = "length_only")
#' )
#' }
#'
#' @seealso `tidy.FIMSFit()`, [get_estimates()], [fit_fims()]
#' @export
glance.FIMSFit <- function(x, ...) {
  opt    <- get_opt(x)
  report <- get_report(x)
  npar   <- get_number_of_parameters(x)

  # parameter counts
  npar_fixed  <- as.integer(npar[["fixed_effects"]])
  npar_random <- as.integer(npar[["random_effects"]])

  # likelihood, opt is an empty list when optimize = FALSE
  optimised    <- length(opt) > 0
  marginal_nll <- if (optimised) opt[["objective"]]     else NA_real_
  log_lik      <- if (optimised) -marginal_nll           else NA_real_
  total_nll    <- if (!is.null(report[["jnll"]])) report[["jnll"]] else NA_real_

  # Count rows that have both observed and expected values.
  # warning: do NOT call get_estimates() here
  json_estimates <- reshape_json_estimates(get_model_output(x))
  nobs <- sum(!is.na(json_estimates[["observed"]]) & !is.na(json_estimates[["expected"]]))

  # information criteria
  aic <- if (optimised) 2 * npar_fixed - 2 * log_lik else NA_real_
  bic <- if (optimised && nobs > 0) {
    npar_fixed * log(nobs) - 2 * log_lik
  } else {
    NA_real_
  }

  # convergence
  max_grad  <- get_max_gradient(x)
  converged <- if (optimised) {
    isTRUE(opt[["convergence"]] == 0L) && isTRUE(max_grad < 0.001)
  } else {
    NA
  }

  # spawning_biomass is a list, one element per population; take the last
  # value from each and store as a list-column (supports multi-population).
  ssb_list <- report[["spawning_biomass"]]
  terminal_ssb <- if (!is.null(ssb_list) && length(ssb_list) > 0) {
    list(vapply(ssb_list, utils::tail, n = 1L, FUN.VALUE = numeric(1L)))
  } else {
    list(NA_real_)
  }

  # run time
  timing <- get_timing(x)
  runtime_secs <- as.numeric(timing[["time_total"]], units = "secs")

  tibble::tibble(
    logLik       = log_lik,
    AIC          = aic,
    BIC          = bic,
    nobs         = nobs,
    npar_fixed   = npar_fixed,
    npar_random  = npar_random,
    marginal_nll = marginal_nll,
    total_nll    = total_nll,
    max_gradient = max_grad,
    converged    = converged,
    terminal_ssb = terminal_ssb,
    fims_version = as.character(get_version(x)),
    runtime_secs = runtime_secs
  )
}

#' @importFrom broom tidy
#' @export
broom::tidy

#' @importFrom broom glance
#' @export
broom::glance

#' @importFrom broom augment
#' @export
broom::augment
