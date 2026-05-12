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

# If broom is installed the generic already exists; if not we define a minimal
# so augment() works without broom being attached.
if (!isGeneric("augment") && !existsMethod("augment", "FIMSFit")) {
  augment <- function(x, ...) UseMethod("augment")
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
