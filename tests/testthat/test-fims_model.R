# test_fims_model ----
## Setup ----

data("data_big", package = "FIMS")
data <- FIMS::FIMSFrame(data_big)

## IO correctness ----
test_that("`fims_model()` composes model components", {
  model <- fims_model(data = data) |>
    fims_growth() |>
    fims_recruitment() |>
    fims_maturity() |>
    fims_observations(fleet = "fleet1")

  #' @description Test that `fims_model()` returns a builder object.
  expect_s3_class(model, "FIMSModel")
  #' @description Test that model components are appended in order.
  expect_equal(
    vapply(model$components, `[[`, character(1), "name"),
    c("growth", "recruitment", "maturity", "observations")
  )
  expect_equal(model$components[[4]]$fleet, "fleet1")

  #' @description Test that parameters are created from the completed component stack.
  expect_s3_class(model$parameters, "tbl_df")
  expect_gt(nrow(model$parameters), 0)
  #' @description Test that parameters can be added after components are chosen.
  model_without_parameters <- model |>
    fims_parameters(model$parameters)
  expect_identical(model_without_parameters$parameters, model$parameters)
})

test_that("`fims_model()` components configure parameter surfaces", {
  model <- fims_model(data = data) |>
    fims_growth() |>
    fims_recruitment(process_distribution = NA_character_) |>
    fims_maturity() |>
    fims_observations(
      fleet = "fleet1",
      selectivity = "DoubleLogistic",
      landings = "Dnorm"
    ) |>
    fims_observations(
      fleet = "survey1",
      selectivity = "Logistic",
      index = "Dnorm"
    )

  parameters <- model$parameters |>
    tidyr::unnest(cols = data)

  #' @description Test that selectivity form is read from model components.
  expect_true(any(
    parameters$module_name == "Selectivity" &
      parameters$module_type == "DoubleLogistic" &
      parameters$fleet_name == "fleet1"
  ))
  expect_true(any(
    parameters$module_name == "Selectivity" &
      parameters$module_type == "Logistic" &
      parameters$fleet_name == "survey1"
  ))
  #' @description Test that observation likelihoods are read from model components.
  expect_true(any(
    parameters$module_name == "Data" &
      parameters$module_type == "Index" &
      parameters$fleet_name == "survey1" &
      parameters$distribution == "Dnorm"
  ))
  expect_true(any(
    parameters$module_name == "Data" &
      parameters$module_type == "Landings" &
      parameters$fleet_name == "fleet1" &
      parameters$distribution == "Dnorm"
  ))
  #' @description Test that recruitment process distribution can be disabled.
  expect_true(any(
    parameters$module_name == "Recruitment" &
      is.na(parameters$distribution)
  ))
})

test_that("`fims_model()` builds surplus-production parameter surfaces", {
  model <- fims_model(data = data) |>
    fims_depletion() |>
    fims_observations(fleet = "fleet1", landings = "Dnorm")

  parameters <- model$parameters |>
    tidyr::unnest(cols = data)

  #' @description Test that observations can be added to surplus-production models.
  expect_equal(model_family_from_components(model), "surplus_production")
  expect_false("selectivity" %in% names(model$components[[2]]))
  #' @description Test that depletion parameters are created from `fims_depletion()`.
  expect_true(any(
    parameters$module_name == "Depletion" &
      parameters$module_type == "PellaTomlinson" &
      parameters$label == "log_growth_rate"
  ))
  expect_true(any(
    parameters$module_name == "Depletion" &
      parameters$label == "log_depletion" &
      parameters$estimation_type == "random_effects"
  ))
  #' @description Test that fleet data likelihoods are carried through.
  expect_true(any(
    parameters$module_name == "Data" &
      parameters$module_type == "Landings" &
      parameters$fleet_name == "fleet1" &
      parameters$distribution == "Dnorm"
  ))
  #' @description Test that age/length composition rows are not added for surplus production.
  expect_false(any(parameters$module_type %in% c("AgeComp", "LengthComp")))
  #' @description Test that surplus-production models initialize through Rcpp.
  initialized_model <- initialize_fims(model)
  expect_named(initialized_model, c("parameters", "model", "modules"))
  expect_s4_class(initialized_model$model, "Rcpp_SurplusProduction")
  expect_s4_class(
    initialized_model$modules$landings_distributions[[1]],
    "Rcpp_DnormDistribution"
  )
})

test_that("`fims_depletion()` can disable process variation", {
  model <- fims_model(data = data) |>
    fims_depletion(process_distribution = NA_character_) |>
    fims_observations(fleet = "fleet1")

  parameters <- model$parameters |>
    tidyr::unnest(cols = data)

  #' @description Test that depletion process distributions can be disabled.
  expect_true(any(
    parameters$module_name == "Depletion" &
      parameters$label == "log_depletion" &
      parameters$estimation_type == "constant"
  ))
  expect_false(any(
    parameters$module_name == "Depletion" &
      parameters$label == "log_sd"
  ))
})

test_that("`fims_observations()` infers the fleet for single-fleet data", {
  single_fleet_data <- data_big |>
    dplyr::filter(is.na(name) | name == "fleet1") |>
    FIMS::FIMSFrame()

  model <- fims_model(data = single_fleet_data) |>
    fims_growth() |>
    fims_recruitment() |>
    fims_maturity() |>
    fims_observations()

  #' @description Test that `fleet` can be omitted when the data have one fleet.
  expect_equal(model$components[[4]]$fleet, "fleet1")
  expect_gt(nrow(model$parameters), 0)
  parameters <- model$parameters |>
    tidyr::unnest(cols = data)
  #' @description Test that omitted selectivity defaults to logistic for age-structured models.
  expect_true(any(
    parameters$module_name == "Selectivity" &
      parameters$module_type == "Logistic" &
      parameters$fleet_name == "fleet1"
  ))
})

test_that("`fims_parameters()` stores edited parameters", {
  model <- fims_model(data = data) |>
    fims_growth() |>
    fims_recruitment() |>
    fims_maturity() |>
    fims_observations(fleet = "fleet1")

  parameters <- model$parameters |>
    tidyr::unnest(cols = data) |>
    dplyr::rows_update(
      tibble::tibble(
        module_name = "Selectivity",
        fleet_name = "fleet1",
        label = "inflection_point",
        value = 1.5
      ),
      by = c("module_name", "fleet_name", "label")
    ) |>
    tidyr::nest(.by = c(model_family, module_name, fleet_name))

  configured_model <- model |>
    fims_parameters(parameters)

  #' @description Test that edited parameters are stored on the model.
  expect_true(any(
    tidyr::unnest(configured_model$parameters, cols = data)$label ==
      "inflection_point" &
      tidyr::unnest(configured_model$parameters, cols = data)$value == 1.5
  ))

  configured_model <- model |>
    fims_parameters(tidyr::unnest(parameters, cols = data))

  #' @description Test that unnested parameter tibbles are accepted and normalized.
  expect_true("data" %in% names(configured_model$parameters))
  expect_true(any(
    tidyr::unnest(configured_model$parameters, cols = data)$label ==
      "inflection_point" &
      tidyr::unnest(configured_model$parameters, cols = data)$value == 1.5
  ))
})

test_that("`fims_parameters()` validates parameter inputs", {
  model <- fims_model(data = data) |>
    fims_growth() |>
    fims_recruitment() |>
    fims_maturity() |>
    fims_observations(fleet = "fleet1")

  #' @description Test that parameters must be supplied as a tibble.
  expect_error(
    model |>
      fims_parameters(list()),
    "parameter tibble"
  )

  #' @description Test that malformed parameter tibbles report missing columns.
  expect_error(
    model |>
      fims_parameters(tibble::tibble(module_name = "Selectivity")),
    "missing required columns"
  )
})

test_that("`fims_model()` refreshes stale parameters", {
  model <- fims_model(data = data) |>
    fims_growth() |>
    fims_recruitment() |>
    fims_maturity() |>
    fims_observations(fleet = "fleet1")

  configured_model <- model |>
    fims_observations(fleet = "fleet1", selectivity = "DoubleLogistic")

  parameters <- configured_model$parameters |>
    tidyr::unnest(cols = data)

  #' @description Test that structural changes refresh stale parameters.
  expect_true(any(
    parameters$module_name == "Selectivity" &
      parameters$module_type == "DoubleLogistic" &
      parameters$fleet_name == "fleet1"
  ))
})

test_that("`fims_model()` preserves edits when adding components", {
  model <- fims_model(data = data) |>
    fims_growth() |>
    fims_recruitment() |>
    fims_maturity() |>
    fims_observations(fleet = "fleet1")

  edited_parameters <- model$parameters |>
    tidyr::unnest(cols = data) |>
    dplyr::rows_update(
      tibble::tibble(
        module_name = "Selectivity",
        module_type = "Logistic",
        fleet_name = "fleet1",
        label = "inflection_point",
        value = 1.5
      ),
      by = c("module_name", "module_type", "fleet_name", "label")
    ) |>
    tidyr::nest(.by = c(model_family, module_name, fleet_name))

  updated_model <- model |>
    fims_parameters(edited_parameters) |>
    fims_observations(fleet = "survey1")

  parameters <- updated_model$parameters |>
    tidyr::unnest(cols = data)

  #' @description Test that user-edited values survive later component additions.
  expect_equal(
    parameters |>
      dplyr::filter(
        module_name == "Selectivity",
        fleet_name == "fleet1",
        label == "inflection_point"
      ) |>
      dplyr::pull(value),
    1.5
  )
  #' @description Test that newly added observation components still add parameters.
  expect_true(any(parameters$fleet_name == "survey1"))
})

test_that("`fims_model()` prints component options", {
  model <- fims_model(data = data) |>
    fims_growth() |>
    fims_recruitment() |>
    fims_maturity() |>
    fims_observations(
      fleet = "fleet1",
      selectivity = "DoubleLogistic",
      landings = "Dnorm"
    )

  printed <- utils::capture.output(print(model))

  #' @description Test that print includes component options.
  expect_true(any(grepl("observations\\(fleet=fleet1", printed)))
  expect_true(any(grepl("selectivity=DoubleLogistic", printed)))
  #' @description Test that print includes parameter state.
  expect_true(any(grepl("parameters: set", printed)))
})

## Error handling ----
test_that("`initialize_fims()` validates composed model dynamics", {
  model <- fims_model(data = data)

  #' @description Test that `initialize_fims()` requires a dynamics component.
  expect_error(
    initialize_fims(model),
    "Add population-dynamics components"
  )

  #' @description Test that incomplete model stacks keep pending parameters.
  incomplete_model <- model |>
    fims_growth() |>
    fims_recruitment() |>
    fims_maturity()
  expect_equal(nrow(incomplete_model$parameters), 0)

  #' @description Test that inferred age-structured dynamics need the core biological components.
  incomplete_model <- model |>
    fims_growth() |>
    fims_recruitment() |>
    fims_observations(fleet = "fleet1")
  expect_equal(nrow(incomplete_model$parameters), 0)

  #' @description Test that `initialize_fims()` rejects incompatible dynamics.
  expect_error(
    model |>
      fims_growth() |>
      fims_depletion(),
    "cannot both be active"
  )

  #' @description Test that observation distributions are validated.
  expect_error(
    model |>
      fims_observations(fleet = "fleet1", index = "Dgamma"),
    "index"
  )

  #' @description Test that multi-fleet data require an explicit fleet.
  expect_error(
    model |>
      fims_observations(),
    "fleet"
  )

  #' @description Test that observation fleets must exist in the data.
  expect_error(
    model |>
      fims_observations(fleet = "missing_fleet"),
    "fleet"
  )

  #' @description Test that recruitment process distribution is validated.
  expect_error(
    model |>
      fims_recruitment(process_distribution = "Dlnorm"),
    "process_distribution"
  )

  #' @description Test that depletion process distribution is validated.
  expect_error(
    model |>
      fims_depletion(process_distribution = "Dlnorm"),
    "process_distribution"
  )
})
