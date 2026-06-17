#' Build a FIMS model from data and components
#'
#' @description
#' `fims_model()` starts a model from data. Add components with native R pipes
#' so each component updates and returns the model object.
#'
#' @param data A data frame or [FIMSFrame()] object.
#' @param model A model builder object returned from [fims_model()].
#' @return A model builder object that can be initialized with
#'   [initialize_fims()].
#' @export
#' @examples
#' \dontrun{
#' data("data_big", package = "FIMS")
#' data_4_model <- FIMSFrame(data_big)
#'
#' model <- fims_model(data_4_model) |>
#'   fims_growth() |>
#'   fims_recruitment() |>
#'   fims_maturity() |>
#'   fims_observations(
#'     fleet = "fleet1",
#'     selectivity = "DoubleLogistic"
#'   )
#' input <- initialize_fims(model)
#' }
fims_model <- function(data) {
  if (missing(data)) {
    cli::cli_abort("The {.var data} argument is required.")
  }

  if (!methods::is(data, "FIMSFrame")) {
    data <- FIMSFrame(data)
  }

  structure(
    list(
      data = data,
      parameters = empty_fims_parameters(),
      components = list()
    ),
    class = "FIMSModel"
  )
}

#' @rdname fims_model
#' @param parameters A parameter tibble, typically edited from
#'   `model$parameters` after model components have been added.
#' @export
fims_parameters <- function(model, parameters) {
  assert_fims_model(model)
  model$parameters <- validate_fims_parameters(parameters)
  model
}

#' @rdname fims_model
#' @param form A character string naming a structural form, such as
#'   `"BevertonHolt"`, `"EWAA"`, `"Logistic"`, or `"PellaTomlinson"`.
#' @param process_distribution A character string naming the process
#'   distribution for a dynamic component, or `NA_character_` for no process
#'   distribution.
#' @export
fims_recruitment <- function(
    model,
    form = c("BevertonHolt"),
    process_distribution = "Dnorm") {
  form <- rlang::arg_match(form)
  process_distribution <- validate_component_choice(
    process_distribution,
    c("Dnorm", NA_character_),
    "process_distribution"
  )
  append_fims_component(
    model,
    fims_component(
      "recruitment",
      form = form,
      process_distribution = process_distribution
    )
  )
}

#' @rdname fims_model
#' @export
fims_growth <- function(model, form = c("EWAA")) {
  form <- rlang::arg_match(form)
  append_fims_component(model, fims_component("growth", form = form))
}

#' @rdname fims_model
#' @export
fims_maturity <- function(model, form = c("Logistic")) {
  form <- rlang::arg_match(form)
  append_fims_component(model, fims_component("maturity", form = form))
}

#' @rdname fims_model
#' @export
fims_depletion <- function(
    model,
    form = c("PellaTomlinson"),
    process_distribution = "Dnorm") {
  form <- rlang::arg_match(form)
  process_distribution <- validate_component_choice(
    process_distribution,
    c("Dnorm", NA_character_),
    "process_distribution"
  )
  append_fims_component(
    model,
    fims_component(
      "depletion",
      form = form,
      process_distribution = process_distribution
    )
  )
}

#' @rdname fims_model
#' @param fleet A character string naming the fleet or survey to configure.
#'   When the data contain one fleet, it can be omitted.
#' @param selectivity A character string naming the selectivity form for
#'   `fleet`. If omitted, age-structured models use `"Logistic"` and
#'   surplus-production models do not add selectivity.
#' @param landings,index,age_comp,length_comp Character strings naming data
#'   likelihood distributions for this fleet's observation components.
#' @export
fims_observations <- function(
    model,
    fleet = NULL,
    selectivity = NULL,
    landings = "Dlnorm",
    index = "Dlnorm",
    age_comp = "Dmultinom",
    length_comp = "Dmultinom") {
  assert_fims_model(model)
  fleet <- resolve_observation_fleet(model, fleet)
  if (!is.null(selectivity)) {
    selectivity <- rlang::arg_match(
      selectivity,
      values = c("Logistic", "DoubleLogistic")
    )
  }
  landings <- validate_component_choice(
    landings, c("Dlnorm", "Dnorm"), "landings"
  )
  index <- validate_component_choice(index, c("Dlnorm", "Dnorm"), "index")
  age_comp <- validate_component_choice(
    age_comp, c("Dmultinom"), "age_comp"
  )
  length_comp <- validate_component_choice(
    length_comp, c("Dmultinom"), "length_comp"
  )
  component_values <- list(
    fleet = fleet,
    landings = landings,
    index = index,
    age_comp = age_comp,
    length_comp = length_comp
  )
  if (!is.null(selectivity)) {
    component_values$selectivity <- selectivity
  }
  append_fims_component(
    model,
    do.call(fims_component, c(list(name = "observations"), component_values))
  )
}

fims_component <- function(name, ...) {
  structure(
    c(list(name = name), list(...)),
    class = "FIMSModelComponent"
  )
}

validate_component_choice <- function(value, choices, arg) {
  if (length(value) != 1) {
    cli::cli_abort("{.arg {arg}} must be a single value.")
  }
  is_allowed_na <- any(is.na(choices))
  if (is.na(value)) {
    if (is_allowed_na) {
      return(value)
    }
    cli::cli_abort("{.arg {arg}} cannot be {.val NA}.")
  }

  valid_choices <- choices[!is.na(choices)]
  if (!value %in% valid_choices) {
    cli::cli_abort(c(
      "{.arg {arg}} must be one of {.val {valid_choices}}.",
      x = "Got {.val {value}}."
    ))
  }
  value
}

assert_fims_model <- function(model) {
  if (!inherits(model, "FIMSModel")) {
    cli::cli_abort(
      "The first argument must be a FIMS model created by {.fn fims_model}."
    )
  }
  invisible(model)
}

validate_fims_parameters <- function(parameters) {
  if (!tibble::is_tibble(parameters)) {
    cli::cli_abort("{.arg parameters} must be a parameter tibble.")
  }

  if (nrow(parameters) == 0) {
    required_empty_columns <- c("model_family", "module_name", "fleet_name", "data")
    missing_columns <- setdiff(required_empty_columns, names(parameters))
    if (length(missing_columns) > 0) {
      cli::cli_abort(c(
        "{.arg parameters} is missing required columns.",
        x = "Missing columns: {.var {missing_columns}}."
      ))
    }
    return(parameters)
  }

  if ("data" %in% names(parameters)) {
    top_level_columns <- c("model_family", "module_name", "fleet_name", "data")
    missing_columns <- setdiff(top_level_columns, names(parameters))
    if (length(missing_columns) > 0) {
      cli::cli_abort(c(
        "{.arg parameters} is missing required columns.",
        x = "Missing columns: {.var {missing_columns}}."
      ))
    }
    if (!all(vapply(parameters$data, tibble::is_tibble, logical(1)))) {
      cli::cli_abort("{.arg parameters$data} must contain tibbles.")
    }
    parameters_unnested <- tidyr::unnest(parameters, cols = data)
  } else {
    parameters_unnested <- parameters
  }

  required_columns <- c(
    "model_family", "module_name", "module_type", "fleet_name", "label",
    "value", "estimation_type", "distribution_type", "distribution"
  )
  missing_columns <- setdiff(required_columns, names(parameters_unnested))
  if (length(missing_columns) > 0) {
    cli::cli_abort(c(
      "{.arg parameters} is missing required columns.",
      x = "Missing columns: {.var {missing_columns}}."
    ))
  }

  if ("data" %in% names(parameters)) {
    return(parameters)
  }

  parameters |>
    tidyr::nest(.by = c(model_family, module_name, fleet_name))
}

append_fims_component <- function(model, component) {
  assert_fims_model(model)
  validate_component_stack(model, component)
  model$components <- Filter(
    \(existing) !identical(component_key(existing), component_key(component)),
    model$components
  )
  model$components <- c(model$components, list(component))
  refresh_fims_model_parameters(model)
}

validate_component_stack <- function(model, component) {
  is_age_component <- component$name %in% age_structured_component_names()
  is_surplus_component <- component$name %in% surplus_production_component_names()

  has_surplus_components <- model_has_any_component(
    model, surplus_production_component_names()
  )
  has_age_components <- model_has_any_component(
    model, age_structured_component_names()
  )

  if ((is_age_component && has_surplus_components) ||
      (is_surplus_component && has_age_components)) {
    cli::cli_abort(
      "Age-structured and surplus-production dynamics cannot both be active."
    )
  }
  invisible(model)
}

model_component_names <- function(model) {
  vapply(model$components, `[[`, character(1), "name")
}

model_has_component <- function(model, name) {
  name %in% model_component_names(model)
}

model_has_any_component <- function(model, names) {
  any(model_component_names(model) %in% names)
}

age_structured_component_names <- function() {
  c("growth", "recruitment", "maturity")
}

required_age_structured_component_names <- function() {
  c("growth", "recruitment", "maturity")
}

surplus_production_component_names <- function() {
  c("depletion")
}

fims_component_functions <- function(names) {
  paste0("fims_", names, "()")
}

component_key <- function(component) {
  if (identical(component$name, "observations")) {
    return(paste(component$name, component$fleet, sep = ":"))
  }
  component$name
}

model_components <- function(model, name) {
  if (is.null(model)) {
    return(list())
  }
  Filter(\(component) identical(component$name, name), model$components)
}

model_component <- function(model, name) {
  if (is.null(model)) {
    return(NULL)
  }
  matches <- model_components(model, name)
  if (length(matches) == 0) {
    return(NULL)
  }
  matches[[length(matches)]]
}

model_component_value <- function(model, name, field, default = NULL) {
  component <- model_component(model, name)
  if (is.null(component) || is.null(component[[field]])) {
    return(default)
  }
  component[[field]]
}

model_fleet_names <- function(model) {
  model$data |>
    get_data() |>
    dplyr::pull(name) |>
    stats::na.omit() |>
    unique() |>
    as.character()
}

resolve_observation_fleet <- function(model, fleet) {
  fleet_names <- model_fleet_names(model)
  if (length(fleet_names) == 0) {
    cli::cli_abort("No fleets were found in the model data.")
  }

  if (is.null(fleet)) {
    if (length(fleet_names) == 1) {
      return(fleet_names[[1]])
    }
    cli::cli_abort(c(
      "{.arg fleet} is required because the data contain multiple fleets.",
      i = "Available fleets are {.val {fleet_names}}."
    ))
  }

  validate_component_choice(fleet, fleet_names, "fleet")
}

model_family_from_components <- function(model) {
  assert_fims_model(model)
  has_age_structured <- model_has_any_component(
    model, age_structured_component_names()
  )
  has_surplus_production <- model_has_any_component(
    model, surplus_production_component_names()
  )

  if (has_age_structured && has_surplus_production) {
    cli::cli_abort(
      "Age-structured and surplus-production dynamics cannot both be active."
    )
  }

  if (has_age_structured) {
    return("catch_at_age")
  }

  if (has_surplus_production) {
    return("surplus_production")
  }

  cli::cli_abort(
    "Add population-dynamics components, such as {.fn fims_growth}, {.fn fims_recruitment}, and {.fn fims_maturity}, or add {.fn fims_depletion}."
  )
}

initialize_fims_model <- function(model) {
  if (!inherits(model, "FIMSModel")) {
    cli::cli_abort("The {.var model} argument must be a FIMSModel object.")
  }

  model_family <- model_family_from_components(model)

  model <- refresh_fims_model_parameters(model)

  if (!tibble::is_tibble(model$parameters) || nrow(model$parameters) == 0) {
    cli::cli_abort(
      "Add enough model components to create parameters before initializing."
    )
  }

  initialize_fims(
    parameters = model$parameters,
    data = model$data
  )
}

#' @export
print.FIMSModel <- function(x, ...) {
  component_labels <- vapply(x$components, fims_component_label, character(1))
  if (length(component_labels) == 0) {
    component_labels <- "<none>"
  }
  cat("FIMS model\n")
  cat("  components:", paste(component_labels, collapse = ", "), "\n")
  cat("  parameters:", if (nrow(x$parameters) == 0) "pending" else "set", "\n")
  invisible(x)
}

fims_component_label <- function(component) {
  options <- component[setdiff(names(component), "name")]
  if (length(options) == 0) {
    return(component$name)
  }
  option_text <- paste(
    names(options),
    vapply(options, \(x) ifelse(is.na(x), "NA", as.character(x)), character(1)),
    sep = "=",
    collapse = ", "
  )
  paste0(component$name, "(", option_text, ")")
}

refresh_fims_model_parameters <- function(model) {
  assert_fims_model(model)
  previous_parameters <- model$parameters
  parameters <- tryCatch(
    build_fims_model_parameters(model),
    error = function(error) {
      if (is_incomplete_fims_model_error(error)) {
        return(empty_fims_parameters())
      }
      stop(error)
    }
  )
  parameters <- preserve_fims_parameter_edits(
    parameters = parameters,
    previous_parameters = previous_parameters
  )
  model$parameters <- parameters
  model
}

preserve_fims_parameter_edits <- function(parameters, previous_parameters) {
  if (!tibble::is_tibble(parameters) ||
      !tibble::is_tibble(previous_parameters) ||
      nrow(parameters) == 0 ||
      nrow(previous_parameters) == 0) {
    return(parameters)
  }

  parameters_were_nested <- "data" %in% names(parameters)
  if (parameters_were_nested) {
    parameters_unnested <- tidyr::unnest(parameters, cols = data)
  } else {
    parameters_unnested <- parameters
  }

  if ("data" %in% names(previous_parameters)) {
    previous_unnested <- tidyr::unnest(previous_parameters, cols = data)
  } else {
    previous_unnested <- previous_parameters
  }

  editable_columns <- intersect(
    c("value", "estimation_type"),
    intersect(names(parameters_unnested), names(previous_unnested))
  )
  key_columns <- intersect(
    c(
      "model_family", "module_name", "module_type", "fleet_name", "label",
      "age", "length", "time", "distribution_type", "distribution"
    ),
    intersect(names(parameters_unnested), names(previous_unnested))
  )

  if (length(editable_columns) == 0 || length(key_columns) == 0) {
    return(parameters)
  }

  previous_updates <- previous_unnested |>
    dplyr::select(dplyr::all_of(c(key_columns, editable_columns))) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(key_columns)), .keep_all = TRUE)

  updated_parameters <- parameters_unnested |>
    dplyr::rows_update(previous_updates, by = key_columns, unmatched = "ignore")

  if (!parameters_were_nested) {
    return(updated_parameters)
  }

  updated_parameters |>
    tidyr::nest(.by = c(model_family, module_name, fleet_name))
}

build_fims_model_parameters <- function(model) {
  create_default_parameters(
    build_fims_model_parameter_plan(model),
    data = model$data
  )
}

build_fims_model_parameter_plan <- function(model) {
  assert_fims_model(model)
  if (length(model_components(model, "observations")) == 0) {
    cli::cli_abort(
      "Add observation components with {.fn fims_observations} before creating parameters."
    )
  }

  model_family <- model_family_from_components(model)

  unique_fleet_types <- model$data |>
    get_data() |>
    dplyr::distinct(name, type) |>
    dplyr::mutate(module_type = snake_to_pascal(type)) |>
    dplyr::mutate(module_type = dplyr::case_when(
      type == "weight_at_age" ~ NA_character_,
      type == "age_to_length_conversion" ~ NA_character_,
      TRUE ~ module_type
    )) |>
    dplyr::filter(!is.na(module_type)) |>
    dplyr::rename(fleet_name = name) |>
    dplyr::select(-type)

  observation_components <- model_components(model, "observations")
  data_config_template <- purrr::map_dfr(
    observation_components,
    fims_observation_data_plan
  )
  unique_fleet_types <- unique_fleet_types |>
    dplyr::semi_join(
      dplyr::distinct(data_config_template, fleet_name),
      by = "fleet_name"
    )

  fleet_data_config <- unique_fleet_types |>
    dplyr::left_join(
      data_config_template,
      by = c("fleet_name", "module_type")
    )

  if (identical(model_family, "surplus_production")) {
    fleet_data_config <- fleet_data_config |>
      dplyr::filter(module_type %in% c("Landings", "Index"))

    depletion_form <- model_component_value(
      model, "depletion", "form", "PellaTomlinson"
    )
    depletion_process_distribution <- model_component_value(
      model, "depletion", "process_distribution", "Dnorm"
    )
    depletion_config <- dplyr::tribble(
      ~module_name, ~module_type, ~distribution_type, ~distribution,
      "Depletion", depletion_form, "process", depletion_process_distribution
    )

    return(dplyr::bind_rows(
      fleet_data_config,
      depletion_config
    ) |>
      dplyr::mutate(model_family = model_family) |>
      dplyr::arrange(fleet_name, module_name) |>
      dplyr::select(
        model_family, module_name, module_type, fleet_name, dplyr::everything()
      ) |>
      tidyr::nest(.by = c(model_family, module_name, fleet_name)))
  }

  validate_fims_model_age_components(model)

  recruitment_form <- model_component_value(
    model, "recruitment", "form", "BevertonHolt"
  )
  recruitment_process_distribution <- model_component_value(
    model, "recruitment", "process_distribution", "Dnorm"
  )
  growth_form <- model_component_value(model, "growth", "form", "EWAA")
  maturity_form <- model_component_value(
    model, "maturity", "form", "Logistic"
  )

  selectivity_config <- fims_observation_selectivity_plan(
    unique_fleet_types = unique_fleet_types,
    observation_components = observation_components
  )

  other_config <- dplyr::tribble(
    ~module_name, ~module_type, ~distribution_type, ~distribution,
    "Recruitment", recruitment_form, "process",
    recruitment_process_distribution,
    "Growth", growth_form, NA_character_, NA_character_,
    "Maturity", maturity_form, NA_character_, NA_character_
  )

  dplyr::bind_rows(
    fleet_data_config,
    selectivity_config,
    other_config
  ) |>
    dplyr::mutate(model_family = model_family) |>
    dplyr::arrange(fleet_name, module_name) |>
    dplyr::select(
      model_family, module_name, module_type, fleet_name, dplyr::everything()
    ) |>
    tidyr::nest(.by = c(model_family, module_name, fleet_name))
}

is_incomplete_fims_model_error <- function(error) {
  message <- conditionMessage(error)
  patterns <- c(
    "Add observation components",
    "Add observation components with {.fn fims_observations} before creating parameters",
    "Age-structured models need growth, recruitment, and maturity",
    "Add population-dynamics components"
  )
  any(vapply(patterns, grepl, logical(1), x = message, fixed = TRUE))
}

empty_fims_parameters <- function() {
  tibble::tibble(
    model_family = character(),
    module_name = character(),
    fleet_name = character(),
    data = list()
  )
}

validate_fims_model_age_components <- function(model) {
  if (is.null(model)) {
    return(invisible(model))
  }

  missing_components <- setdiff(
    required_age_structured_component_names(),
    model_component_names(model)
  )

  if (length(missing_components) > 0) {
    missing_calls <- fims_component_functions(missing_components)
    cli::cli_abort(c(
      "Age-structured models need growth, recruitment, and maturity components.",
      i = "Add missing components with {.fn {missing_calls}}."
    ))
  }

  invisible(model)
}

fims_observation_data_plan <- function(component) {
  dplyr::tribble(
    ~fleet_name, ~module_name, ~module_type, ~distribution_type, ~distribution,
    component$fleet, "Data", "Landings", "Data", component$landings,
    component$fleet, "Data", "Index", "Data", component$index,
    component$fleet, "Data", "AgeComp", "Data", component$age_comp,
    component$fleet, "Data", "LengthComp", "Data", component$length_comp
  )
}

fims_observation_selectivity_plan <- function(
    unique_fleet_types,
    observation_components) {
  purrr::map_dfr(
    observation_components,
    \(component) {
      tibble::tibble(
        fleet_name = component$fleet,
        module_name = "Selectivity",
        module_type = observation_selectivity_form(component)
      )
    }
  ) |>
    dplyr::semi_join(
      dplyr::distinct(unique_fleet_types, fleet_name),
      by = "fleet_name"
    )
}

observation_selectivity_form <- function(component) {
  if (is.null(component$selectivity)) {
    return("Logistic")
  }
  component$selectivity
}

snake_to_pascal <- function(snake_strings) {
  purrr::map_chr(snake_strings, \(x) {
    parts <- strsplit(x, "_")[[1]]
    paste(
      toupper(substring(parts, 1, 1)),
      substring(parts, 2),
      sep = "",
      collapse = ""
    )
  })
}
