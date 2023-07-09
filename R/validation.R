validate <- function(schema, request) {
  UseMethod("validate")
}

validate.default <- function(schema, request) {
  rlang::abort("Unimplemented")
}

validate.js_schema <- function(schema, request) {
  all(purrr::map2_lgl(
    .x = schema, 
    .y = request, 
    .f = \(x, y) validate(x, y)
    ))
}

validate.js_schema_object <- function(schema, request) {
  print("validate.js_schema_object")
  required_attributes <- schema %>%
    purrr::keep( ~ inherits(.x, "js_schema_component") && .x$required)

  if(!all(required_attributes %in% names(request))) {
    rlang::abort("Missing mandatory!")
  }

  all(
    purrr::map2_lgl(
      .x = schema[names(request)],
      .y = request,
      .f = \(x, y) validate(x, y)
    )
  )
}

# TODO handle missing & default
validate.js_schema_integer <- function(schema, request) {
  print("validate.js_schema_integer")
  stopifnot(inherits(schema, "js_schema_integer"))

  # TODO make more robust
  request <- as.integer(jsonlite::fromJSON(request))

  check_for_bounds(schema, request)
  TRUE
}


# TODO handle missing & default
validate.js_schema_numeric <- function(schema, request) {
  print("validate.js_schema_object")
  stopifnot(inherits(schema, "js_schema_numeric"))

  # TODO make more robust
  request <- as.numeric(jsonlite::fromJSON(request))

  check_for_bounds(schema, request)
  TRUE
}

validate.js_schema_string <- function(schema, request) {
  print("validate.js_schema_object")

  stopifnot(inherits(schema, "js_schema_string"))

  # TODO make more robust
  request <- as.character(jsonlite::fromJSON(request))

  if(!is.null(schema$pattern) && !grepl(schema$pattern, request)) {
    rlang::abort("pattern mismatch")
  }
  if(!is.null(schema$min_length) && nchar(request) < schema$min_length) {
    rlang::abort("too short")
  }
  if(!is.null(schema$max_length) && nchar(request) > schema$max_length) {
    rlang::abort("too long")
  }

  TRUE
}


check_for_bounds <- function(schema, request) {
  if(!is.null(schema$multiple_of) && request %% schema$multiple_of != 0) {
    rlang::abort("not multiple of")
  }
  if(!is.null(schema$minimum) && request < schema$minimum) {
    rlang::abort("too low")
  }
  if(!is.null(schema$maximum) && request > schema$maximum) {
    rlang::abort("too high")
  }
  if(!is.null(schema$exclusive_minimum) && request <= schema$exclusive_minimum) {
    rlang::abort("too low")
  }
  if(!is.null(schema$exclusive_maximum) && request >= schema$exclusive_maximum) {
    rlang::abort("too high")
  }
}