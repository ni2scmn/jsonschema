validate <- function(schema, request) {
  UseMethod("validate")
}

validate.default <- function(schema, request) {
  rlang::abort("Unimplemented")
}

validate.js_schema <- function(schema, request) {
  validate(schema$root, request)
}

validate.js_schema_object <- function(schema, request) {
  print("validate.js_schema_object")

  # check for required attributes
  required_attributes <- schema$props %>%
    purrr::keep( 
      ~ inherits(.x, "js_schema_component") &&
      .x$required
      )

  if(!all(names(required_attributes) %in% names(request))) {
    rlang::abort("Missing mandatory!")
  }

  # check for min/max properties
  if(!is.null(schema$min_properties)) {
    if(length(request) < schema$min_properties) {
      rlang::abort("Too few properties!")
    }
  }

  if(!is.null(schema$max_properties)) {
    if(length(request) > schema$max_properties) {
      rlang::abort("Too many properties!")
    }
  }

  # check for pattern properties
  if(!is.null(schema$pattern_properties)) {
    if(!all(grepl(schema$pattern_properties, names(request)))) {
      rlang::abort("Pattern mismatch!")
    }
  }

  # check for additional properties
  if(!is.null(schema$additional_properties)) {
    if(!schema$additional_properties) {
      if(!all(names(request) %in% names(schema$props))) {
        rlang::abort("Additional properties not allowed!")
      }
    }
  }

  # TODO check for dependencies
  # TODO check for required itself
  all(
    purrr::map2_lgl(
      .x = schema$props[names(request)],
      .y = request,
      .f = \(x, y) validate(x, y)
    )
  )
}

validate.js_schema_array <- function(schema, request) {
  print("validate.js_schema_array")
  stopifnot(inherits(schema, "js_schema_array"))

  # TODO check for better solution
  # stopifnot(inherits(request, "list"))

  # check if length is in bounds
  if(!is.null(schema$min_items)) {
    if(length(request) < schema$min_items) {
      rlang::abort("Too few items!")
    }
  }

  if(!is.null(schema$max_items)) {
    if(length(request) > schema$max_items) {
      rlang::abort("Too many items!")
    }
  }

  # check unique items
  # TODO check if feasible
  item_hashes <- purrr::map(request, digest::digest)
  if(anyDuplicated(item_hashes)) {
    rlang::abort("Duplicate items!")
  }

  TRUE
}

# TODO handle missing & default
validate.js_schema_integer <- function(schema, request) {
  print("validate.js_schema_integer")
  stopifnot(inherits(schema, "js_schema_integer"))

  # TODO make more robust
  request <- as.integer(request)

  check_for_bounds(schema, request)
  TRUE
}


# TODO handle missing & default
validate.js_schema_numeric <- function(schema, request) {
  print("validate.js_schema_numeric")
  stopifnot(inherits(schema, "js_schema_numeric"))

  # TODO make more robust
  request <- as.numeric(request)

  check_for_bounds(schema, request)
  TRUE
}

validate.js_schema_string <- function(schema, request) {
  print("validate.js_schema_string")

  stopifnot(inherits(schema, "js_schema_string"))

  # TODO make more robust
  request <- as.character(request)

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