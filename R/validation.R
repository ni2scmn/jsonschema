validate_common <- function(schema, request) {
  # TODO function isRoot?
  if (inherits(schema, "js_schema")) {
    return(TRUE)
  }
  print("validate_common")
  if (!rlang::is_empty(schema$const)) {
    validate_const(schema, request)
  }

  if (!rlang::is_empty(schema$enum)) {
    validate_enum(schema, request)
  }

  if (schema$required && rlang::is_empty(request)) {
    rlang::abort("Missing mandatory!")
  }

  TRUE
}


validate <- function(schema, request) {
  validate_common(schema, request) &&
    UseMethod("validate", schema)
}

validate.default <- function(schema, request) {
  stop("validate not implemented for schema type: ", class(schema))
}

validate.js_schema <- function(schema, request) {
  validate(schema$root, request)
}

validate.js_schema_object <- function(schema, request) {
  print("validate.js_schema_object")


  # check for min/max properties
  if (!rlang::is_empty(schema$min_properties)) {
    if (length(request) < schema$min_properties) {
      rlang::abort("Too few properties!")
    }
  }

  if (!rlang::is_empty(schema$max_properties)) {
    if (length(request) > schema$max_properties) {
      rlang::abort("Too many properties!")
    }
  }

  # check for pattern properties
  if (!rlang::is_empty(schema$pattern_properties)) {
    if (!all(grepl(schema$pattern_properties, names(request)))) {
      rlang::abort("Pattern mismatch!")
    }
  }

  # check for additional properties
  if (!rlang::is_empty(schema$additional_properties)) {
    if (!schema$additional_properties) {
      if (!all(names(request) %in% names(schema$props))) {
        rlang::abort("Additional properties not allowed!")
      }
    }
  }

  # TODO check for dependencies
  all(
    purrr::imap_lgl(
      .x = schema$props,
      .f = \(prop, prop_name) validate(prop, request[[prop_name]])
    )
  )
}

validate.js_schema_array <- function(schema, request) {
  print("validate.js_schema_array")
  stopifnot(inherits(schema, "js_schema_array"))

  # TODO check for better solution
  # stopifnot(inherits(request, "list"))

  # check if length is in bounds
  if (!rlang::is_empty(schema$min_items)) {
    if (length(request) < schema$min_items) {
      rlang::abort("Too few items!")
    }
  }

  if (!rlang::is_empty(schema$max_items)) {
    if (length(request) > schema$max_items) {
      rlang::abort("Too many items!")
    }
  }

  # check unique items
  # TODO check if feasible
  item_hashes <- purrr::map(request, digest::digest)
  if (anyDuplicated(item_hashes)) {
    rlang::abort("Duplicate items!")
  }

  TRUE
}

validate.js_schema_boolean <- function(schema, request) {
  if (!rlang::is_scalar_logical(request)) {
    rlang::abort("boolean invalid")
  }
  TRUE
}

validate.js_schema_null <- function(schema, request) {
  if (!rlang::is_empty(request)) {
    rlang::abort("null invalid")
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

  if (!rlang::is_empty(schema$pattern) && !grepl(schema$pattern, request)) {
    rlang::abort("pattern mismatch")
  }
  if (!rlang::is_empty(schema$min_length) && nchar(request) < schema$min_length) {
    rlang::abort("too short")
  }
  if (!rlang::is_empty(schema$max_length) && nchar(request) > schema$max_length) {
    rlang::abort("too long")
  }

  TRUE
}


check_for_bounds <- function(schema, request) {
  if (!rlang::is_empty(schema$multiple_of) && request %% schema$multiple_of != 0) {
    rlang::abort("not multiple of")
  }
  if (!rlang::is_empty(schema$minimum) && request < schema$minimum) {
    rlang::abort("too low")
  }
  if (!rlang::is_empty(schema$maximum) && request > schema$maximum) {
    rlang::abort("too high")
  }
  if (!rlang::is_empty(schema$exclusive_minimum) && request <= schema$exclusive_minimum) {
    rlang::abort("too low")
  }
  if (!rlang::is_empty(schema$exclusive_maximum) && request >= schema$exclusive_maximum) {
    rlang::abort("too high")
  }
}
