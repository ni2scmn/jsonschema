#' @export
to_json_schema <- function(schema_obj, output_format = c("yaml", "json")) {
  output_format <- rlang::arg_match(output_format)

  if (output_format == "yaml") {
    yaml::as.yaml(
      to_json_schema_int(schema_obj)
    )
  } else if (output_format == "json") {
    jsonlite::toJSON(
      to_json_schema_int(schema_obj),
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    )
  } else {
    rlang::abort("Unknown output format")
  }
}

#' @export
to_json_schema_int <- function(schema_obj) {
  UseMethod("to_json_schema_int", schema_obj)
}

#' @export
to_json_schema_int.default <- function(schema_obj) {
  rlang::abort(
    paste0("function `to_json_schema` not implemented for class `", class(schema_obj), "`")
  )
}

#' @export
to_json_schema_int.js_schema <- function(schema_obj) {
  to_json_schema_int(schema_obj$root)
}

#' @export
to_json_schema_int.js_schema_string <- function(schema_obj) {
  utils::modifyList(
    translate_common_comp(schema_obj),
    list(
      type = "string",
      minLength = schema_obj$min_length,
      maxLength = schema_obj$max_length,
      pattern = schema_obj$pattern,
      format = schema_obj$format
    )
  )
}

#' @export
to_json_schema_int.js_schema_boolean <- function(schema_obj) {
  utils::modifyList(
    translate_common_comp(schema_obj),
    list(
      type = "boolean"
    )
  )
}

#' @export
to_json_schema_int.js_schema_null <- function(schema_obj) {
  # TODO type: ~ is correct for yaml
  c(
    list(
      type = "null"
    ),
    translate_common_comp(schema_obj)
  )
}

#' @export
to_json_schema_int.js_schema_integer <- function(schema_obj) {
  utils::modifyList(
    translate_common_comp(schema_obj),
    list(
      type = "integer",
      minimum = schema_obj$minimum,
      maximum = schema_obj$maximum,
      exclusiveMinimum = schema_obj$exclusive_minimum,
      exclusiveMaximum = schema_obj$exclusive_maximum,
      multipleOf = schema_obj$multiple_of
    )
  )
}

#' @export
to_json_schema_int.js_schema_numeric <- function(schema_obj) {
  utils::modifyList(
    translate_common_comp(schema_obj),
    list(
      type = "integer",
      minimum = schema_obj$minimum,
      maximum = schema_obj$maximum,
      exclusiveMinimum = schema_obj$exclusive_minimum,
      exclusiveMaximum = schema_obj$exclusive_maximum,
      multipleOf = schema_obj$multiple_of
    )
  )
}

#' @export
to_json_schema_int.js_schema_array <- function(schema_obj) {
  utils::modifyList(
    translate_common_comp(schema_obj),
    list(
      type = "array",
      items = to_json_schema_int(schema_obj$items),
      minItems = schema_obj$min_items,
      maxItems = schema_obj$max_items,
      uniqueItems = schema_obj$unique_items
    )
  )
}

#' @export
to_json_schema_int.js_schema_object <- function(schema_obj) {
  required_childs <- purrr::keep(
    schema_obj$props,
    function(x) x$required
  ) %>%
    names() %>%
    as.list()

  if (length(required_childs) == 0) {
    required_childs <- NULL
  }

  utils::modifyList(
    translate_common_comp(schema_obj),
    list(
      type = "object",
      properties = purrr::map(
        schema_obj$props,
        to_json_schema_int
      ),
      minProperties = schema_obj$min_properties,
      maxProperties = schema_obj$max_properties,
      patternProperties = schema_obj$pattern_properties,
      additional_properties = schema_obj$additional_properties,
      required = required_childs
    )
  )
}

from_json_schema <- function(schema) {
  rlang::abort("UNIMPLEMENTED")
}


translate_common_comp <- function(element) {
  # TODO remove in future?
  stopifnot(
    inherits(element, "js_schema_component")
  )

  # TODO check handling of enum, const, default, examples
  # title, description, comment should work
  # remove required on component level and collect it on object level

  list(
    title = element$title,
    description = element$description,
    "$comment" = element$comment,
    examples = element$examples,
    enum = element$enum,
    const = element$const,
    default = element$default
  ) %>%
    purrr::keep(~ !rlang::is_empty(.x))
}
