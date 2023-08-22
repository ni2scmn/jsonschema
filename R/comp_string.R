#' TODO string
#'
#' @param .min_length non negative integer describing minimum length of the string
#' @param .max_length non negative integer describing maximum length of the string
#' @param .pattern scalar string describing regular expression pattern that the string must match
#' @param .format character vector of strings describing format of the string
#' @inheritDotParams js_common_attributes
#'
#' @export
js_string <- function(
    .min_length = NULL,
    .max_length = NULL,
    .pattern = NULL,
    # TODO maybe add validations to some of them?
    # https://json-schema.org/understanding-json-schema/reference/string.html#built-in-formats
    .format = c(
      "date-time", "date", "time", "duration",
      "email", "idn-email", "hostname", "idn-hostname",
      "ipv4", "ipv6", "uuid", "uri", "uri-reference",
      "iri", "iri-reference", "uri-template", "json-pointer",
      "relative-json-pointer", "regex"
    ),
    ...) {
  stopifnot(
    "`.min_length` must be non negative integerish or NULL" =
      rlang::is_scalar_integerish(.min_length) || rlang::is_empty(.min_length),
    "`.max_length` must be non negative integerish or NULL" =
      rlang::is_scalar_integerish(.max_length) || rlang::is_empty(.max_length),
    "`.pattern` must be scalar string or NULL" =
      rlang::is_scalar_character(.pattern) || rlang::is_empty(.pattern)
  )

  stopifnot(
    "`.max_length` must not be smaller than `.min_length`" =
      rlang::is_empty(.min_length) || rlang::is_empty(.max_length) || .min_length <= .max_length
  )

  structure(
    list(
      min_length = as.integer(.min_length),
      max_length = as.integer(.max_length),
      pattern = .pattern
    ),
    class = c("js_schema_string", "js_schema_component")
  ) %>%
    js_common_attributes(...)
}
