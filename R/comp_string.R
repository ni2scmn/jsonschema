js_string <- function(
    .min_length = NULL,
    .max_length = NULL,
    .pattern = NULL,

    # TODO `format` attribute
    # -> https://json-schema.org/understanding-json-schema/reference/string.html#built-in-formats

    # GENERIC ARGS
    .default = NULL,
    .required = FALSE) {

  stopifnot(
    "`.min_length` must be non negative integer or NULL" =
      rlang::is_scalar_integerish(.min_length) || is.null(.min_length),
    "`.max_length` must be non negative integer or NULL" =
      rlang::is_scalar_integerish(.max_length) || is.null(.max_length),
    "`.pattern` must be scalar string or NULL" =
      rlang::is_scalar_character(.pattern) || is.null(.pattern),
    "`.default` must be scalar string or NULL" =
      rlang::is_scalar_character(.default) || is.null(.default),
    "`.required` must be scalar logical" =
      rlang::is_scalar_logical(.required)
  )

  stopifnot(
    "`.max_length` must not be smaller than `.min_length`" =
      is.null(.min_length) || is.null(.max_length) || .min_length <= .max_length,
    "`.default` must be NULL when `.required` is TRUE" =
      is.null(.default) || !.required
  )

  structure(
    list(
      min_length = .min_length,
      max_length = .max_length,
      pattern = .pattern,
      default = .default,
      required = .required
    ),
    class = c("js_schema_string", "js_schema_component")
  )
}