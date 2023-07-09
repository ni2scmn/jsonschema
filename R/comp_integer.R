js_integer <- function(
    .multiple_of = NULL,
    .minimum = NULL,
    .maximum = NULL,
    .exclusive_minimum = NULL,
    .exclusive_maximum = NULL,

    # GENERIC ARGS
    .default = NULL,
    .required = FALSE) {

  stopifnot(
    "`.multiple_of` must be scalar numeric or NULL" =
      rlang::is_scalar_integerish(.multiple_of) || is.null(.multiple_of),
    "`.minimum` must be scalar numeric or NULL" =
      rlang::is_scalar_integerish(.minimum) || is.null(.minimum),
    "`.maximum` must be scalar numeric or NULL" =
      rlang::is_scalar_integerish(.maximum) || is.null(.maximum),
    "`.exclusive_minimum` must be scalar numeric or NULL" =
      rlang::is_scalar_integerish(.exclusive_minimum) || is.null(.exclusive_minimum),
    "`.exclusive_maximum` must be scalar numeric or NULL" =
      rlang::is_scalar_integerish(.exclusive_maximum) || is.null(.exclusive_maximum),
    "`.default` must be scalar string or NULL" =
      rlang::is_scalar_character(.default) || is.null(.default),
    "`.required` must be scalar logical" =
      rlang::is_scalar_logical(.required)
  )

  structure(
    list(
      multiple_of = .multiple_of,
      minimum = .minimum,
      maximum = .maximum,
      exclusive_minimum = .exclusive_minimum,
      exclusive_maximum = .exclusive_maximum,

      default = .default,
      required = .required
    ),
    class = c("js_schema_integer", "js_schema_component")
  )
}