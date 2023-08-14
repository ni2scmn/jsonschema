# TODO check valide is integer
# TODO should be able to define both minimum and exclusiveMinimum
#' TODO integer
#'
#' @param .multiple_of force component to be a multiple of that number
#' @param .minimum inclusive minimum valid value
#' @param .maximum inclusive maximum valid value
#' @param .exclusive_minimum exclusive minimum valid value
#' @param .exclusive_maximum exclusive maximum valid value
#' @inheritDotParams js_common_attributes
#'
#' @export
js_integer <- function(
    .multiple_of = NULL,
    .minimum = NULL,
    .maximum = NULL,
    .exclusive_minimum = NULL,
    .exclusive_maximum = NULL,
    ...) {
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
      rlang::is_scalar_integerish(.exclusive_maximum) || is.null(.exclusive_maximum)
  )

  structure(
    list(
      multiple_of = .multiple_of,
      minimum = .minimum,
      maximum = .maximum,
      exclusive_minimum = .exclusive_minimum,
      exclusive_maximum = .exclusive_maximum
    ),
    class = c("js_schema_integer", "js_schema_component")
  ) %>%
    js_common_attributes(...)
}
