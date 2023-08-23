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
    "`.multiple_of` must be scalar integerish or NULL" =
      rlang::is_scalar_integerish(.multiple_of) || rlang::is_empty(.multiple_of),
    "`.minimum` must be scalar integerish or NULL" =
      rlang::is_scalar_integerish(.minimum) || rlang::is_empty(.minimum),
    "`.maximum` must be scalar integerish or NULL" =
      rlang::is_scalar_integerish(.maximum) || rlang::is_empty(.maximum),
    "`.exclusive_minimum` must be scalar integerish or NULL" =
      rlang::is_scalar_integerish(.exclusive_minimum) || rlang::is_empty(.exclusive_minimum),
    "`.exclusive_maximum` must be scalar integerish or NULL" =
      rlang::is_scalar_integerish(.exclusive_maximum) || rlang::is_empty(.exclusive_maximum)
  )

  structure(
    list(
      multiple_of = as.integer(.multiple_of),
      minimum = as.integer(.minimum),
      maximum = as.integer(.maximum),
      exclusive_minimum = as.integer(.exclusive_minimum),
      exclusive_maximum = as.integer(.exclusive_maximum)
    ),
    class = c("js_schema_integer", "js_schema_component")
  ) %>%
    js_common_attributes(...)
}
