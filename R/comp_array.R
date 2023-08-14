# TODO prefixItems?
# TODO items can be set to false indicating only prefixItems can be included
# TODO unevaluatedItems?

#' TODO array
#'
#' @param .items schema component describing array items
#' @param .min_items minimum item count
#' @param .max_items maximum item count
#' @param .unique_items boolean indicating if items must be unique
#' @inheritDotParams js_common_attributes
#'
#' @export
js_array <- function(
    .items,
    .min_items = NULL,
    .max_items = NULL,
    .unique_items = NULL,
    ...
    # TODO additional_items
    # TODO contains, minContains, maxContains
    # .contains = NULL,
    # .min_contains = NULL,
    # .max_contains = NULL
    ) {
  # TODO check items type? -> js_schema_component

  stopifnot(
    "`.min_items` must be non negative integer or NULL" =
      rlang::is_scalar_integerish(.min_items) || is.null(.min_items),
    "`.max_items` must be non negative integer or NULL" =
      rlang::is_scalar_integerish(.max_items) || is.null(.max_items),
    "`.unique_items` must be scalar boolean or NULL" =
      rlang::is_scalar_logical(.unique_items) || is.null(.unique_items)
  )

  structure(
    list(
      items = .items,
      min_items = .min_items,
      max_items = .max_items,
      unique_items = .unique_items
    ),
    class = c("js_schema_array", "js_schema_component")
  ) %>%
    js_common_attributes(...)
}
