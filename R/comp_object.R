# TODO unevaluatedProperties?
# TODO propertyNames?

# TODO dependentRequired
# TODO dependentSchemas

# check for both if it should be included in common
# TODO if-then-else
# TODO implication?

# TODO fix pattern properties
# https://json-schema.org/understanding-json-schema/reference/object.html#id4

#' TODO object
#'
#' @param .min_properties minimum number of properties
#' @param .max_properties maximum number of properties
#' @param .pattern_properties TODO
#' @param .additional_properties boolean indicating if additional properties are allowed
#' @inheritDotParams js_common_attributes
#'
#' @export
js_object <- function(
    ...,
    .min_properties = NULL,
    .max_properties = NULL,
    .pattern_properties = NULL,
    .additional_properties = NULL) {
  # TODO check dependencies
  # TODO check property_names

  # TODO additional_items
  # TODO contains ??

  stopifnot(
    "`.min_properties` must be non negative integer or NULL" =
      rlang::is_scalar_integerish(.min_properties) || is.null(.min_properties),
    "`.max_properties` must be non negative integer or NULL" =
      rlang::is_scalar_integerish(.max_properties) || is.null(.max_properties),
    "`.pattern_properties` must be scalar string or NULL" =
      rlang::is_scalar_character(.pattern_properties) || is.null(.pattern_properties),
    "`.additional_properties` must be scalar boolean or NULL" =
      rlang::is_scalar_logical(.additional_properties) || is.null(.additional_properties)
  )

  dot_args <- rlang::list2(...)

  if (any(names(dot_args) == "")) {
    rlang::abort("Object schema does not support unnamed arguments")
  }

  given_common_keywords <- intersect(
    names(dot_args),
    rlang::fn_fmls_names(js_common_attributes)
  )

  given_props <- setdiff(
    names(dot_args),
    given_common_keywords
  )

  obj <- structure(
    list(
      props = dot_args[given_props],
      min_properties = .min_properties,
      max_properties = .max_properties,
      pattern_properties = .pattern_properties,
      additional_properties = .additional_properties
    ),
    class = c("js_schema_object", "js_schema_component")
  )

  do.call(
    what = js_common_attributes,
    args = utils::modifyList(
      dot_args[given_common_keywords],
      list(.element = obj)
    )
  )
}
