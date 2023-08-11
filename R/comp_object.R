# TODO unevaluatedProperties?
# TODO propertyNames?

# TODO dependentRequired
# TODO dependentSchemas

# check for both if it should be included in common
# TODO if-then-else
# TODO implication?

# TODO fix pattern properties
# https://json-schema.org/understanding-json-schema/reference/object.html#id4
js_object <- function(
  ...,
  .min_properties = NULL,
  .max_properties = NULL,
  .pattern_properties = NULL,
  .additional_properties = NULL,
  .dependencies = NULL,
  .property_names = NULL,
  .required = FALSE) {

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
      rlang::is_scalar_logical(.additional_properties) || is.null(.additional_properties),

    "`.required` must be scalar logical" =
      rlang::is_scalar_logical(.required)
  )

  structure(
    list(
      props = list(...),
      min_properties = .min_properties,
      max_properties = .max_properties,
      pattern_properties = .pattern_properties,
      additional_properties = .additional_properties,
      dependencies = .dependencies,
      property_names = .property_names,
      required = .required
    ),
    class = c("js_schema_object", "js_schema_component")
  )
}