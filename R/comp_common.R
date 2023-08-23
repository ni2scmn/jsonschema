# TODO readOnly, writeOnly?
# TODO deprecated?
# TODO '$schema' version?
# TODO mandatory on element level?

#' Utility function for common schema keywords
#'
#' @param .element js_schema_component
#' @param .title title for schema component
#' @param .description longer description for schema component
#' @param .comment '$comment' tag for internal comments of schema component
#' @param .examples TODO doc
#' @param .enum restrict element by list of valid elements
#' @param .const restrict element by constant value that must be met
#' @param .default default value for component if not explicitly set
#' @param .required logical indicating if element is required
#'
#' @return modified .element
#'
js_common_attributes <- function(
    .element,
    # describing attributes
    .title = NULL,
    .description = NULL,
    .comment = NULL, # '$comment in schema'
    .examples = NULL,
    # validating attributes
    .enum = NULL, # TODO check type of default value?
    .const = NULL,
    .default = NULL, # TODO check type of default value?
    .required = FALSE) {
  stopifnot(
    "`.element` must be instance of js_schema_component" =
      inherits(.element, "js_schema_component"),
    "`.title` must be scalar string or NULL" =
      rlang::is_scalar_character(.title) || rlang::is_empty(.title),
    "`.description` must be scalar string or NULL" =
      rlang::is_scalar_character(.description) || rlang::is_empty(.description),
    "`.comment` must be scalar string or NULL" =
      rlang::is_scalar_character(.comment) || rlang::is_empty(.comment)

    # TODO which requirements must satify .examples
    # "`.examples` must be scalar vector or NULL" =
    #   rlang::is_scalar_vector(.examples) || rlang::is_empty(.examples),

    # TODO which type does enum have in case of object
    # "`.enum` must be vector or NULL" =
    #   inherits(.element, "js_schema_object") ||
    #     rlang::is_vector(.enum) ||
    #     rlang::is_empty(.enum),

    # TODO check correct type check
    # "`.const` must be scalar vector or NULL" =
    #   rlang::is_scalar_vector(.const) || rlang::is_empty(.const),
    # TODO check correct type check
    # "`.default` must be scalar string or NULL" =
    #   rlang::is_scalar_character(.default) || rlang::is_empty(.default)
  )

  if (inherits(.element, "js_schema_object") && !rlang::is_empty(.enum)) {
    rlang::warn("No validation for enums of object type implemented")
  }

  if (inherits(.element, "js_schema_integer")) {
    if (!rlang::is_empty(.const)) {
      .const <- as.integer(.const)
    }
    if (!rlang::is_empty(.default)) {
      .default <- as.integer(.default)
    }
    if (!rlang::is_empty(.enum)) {
      .enum <- as.integer(.enum)
    }
  }

  utils::modifyList(
    .element,
    list(
      title = .title,
      description = .description,
      comment = .comment,
      examples = .examples,
      enum = .enum,
      const = .const,
      default = .default,
      required = .required
    )
  )
}
