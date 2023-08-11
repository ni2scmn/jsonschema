# TODO readOnly, writeOnly?
# TODO deprecated?
# TODO '$schema' version?

ps_common_attributes <- function(
  .element,

  # describing attributes
  .title = NULL,
  .description = NULL,
  .comment = NULL, # '$comment in schema'
  .examples = NULL,

  # validating attributes
  .enum = NULL, # TODO check type of default value?
  .const = NULL,
  .default = NULL # TODO check type of default value?
) {

  stopifnot(
    "`.element` must be instance of js_schema_component" = 
      inherits(.element, "js_schema_component"),
    "`.title` must be scalar string or NULL" =
      rlang::is_scalar_character(.title) || is.null(.title),
    "`.description` must be scalar string or NULL" =
      rlang::is_scalar_character(.description) || is.null(.description),
    "`.comment` must be scalar string or NULL" =
      rlang::is_scalar_character(.comment) || is.null(.comment),

    # TODO which requirements must satify .examples
    # "`.examples` must be scalar vector or NULL" =
    #   rlang::is_scalar_vector(.examples) || is.null(.examples),

    # TODO which type does enum have in case of object
    "`.enum` must be scalar vector or NULL" =
      inherits(.element, "js_schema_object") || rlang::is_scalar_vector(.enum) || is.null(.enum),

    "`.const` must be scalar vector or NULL" =
      rlang::is_scalar_vector(.const) || is.null(.const),


    "`.default` must be scalar string or NULL" =
      rlang::is_scalar_character(.default) || is.null(.default),
  )

  if(inherits(.element, "js_schema_object") && !is.null(.enum)) {
    rlang::warn("No validation for enums of object type implemented")
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
      default = .default
    )
  )

  # structure(
  #   list(
  #     enum = .enum,
  #     const = .const,
  #     title = .title,
  #     description = .description,
  #     default = .default,
  #     examples = .examples,
  #     comment = .comment
  #   ),
  #   class = c("plumber_req_common", "plumber_req_schema")
  # )
}