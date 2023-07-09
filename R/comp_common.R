ps_common_attributes <- function(
  entity,
  .enum = NULL,
  .const = NULL,
  .title = NULL,
  .description = NULL,
  .default = NULL,
  .examples = NULL,
  .comment = NULL
) {
  
  stopifnot(
    "`.enum` must be scalar vector or NULL" =
      rlang::is_scalar_vector(.enum) || is.null(.enum),
    "`.const` must be scalar vector or NULL" =
      rlang::is_scalar_vector(.const) || is.null(.const),
    "`.title` must be scalar string or NULL" =
      rlang::is_scalar_character(.title) || is.null(.title),
    "`.description` must be scalar string or NULL" =
      rlang::is_scalar_character(.description) || is.null(.description),
    "`.default` must be scalar string or NULL" =
      rlang::is_scalar_character(.default) || is.null(.default),
    "`.examples` must be scalar vector or NULL" =
      rlang::is_scalar_vector(.examples) || is.null(.examples),
    "`.comment` must be scalar string or NULL" =
      rlang::is_scalar_character(.comment) || is.null(.comment)
  )

  structure(
    list(
      enum = .enum,
      const = .const,
      title = .title,
      description = .description,
      default = .default,
      examples = .examples,
      comment = .comment
    ),
    class = c("plumber_req_common", "plumber_req_schema")
  )
}