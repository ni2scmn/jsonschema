common_attrs_check <- function(
    .element,
    .examples = NULL,
    .enum = NULL,
    .const = NULL,
    .default = NULL) {
  UseMethod("common_attrs_check", .element)
}

common_attrs_check.default <- function(
    .element,
    .examples = NULL,
    .enum = NULL,
    .const = NULL,
    .default = NULL) {
  stop("common_attrs_check not implemented for schema type: ", class(.element))
}

common_attrs_check.js_schema_integer <- function(
    .element,
    .examples = NULL,
    .enum = NULL,
    .const = NULL,
    .default = NULL) {
  stopifnot(
    # TODO refine ``.examples`` check
    "`.examples` must be a vector or NULL" =
      rlang::is_vector(.examples) || is.null(.examples),
    "`.enum` must be integerish vector or NULL" =
      rlang::is_integerish(.enum) || is.null(.enum),
    "`.const` must be scalar integerish vector or NULL" =
      rlang::is_scalar_integerish(.const) || is.null(.const),
    "`.default` must be scalar integerish vector or NULL" =
      rlang::is_scalar_integerish(.default) || is.null(.default)
  )

  list(
    examples = .examples,
    enum = .enum,
    const = .const,
    default = .default
  )
}

common_attrs_check.js_schema_numeric <- function(
    .element,
    .examples = NULL,
    .enum = NULL,
    .const = NULL,
    .default = NULL) {
  stopifnot(
    # TODO refine ``.examples`` check
    "`.examples` must be a vector or NULL" =
      rlang::is_vector(.examples) || is.null(.examples),
    "`.enum` must be numericish vector or NULL" =
      is.numeric(.enum) || is.null(.enum),
    "`.const` must be scalar numericish vector or NULL" =
      is.numeric(.enum) && length(.enum) == 1 || is.null(.const),
    "`.default` must be scalar numericish vector or NULL" =
      is.numeric(.default) && length(.default) == 1 || is.null(.default)
  )

  list(
    examples = .examples,
    enum = .enum,
    const = .const,
    default = .default
  )
}

common_attrs_check.js_schema_boolean <- function(
    .element,
    .examples = NULL,
    .enum = NULL,
    .const = NULL,
    .default = NULL) {
  stopifnot(
    # TODO refine ``.examples`` check
    "`.examples` must be a vector or NULL" =
      rlang::is_vector(.examples) || is.null(.examples),
    "`.enum` cant be set for boolean components" =
      is.null(.enum),
    "`.const` must be scalar logical vector or NULL" =
      rlang::is_scalar_logical(.const) || is.null(.const),
    "`.default` must be scalar logical vector or NULL" =
      rlang::is_scalar_logical(.default) || is.null(.default)
  )

  list(
    examples = .examples,
    enum = .enum,
    const = .const,
    default = .default
  )
}

common_attrs_check.js_schema_string <- function(
    .element,
    .examples = NULL,
    .enum = NULL,
    .const = NULL,
    .default = NULL) {
  stopifnot(
    # TODO refine ``.examples`` check
    "`.examples` must be a vector or NULL" =
      rlang::is_vector(.examples) || is.null(.examples),
    "`.enum` must be character vector or NULL" =
      rlang::is_character(.enum) || is.null(.enum),
    "`.const` must be scalar character vector or NULL" =
      rlang::is_scalar_character(.const) || is.null(.const),
    "`.default` must be scalar character vector or NULL" =
      rlang::is_scalar_character(.default) || is.null(.default)
  )

  list(
    examples = .examples,
    enum = .enum,
    const = .const,
    default = .default
  )
}

common_attrs_check.js_schema_null <- function(
    .element,
    .examples = NULL,
    .enum = NULL,
    .const = NULL,
    .default = NULL) {
  stopifnot(
    # TODO refine ``.examples`` check
    "`.examples` cant be set for null components" =
      is.null(.examples),
    "`.enum` cant be set for null components" =
      is.null(.enum),
    "`.const` cant be set for null components" =
      is.null(.const),
    "`.default` cant be set for null components" =
      is.null(.default)
  )

  list(
    examples = .examples,
    enum = .enum,
    const = .const,
    default = .default
  )
}

common_attrs_check.js_schema_array <- function(
    .element,
    .examples = NULL,
    .enum = NULL,
    .const = NULL,
    .default = NULL) {
  stopifnot(
    # TODO refine ``.examples`` check
    # "`.examples` must be a vector or NULL" =
    #   rlang::is_vector(.examples) || is.null(.examples),
    # "`.enum` cant be set for array components" =
    #   is.null(.enum),
    # "`.const` cant be set for array components" =
    #   is.null(.const),
    # TODO correct default for arrays?
    # "`.default` cant be set for array components" =
    #   is.null(.default)
  )

  rlang::warn("common_attrs_check not implemented for array")

  list(
    examples = .examples,
    enum = .enum,
    const = .const,
    default = .default
  )
}

common_attrs_check.js_schema_object <- function(
    .element,
    .examples = NULL,
    .enum = NULL,
    .const = NULL,
    .default = NULL) {
  stopifnot(
    # TODO refine ``.examples`` check
    # "`.examples` must be a vector or NULL" =
    #   rlang::is_vector(.examples) || is.null(.examples),
    # "`.enum` cant be set for object components" =
    #   is.null(.enum),
    # "`.const` cant be set for object components" =
    #   is.null(.const),
    # TODO correct default for objects?
    # "`.default` cant be set for object components" =
    #   is.null(.default)
  )

  rlang::warn("common_attrs_check not implemented for object")

  list(
    examples = .examples,
    enum = .enum,
    const = .const,
    default = .default
  )
}
