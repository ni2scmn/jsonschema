validate_const <- function(schema, request) {
  if (rlang::hash(schema$const) != rlang::hash(request)) {
    rlang::abort("Value does not match const value")
  }
}

validate_enum <- function(schema, request) {
  hash_enum <- purrr::map_chr(schema$enum, rlang::hash)
  hash_request <- rlang::hash(request)
  if (!hash_request %in% hash_enum) {
    rlang::abort("Value does not match enum value")
  }
}
