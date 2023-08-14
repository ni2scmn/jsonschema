validate_const <- function(schema, request) {
  if (rlang::hash(schema$const) != rlang::hash(request)) {
    rlang::abort("Value does not match const value")
  }
}
