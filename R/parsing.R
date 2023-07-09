parse <- function(schema, request_string) {
  request <- jsonlite::fromJSON(
    txt = request_string,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE,
    simplifyMatrix = TRUE,
    flatten = FALSE
  )

  if(!validate(schema, request)) rlang::abort("Invalid request")

  request
}