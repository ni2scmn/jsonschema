#' TODO boolean
#' 
#' @inheritDotParams js_common_attributes
#' 
#' @export 
js_boolean <- function(...) {
  structure(
    list(),
    class = c("js_schema_boolean", "js_schema_component")
  ) %>% 
  js_common_attributes(...)
}