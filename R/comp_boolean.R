
#' TODO boolean
#' 
#' @inheritDotParams js_common_attributes
#' 
#' @export 
js_boolean <- function(...) {

  # common_args <- rlang::list2(...)
  # TODO check that no other common schema keywords are used

  structure(
    list(),
    class = c("js_schema_boolean", "js_schema_component")
  ) %>% 
  js_common_attributes(...)
}