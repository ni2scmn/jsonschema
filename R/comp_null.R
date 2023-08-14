#' TODO null
#'
#' @inheritDotParams js_common_attributes
#'
#' @export
js_null <- function(...) {
  structure(
    list(),
    class = c("js_schema_null", "js_schema_component")
  ) %>%
    js_common_attributes(...)
}
