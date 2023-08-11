# TODO '$id' -> Backlog

js_schema <- function(root) {
  stopifnot(
    "Root must be a schema component" = 
      inherits(root, "js_schema_component")
  )

  structure(
    list(
      root = root
    ),
    class = c("js_schema")
  )
}