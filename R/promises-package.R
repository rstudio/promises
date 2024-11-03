#' @useDynLib promises, .registration = TRUE
"_PACKAGE"

ignore_unused_imports <- function() {
  fastmap::fastqueue
}

release_bullets <- function() {
  c(
    "Update staticimports: `staticimports::import()`"
  )
}
