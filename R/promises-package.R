#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

otel_tracer_name <- "io.github.rstudio/promises"

ignore_unused_imports <- function() {
  fastmap::fastqueue
}

release_bullets <- function() {
  c(
    "Remove `otelsdk` dependency",
    "Update staticimports: `staticimports::import()`",
    "Revert `future_promise()` example to use `\\donttest{}`, not `\\dontrun{}`",
    "Revert `skip_on_os(\"mac\")` in `future_promise()` test"
  )
}
