#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom later later
#' @importFrom lifecycle deprecated
#' @importFrom R6 R6Class
## usethis namespace: end
NULL

ignore_unused_imports <- function() {
  fastmap::fastqueue
}

release_bullets <- function() {
  c(
    "Update staticimports: `staticimports::import()`",
    "Revert `future_promise()` example to use `\\donttest{}`, not `\\dontrun{}` - https://github.com/rstudio/promises/pull/134",
    "Revert `skip_on_os(\"mac\")` in `future_promise()` test - https://github.com/rstudio/promises/pull/134"
  )
}
