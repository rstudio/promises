#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom monads %>>%
#' @export
monads::`%>>%`

#' @export
format.promise <- function(x, ...) {
  p <- attr(x, "promise_impl", exact = TRUE)
  p$format()
}

#' @export
print.promise <- function(x, ...) {
  cat(paste(format(x), collapse = "\n"), "\n", sep = "")
}
