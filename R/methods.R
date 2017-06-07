#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom monads %>>%
#' @export
monads::`%>>%`

#' @export
fmap.promise <- function(.m, .f, ...) {
  .m$then(.f)
}

#' @export
format.promise <- function(x, ...) {
  p <- attr(x, "promise_impl", exact = TRUE)
  p$format()
}

#' @export
print.promise <- function(x, ...) {
  cat(paste(format(x), collapse = "\n"), "\n", sep = "")
}
