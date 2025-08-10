#' Determine whether an non-evaluated call is parenthesized
#'
#' @param a non-evaluated expression
#' @return logical - TRUE if expression is parenthesized, FALSE otherwise.
#' @noRd
is_parenthesized <- function(expr) {
  is.call(expr) && identical(expr[[1L]], quote(`(`))
}


#' Check whether expression is enclosed in curly braces.
#'
#' @param  expr An expression to be tested.
#' @return logical - TRUE if expr is enclosed in `{`, FALSE otherwise.
#' @noRd
is_funexpr <- function(expr) {
  is.call(expr) && identical(expr[[1L]], quote(`{`))
}

#' Check whether expression has double or triple colons
#'
#' @param  expr An expression to be tested.
#' @return logical - TRUE if expr contains `::` or `:::`, FALSE otherwise.
#' @noRd
is_colexpr <- function(expr) {
  is.call(expr) &&
    (identical(expr[[1L]], quote(`::`)) || identical(expr[[1L]], quote(`:::`)))
}


#' Determine whether an expression counts as a function in a magrittr chain.
#'
#' @param a non-evaluated expression.
#' @return logical - TRUE if expr represents a function, FALSE otherwise.
#' @noRd
is_function <- function(expr) {
  is.symbol(expr) || is.function(expr)
}

is_lambda <- function(expr) {
  is.call(expr) && identical(expr[[1L]], quote(`function`))
}
