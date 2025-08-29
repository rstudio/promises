#' @importFrom magrittr %>%
#' @export
magrittr::"%>%"

#' @importFrom magrittr %T>%
#' @export
magrittr::"%T>%"


#' Promise pipe operators
#'
#' With R 4.1, the promise pipe operators are `r lifecycle::badge('superseded')` by [`then()`], [`catch()`], and
#' [`finally()`] methods when used in tandem with the function shorthand (`\(x) rhs(x)`) and `|>`.
#'
#' Promise-aware pipe operators, in the style of [magrittr](https://CRAN.R-project.org/package=magrittr/vignettes/magrittr.html).
#' Like magrittr pipes, these operators can be used to chain together pipelines
#' of promise-transforming operations. Unlike magrittr pipes, these pipes wait
#' for promise resolution and pass the unwrapped value (or error) to the `rhs`
#' function call.
#'
#' The `>` variants are for handling successful resolution, the `!` variants are
#' for handling errors. The `T` variants of each return the lhs instead of the
#' rhs, which is useful for pipeline steps that are used for side effects
#' (printing, plotting, saving).
#'
#' 1. \code{promise \%...>\% func()} is equivalent to \code{promise \%>\% then(func)}.
#' 2. \code{promise \%...!\% func()} is equivalent to \code{promise \%>\% catch(func)}.
#' 3. \code{promise \%...T>\% func()} is equivalent to \code{promise \%T>\% then(func)}.
#' 4. \code{promise \%...T!\% func()} is equivalent to \code{promise \%T>\%
#' catch(func)} or \code{promise \%>\% catch(func, tee = TRUE)}.
#'
#' One situation where 3. and 4. above break down is when `func()` throws an
#' error, or returns a promise that ultimately fails. In that case, the failure
#' will be propagated by our pipe operators but not by the
#' magrittr-plus-function "equivalents".
#'
#' For simplicity of implementation, we do not support the magrittr feature of
#' using a `.` at the head of a pipeline to turn the entire pipeline into a
#' function instead of an expression.
#'
#' @param lhs A promise object.
#' @param rhs A function call using the magrittr semantics. It can return either
#'   a promise or non-promise value, or throw an error.
#'
#' @examples
#' \dontrun{
#' library(mirai)
#'
#' mirai(cars) %...>%
#'   head(5) %...T>%
#'   print()
#'
#' # If the read.csv fails, resolve to NULL instead
#' mirai(read.csv("http://example.com/data.csv")) %...!%
#'   { NULL }
#' }
#'
#' @return A new promise.
#'
#' @seealso https://rstudio.github.io/promises/articles/promises_03_overview.html#using-pipes
#'
#' @name pipes
#' @export
`%...>%` <- function(lhs, rhs) {
  # the parent environment
  parent <- parent.frame()

  # the environment in which to evaluate pipeline
  env <- new.env(parent = parent)

  parts <- match.call()
  func <- pipeify_rhs(parts[[3L]], env)
  then(lhs, func)
}

#' @rdname pipes
#' @export
`%...T>%` <- function(lhs, rhs) {
  # the parent environment
  parent <- parent.frame()

  # the environment in which to evaluate pipeline
  env <- new.env(parent = parent)

  parts <- match.call()
  func <- pipeify_rhs(parts[[3L]], env)
  lhs %>%
    then(func) %>%
    then(function(value) lhs)
}

#' @rdname pipes
#' @export
`%...!%` <- function(lhs, rhs) {
  # the parent environment
  parent <- parent.frame()

  # the environment in which to evaluate pipeline
  env <- new.env(parent = parent)

  parts <- match.call()
  func <- pipeify_rhs(parts[[3L]], env)
  catch(lhs, func)
}

#' @rdname pipes
#' @export
`%...T!%` <- function(lhs, rhs) {
  # the parent environment
  parent <- parent.frame()

  # the environment in which to evaluate pipeline
  env <- new.env(parent = parent)

  parts <- match.call()
  func <- pipeify_rhs(parts[[3L]], env)
  catch(lhs, func, tee = TRUE)
}


has.visible <- function(func) {
  ".visible" %in% names(formals(func))
}

pipeify_rhs <- function(rhs, env) {
  if (is_parenthesized(rhs)) {
    rhs <- eval(rhs, env, env)
  }

  rhs <- if (is_funexpr(rhs)) {
    rhs
  } else if (is_lambda(rhs)) {
    # We can remove this conditional if we want this behavior to be supported.
    # The next conditional checks for is_lambda too, and does the right thing.
    # Keeping the error for now in deference to magrittr pipe behavior.
    stop("Anonymous functions must be parenthesized")
  } else if (is_function(rhs) || is_colexpr(rhs) || is_lambda(rhs)) {
    # This block diverges from magrittr because we have an optional .visible
    # argument that can be passed to the function. If the function takes a
    # parameter called .visible then we will pass it, otherwise no.

    real_rhs <- if (is.function(rhs)) {
      rhs
    } else {
      eval(rhs, env, env)
    }

    if (has.visible(real_rhs)) {
      as.call(list(rhs, quote(.), .visible = quote(.visible)))
    } else {
      as.call(list(rhs, quote(.)))
    }
  } else if (is_first(rhs)) {
    prepare_first(rhs)
  } else {
    rhs
  }

  eval(call("function", as.pairlist(alist(. = , .visible = )), rhs), env, env)
}
