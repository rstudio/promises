#' @importFrom magrittr %>%
#' @export
magrittr::"%>%"

#' @importFrom magrittr %T>%
#' @export
magrittr::"%T>%"

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

#' @export
`%T...!%` <- function(lhs, rhs) {
  # the parent environment
  parent <- parent.frame()

  # the environment in which to evaluate pipeline
  env <- new.env(parent = parent)

  parts <- match.call()
  func <- pipeify_rhs(parts[[3L]], env)
  catch(lhs, func)
  lhs
}

pipeify_rhs <- function(rhs, env) {
  if (is_parenthesized(rhs)) {
    rhs <- eval(rhs, env, env)
  }

  rhs <- if (is_funexpr(rhs)) {
    rhs
  } else if (is_function(rhs) || is_colexpr(rhs)) {
    prepare_function(rhs)
  } else if (is_first(rhs)) {
    prepare_first(rhs)
  } else {
    rhs
  }

  eval(call("function", as.pairlist(alist(.=)), rhs), env, env)
}
