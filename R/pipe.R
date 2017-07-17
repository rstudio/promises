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
`%...T!%` <- function(lhs, rhs) {
  # the parent environment
  parent <- parent.frame()

  # the environment in which to evaluate pipeline
  env <- new.env(parent = parent)

  parts <- match.call()
  func <- pipeify_rhs(parts[[3L]], env)
  catch(lhs, func)
  invisible(lhs)
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
  } else if (is_function(rhs) || is_colexpr(rhs)) {
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

  eval(call("function", as.pairlist(alist(.=, .visible=)), rhs), env, env)
}
