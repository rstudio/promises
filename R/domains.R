promiseDomain <- list(
  onThen = function(onFulfilled, onRejected) {
    domain <- current_promise_domain()

    if (is.null(domain))
      return()
    if (is.null(onFulfilled) && is.null(onRejected))
      return()

    results <- list()
    if (!is.null(onFulfilled)) {
      newOnFulfilled <- domain$wrapOnFulfilled(onFulfilled)
      results$onFulfilled <- function(...) {
        with_promise_domain(domain, newOnFulfilled(...))
      }
    }
    if (!is.null(onRejected)) {
      newOnRejected <- domain$wrapOnRejected(onRejected)
      results$onRejected <- function(...) {
        with_promise_domain(domain, newOnRejected(...))
      }
    }
    results
  }
)

globals <- new.env(parent = emptyenv())

current_promise_domain <- function() {
  globals$domain
}

#' @export
with_promise_domain <- function(domain, expr, replace = FALSE) {
  oldval <- current_promise_domain()
  if (replace)
    globals$domain <- domain
  else
    globals$domain <- compose_domains(oldval, domain)
  on.exit(globals$domain <- oldval)

  force(expr)
}

#' @export
new_promise_domain <- function(
  wrapOnFulfilled = identity,
  wrapOnRejected = identity,
  ...
) {
  list2env(list(
    wrapOnFulfilled = wrapOnFulfilled,
    wrapOnRejected = wrapOnRejected,
    ...
  ), parent = emptyenv())
}


compose_domains <- function(base, new) {
  if (is.null(base)) {
    return(new)
  }

  list(
    wrapOnFulfilled = function(onFulfilled) {
      new$wrapOnFulfilled(
        base$wrapOnFulfilled(onFulfilled)
      )
    },
    wrapOnRejected = function(onRejected) {
      new$wrapOnRejected(
        base$wrapOnRejected(onRejected)
      )
    }
  )
}
