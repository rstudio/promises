tryCatch <- function(expr, ..., finally) {
  base::tryCatch(
    withCallingHandlers(
      expr,
      error = function(e) {
        promiseDomain$onError(e)
      }
    ),
    ...,
    finally = finally
  )
}

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
        reenter_promise_domain(domain, newOnFulfilled(...))
      }
    }
    if (!is.null(onRejected)) {
      newOnRejected <- domain$wrapOnRejected(onRejected)
      results$onRejected <- function(...) {
        reenter_promise_domain(domain, newOnRejected(...))
      }
    }
    results
  },
  onError = function(error) {
    domain <- current_promise_domain()
    domain$onError(error)
  }
)

globals <- new.env(parent = emptyenv())

current_promise_domain <- function() {
  globals$domain
}

#' Promise domains
#'
#' Promise domains are used to temporarily set up custom environments that
#' intercept and influence the registration of callbacks. Create new promise
#' domain objects using `new_promise_domain`, and temporarily activate a promise
#' domain object (for the duration of evaluating a given expression) using
#' `with_promise_domain`.
#'
#' While `with_promise_domain` is on the call stack, any calls to [then()] (or
#' higher level functions or operators, like [catch()] or the various [pipes])
#' will belong to the promise domain. In addition, when a `then` callback that
#' belongs to a promise domain is invoked, then any new calls to `then` will
#' also belong to that promise domain. In other words, a promise domain
#' "infects" not only the immediate calls to `then`, but also to "nested" calls
#' to `then`.
#'
#' For more background, read the
#' [original design doc](https://gist.github.com/jcheng5/b1c87bb416f6153643cd0470ac756231).
#'
#' For examples, see the source code of the Shiny package, which uses promise
#' domains extensively to manage graphics devices and reactivity.
#'
#' @param domain A promise domain object to install while `expr` is evaluated.
#' @param expr Any R expression, to be evaluated under the influence of
#'   `domain`.
#' @param replace If `FALSE`, then the effect of the `domain` will be added
#'   to the effect of any currently active promise domain(s). If `TRUE`, then
#'   the current promise domain(s) will be ignored for the duration of the
#'   `with_promise_domain` call.
#'
#' @export
with_promise_domain <- function(domain, expr, replace = FALSE) {
  oldval <- current_promise_domain()
  if (replace)
    globals$domain <- domain
  else
    globals$domain <- compose_domains(oldval, domain)
  on.exit(globals$domain <- oldval)

  globals$domain$wrapSync(expr)
}

# Like with_promise_domain, but doesn't include the wrapSync call.
reenter_promise_domain <- function(domain, expr, replace = FALSE) {
  oldval <- current_promise_domain()
  if (replace)
    globals$domain <- domain
  else
    globals$domain <- compose_domains(oldval, domain)
  on.exit(globals$domain <- oldval)

  force(expr)
}

#' @param wrapOnFulfilled A function that takes a single argument: a function
#'   that was passed as an `onFulfilled` argument to [then()]. The
#'   `wrapOnFulfilled` function should return a function that is suitable for
#'   `onFulfilled` duty.
#' @param wrapOnRejected A function that takes a single argument: a function
#'   that was passed as an `onRejected` argument to [then()]. The
#'   `wrapOnRejected` function should return a function that is suitable for
#'   `onRejected` duty.
#' @param wrapSync A function that takes a single argument: a (lazily evaluated)
#'   expression that the function should [force()]. This expression represents
#'   the `expr` argument passed to [with_promise_domain()]; `wrapSync` allows
#'   the domain to manipulate the environment before/after `expr` is evaluated.
#' @param ... Arbitrary named values that will become elements of the promise
#'   domain object, and can be accessed as items in an environment (i.e. using
#'   `[[` or `$`).
#' @rdname with_promise_domain
#' @export
new_promise_domain <- function(
  wrapOnFulfilled = identity,
  wrapOnRejected = identity,
  wrapSync = force,
  onError = force,
  ...
) {
  list2env(list(
    wrapOnFulfilled = wrapOnFulfilled,
    wrapOnRejected = wrapOnRejected,
    wrapSync = wrapSync,
    onError = onError,
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
    },
    # Only include the new wrapSync, assuming that we've already applied the
    # base domain's wrapSync. This assumption won't hold if we either export
    # compose_domains in the future, or if we use it in cases where the base
    # domain isn't currently active.
    wrapSync = new$wrapSync,
    onError = function(e) {
      base$onError(e)
      new$onError(e)
    }
  )
}
