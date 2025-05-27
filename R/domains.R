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

spliceOnFinally <- function(onFinally) {
  list(
    onFulfilled = finallyToFulfilled(onFinally),
    onRejected = finallyToRejected(onFinally)
  )
}

finallyToFulfilled <- function(onFinally) {
  force(onFinally)
  function(value, .visible) {
    onFinally()
    if (.visible) value else invisible(value)
  }
}

finallyToRejected <- function(onFinally) {
  force(onFinally)
  function(reason) {
    onFinally()
    stop(reason)
  }
}

promiseDomain <- list(
  onThen = function(onFulfilled, onRejected, onFinally) {
    force(onFulfilled)
    force(onRejected)
    force(onFinally)

    # Verify that if onFinally is non-NULL, onFulfilled and onRejected are NULL
    if (
      !is.null(onFinally) && (!is.null(onFulfilled) || !is.null(onRejected))
    ) {
      stop(
        "A single `then` call cannot combine `onFinally` with `onFulfilled`/`onRejected`"
      )
    }

    domain <- current_promise_domain()

    shouldWrapFinally <- !is.null(onFinally) &&
      !is.null(domain) &&
      !is.null(domain$wrapOnFinally)

    newOnFinally <- if (shouldWrapFinally) {
      domain$wrapOnFinally(onFinally)
    } else {
      onFinally
    }

    if (!is.null(newOnFinally)) {
      spliced <- spliceOnFinally(newOnFinally)
      onFulfilled <- spliced$onFulfilled
      onRejected <- spliced$onRejected
    }

    shouldWrapFulfilled <- !is.null(onFulfilled) &&
      !is.null(domain) &&
      !shouldWrapFinally
    shouldWrapRejected <- !is.null(onRejected) &&
      !is.null(domain) &&
      !shouldWrapFinally

    results <- list(
      onFulfilled = if (shouldWrapFulfilled)
        domain$wrapOnFulfilled(onFulfilled) else onFulfilled,
      onRejected = if (shouldWrapRejected)
        domain$wrapOnRejected(onRejected) else onRejected
    )
    results <- results[!vapply(results, is.null, logical(1))]
    # If there's a domain, ensure that before any callback is invoked, we
    # reenter the domain. This is important for this kind of code:
    #
    #     with_promise_domain(domain, {
    #       async_sleep(0.1) %...>% {
    #         async_sleep(0.1) %...>% {
    #           # Without re-entry, this would be outside the domain!
    #         }
    #       }
    #     })
    results <- lapply(results, wrap_callback_reenter, domain = domain)
    results
  },
  onError = function(error) {
    domain <- current_promise_domain()
    if (is.null(domain)) return()
    domain$onError(error)
  }
)

wrap_callback_reenter <- function(callback, domain) {
  force(callback)
  force(domain)

  # We can't simply take `...` as arguments and call `callback(...)`. There are
  # parts of this package that will inspect formals() to see if there's a
  # `.visible` parameter in the callback. Using `match.call()` here ensures that
  # the callback is called with the same arguments as it would have been if it
  # were called directly.
  #
  # IMPORTANT NOTE: This technique changes callback arguments from lazy eval to
  # eager eval--make sure you're OK with that before using!
  wrapper <- function() {
    # Evaluate the arguments in the caller's environment
    call <- match.call()
    # The `[-1]` is to drop the function name from the call--we just want the
    # arguments
    args <- lapply(call[-1], eval, parent.frame())
    # Create and evaluate the callback call in our environment, wrapped in reenter_promise_domain
    reenter_promise_domain(
      domain,
      rlang::exec(callback, !!!args),
      replace = TRUE
    )
  }
  # Copy the formals from the original callback to the wrapper, so that the
  # wrapper can be called with the same arguments as the original callback.
  formals(wrapper) <- formals(callback)

  wrapper
}

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
  if (replace) globals$domain <- domain else
    globals$domain <- compose_domains(oldval, domain)
  on.exit(globals$domain <- oldval)

  if (!is.null(domain)) domain$wrapSync(expr) else force(expr)
}

# Like with_promise_domain, but doesn't include the wrapSync call.
reenter_promise_domain <- function(domain, expr, replace) {
  oldval <- current_promise_domain()
  if (replace) globals$domain <- domain else
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
#' @param onError A function that takes a single argument: an error. `onError`
#'   will be called whenever an exception occurs in a domain (that isn't caught
#'   by a `tryCatch`). Providing an `onError` callback doesn't cause errors to
#'   be caught, necessarily; instead, `onError` callbacks behave like calling
#'   handlers.
#' @param ... Arbitrary named values that will become elements of the promise
#'   domain object, and can be accessed as items in an environment (i.e. using
#'   `[[` or `$`).
#' @param wrapOnFinally A function that takes a single argument: a function
#'   that was passed as an `onFinally` argument to [then()]. The
#'   `wrapOnFinally` function should return a function that is suitable for
#'   `onFinally` duty. If `wrapOnFinally` is `NULL` (the default), then the
#'   domain will use both `wrapOnFulfilled` and `wrapOnRejected` to wrap the
#'   `onFinally`. If it's important to distinguish between normal
#'   fulfillment/rejection handlers and finally handlers, then be sure to
#'   provide `wrapOnFinally`, even if it's just [base::identity()].
#' @rdname with_promise_domain
#' @export
new_promise_domain <- function(
  wrapOnFulfilled = identity,
  wrapOnRejected = identity,
  wrapSync = force,
  onError = force,
  ...,
  wrapOnFinally = NULL
) {
  list2env(
    list(
      wrapOnFulfilled = wrapOnFulfilled,
      wrapOnRejected = wrapOnRejected,
      wrapOnFinally = wrapOnFinally,
      wrapSync = wrapSync,
      onError = onError,
      ...
    ),
    parent = emptyenv()
  )
}


compose_domains <- function(base, new) {
  if (is.null(base)) {
    return(new)
  }

  list(
    wrapOnFulfilled = function(onFulfilled) {
      # Force eager evaluation of base$wrapOnFulfilled(onFulfilled)
      base <- base$wrapOnFulfilled(onFulfilled)
      new$wrapOnFulfilled(base)
    },
    wrapOnRejected = function(onRejected) {
      # Force eager evaluation of base$wrapOnRejected(onRejected)
      base <- base$wrapOnRejected(onRejected)
      new$wrapOnRejected(base)
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

without_promise_domain <- function(expr) {
  with_promise_domain(NULL, expr, replace = TRUE)
}
