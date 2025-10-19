#' @import later
NULL

# Promise constructor using classed environment
Promise <- function() {
  # Create private environment
  private <- new.env(parent = emptyenv())
  private$state <- "pending"
  private$value <- NULL
  private$visible <- TRUE
  private$publicResolveRejectCalled <- FALSE
  private$onFulfilled <- list()
  private$onRejected <- list()
  private$onFinally <- list()
  private$rejectionHandled <- FALSE

  # Create public environment
  self <- new.env(parent = emptyenv())

  # Private methods
  private$doResolve <- function(value) {
    val <- withVisible(value)
    value <- val$value
    visible <- val$visible

    if (is.promising(value)) {
      value <- as.promise(value)
      if (identical(self, attr(value, "promise_impl", exact = TRUE))) {
        return(private$doReject(simpleError(
          "Chaining cycle detected for promise"
        )))
      }
      # This then() call doesn't need promise domains; semantically, it doesn't
      # really exist, as it's just a convenient way to implement the new promise
      # inhabiting the old promise's corpse.
      without_promise_domain({
        value$then(
          private$doResolve,
          private$doReject
        )
      })
    } else {
      private$doResolveFinalValue(value, visible)
    }
  }

  private$doReject <- function(reason) {
    if (is.promising(reason)) {
      reason <- as.promise(reason)
      # This then() call doesn't need promise domains; semantically, it doesn't
      # really exist, as it's just a convenient way to implement the new promise
      # inhabiting the old promise's corpse.
      without_promise_domain({
        reason$then(
          private$doResolve,
          private$doReject
        )
      })
    } else {
      private$doRejectFinalReason(reason)
    }
  }

  # These "final" versions of resolve/reject are for when we've
  # established that the value/reason is not itself a promise.
  private$doResolveFinalValue <- function(value, visible) {
    private$value <- value
    private$visible <- visible
    private$state <- "fulfilled"

    later::later(function() {
      lapply(private$onFulfilled, function(f) {
        f(private$value, private$visible)
      })
      private$onFulfilled <- list()
    })
  }

  private$doRejectFinalReason <- function(reason) {
    private$value <- reason
    private$state <- "rejected"

    later::later(function() {
      lapply(private$onRejected, function(f) {
        private$rejectionHandled <- TRUE
        f(private$value)
      })
      private$onRejected <- list()

      later::later(
        function() {
          if (!private$rejectionHandled) {
            # warning() was unreliable here
            cat(
              file = stderr(),
              "Unhandled promise error: ",
              reason$message,
              "\n",
              sep = ""
            )
          }
        }
      )
    })
  }

  # Public methods
  self$status <- function() {
    private$state
  }

  self$resolve <- function(value) {
    # Only allow this to be called once, then no-op.
    if (private$publicResolveRejectCalled) {
      return(invisible())
    }
    private$publicResolveRejectCalled <- TRUE

    tryCatch(
      {
        # Important: Do not trigger evaluation of value before
        # passing to doResolve. doResolve calls withVisible() on
        # value, so evaluating it before that point will cause
        # the visibility to be lost.
        private$doResolve(value)
      },
      error = function(err) {
        private$doReject(err)
      }
    )

    invisible()
  }

  self$reject <- function(reason) {
    # Only allow this to be called once, then no-op.
    if (private$publicResolveRejectCalled) {
      return(invisible())
    }
    private$publicResolveRejectCalled <- TRUE

    tryCatch(
      {
        force(reason)
        if (is.character(reason)) {
          reason <- simpleError(reason)
        }
        private$doReject(reason)
      },
      error = function(err) {
        private$doReject(err)
      }
    )

    invisible()
  }

  self$then <- function(onFulfilled = NULL, onRejected = NULL, onFinally = NULL) {
    onFulfilled <- normalizeOnFulfilled(onFulfilled)
    onRejected <- normalizeOnRejected(onRejected)
    if (!is.function(onFinally)) {
      onFinally <- NULL
    }

    promise2 <- promise(function(resolve, reject) {
      res <- promiseDomain$onThen(onFulfilled, onRejected, onFinally)

      if (!is.null(res)) {
        onFulfilled <- res$onFulfilled
        onRejected <- res$onRejected
      }

      handleFulfill <- function(value, visible) {
        if (is.function(onFulfilled)) {
          resolve(onFulfilled(value, visible))
        } else {
          resolve(if (visible) value else invisible(value))
        }
      }

      handleReject <- function(reason) {
        if (is.function(onRejected)) {
          # Yes, resolve, not reject.
          resolve(onRejected(reason))
        } else {
          # Yes, reject, not resolve.
          reject(reason)
        }
      }

      if (private$state == "pending") {
        private$onFulfilled <- c(
          private$onFulfilled,
          list(
            handleFulfill
          )
        )
        private$onRejected <- c(
          private$onRejected,
          list(
            handleReject
          )
        )
      } else if (private$state == "fulfilled") {
        later::later(function() {
          handleFulfill(private$value, private$visible)
        })
      } else if (private$state == "rejected") {
        later::later(function() {
          private$rejectionHandled <- TRUE
          handleReject(private$value)
        })
      } else {
        stop("Unexpected state ", private$state)
      }
    })

    invisible(promise2)
  }

  self$catch <- function(onRejected) {
    invisible(self$then(onRejected = onRejected))
  }

  self$finally <- function(onFinally) {
    invisible(self$then(
      onFinally = onFinally
    ))
  }

  self$format <- function() {
    if (private$state == "pending") {
      "<Promise [pending]>"
    } else {
      classname <- class(private$value)[[1]]
      if (length(classname) == 0) {
        classname <- ""
      }

      sprintf("<Promise [%s: %s]>", private$state, classname)
    }
  }

  self$.__enclos_env__ <- list(private = private)
  class(self) <- "Promise"
  self
}

normalizeOnFulfilled <- function(onFulfilled) {
  if (!is.function(onFulfilled)) {
    if (!is.null(onFulfilled)) {
      warning("`onFulfilled` must be a function or `NULL`")
    }
    return(NULL)
  }

  args <- formals(onFulfilled)
  arg_count <- length(args)

  if (arg_count >= 2 && names(args)[[2]] == ".visible") {
    onFulfilled
  } else if (arg_count > 0) {
    function(value, .visible) {
      if (isTRUE(.visible)) {
        onFulfilled(value)
      } else {
        onFulfilled(invisible(value))
      }
    }
  } else {
    function(value, .visible) {
      onFulfilled()
    }
  }
}

normalizeOnRejected <- function(onRejected) {
  if (!is.function(onRejected)) {
    if (!is.null(onRejected)) {
      warning("`onRejected` must be a function or `NULL`")
    }
    return(NULL)
  }

  args <- formals(onRejected)
  arg_count <- length(args)

  if (arg_count >= 1) {
    onRejected
  } else if (arg_count == 0) {
    function(reason) {
      onRejected()
    }
  }
}

#' Create a new promise object
#'
#' `promise()` creates a new promise. A promise is a placeholder object for the
#' eventual result (or error) of an asynchronous operation. This function is not
#' generally needed to carry out asynchronous programming tasks; instead, it is
#' intended to be used mostly by package authors who want to write asynchronous
#' functions that return promises.
#'
#' The `action` function should be a piece of code that returns quickly, but
#' initiates a potentially long-running, asynchronous task. If/when the task
#' successfully completes, call `resolve(value)` where `value` is the result of
#' the computation (like the return value). If the task fails, call
#' `reject(reason)`, where `reason` is either an error object, or a character
#' string.
#'
#' It's important that asynchronous tasks kicked off from `action` be coded very
#' carefully--in particular, all errors must be caught and passed to `reject()`.
#' Failure to do so will cause those errors to be lost, at best; and the caller
#' of the asynchronous task will never receive a response (the asynchronous
#' equivalent of a function call that never returns, i.e. hangs).
#'
#' The return value of `action` will be ignored.
#'
#' @section `action=` formulas:
#'
#' `r lifecycle::badge("superseded")`
#'
#' With `{promises}` depending on R >= 4.1, the shorthand of a formula, `~
#' fn(.)` for `action` is no longer recommended by the `{promises}` package or
#' tidyverse (for example,
#' [`{purrr}`](https://github.com/tidyverse/purrr/commit/670c3ed9920f15da0d4175068ecddc41f0f1f335#diff-c4dcc43795da5c7f6bf5a94d957b5507ce795fedd6d3eb092ccad03678c4f76dR15))
#' as we now have access to the function shorthand, `\(x) fn(x)`. Please update
#' your `action` code to use the new function shorthand syntax `\(resolve,
#' reject) resolve(arg1, arg2)` instead of `~ { resolve(arg1, arg2) }`. The
#' magically created `resolve`/`reject` functions can be confusing when chained
#' with other methods.
#'
#' @param action A function with signature `function(resolve, reject)`.
#'
#' @return A promise object (see \code{\link{then}}).
#'
#' @examples
#' # Create a promise that resolves to a random value after 2 secs
#' p1 <- promise(\(resolve, reject) {
#'   later::later(\() resolve(runif(1)), delay = 2)
#' })
#'
#' p1 |> then(print)
#'
#' # Create a promise that errors immediately
#' p2 <- promise(\(resolve, reject) {
#'   reject("An error has occurred")
#' })
#' then(p2,
#'   onFulfilled = \(value) message("Success"),
#'   onRejected = \(err) message("Failure")
#' )
#'
#' @export
promise <- function(action) {
  if (is.function(action)) {
    if (length(formals(action)) != 2) {
      stop("'action' function must have two arguments")
    }
  } else if (inherits(action, "formula")) {
    if (length(action) != 2) {
      stop("'action' must be a function or one-sided formula")
    }
  } else {
    stop("Invalid action argument--should be a function")
  }

  p <- Promise()

  tryCatch(
    {
      if (is.function(action)) {
        action(p$resolve, p$reject)
      } else if (inherits(action, "formula")) {
        eval(
          action[[2]],
          list(
            resolve = p$resolve,
            reject = p$reject,
            return = function(value) {
              warning("Can't return a value from a promise, use resolve/reject")
            }
          ),
          environment(action)
        )
      }
    },
    error = function(e) {
      if (p$status() == "pending") {
        p$reject(e)
      } else {
        # Too late to do anything useful. Just notify.
        warning(e)
      }
    }
  )
  structure(
    list(
      then = p$then,
      catch = p$catch,
      finally = p$finally
    ),
    class = "promise",
    promise_impl = p
  )
}

#' Create a resolved or rejected promise
#'
#' Helper functions to conveniently create a promise that is resolved to the
#' given value (or rejected with the given reason).
#'
#' @param value A value, or promise, that the new promise should be resolved to.
#'   This expression will be lazily evaluated, and if evaluating the expression
#'   raises an error, then the new promise will be rejected with that error as
#'   the reason.
#' @param reason An error message string, or error object.
#'
#' @examples
#' promise_resolve(mtcars) |>
#'   then(head) |>
#'   then(print)
#'
#' promise_reject("Something went wrong") |>
#'   catch(tee = TRUE, \(e) message(conditionMessage(e)))
#'
#' @export
promise_resolve <- function(value) {
  promise(function(resolve, reject) resolve(value))
}

#' @rdname promise_resolve
#' @export
promise_reject <- function(reason) {
  promise(function(resolve, reject) reject(reason))
}

#' Coerce to a promise
#'
#' Use `is.promise` to determine whether an R object is a promise. Use
#' `as.promise` (an S3 generic method) to attempt to coerce an R object to a
#' promise, and `is.promising` (another S3 generic method) to test whether
#' `as.promise` is supported. [mirai::mirai] objects have an `as.promise` method
#' defined in the mirai package, and this package provides one for converting
#' [future::Future] objects into promises.
#'
#' @param x An R object to test or coerce.
#'
#' @return `as.promise` returns a promise object, or throws an error if the
#'   object cannot be converted.
#'
#'   `is.promise` returns `TRUE` if the given value is a promise object, and
#'   `FALSE` otherwise.
#'
#'   `is.promising` returns `TRUE` if the given value is a promise object or
#'   if it can be converted to a promise object using `as.promise`, and `FALSE`
#'   otherwise.
#'
#' @export
is.promise <- function(x) {
  inherits(x, "promise")
}

#' @rdname is.promise
#' @export
is.promising <- function(x) {
  UseMethod("is.promising")
}

#' @export
is.promising.default <- function(x) {
  FALSE
}

#' @export
is.promising.promise <- function(x) {
  TRUE
}

#' @export
is.promising.Future <- function(x) {
  TRUE
}

#' @rdname is.promise
#' @export
as.promise <- function(x) {
  UseMethod("as.promise", x)
}

#' @export
as.promise.promise <- function(x) {
  x
}

#' @export
as.promise.Future <- function(x) {
  # We want to create a promise only once for each Future object, and cache it
  # as an attribute. This spares us from having multiple polling loops waiting
  # for the same Future.
  #
  ## Note: 2025-07-barret:
  ## Active bindings will not work as `{future}` does not proactively retrieve results. A poll must be used.
  #
  ## Note: 2025-08-barret:
  ## From conversation with Henrik, while it may be possible to use `immediateCondition` within future to _immediately_ send a value back from a worker to the main R session,
  ## `future::resolved()` is still required to pull the _immediate_ value into the R session (even when the future has not fully resolved yet).
  ## Therefore, the polling mechanism below is still required.
  ## For comparison, mirai has a background process that waits for results and will interrupt the main R session with the value.

  cached <- attr(x, "converted_promise", exact = TRUE)
  if (!is.null(cached)) {
    return(cached)
  }

  p <- promise(function(resolve, reject) {
    poll_interval <- 0.1
    check <- function() {
      # timeout = 0 is important, the default waits for 200ms
      is_resolved <- tryCatch(
        {
          future::resolved(x, timeout = 0)
        },
        FutureError = function(e) {
          reject(e)
          TRUE
        }
      )
      if (is_resolved) {
        tryCatch(
          {
            result <- future::value(x, signal = TRUE)
            resolve(result)
          },
          FutureError = function(e) {
            reject(e)
            TRUE
          },
          error = function(e) {
            reject(e)
          }
        )
      } else {
        later::later(check, poll_interval)
      }
    }
    check()
  })

  # Store the new promise for next time
  attr(x, "converted_promise") <- p
  p
}

#' @export
as.promise.default <- function(x) {
  # TODO: If x is an error or try-error, should this return a rejected promise?
  stop(
    "Don't know how to convert object of class ",
    class(x)[[1L]],
    " into a promise"
  )
}

#' Fulfill a promise
#'
#' Use these functions to satisfy a promise with either success (\code{resolve})
#' or failure (\code{reject}). These functions are not exported, but rather, are
#' passed as arguments to the \code{action} function you pass to a [promise]
#' constructor.
#'
#' @param value The result from a successful calculation.
#' @param reason An error or string that explains why the operation failed.
#'
#' @keywords internal
resolve <- function(value = NULL) {
  stop("resolve() must be called from within a promise constructor")
}

#' @rdname resolve
#' @keywords internal
reject <- function(reason) {
  stop("reject() must be called from within a promise constructor")
}
