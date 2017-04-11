#' @import R6
Promise <- R6::R6Class("Promise",
  private = list(
    state = "pending",
    value = NULL,
    publicResolveRejectCalled = FALSE,
    onFulfilled = list(),
    onRejected = list(),
    onFinally = list(),
    rejectionHandled = FALSE,

    # Private resolve/reject differs from public resolve/reject
    # in that the private versions are allowed to be called
    # more than once, whereas public ones no-op after the first
    # time they are invoked.
    doResolve = function(value) {
      if (is.promise(value)) {
        value$then(
          private$doResolve,
          private$doReject
        )
      } else {
        private$doResolveFinalValue(value)
      }
    },
    doReject = function(reason) {
      if (is.promise(reason)) {
        reason$then(
          private$doResolve,
          private$doReject
        )
      } else {
        private$doRejectFinalReason(reason)
      }
    },
    # These "final" versions of resolve/reject are for when we've
    # established that the value/reason is not itself a promise.
    doResolveFinalValue = function(value) {
      private$value <- value
      private$state <- "fulfilled"

      later::later(function() {
        lapply(private$onFulfilled, function(f) {
          f(private$value)
        })
        private$onFulfilled <- list()
      })
    },
    doRejectFinalReason = function(reason) {
      private$value <- reason
      private$state <- "rejected"

      later::later(function() {
        lapply(private$onRejected, function(f) {
          private$rejectionHandled <- TRUE
          f(private$value)
        })
        private$onRejected <- list()

        if (!private$rejectionHandled) {
          reg.finalizer(self, function(obj) {
            if (!private$rejectionHandled) {
              # warning() was unreliable here, not sure why
              cat(file=stderr(), "Unhandled promise error: ", reason$message, "\n", sep = "")
              shiny::printError(reason)
            }
          }, onexit = TRUE)
        }
      })
    }
  ),
  public = list(
    # "pending", "fulfilled", "rejected"
    status = function() {
      private$state
    },
    resolve = function(value) {
      # Only allow this to be called once, then no-op.
      if (private$publicResolveRejectCalled)
        return(invisible())
      private$publicResolveRejectCalled <- TRUE

      tryCatch(
        {
          force(value)
          private$doResolve(value)
        },
        error = function(err) {
          private$doReject(err)
        }
      )

      invisible()
    },
    reject = function(reason) {
      # Only allow this to be called once, then no-op.
      if (private$publicResolveRejectCalled)
        return(invisible())
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
    },
    then = function(onFulfilled = NULL, onRejected = NULL) {
      promise2 <- new_promise(function(resolve, reject) {

        res <- promiseDomain$onThen(onFulfilled, onRejected)
        if (!is.null(res)) {
          onFulfilled <- res$onFulfilled
          onRejected <- res$onRejected
        }

        handleFulfill <- function(value) {
          if (is.function(onFulfilled)) {
            resolve(onFulfilled(value))
          } else {
            resolve(value)
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
          private$onFulfilled <- c(private$onFulfilled, list(
            handleFulfill
          ))
          private$onRejected <- c(private$onRejected, list(
            handleReject
          ))
        } else if (private$state == "fulfilled") {
          later::later(~handleFulfill(private$value))
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
    },
    catch = function(onRejected) {
      invisible(self$then(onRejected = onRejected))
    },
    finally = function(onFinally) {
      invisible(self$then(
        onFulfilled = function(value) {
          onFinally()
          value
        },
        onRejected = function(reason) {
          onFinally()
          stop(reason)
        }
      ))
    },
    format = function() {
      if (private$state == "pending") {
        "<Promise [pending]>"
      } else if (private$state == "fulfilled") {
        val <- paste(capture.output(print(private$value)), collapse = "\n")
        strsplit(sprintf("<Promise [fulfilled]>\n%s", val), "\n")[[1]]
      } else if (private$state == "rejected") {
        err <- paste(capture.output(print(private$value)), collapse = "\n")
        strsplit(sprintf("<Promise [rejected]>\n%s", err), "\n")[[1]]
      }
    }
  )
)

#' Create a new promise object
#'
#' \code{new_promise} creates a new promise. A promise is a placeholder object
#' for the eventual result (or error) of an asynchronous operation. This
#' function is not generally needed to carry out asynchronous programming tasks;
#' instead, it is intended to be used mostly by package authors who want to
#' write asynchronous functions that return promises.
#'
#' The \code{action} function/formula should be a piece of code that returns
#' quickly, but initiates a potentially long-running, asynchronous task. If/when
#' the task successfully completes, call \code{resolve(value)} where
#' \code{value} is the result of the computation (like the return value). If the
#' task fails, call \code{reject(reason)}, where \code{reason} is either an
#' error object, or a character string.
#'
#' It's important that asynchronous tasks kicked off from \code{action} be coded
#' very carefully--in particular, all errors must be caught and passed to
#' \code{reject()}. Failure to do so will cause those errors to be lost, at
#' best; and the caller of the asynchronous task will never receive a response
#' (the asynchronous equivalent of a function call that never returns, i.e.
#' hangs).
#'
#' The return value of \code{action} will be ignored.
#'
#' @param action Either a function with signature \code{function(resolve,
#'   reject)}, or a one-sided formula. See Details.
#'
#' @return A promise object (see \code{\link{then}}).
#'
#' @examples
#' # TODO
#'
#' @export
new_promise <- function(action) {
  if (inherits(action, "formula")) {
    if (length(action) != 2) {
      stop("'action' must be a function or one-sided formula")
    }
  } else if (is.function(action)) {
    if (length(formals(action)) != 2) {
      stop("'action' function must have two arguments")
    }
  } else {
    stop("Invalid action argument--must be a function or formula")
  }

  p <- Promise$new()

  tryCatch(
    {
      if (is.function(action)) {
        action(p$resolve, p$reject)
      } else if (inherits(action, "formula")) {
        eval(action[[2]], list(
          resolve = p$resolve,
          reject = p$reject,
          return = function(value) {
            warning("Can't return a value from a promise, use resolve/reject")
          }
        ), environment(action))
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

#' @export
is.promise <- function(x) {
  inherits(x, "promise")
}

#' Fulfill a promise
#'
#' Use these functions to satisfy a promise with either success (\code{resolve})
#' or failure (\code{reject}).
#'
#' @param value The result from a successful calculation.
#' @param reason An error or string that explains why the operation failed.
#'
#' @keywords internal
#' @export
resolve <- function(value = NULL) {
  stop("resolve() must be called from within a promise constructor")
}

#' @keywords internal
#' @export
reject <- function(reason) {
  stop("reject() must be called from within a promise constructor")
}

#' @rdname new_promise
#' @export
resolved <- function(value) {
  new_promise(function(resolve, reject) {
    resolve(value)
  })
}

#' @rdname new_promise
#' @export
rejected <- function(reason) {
  new_promise(function(resolve, reject) {
    reject(reason)
  })
}
