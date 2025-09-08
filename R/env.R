#' Environment-based Promise implementation
#'
#' This is an environment-based implementation of the Promise class,
#' converted from the original R6 implementation.

#' Create a new promise using native environments
#' @return A promise object
promise_env <- function() {
  # Create the environment that will hold all state
  env <- new.env(parent = emptyenv())

  # Initialize private state
  env$state <- "pending"
  env$value <- NULL
  env$visible <- TRUE
  env$publicResolveRejectCalled <- FALSE
  env$onFulfilled <- list()
  env$onRejected <- list()
  env$onFinally <- list()
  env$rejectionHandled <- FALSE

  # Private methods
  env$doResolveFinalValue <- function(value, visible) {
    env$value <- value
    env$visible <- visible
    env$state <- "fulfilled"

    later::later(function() {
      lapply(env$onFulfilled, function(f) {
        f(env$value, env$visible)
      })
      env$onFulfilled <- list()
    })
  }

  env$doRejectFinalReason <- function(reason) {
    env$value <- reason
    env$state <- "rejected"

    later::later(function() {
      lapply(env$onRejected, function(f) {
        env$rejectionHandled <- TRUE
        f(env$value)
      })
      env$onRejected <- list()

      later::later(
        function() {
          if (!env$rejectionHandled) {
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

  env$doResolve <- function(value) {
    val <- withVisible(value)
    value <- val$value
    visible <- val$visible

    if (is.promising(value)) {
      value <- as.promise(value)
      if (identical(env, attr(value, "promise_impl", exact = TRUE))) {
        return(env$doReject(simpleError(
          "Chaining cycle detected for promise"
        )))
      }
      # This then() call doesn't need promise domains; semantically, it doesn't
      # really exist, as it's just a convenient way to implement the new promise
      # inhabiting the old promise's corpse.
      without_promise_domain({
        value$then(
          env$doResolve,
          env$doReject
        )
      })
    } else {
      env$doResolveFinalValue(value, visible)
    }
  }

  env$doReject <- function(reason) {
    if (is.promising(reason)) {
      reason <- as.promise(reason)
      # This then() call doesn't need promise domains; semantically, it doesn't
      # really exist, as it's just a convenient way to implement the new promise
      # inhabiting the old promise's corpse.
      without_promise_domain({
        reason$then(
          env$doResolve,
          env$doReject
        )
      })
    } else {
      env$doRejectFinalReason(reason)
    }
  }

  # Public methods
  env$status <- function() {
    env$state
  }

  env$resolve <- function(value) {
    # Only allow this to be called once, then no-op.
    if (env$publicResolveRejectCalled) {
      return(invisible())
    }
    env$publicResolveRejectCalled <- TRUE

    tryCatch(
      {
        # Important: Do not trigger evaluation of value before
        # passing to doResolve. doResolve calls withVisible() on
        # value, so evaluating it before that point will cause
        # the visibility to be lost.
        env$doResolve(value)
      },
      error = function(err) {
        env$doReject(err)
      }
    )

    invisible()
  }

  env$reject <- function(reason) {
    # Only allow this to be called once, then no-op.
    if (env$publicResolveRejectCalled) {
      return(invisible())
    }
    env$publicResolveRejectCalled <- TRUE

    tryCatch(
      {
        force(reason)
        if (is.character(reason)) {
          reason <- simpleError(reason)
        }
        env$doReject(reason)
      },
      error = function(err) {
        env$doReject(err)
      }
    )

    invisible()
  }

  env$then <- function(
    onFulfilled = NULL,
    onRejected = NULL,
    onFinally = NULL
  ) {
    onFulfilled <- normalizeOnFulfilled(onFulfilled)
    onRejected <- normalizeOnRejected(onRejected)
    if (!is.function(onFinally)) {
      onFinally <- NULL
    }

    promise2 <- promise()

    # Execute the action for promise2
    tryCatch(
      {
        res <- promiseDomain$onThen(onFulfilled, onRejected, onFinally)

        if (!is.null(res)) {
          onFulfilled <- res$onFulfilled
          onRejected <- res$onRejected
        }

        handleFulfill <- function(value, visible) {
          if (is.function(onFulfilled)) {
            promise2$resolve(onFulfilled(value, visible))
          } else {
            promise2$resolve(if (visible) value else invisible(value))
          }
        }

        handleReject <- function(reason) {
          if (is.function(onRejected)) {
            # Yes, resolve, not reject.
            promise2$resolve(onRejected(reason))
          } else {
            # Yes, reject, not resolve.
            promise2$reject(reason)
          }
        }

        if (env$state == "pending") {
          env$onFulfilled <- c(
            env$onFulfilled,
            list(handleFulfill)
          )
          env$onRejected <- c(
            env$onRejected,
            list(handleReject)
          )
        } else if (env$state == "fulfilled") {
          later::later(function() {
            handleFulfill(env$value, env$visible)
          })
        } else if (env$state == "rejected") {
          later::later(function() {
            env$rejectionHandled <- TRUE
            handleReject(env$value)
          })
        } else {
          stop("Unexpected state ", env$state)
        }
      },
      error = function(e) {
        if (promise2$status() == "pending") {
          promise2$reject(e)
        } else {
          warning(e)
        }
      }
    )

    invisible(promise2)
  }

  env$catch <- function(onRejected) {
    invisible(env$then(onRejected = onRejected))
  }

  env$finally <- function(onFinally) {
    invisible(env$then(onFinally = onFinally))
  }

  env$format <- function() {
    if (env$state == "pending") {
      "<Promise [pending]>"
    } else {
      classname <- class(env$value)[[1]]
      if (length(classname) == 0) {
        classname <- ""
      }

      sprintf("<Promise [%s: %s]>", env$state, classname)
    }
  }

  promise_obj(env)
}

#' Create a promise wrapper structure
#' @param promise_impl The environment-based promise implementation
#' @return A promise object with the standard promise interface
promise_obj <- function(promise_impl) {
  structure(
    list(
      then = promise_impl$then,
      catch = promise_impl$catch,
      finally = promise_impl$finally
    ),
    class = "promise",
    promise_impl = promise_impl
  )
}

#' Constructor function that matches the original promise() interface
#' @param action A function with signature `function(resolve, reject)`
#' @return A promise object
promise <- function(action) {
  prom <- promise_env()

  # Execute the action if provided
  if (!is.null(action)) {
    if (is.function(action)) {
      if (length(formals(action)) != 2) {
        stop("'action' function must have two arguments")
      }
    } else if (inherits(action, "formula")) {
      if (length(action) != 2) {
        stop("'action' must be a function or one-sided formula")
      }
    } else {
      stop("Invalid action argument--must be a function or formula")
    }

    # tryCatch(
    #   {
    if (is.function(action)) {
      action(prom$resolve, prom$reject)
    } else if (inherits(action, "formula")) {
      eval(
        action[[2]],
        list(
          resolve = prom$resolve,
          reject = prom$reject,
          return = function(value) {
            warning(
              "Can't return a value from a promise, use resolve/reject"
            )
          }
        ),
        environment(action)
      )
    }
    #   },
    #   error = function(e) {
    #     if (prom$status() == "pending") {
    #       prom$reject(e)
    #     } else {
    #       # Too late to do anything useful. Just notify.
    #       warning(e)
    #     }
    #   }
    # )
  }
  structure(
    list(
      then = prom$then,
      catch = prom$catch,
      finally = prom$finally
    ),
    class = "promise",
    promise_impl = prom
  )
}
