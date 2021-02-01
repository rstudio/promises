library(later)

# Create a promise that can be EXTernally resolved/rejected/inspected
ext_promise <- function() {
  res <- NULL
  p <- promise(function(resolve, reject) {
    res <<- list(resolve = resolve, reject = reject)
  })

  list(
    promise = p,
    resolve = res$resolve,
    reject = res$reject,
    status = attr(p, "promise_impl", exact = TRUE)$status
  )
}

# Block until all pending later tasks have executed
wait_for_it <- function(timeout = 30) {
  start <- Sys.time()
  while (!loop_empty()) {
    if (difftime(Sys.time(), start, units = "secs") > timeout) {
      stop("Waited too long")
    }
    run_now()
    Sys.sleep(0.01)
  }
}

# Block until the promise is resolved/rejected. If resolved, return the value.
# If rejected, throw (yes throw, not return) the error.
extract <- function(promise) {
  promise_value <- NULL
  error <- NULL
  promise %...>%
    (function(value) promise_value <<- value) %...!%
    (function(reason) error <<- reason)

  wait_for_it()
  if (!is.null(error))
    stop(error)
  else
    promise_value
}

resolve_later <- function(value, delaySecs) {
  force(value)
  promise(~later::later(~resolve(value), delaySecs))
}

# Prevent "Unhandled promise error" warning that happens if you don't handle the
# rejection of a promise
squelch_unhandled_promise_error <- function(promise) {
  promise %...!% {}
}

.GlobalEnv$.Last <- function() {
  # Detect unexpected "Unhandled promise error" warnings.
  wait_for_it()
}

create_counting_domain <- function(trackFinally = FALSE) {
  counts <- list2env(parent = emptyenv(), list(
    onFulfilledBound = 0L,
    onFulfilledCalled = 0L,
    onFulfilledActive = 0L,
    onRejectedBound = 0L,
    onRejectedCalled = 0L,
    onRejectedActive = 0L
  ))

  incr <- function(field) {
    field <- as.character(substitute(field))
    counts[[field]] <- counts[[field]] + 1L
  }

  decr <- function(field) {
    field <- as.character(substitute(field))
    counts[[field]] <- counts[[field]] - 1L
  }

  pd <- new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      incr(onFulfilledBound)
      function(...) {
        incr(onFulfilledCalled)
        incr(onFulfilledActive)
        on.exit(decr(onFulfilledActive))

        onFulfilled(...)
      }
    },
    wrapOnRejected = function(onRejected) {
      incr(onRejectedBound)
      function(...) {
        incr(onRejectedCalled)
        incr(onRejectedActive)
        on.exit(decr(onRejectedActive))

        onRejected(...)
      }
    },
    counts = counts
  )

  if (trackFinally) {
    counts$onFinallyBound <- 0L
    counts$onFinallyCalled <- 0L
    counts$onFinallyActive <- 0L

    pd$wrapOnFinally <- function(onFinally) {
      incr(onFinallyBound)
      function() {
        incr(onFinallyCalled)
        incr(onFinallyActive)
        on.exit(incr(onFinallyActive))

        onFinally()
      }
    }
  }

  pd
}
