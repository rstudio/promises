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
wait_for_it <- function() {
  while (!loop_empty()) {
    run_now()
    Sys.sleep(0.1)
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
