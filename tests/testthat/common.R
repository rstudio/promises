library(later)

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

wait_for_it <- function() {
  while (!loop_empty()) {
    run_now()
    Sys.sleep(0.1)
  }
}

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

squelch_unhandled_promise_error <- function(promise) {
  promise %...!% {}
}
