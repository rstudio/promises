#' Wait for a Promise
#'
#' Synchronizes a promise by waiting for it to complete and then retrieving its
#' value.
#'
#' @param promise A promise object, or an object that can be converted to one
#'   with [as.promise()] (such as a future or a mirai).
#' @param loop the \pkg{later} loop on which the promise has been scheduled.
#'   Defaults to [later::current_loop()].
#'
#' @return The value of the resolved promise. An error will be thrown if the
#'   promise is rejected.
#'
#' @section Event loop:
#' `wait_for()` runs a single event loop, given by `loop`. This must be the
#' loop that runs the promise's callbacks. If it is not, `wait_for()` waits
#' indefinitely. The default, [later::current_loop()], is the right choice
#' whenever you did not create a custom loop yourself. Do not call
#' `wait_for()` from inside a `later` callback to wait on work that depends
#' on that callback finishing.
#'
#' @section Interrupts:
#' An interrupt (Esc in RStudio, Ctrl+C in a terminal) means "stop waiting".
#' It exits `wait_for()` and leaves the promise pending. Callbacks that have
#' not started yet stay on the loop and run the next time the loop runs. If
#' the interrupt lands while a callback is running, that callback stops part
#' way and is not run again, so promises that depend on it may never settle.
#' `wait_for()` does not cancel the underlying work, because only the creator
#' of the work knows how to cancel it.
#'
#' @examples
#' # Returns a promise for the sum of e1 + e2, with a 1 sec delay
#' slowly_add <- function(e1, e2) {
#'   promise(\(resolve, reject) {
#'     later::later(\() resolve(e1 + e2), delay = 1)
#'   })
#' }
#'
#' # Returns 3 after 1 second
#' res <- wait_for(slowly_add(1, 2))
#' res
#'
#' @export
wait_for <- function(promise, loop = later::current_loop()) {
  if (!is.promising(promise)) {
    stop("wait_for() requires a promise object")
  }
  impl <- attr(as.promise(promise), "promise_impl", exact = TRUE)
  while (impl$status() == "pending") {
    later::run_now(Inf, loop = loop)
  }
  private <- impl$.__enclos_env__$private
  if (impl$status() == "rejected") {
    # The caller now owns the error, so don't also report it as unhandled
    private$rejectionHandled <- TRUE
    stop(private$value)
  }
  if (private$visible) {
    return(private$value)
  }
  invisible(private$value)
}
