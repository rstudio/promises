#' Access the results of a promise
#'
#' Use the `then` function to access the eventual result of a promise (or, if the operation fails, the reason for that failure). Regardless of the state of the promise, the call to `then` is non-blocking, that is, it returns immediately; so what it does *not* do is immediately return the result value of the promise. Instead, you pass logic you want to execute to `then`, in the form of function callbacks (or formulas, see Details). If you provide an `onFulfilled` callback, it will be called upon the promise's successful resolution, with a single argument `value`: the result value If you provide an `onRejected` callback, it will be called if the operation fails, with a single argument `reason`: the error that caused the failure.
#'
#' @section Formulas:
#'
#' @section Chaining promises:
#'
#' The first parameter of `then` is a promise; given the stated purpose of the function, this should be no surprise. However, what may be surprising is that the return value of `then` is also a (newly created) promise. This new promise waits for the original promise to be fulfilled or rejected, and for `onFulfilled` or `onRejected` to be called. The result of (or error raised by) calling `onFulfilled`/`onRejected` will be used to fulfill (reject) the new promise.
#'
#' ```
#' promise_a <- get_data_frame_async()
#' promise_b <- then(promise_a, onFulfilled = head)
#' ```
#'
#' In this example, assuming `get_data_frame_async` returns a promise that eventually resolves to a data frame, `promise_b` will eventually resolve to the first 10 or fewer rows of that data frame.
#'
#' Note that the new promise is considered fulfilled or rejected based on whether `onFulfilled`/`onRejected` returns a value or throws an error, not on whether the original promise was fulfilled or rejected. In other words, it's possible to turn failure to success and success to failure. Consider this example, where we expect `some_async_operation` to fail, and want to consider it an error if it doesn't:
#'
#' ```
#' promise_c <- some_async_operation()
#' promise_d <- then(promise_c,
#'   onFulfilled = function(value) {
#'     stop("That's strange, the operation didn't fail!")
#'   },
#'   onRejected = function(reason) {
#'     # Great, the operation failed as expected
#'     NULL
#'   }
#' )
#' ```
#'
#' Now, `promise_d` will be rejected if `promise_c` is fulfilled, and vice versa.
#'
#' **Warning:** Be very careful not to accidentally turn failure into success, if your error handling code is not the last item in a chain!
#'
#' ```
#' some_async_operation() %>%
#'   catch(function(reason) {
#'     warning("An error occurred: ", reason)
#'   }) %>%
#'   then(function() {
#'     message("I guess we succeeded...?")  # No!
#'   })
#' ```
#'
#' In this example, the `catch` callback does not itself throw an error, so the subsequent `then` call will consider its promise fulfilled!
#'
#' @section Convenience functions:
#'
#' For readability and convenience, we provide `catch` and `finally` functions.
#'
#' The `catch` function is equivalent to `then`, but without the `onFulfilled` argument. It is typically used at the end of a promise chain to perform error handling/logging.
#'
#' The `finally` function is similar to `then`, but takes a single no-argument function (or formula) that will be executed upon completion of the promise, regardless of whether the result is success or failure. It is typically used at the end of a promise chain to perform cleanup tasks, like closing file handles or database connections. Unlike `then` and `catch`, the return value of `finally` is ignored; however, if an error is thrown in `finally`, that error will be propagated forward into the returned promise.
#'
#' @param onFulfilled A function taking the argument `value` (or a formula--see
#'   Details). The function can return a value
#'
#' @export
then <- function(promise, onFulfilled = NULL, onRejected = NULL) {
  if (!is.null(onFulfilled))
    onFulfilled <- rlang::as_function(onFulfilled)
  if (!is.null(onRejected))
    onRejected <- rlang::as_function(onRejected)
  promise$then(onFulfilled = onFulfilled, onRejected = onRejected)
}

#' @rdname then
#' @export
catch <- function(promise, onRejected) {
  if (!is.null(onRejected))
    onRejected <- rlang::as_function(onRejected)
  promise$catch(onRejected)
}

#' @rdname then
#' @export
finally <- function(promise, onFinally) {
  if (!is.null(onFinally))
    onFinally <- rlang::as_function(onFinally)
  promise$finally(onFinally)
}
