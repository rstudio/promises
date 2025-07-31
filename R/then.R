#' Access the results of a promise
#'
#' Use the `then` function to access the eventual result of a promise (or, if
#' the operation fails, the reason for that failure). Regardless of the state of
#' the promise, the call to `then` is non-blocking, that is, it returns
#' immediately; so what it does *not* do is immediately return the result value
#' of the promise. Instead, you pass logic you want to execute to `then`, in the
#' form of function callbacks. If you provide an
#' `onFulfilled` callback, it will be called upon the promise's successful
#' resolution, with a single argument `value`: the result value. If you provide
#' an `onRejected` callback, it will be called if the operation fails, with a
#' single argument `reason`: the error that caused the failure.
#'
#' @section Formulas:
#'
#' `r lifecycle::badge("superseded")`
#'
#' With `{promises}` depending on R >= 4.1, the shorthand of a formula, `~
#' fn(.)` is no longer recommended by the `{promises}` package or tidyverse (for
#' example,
#' [`{purrr}`](https://github.com/tidyverse/purrr/commit/670c3ed9920f15da0d4175068ecddc41f0f1f335#diff-c4dcc43795da5c7f6bf5a94d957b5507ce795fedd6d3eb092ccad03678c4f76dR15))
#' as we now have access to the function shorthand, `\(x) fn(x)`. Please update
#' your code to use the new function shorthand syntax `\(x) fn(x, arg1, args2)`
#' instead of `~ fn(., arg1, arg2)`. The `.` can be confusing when chained with
#' other methods.
#'
#' @section Chaining promises:
#'
#' The first parameter of `then` is a promise; given the stated purpose of the
#' function, this should be no surprise. However, what may be surprising is that
#' the return value of `then` is also a (newly created) promise. This new
#' promise waits for the original promise to be fulfilled or rejected, and for
#' `onFulfilled` or `onRejected` to be called. The result of (or error raised
#' by) calling `onFulfilled`/`onRejected` will be used to fulfill (reject) the
#' new promise.
#'
#' ```r
#' promise_a <- get_data_frame_async()
#' promise_b <- then(promise_a, onFulfilled = head)
#' ```
#'
#' In this example, assuming `get_data_frame_async` returns a promise that
#' eventually resolves to a data frame, `promise_b` will eventually resolve to
#' the first 10 or fewer rows of that data frame.
#'
#' Note that the new promise is considered fulfilled or rejected based on
#' whether `onFulfilled`/`onRejected` returns a value or throws an error, not on
#' whether the original promise was fulfilled or rejected. In other words, it's
#' possible to turn failure to success and success to failure. Consider this
#' example, where we expect `some_async_operation` to fail, and want to consider
#' it an error if it doesn't:
#'
#' ```r
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
#' Now, `promise_d` will be rejected if `promise_c` is fulfilled, and vice
#' versa.
#'
#' **Warning:** Be very careful not to accidentally turn failure into success,
#' if your error handling code is not the last item in a chain!
#'
#' ```r
#' some_async_operation() |>
#'   catch(function(reason) {
#'     warning("An error occurred: ", reason)
#'   }) |>
#'   then(function() {
#'     message("I guess we succeeded...?")  # No!
#'   })
#' ```
#'
#' In this example, the `catch` callback does not itself throw an error, so the
#' subsequent `then` call will consider its promise fulfilled!
#'
#' @section Convenience functions:
#'
#' For readability and convenience, we provide `catch` and `finally` functions.
#'
#' The `catch` function is equivalent to `then`, but without the `onFulfilled`
#' argument. It is typically used at the end of a promise chain to perform error
#' handling/logging.
#'
#' The `finally` function is similar to `then`, but takes a single no-argument
#' function that will be executed upon completion of the promise,
#' regardless of whether the result is success or failure. It is typically used
#' at the end of a promise chain to perform cleanup tasks, like closing file
#' handles or database connections. Unlike `then` and `catch`, the return value
#' of `finally` is ignored; however, if an error is thrown in `finally`, that
#' error will be propagated forward into the returned promise.
#'
#' @section Visibility:
#'
#' `onFulfilled` functions can optionally have a second parameter `visible`,
#' which will be `FALSE` if the result value is [invisible][base::invisible()].
#'
#' @param promise A promise object. The object can be in any state.
#'
#' @param onFulfilled A function that will be invoked if the promise value
#'   successfully resolves. When invoked, the function will be called with a
#'   single argument: the resolved value. Optionally, the function can take a
#'   second parameter `.visible` if you care whether the promise was resolved
#'   with a visible or invisible value. The function can return a value or a
#'   promise object, or can throw an error; these will affect the resolution of
#'   the promise object that is returned by `then()`.
#'
#' @param onRejected A function taking the argument `error`. The function can
#'   return a value or a promise object, or can throw an error. If `onRejected`
#'   is provided and doesn't throw an error (or return a promise that fails)
#'   then this is the async equivalent of catching an error.
#' @param ... Ignored.
#' @export
then <- function(
  promise,
  onFulfilled = NULL,
  onRejected = NULL,
  ...,
  tee = FALSE
) {
  check_dots_empty()
  check_bool(tee)

  promise <- as.promise(promise)

  if (!is.null(onFulfilled)) {
    onFulfilled <- as_function(onFulfilled)
  }
  if (!is.null(onRejected)) {
    onRejected <- as_function(onRejected)
  }
  then_prom <-
    if (!tee) {
      promise$then(onFulfilled = onFulfilled, onRejected = onRejected)
    } else {
      promise$then(
        onFulfilled = function(value, visible) {
          onFulfilled(value, visible)

          if (visible) {
            value
          } else {
            invisible(value)
          }
        },
        onRejected = function(reason) {
          onRejected(reason)
          stop(reason) # Re-throw the error to propagate it
        }
      )
    }
  invisible(then_prom)
}

#' @param tee If `TRUE`, ignore the return value of the callback, and use the
#'   original value instead. This is useful for performing operations with
#'   side-effects, particularly logging to the console or a file. If the
#'   callback itself throws an error, and `tee` is `TRUE`, that error will still
#'   be used to fulfill the the returned promise (in other words, `tee` only has
#'   an effect if the callback does not throw).
#' @rdname then
#' @export
catch <- function(promise, onRejected, ..., tee = FALSE) {
  check_dots_empty()
  promise <- as.promise(promise)
  check_bool(tee)

  if (!is.null(onRejected)) {
    onRejected <- as_function(onRejected)
  }

  if (!tee) {
    return(promise$catch(onRejected))
  } else {
    promise$catch(function(reason) {
      onRejected(reason)
      stop(reason)
    })
  }
}

#' @rdname then
#'
#' @param onFinally A function with no arguments, to be called when the async
#'   operation either succeeds or fails. Usually used for freeing resources that
#'   were used during async operations.
#'
#' @export
finally <- function(promise, onFinally) {
  promise <- as.promise(promise)

  if (!is.null(onFinally)) {
    onFinally <- as_function(onFinally)
  }
  promise$finally(onFinally)
}
