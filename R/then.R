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
#' @param onFulfilled A function that will be invoked if the promise value
#'   successfully resolves. When invoked, the function will be called with a
#'   single argument: the resolved value. Optionally, the function can take a
#'   second parameter `.visible` if you care whether the promise was resolved
#'   with a visible or invisible value. The function can return a value or a
#'   promise object, or can throw an error; these will affect the resolution of
#'   the promise object that is returned by `then()`.
#' @param onRejected A function taking the argument `error`. The function can
#'   return a value or a promise object, or can throw an error. If `onRejected`
#'   is provided and doesn't throw an error (or return a promise that fails)
#'   then this is the async equivalent of catching an error.
#' @param ... Ignored.
#' @param tee If `TRUE`, ignore the return value of the callback, and use the
#'   original value instead. This is useful for performing operations with
#'   side-effects, particularly logging to the console or a file. If the
#'   callback itself throws an error, and `tee` is `TRUE`, that error will still
#'   be used to fulfill the the returned promise (in other words, `tee` only has
#'   an effect if the callback does not throw).
#' @export
then <- function(
  promise,
  onFulfilled = NULL,
  onRejected = NULL,
  ...,
  tee = FALSE
) {
  check_dots(...)
  check_tee(tee)

  promise <- as.promise(promise)

  if (!is.null(onFulfilled) && !is.function(onFulfilled)) {
    onFulfilled <- rlang::as_function(onFulfilled)
  }
  if (!is.null(onRejected) && !is.function(onRejected)) {
    onRejected <- rlang::as_function(onRejected)
  }
  then_prom <-
    if (!tee) {
      promise$then(onFulfilled = onFulfilled, onRejected = onRejected)
    } else {
      # tee == TRUE

      # Must normalize the callbacks to ensure they accept their arguments

      onFulfilledTee <- NULL
      if (!is.null(onFulfilled)) {
        onFulfilled <- normalizeOnFulfilled(onFulfilled)
        onFulfilledTee <- function(value, .visible) {
          onFulfilled(value, .visible)
          if (.visible) value else invisible(value)
        }
      }

      onRejectedTee <- NULL
      if (!is.null(onRejected)) {
        onRejected <- normalizeOnRejected(onRejected)
        onRejectedTee <- function(reason) {
          onRejected(reason)
          stop(reason) # Re-throw the error to propagate it
        }
      }

      promise$then(
        onFulfilled = onFulfilledTee,
        onRejected = onRejectedTee
      )
    }
  invisible(then_prom)
}

#' @rdname then
#' @export
catch <- function(promise, onRejected, ..., tee = FALSE) {
  check_dots(...)
  check_tee(tee)

  promise <- as.promise(promise)

  if (!is.null(onRejected) && !is.function(onRejected)) {
    onRejected <- rlang::as_function(onRejected)
  }

  if (!tee) {
    promise$catch(onRejected)
  } else {
    onRejected <- normalizeOnRejected(onRejected)
    promise$catch(function(reason) {
      onRejected(reason)
      stop(reason)
    })
  }
}

#' @param onFinally A function with no arguments, to be called when the async
#'   operation either succeeds or fails. Usually used for freeing resources that
#'   were used during async operations.
#' @rdname then
#' @export
finally <- function(promise, onFinally) {
  promise <- as.promise(promise)

  if (!is.null(onFinally) && !is.function(onFinally)) {
    onFinally <- rlang::as_function(onFinally)
  }
  promise$finally(onFinally)
}

check_dots <- function(..., call = parent.frame()) {
  if (nargs()) {
    rlang::check_dots_empty(call = call)
  }
}

check_tee <- function(tee) {
  if (is.logical(tee)) {
    return()
  }

  rlang::abort(
    "`tee` must be `TRUE` or `FALSE`",
    call = parent.frame()
  )
}
check_hybrid_callback <- function(callback, name) {
  if (!is.null(callback) && !is.function(callback)) {
    stop(sprintf("`%s=` must be a function or `NULL`", name))
  }
}


#' Asynchronous or synchronous `then()`
#'
#' This is a helper function that behaves like `then`, however if
#' `is.promising()` returns `FALSE` then the handlers will be executed
#' immediately.
#'
#' Execution paths:
#' * If `expr` evaluates to a promise (`p`), it will call `p |> then(on_success,
#'   on_failure)`.
#' * If `expr` evaluates to a non-promise value (`x`), it will call
#'   `on_success(x)`.
#' * If `expr` throws an error (`e`) during calculation, it will call
#'   `on_failure(e)`.
#'
#' In all cases, the `on_success` and `on_failure` callbacks are executed (when
#' provided).
#'
#' @section Utility:
#'
#' This function is useful for writing functions that need to execute followup
#' behavior _now_ or within a promise. This is different behavior than `then()`
#' where *everything* is made into a promise.
#'
#' `hybrid_then()` allows authors to keep synchronous execution on the same
#' _tick_ without requiring the use of a followup promise. This is particularly
#' appealing for situations where the author does not control the execution flow
#' for items that may be either synchronous or asynchronous, such as within
#' `{plumber2}`.
#'
#' @section Error Handling:
#'
#' If no `on_failure` callback is provided and an error occurs, the error is
#' re-thrown immediately (for synchronous errors) or propagated through the
#' returned promise (for asynchronous errors).
#'
#' If an `on_failure` callback is provided but it throws an error, that new
#' error replaces the original error. With `tee = TRUE`, even if `on_failure`
#' executes successfully, the original error is still re-thrown.
#'
#' @section Callback Return Values:
#'
#' Callbacks can return any value, including promises. If a callback returns a
#' promise, the entire `hybrid_then()` call will return a promise, even if the
#' input was synchronous. This allows seamless transitions between synchronous
#' and asynchronous execution.
#'
#' @param expr An expression that evaluates to either a promise or a non-promise
#'   value.
#' @param on_success A function to be called when no error occurs synchronously
#'   or asynchronously. When invoked, the function will be called with a single
#'   argument: the resolved value. Optionally, the function can take a second
#'   parameter `.visible` if you care whether the promise was resolved with a
#'   visible or invisible value. Can return a value or a promise.
#' @param on_failure A function to be called if an error occurs synchronously or
#'   asynchronously. Takes one argument: the error object. Can return a value or
#'   a promise to recover from the error, or throw a new error. If `on_failure`
#'   is provided and doesn't throw an error (or return a promise that fails)
#'   then this is the async equivalent of catching an error.
#' @param ... Reserved for future use. Currently must be empty.
#' @param tee If `TRUE`, ignore the return value of the callback, and use the
#'   original value of `expr` as the result. For `on_failure` with `tee = TRUE`,
#'   the callback executes but the original error is re-thrown afterward.
#' @return
#'   * If `expr` evaluates to a promise, a promise with a single followup
#'     promise to handle the `on_success` or `on_failure` callbacks.
#'   * If `expr` evaluates to a non-promise value, the result of the synchronous
#'     operation after being processed by `on_success` or `on_failure`.
#'   * If a callback returns a promise, the result is always a promise.
#' @seealso [then()], [is.promising()], [promise_resolve()]
#' @export
#' @examples
#' # Basic usage - works with both sync and async values
#' add_to <- function(x, k) {
#'   hybrid_then(
#'     x,
#'     on_success = function(value) {
#'       value + k
#'     },
#'     on_failure = function(err) {
#'       message("Error: ", err$message)
#'       NA_real_
#'     }
#'   )
#' }
#'
#' # Synchronous
#' 42 |> add_to(100)
#' #> [1] 142
#'
#' # Synchronous error
#' add_to({stop("Bad input!")}, 8)
#' #> Error: Bad input!
#' #> [1] NA
#'
#' \dontrun{
#' # Asynchronous
#' promise_resolve(42) |>
#'   add_to(8) |>
#'   then(print)
#' # When resolved...
#' #> [1] 50
#'
#' # Error handling - asynchronous
#' promise_resolve(stop("Bad async input!")) |>
#'   add_to(8) |>
#'   then(print)
#' # When resolved...
#' #> Error: Bad async input!
#' #> [1] NA
#'
#' # Chaining multiple operations
#' # (Move the `promise_resolve()` around to see sync vs async behavior)
#' 1 |>
#'   hybrid_then(on_success = \(x) x + 1) |>
#'   hybrid_then(on_success = \(x) promise_resolve(x * 2)) |>
#'   hybrid_then(on_success = \(x) x - 1) |>
#'   hybrid_then(print)
#' # When resolved...
#' #> [1] 3
#' }
hybrid_then <- function(
  expr,
  on_success = NULL,
  on_failure = NULL,
  ...,
  tee = FALSE
) {
  check_dots(...)
  check_tee(tee)

  check_hybrid_callback(on_success, "on_success")
  check_hybrid_callback(on_failure, "on_failure")

  # Add support for `.visible`
  on_success <- normalizeOnFulfilled(on_success)
  on_failure <- normalizeOnRejected(on_failure)

  result_is_sync_error <- FALSE

  result_visible <-
    withVisible(
      tryCatch(
        expr,
        error = function(e) {
          # If no `on_failure` callback, re-throw the error
          if (is.null(on_failure)) {
            stop(e)
          }

          # Perform synchronous `on_failure` callback now
          result_on_error <- withVisible(on_failure(e))
          if (tee) {
            # Re-throw the error
            stop(e)
          } else {
            result_is_sync_error <<- TRUE
            return_with_visible(result_on_error)
          }
        }
      )
    )

  if (result_is_sync_error) {
    # Return synchronous `tee=FALSE` `result_on_error` from above
    return(return_with_visible(result_visible))
  }

  if (is.promising(result_visible$value)) {
    # Return async result
    # Will handle async success, failure callbacks (and `tee`)
    then(return_with_visible(result_visible), on_success, on_failure, tee = tee)
  } else {
    # If no `on_success` callback, return sync result now
    if (is.null(on_success)) {
      return(return_with_visible(result_visible))
    }

    # Peform synchronous `on_success` callback now
    result_on_success_visible <-
      withVisible(
        on_success(
          result_visible$value,
          result_visible$visible
        )
      )

    # Return synchronous result
    if (tee) {
      return_with_visible(result_visible)
    } else {
      return_with_visible(result_on_success_visible)
    }
  }
}

return_with_visible <- function(visible_info) {
  if (visible_info$visible) {
    visible_info$value
  } else {
    invisible(visible_info$value)
  }
}
