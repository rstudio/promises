#' Combine multiple promise objects
#'
#' Use `promise_all` to wait for multiple promise objects to all be successfully
#' fulfilled. Use `promise_race` to wait for the first of multiple promise
#' objects to be either fulfilled or rejected.
#'
#' @param ... Promise objects. Either all arguments must be named, or all
#'   arguments must be unnamed. If `list` is provided, then these arguments are
#'   ignored.
#' @param list A list of promise objects--an alternative to `...`.
#'
#' @return A promise.
#'
#'   For `promise_all`, if all of the promises were successful, the returned
#'   promise will resolve to a list of the promises' values; if any promise
#'   fails, the first error to be encountered will be used to reject the
#'   returned promise.
#'
#'   For `promise_race`, the first of the promises to either fulfill or reject
#'   will be passed through to the returned promise.
#'
#' @examples
#' # TODO
#'
#' @export
promise_all <- function(..., list = NULL) {
  if (missing(list)) {
    list <- list(...)
  }

  if (length(list) == 0) {
    return(promise(~resolve(list())))
  }

  # Verify that list members are either all named or all unnamed
  nameCount <- sum(nzchar(names(list)))
  if (nameCount != 0 && nameCount != length(list)) {
    stop("promise_all expects promise arguments (or list) to be either all named or all unnamed")
  }

  done <- list()
  results <- list()

  promise(function(resolve, reject) {
    keys <- if (is.null(names(list))) {
      1:length(list)
    } else {
      names(list)
    }

    lapply(keys, function(key) {
      done[[key]] <<- FALSE

      then(list[[key]],
        onFulfilled = function(value) {
          # Save the result so we can return it to the user.
          results[[key]] <<- value

          # Record the fact that the promise was completed.
          done[[key]] <<- TRUE
          # If all of the tasks are done, resolve.
          if (all(as.logical(done))) {
            resolve(results)
          }
        },
        onRejected = function(reason) {
          # TODO: Cancel promises that are still running
          reject(reason)
        }
      )
    })
  })
}

#' @rdname promise_all
#' @export
promise_race <- function(..., list = NULL) {
  if (missing(list)) {
    list <- list(...)
  }

  promise(function(resolve, reject) {
    lapply(list, function(promise) {
      then(promise, resolve, reject)
    })
  })
}

#' @param X A vector (atomic or list) or an expression object. Other objects
#'   (including classed objects) will be coerced by base::as.list.
#' @param FUN The function to be applied to each element of `X`. The function is
#'   permitted, but not required, to return a promise.
#' @rdname promise_all
#' @export
promise_lapply <- function(X, FUN, ...) {
  FUN <- match.fun(FUN)
  if (!is.vector(X) || is.object(X))
    X <- as.list(X)
  X_names <- names(X)
  results <- vector("list", length(X))

  do_next <- function(pos) {
    if (pos > length(results)) {
      return(stats::setNames(results, X_names))
    } else {
      this_result <- FUN(X[[pos]], ...)
      as.promise(this_result) %...>%
        (function(this_value) {
          results[[pos]] <<- this_value
          do_next(pos + 1)
        })
    }
  }

  promise(function(resolve, reject) {
    resolve(do_next(1))
  })
}
