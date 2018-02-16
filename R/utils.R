#' Combine multiple promise objects
#'
#' Use `promise_all` to wait for multiple promise objects to all be successfully
#' fulfilled. Use `promise_race` to wait for the first of multiple promise
#' objects to be either fulfilled or rejected.
#'
#' @param ... Promise objects. Either all arguments must be named, or all
#'   arguments must be unnamed. If `.list` is provided, then these arguments are
#'   ignored.
#' @param .list A list of promise objects--an alternative to `...`.
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
#' \dontrun{
#' p1 <- promise(~later::later(~resolve(1), delay = 1))
#' p2 <- promise(~later::later(~resolve(2), delay = 2))
#'
#' # Resolves after 1 second, to the value: 1
#' promise_race(p1, p2) %...>% {
#'   cat("promise_race:\n")
#'   str(.)
#' }
#'
#' # Resolves after 2 seconds, to the value: list(1, 2)
#' promise_all(p1, p2) %...>% {
#'   cat("promise_all:\n")
#'   str(.)
#' }
#' }
#'
#' @export
promise_all <- function(..., .list = NULL) {
  if (missing(.list)) {
    .list <- list(...)
  }

  if (length(.list) == 0) {
    return(promise(~resolve(list())))
  }

  # Verify that .list members are either all named or all unnamed
  nameCount <- sum(nzchar(names(.list)))
  if (nameCount != 0 && nameCount != length(.list)) {
    stop("promise_all expects promise arguments (or list) to be either all named or all unnamed")
  }

  done <- list()
  results <- list()

  promise(function(resolve, reject) {
    keys <- if (is.null(names(.list))) {
      1:length(.list)
    } else {
      names(.list)
    }

    lapply(keys, function(key) {
      done[[key]] <<- FALSE
      # Forces correct/deterministic ordering of the result list's elements
      results[[key]] <<- NA

      then(.list[[key]],
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
promise_race <- function(..., .list = NULL) {
  if (missing(.list)) {
    .list <- list(...)
  }

  promise(function(resolve, reject) {
    lapply(.list, function(promise) {
      then(promise, resolve, reject)
    })
  })
}

#' Promise-aware lapply/map
#'
#' Similar to [`base::lapply()`] or [`purrr::map`], but promise-aware: the `.f`
#' function is permitted to return promises, and while `lapply` returns a list,
#' `promise_map` returns a promise that resolves to a similar list (of resolved
#' values only, no promises).
#'
#' `promise_map` processes elements of `.x` serially; that is, if `.f(.x[[1]])`
#' returns a promise, then `.f(.x[[2]])` will not be invoked until that promise
#' is resolved. If any such promise rejects (errors), then the promise returned
#' by `promise_map` immediately rejects with that err.
#'
#' @param .x A vector (atomic or list) or an expression object (but not a
#'   promise). Other objects (including classed objects) will be coerced by
#'   base::as.list.
#' @param .f The function to be applied to each element of `.x`. The function is
#'   permitted, but not required, to return a promise.
#' @param ... Optional arguments to `.f`.
#' @return A promise that resolves to a list (of values, not promises).
#'
#' @examples
#' \dontrun{
#' # Waits x seconds, then returns x*10
#' wait_this_long <- function(x) {
#'   promise(~later::later(~{
#'     resolve(x*10)
#'   }, delay = x))
#' }
#'
#' promise_map(list(A=1, B=2, C=3), wait_this_long) %...>%
#'   print()
#' }
#'
#' @export
promise_map <- function(.x, .f, ...) {
  .f <- match.fun(.f)
  if (!is.vector(.x) || is.object(.x))
    .x <- as.list(.x)
  x_names <- names(.x)
  results <- vector("list", length(.x))

  do_next <- function(pos) {
    if (pos > length(results)) {
      return(stats::setNames(results, x_names))
    } else {
      # The next line may throw, that's fine, it will be caught by resolve() and
      # reject the promise
      this_result <- .f(.x[[pos]], ...)
      promise_resolve(this_result) %...>%
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

#' Promise-aware version of Reduce
#'
#' Similar to [`purrr::reduce`] (left fold), but the function `.f` is permitted
#' to return a promise. `promise_reduce` will wait for any returned promise to
#' resolve before invoking `.f` with the next element; in other words, execution
#' is serial. `.f` can return a promise as output but should never encounter a
#' promise as input (unless `.x` itself is a list of promises to begin with, in
#' which case the second parameter would be a promise).
#'
#' @param .x A vector or list to reduce. (Not a promise.)
#' @param .f A function that takes two parameters. The first parameter will be
#'   the "result" (initially `.init`, and then set to the result of the most
#'   recent call to `func`), and the second parameter will be an element of `.x`.
#' @param ... Other arguments to pass to `.f`
#' @param .init The initial result value of the fold, passed into `.f` when it
#'   is first executed.
#'
#' @return A promise that will resolve to the result of calling `.f` on the last
#'   element (or `.init` if `.x` had no elements). If any invocation of `.f`
#'   results in an error or a rejected promise, then the overall
#'   `promise_reduce` promise will immediately reject with that error.
#'
#' @examples
#' \dontrun{
#' # Returns a promise for the sum of e1 + e2, with a 0.5 sec delay
#' slowly_add <- function(e1, e2) {
#'   promise(~later::later(~resolve(e1 + e2), delay = 0.5))
#' }
#'
#' # Prints 55 after a little over 5 seconds
#' promise_reduce(1:10, slowly_add, .init = 0) %...>% print()
#' }
#'
#' @export
promise_reduce <- function(.x, .f, ..., .init) {
  p <- promise_resolve(.init)
  lapply(.x, function(item) {
    p <<- p %...>% .f(item, ...)
  })
  p
}
