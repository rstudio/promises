#' @export
promise_all <- function(..., list = NULL) {
  if (missing(list)) {
    list <- list(...)
  }

  # TODO: Verify that list is either all named or all unnamed

  if (length(list) == 0) {
    return(resolved(list()))
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
