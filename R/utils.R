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
