debug_msg_can_print <- FALSE
debug_msg <- function(...) {
  if (debug_msg_can_print) {
    message(...)
  }
}

assert_work_queue_pkgs <- local({
  val <- NULL
  function() {
    if (!is.null(val)) {
      return()
    }
    for (pkg in list(
      list(name = "future", version = "1.21.0"),
      list(name = "fastmap", version = "1.1.0")
    )) {
      if (!is_installed(pkg$name, pkg$version)) {
        stop(
          "Package `",
          pkg$name,
          "` (",
          pkg$version,
          ") needs to be installed"
        )
      }
    }
    val <<- TRUE
    return()
  }
})
future_worker_is_free <- function() {
  future::nbrOfFreeWorkers() > 0
}


Delay <- R6::R6Class(
  "Delay",
  private = list(
    delay_count = 0
  ),
  public = list(
    is_reset = function() {
      private$delay_count == 0
    },
    reset = function() {
      private$delay_count <- 0
    },

    increase = function() {
      private$delay_count <- private$delay_count + 1
    },

    delay = function() {
      stop("$delay() not implemented")
    }
  ),
  active = list(
    count = function() {
      private$delay_count
    }
  )
)

ExpoDelay <- R6::R6Class(
  "ExpoDelay",
  inherit = Delay,
  private = list(
    base = 1 / 100,
    min_seconds = 0.01,
    max_seconds = 2
  ),
  public = list(
    initialize = function(
      base = 1 / 100,
      min_seconds = 0.01,
      max_seconds = 2
    ) {
      stopifnot(length(base) == 1 && is.numeric(base) && base >= 0)
      stopifnot(
        length(min_seconds) == 1 && is.numeric(min_seconds) && min_seconds >= 0
      )
      stopifnot(
        length(max_seconds) == 1 && is.numeric(max_seconds) && max_seconds >= 0
      )

      private$base <- base
      private$max_seconds <- max_seconds
      private$min_seconds <- min_seconds

      self
    },

    # return number of milliseconds until next attempt
    # will randomly backoff to avoid extra work on long poll times
    delay = function() {
      # calculate expo backoff value
      expo_val <- private$base * ((2^private$delay_count) - 1)
      # find random value
      random_val <- runif(n = 1, max = min(private$max_seconds, expo_val))
      # perform `min()` on second step to avoid `runif(1, min = 5, max = 4)` which produces `NaN`
      max(private$min_seconds, random_val)
    }
  )
)


# Situations
# √ No future workers are busy. All future calls are `future_promises()`
#  * Can be accomplished using future job followup promise which calls `$attempt_work()`
# √ All future workers are busy with other tasks (but will become available).
#  * Require using delay
# √ While processing the first batch, existing future workers are taken over
#  * Require delay

# FIFO queue of workers
#' Future promise work queue
#'
#'
#' An \pkg{R6} class to help with scheduling work to be completed. `WorkQueue` will only execute work if the `can_proceed()` returns `TRUE`. For the use case of `future`, `can_proceed()` defaults to `future::nbrOfFreeWorkers() > 0` which will not allow for work to be executed if a \pkg{future} worker is not available.
#'
#' `WorkQueue` will constantly try to start new work once prior work item finishes.  However, if `can_proceed()` returns `FALSE` (no future workers are available) and there is more work to be done, then work is attempted later a random amount of time later using exponential backoff.  The exponential backoff will cap out at 10 seconds to prevent unnecessarily large wait times.
#'
#' Each time `WorkQueue` tries to start more work, it will repeat until `can_proceed()` returns `FALSE` or there is no more work in the `queue`.
#'
#' @section Global event loop:
#'
#' The global loop is used by default as the internal `WorkQueue` "delayed check" uses a single delay check for the whole queue, rather than having each item in the queue attempt to process.
#' This behavior might change in the future, but we are not exactly sure how at this point.
#'
#' If a private `later` loop wants to become synchronous by running until all jobs are completed but is waiting on a `future_promise()`, the private loop will not complete unless the global loop is allowed to move forward.
#'
#' However, it is possible to use a private loop inside a user-defined `WorkQueue` may work which can be provided directly to `future_promise(queue=custom_queue)`. Having a concrete example (or need) will help us understand the problem better. If you have an example, please reach out .
#'
#' @seealso [future_promise_queue()] which returns a `WorkQueue` which is cached per R session.
#' @keywords internal
WorkQueue <- R6::R6Class(
  "WorkQueue",

  # TODO - private loop proposal:
  # The queued data would actually be a list of queues whose _key_ matches
  # the loop ID. This would require that `schedule_work()` take in `loop` and have each loop have its own queue.
  # The scheduled work in each queue would contain the `work` function and `submission_time`.
  # Once `can_proceed_fn()` returns TRUE, the queue with the earliest `submission_time` should be processed.
  # This concept is similar to a merge sort when trying to merge two pre-sorted lists.
  # Check time: O(k), k = number of later loops ever registered. (This could become big!)
  # Maybe, if `ID != 0`, the queue is removed if the number of elements goes to 0.
  # Check time: O(kk), kk <= k, kk = number of _active_ later loops.
  #
  # Thought process, let's say chromote used the global WorkQueue
  # and wanted to have its local loop synchronize (.. while(later::loop_empty(local_loop)) later::run_now(local_loop))  ..).
  # [ ] In `do_work()`, Get loop from `later::current_loop()`
  #   * If is global loop, get item with earliest submission time
  #   * If is private loop (ID != 0), get first item in private queue
  # [ ] Validate that an work item finishing in loop X, has a promise created using loop X
  # [ ] Would each private queue need its own delay check?
  #
  #
  # Implementation question:
  # * Would it be better if we have a larger WorkQueue class that managed many WorkQueues for each loop?
  #    * This would allow for each loop to have its own delay check, delay counter, and loop
  private = list(
    queue = "fastmap::fastqueue()",
    can_proceed_fn = "future_worker_is_free()",
    can_proceed_cache_val = NULL,
    # only _really_ used for delay checking
    loop = "later::global_loop()",
    delay = "ExpoDelay$new()",

    # Used as a semaphore to make sure only 1 call of `attempt_work()` is delayed
    cancel_delayed_attempt_work = NULL,

    # Increment delay. Used with ExpoDelay
    increase_delay = function() {
      debug_msg("increase_delay()...", private$delay$count)
      # Increment delay and try again later
      private$delay$increase()
    },

    # Reset delay and cancel any delayed `attempt_work()` calls
    reset_delay = function() {
      debug_msg("reset_delay()")
      private$delay$reset()

      # disable any delayed executions of `attempt_work`
      if (is.function(private$cancel_delayed_attempt_work)) {
        private$cancel_delayed_attempt_work()
      }
      private$cancel_delayed_attempt_work <- NULL
    },

    # Returns a logical which let's work begin
    can_proceed = function() {
      can_proceed_cache_val <- private$can_proceed_cache_val
      # Return early if no work can be submitted.
      if (!is.null(can_proceed_cache_val)) {
        return(can_proceed_cache_val)
      }

      ret <- isTRUE(private$can_proceed_fn())

      # If we can no longer proceed, then we should avoid asking if the status has changed within the current {later} loop execution
      # The value will be reset after have giving {future} the possibility to update the number of available workers
      # While there is a possibility that a fast {future} job could finish in the middle of submitting many `future_promise()` job requests,
      #   the time saved by not asking the {future} cluster many many status questions is faster overall
      # https://github.com/rstudio/promises/pull/78
      if (is_false(ret)) {
        private$can_proceed_cache_val <- FALSE
        # Reset `$can_proceed()` functionality in the next event loop execution
        later::later(
          loop = private$loop,
          delay = 0,
          function() {
            private$can_proceed_cache_val <- NULL
          }
        )
      }
      ret
    },

    # Function to attempt as much work as possible
    # If no workers are available and a queue has elements,
    #   If a delayed check has already been registered, Return
    #   Else, check again after some delay
    attempt_work = function(can_delay = FALSE) {
      debug_msg('attempt_work()')
      # If nothing to start, return early
      if (private$queue$size() == 0) {
        return()
      }

      # If we are not waiting on someone else, we can do work now
      while ((private$queue$size() > 0) && private$can_proceed()) {
        # Do work right away
        private$reset_delay()
        private$do_work()
      }

      # If there are still items to be processed, but we can not proceed...
      if (private$queue$size() > 0 && !private$can_proceed()) {
        # If we are allowed to delay (default FALSE), or nothing is currently delaying
        if (can_delay || is.null(private$cancel_delayed_attempt_work)) {
          # Try again later
          private$increase_delay()
          private$cancel_delayed_attempt_work <-
            later::later(
              loop = private$loop,
              delay = private$delay$delay(),
              function() {
                private$attempt_work(can_delay = TRUE)
              }
            )
        }
      }
    },

    # Actually process an item in the queue
    do_work = function() {
      debug_msg("do_work()")

      # Get first item in queue
      work_fn <- private$queue$remove()

      # Safety check...
      # If nothing is returned, no work to be done. Return early
      if (!is.function(work_fn)) {
        return()
      }

      # Do scheduled work
      debug_msg("execute work")
      future_job <- work_fn()

      # Try to attempt work immediately after the future job has finished
      # (whether it succeeds or fails doesn't matter)
      continue_work <- function() {
        debug_msg("finished work. queue size: ", private$queue$size())
        private$attempt_work()
      }

      # We're not using finally() here because we don't want rejections to be
      # propagated (which would result in a warning). Any warnings will be
      # handled by the user using a different promise object.
      # https://github.com/rstudio/promises/issues/86
      then(future_job, onFulfilled = continue_work, onRejected = continue_work)

      return()
    }
  ),
  public = list(
    #' @description Create a new `WorkQueue`
    #' @param can_proceed Function that should return a logical value. If `TRUE` is returned, then the next scheduled work will be executed. By default, this function checks if \code{\link[future:nbrOfWorkers]{future::nbrOfFreeWorkers()} > 0}
    #' @param queue Queue object to use to store the scheduled work. By default, this is a "First In, First Out" queue using [fastmap::fastqueue()]. If using your own queue, it should have the methods `$add(x)`, `$remove()`, `$size()`.
    #' @param loop \pkg{later} loop to use for calculating the next delayed check. Defaults to [later::global_loop()].
    initialize = function(
      # defaults to a future::plan agnostic function
      can_proceed = future_worker_is_free,
      queue = fastmap::fastqueue(), # FIFO
      loop = later::global_loop()
    ) {
      stopifnot(is.function(can_proceed))
      stopifnot(
        is.function(queue$add) &&
          is.function(queue$remove) &&
          is.function(queue$size)
      )
      stopifnot(inherits(loop, "event_loop"))
      delay <- ExpoDelay$new()
      stopifnot(inherits(delay, "Delay"))

      private$can_proceed_fn <- can_proceed
      private$queue <- queue
      private$loop <- loop
      private$delay <- delay

      # make sure delay is reset
      private$reset_delay()

      self
    },

    # add to schedule only
    #' Schedule work
    #' @param fn function to execute when `can_proceed()` returns `TRUE`.
    schedule_work = function(fn) {
      debug_msg("schedule_work()")
      stopifnot(is.function(fn))
      private$queue$add(fn)

      private$attempt_work()

      invisible(self)
    }
  )
)


#' @describeIn future_promise Default `future_promise()` work queue to use. This function returns a [WorkQueue] that is cached per R session.
#' @seealso [`WorkQueue`]
#' @export
future_promise_queue <- local({
  future_promise_queue_ <- NULL
  function() {
    if (is.null(future_promise_queue_)) {
      assert_work_queue_pkgs()
      future_promise_queue_ <<- WorkQueue$new()
    }
    future_promise_queue_
  }
})


#' \pkg{future} promise
#'
#'
#' When submitting \pkg{future} work, \pkg{future} (by design) will block the main R session until a worker becomes available.
#' This occurs when there is more submitted \pkg{future} work than there are available \pkg{future} workers.
#' To counter this situation, we can create a promise to execute work using future (using `future_promise()`) and only begin the work if a \pkg{future} worker is available.
#'
#' Using `future_promise()` is recommended whenever a continuous runtime is used, such as with \pkg{plumber} or \pkg{shiny}.
#'
#' For more details and examples, please see the [`vignette("future_promise", "promises")`](https://rstudio.github.io/promises/articles/promises_05b_future_promise.html) vignette.
#' @describeIn future_promise Creates a [promise()] that will execute the `expr` using [future::future()].
#' @inheritParams future::future
#' @param expr An R expression. While the `expr` is eventually sent to [`future::future()`], please use the same precautions that you would use with regular `promises::promise()` expressions. `future_promise()` may have to hold the `expr` in a [promise()] while waiting for a \pkg{future} worker to become available.
#' @param ... extra parameters provided to [`future::future()`]
#' @param queue A queue that is used to schedule work to be done using [future::future()].  This queue defaults to [future_promise_queue()] and requires that method `queue$schedule_work(fn)` exist.  This method should take in a function that will execute the promised \pkg{future} work.
#' @return Unlike [`future::future()`], `future_promise()` returns a [promise()] object that will eventually resolve the \pkg{future} `expr`.
#' @examples
#' \dontrun{# Relative start time
#' start <- Sys.time()
#' # Helper to force two `future` workers
#' with_two_workers <- function(expr) {
#'   if (!require("future")) {
#'     message("`future` not installed")
#'     return()
#'   }
#'   old_plan <- future::plan(future::multisession, workers = 2)
#'   on.exit({future::plan(old_plan)}, add = TRUE)
#'   start <<- Sys.time()
#'   force(expr)
#'   while(!later::loop_empty()) {Sys.sleep(0.1); later::run_now()}
#'   invisible()
#' }
#' # Print a status message. Ex: `"PID: XXX; 2.5s promise done"`
#' print_msg <- function(pid, msg) {
#'   message(
#'     "PID: ", pid, "; ",
#'     round(difftime(Sys.time(), start, units = "secs"), digits = 1), "s " ,
#'     msg
#'   )
#' }
#'
#' # `"promise done"` will appear after four workers are done and the main R session is not blocked
#' # The important thing to note is the first four times will be roughly the same
#' with_two_workers({
#'   promise_resolve(Sys.getpid()) |>
#'     then(\(x) {print_msg("promise done")})
#'   for (i in 1:6) {
#'     future::future({Sys.sleep(1); Sys.getpid()}) |>
#'       then(\(x) {print_msg("future done")})
#'   }
#' })
#' {
#' #> PID: XXX; 2.5s promise done
#' #> PID: YYY; 2.6s future done
#' #> PID: ZZZ; 2.6s future done
#' #> PID: YYY; 2.6s future done
#' #> PID: ZZZ; 2.6s future done
#' #> PID: YYY; 3.4s future done
#' #> PID: ZZZ; 3.6s future done
#' }
#'
#' # `"promise done"` will almost immediately, before any workers have completed
#' # The first two `"future done"` comments appear earlier the example above
#' with_two_workers({
#'   promise_resolve(Sys.getpid()) |>
#'     then(\(x) {print_msg("promise")})
#'   for (i in 1:6) {
#'     future_promise({Sys.sleep(1); Sys.getpid()}) |>
#'       then(\(x) {print_msg("future done")})
#'   }
#' })
#' {
#' #> PID: XXX; 0.2s promise done
#' #> PID: YYY; 1.3s future done
#' #> PID: ZZZ; 1.4s future done
#' #> PID: YYY; 2.5s future done
#' #> PID: ZZZ; 2.6s future done
#' #> PID: YYY; 3.4s future done
#' #> PID: ZZZ; 3.6s future done
#' }}
#' @export
future_promise <- function(
  expr = NULL,
  envir = parent.frame(),
  ...,
  substitute = TRUE,
  queue = future_promise_queue()
) {
  # make sure queue is the right structure
  stopifnot(
    is.function(queue$schedule_work) &&
      length(formals(queue$schedule_work)) >= 1
  )

  if (substitute) {
    expr <- substitute(expr)
  }

  # Force all variables to curb values changing before execution
  # Does NOT fix R environment values changing
  force(envir)
  force(substitute)
  force(list(...))

  ## Record future object (but do not start it yet; lazy = TRUE)
  future_job <- future::future(
    expr,
    envir = envir,
    substitute = FALSE,
    ...,
    lazy = TRUE
  )

  promise(function(resolve, reject) {
    # add to queue
    queue$schedule_work(function() {
      # Resolve the outer promising job value
      # Kick off the future job
      resolve(future_job)
      # Return a promising object that can be chained by the `queue` after executing this _work_
      future_job
    })
  })
}
