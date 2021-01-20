
debug_msg_can_print <- FALSE
debug_msg <- function(...) {
  if (debug_msg_can_print) {
    message(...)
  }
}

assert_work_queue_pkgs <- local({
  val <- NULL
  function() {
    if (!is.null(val)) return()
    for (pkg in list(
      list(name = "future", version = "1.21.0"),
      list(name = "fastmap", version = "1.0.1.9000")
    )) {
      if (!is_available(pkg$name, pkg$version)) {
        stop("Package `", pkg$name, "` (", pkg$version, ") needs to be installed")
      }
    }
    val <<- TRUE
    return()
  }
})
future_worker_is_free <- function() {
  future::nbrOfFreeWorkers() > 0
}


Delay <- R6::R6Class("Delay",
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
  ), active = list(
    count = function() {
      private$delay_count
    }
  )
)

ExpoDelay <- R6::R6Class("ExpoDelay",
  inherit = Delay,
  private = list(
    base = 1 / 100,
    min_seconds = 0.01,
    max_seconds = 5
  ),
  public = list(
    initialize = function(
      base = 1 / 100,
      min_seconds = 0.01,
      max_seconds = 10
    ) {
      stopifnot(length(base) == 1 && is.numeric(base) && base >= 0)
      stopifnot(length(min_seconds) == 1 && is.numeric(min_seconds) && min_seconds >= 0)
      stopifnot(length(max_seconds) == 1 && is.numeric(max_seconds) && max_seconds >= 0)

      private$base <- base
      private$max_seconds <- max_seconds
      private$min_seconds <- min_seconds

      self
    },

    # return number of milliseconds until next attempt
    # will randomly backoff to avoid extra work on long poll times
    delay = function() {
      # calculate expo backoff value
      expo_val <- private$base * ((2 ^ private$delay_count) - 1)
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
#' An \pkg{R6} class to help with scheduling work to be completed. `WorkQueue` will only execute work if the `can_proceed()` reutrns `TRUE`. For the use case of `future`, `can_proceed()` defaults to `future::nbrOfFreeWorkers() > 0` which will not allow for work to be executed if a \pkg{future} worker is not available.
#'
#' `WorkQueue` will constantly try to start new work once prior work item finishes.  However, if `can_proceed()` returns `FALSE` (no future workers are available) and there is more work to be done, then work is attempted later a random amount of time later using exponential backoff.  The exponential backoff will cap out at 10 seconds to prevent unnecessarily large wait times.
#'
#' Each time `WorkQueue` tries to start more work, it will repeat until `can_proceed()` returns `FALSE` or there is no more work in the `queue`.
#'
#' @export
WorkQueue <- R6::R6Class("WorkQueue",
  private = list(
    queue = "fastmap::fastqueue()",
    can_proceed_fn = "future_worker_is_free()",
    loop = "later::current_loop()",
    delay = "ExpoDelay$new()",

    cancel_delay_fn = NULL,

    increase_delay = function() {
      debug_msg("increase_delay()...", private$delay$count)
      # Increment delay and try again later
      private$delay$increase()
    },

    reset_delay = function() {
      debug_msg("reset_delay()")
      private$delay$reset()
      if (is.function(private$cancel_delay_fn)) {
        private$cancel_delay_fn()
      }
      private$cancel_delay_fn <- NULL
    },

    can_proceed = function() {
      isTRUE(private$can_proceed_fn())
    },

    attempt_work = function(can_check_delay = FALSE) {
      debug_msg('attempt_work()')
      # If nothing to start, return early
      if (private$queue$size() == 0) return()

      # If we are not waiting on someone else, we can do work now
      while ((private$queue$size() > 0) && private$can_proceed()) {
        # Do work right away
        private$reset_delay()
        private$do_work()
      }

      # If there are still items to be processed, but we can not proceed...
      if (private$queue$size() > 0 && ! private$can_proceed()) {
        # If we are allowed to delay (default FALSE), or nothing is currently delaying
        if (can_check_delay || is.null(private$cancel_delay_fn)) {

          # Try again later
          private$increase_delay()
          private$cancel_delay_fn <-
            later::later(
              loop = private$loop,
              delay = private$delay$delay(),
              function() {
                private$attempt_work(can_check_delay = TRUE)
              }
            )
        }
      }
    },

    do_work = function() {
      debug_msg("do_work()")

      # Get first item in queue
      work_fn <- private$queue$remove()

      # Safety check...
      # If nothing is returned, no work to be done. Return early
      if (!is.function(work_fn)) return()

      # Do scheduled work
      debug_msg("execute work")
      future_job <- work_fn()

      # If there is queue'ed work, try to do work once future job has finished
      then(future_job, function(work_fn_value) {
        debug_msg("finished work. queue size: ", private$queue$size())
        private$attempt_work()
      })

      return()
    }
  ),
  public = list(
    #' @description Create a new `WorkQueue`
    #' @param can_proceed Function that should return a logical value. If `TRUE` is returned, then the next scheduled work will be executed. By default, this function uses checks \code{\link[future:nbrOfWorkers]{future::nbrOfFreeWorkers()} > 0}
    #' @param queue Queue object to use to store the scheduled work. By default, this is a "First In, First Out" queue using [fastmap::fastqueue()]. If using your own queue, it should have the methods `$add(x)`, `$remove()`, `$size()`.
    #' @param loop \pkg{later} loop to use. Defaults to [later::current_loop()].
    initialize = function(
      # defaults to a future::plan agnostic function
      can_proceed = future_worker_is_free,
      queue = fastmap::fastqueue(), # FIFO
      loop = later::current_loop()
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


#' @describeIn future_promise Default `future_promise()` work queue to use. This function uses [WorkQueue] for its implementation.
#' @seealso [WorkQueue]
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
#' When submitting \pkg{future} work, \pkg{future} (by design) will block the main R session until no worker is free.
#' This can occur when submitting more \pkg{future} work than there are \pkg{future} workers.
#'
#' If your [future::plan()] allows for more workers than you expect to require simultaneously, you are encouraged to continue to use [future::future()] like normal.  However, if you generate more simultaneous \pkg{future} work than there are available workers AND you would like to keep the main R sessino free, then you should use `future_promise()`.
#'
#' Typically, the expression to be executed can be delayed in a [promise()] before submission to a \pkg{future} worker.
#'
#' @section `promise` vs `future`:
#'
#' With future blocking on the main R session, this prevents values from being changed.  It is known that evironments can change their values unexpectedly. As with [promise()], it is recommended to pull out important variables and force them to a local value before creating your `future_promise()`.
#'
#' For example, the environtment `env` below will have its value `a` changed before it is used.
#' ```r
#' {
#'   env <- new.env()
#'   env$a <- 1
#'
#'   promise_resolve(TRUE) %...>%
#'     { Sys.sleep(1); env$a } %...>%
#'     { print(.) }
#'   env$a <- 2
#'   print("done")
#' }
#' #> [1] "done"
#' #> [1] 2
#' ```
#'
#' To address this, we can capture the value `a` or turn the environment into a `list()`.
#' ```r
#' {
#'   env <- new.env()
#'   env$a <- 1
#'
#'   a_val <- env$a
#'   promise_resolve(TRUE) %...>%
#'     { Sys.sleep(1); a_val } %...>%
#'     { print(.) }
#'   env$a <- 2
#'   print("done")
#' }
#' #> [1] "done"
#' #> [1] 1
#' ```
#'
#' When using `future_promise()`, be sure to scope your variables if the context can change. `future_promise()` will behave more like a [promise()] that executes using [future::future()].
#'
#' Changing context example:
#' ```r
# items <- list()
# for (i in 1:10) {
#   items <- c(items, list(
#     promise(function(resolve, reject) {
#       later::later(function() { resolve(i) })
#     })
#   ))
# }
# promise_all(.list = items) %...>%
#   { print(unlist(.)) }
# #> [1] 10 10 10 10 10 10 10 10 10 10
#' ```
#'
#' Scoped context example:
#' ```r
#' lapply(1:10, function(i) {
#'   promise(function(resolve, reject) {
#'     later::later(function() { resolve(i) })
#'   })
#' }) %>%
#'   promise_all(.list = .) %...>%
#'   { print(unlist(.)) }
#' #> [1]  1  2  3  4  5  6  7  8  9 10
#' ```
#'
#' @describeIn future_promise Creates a [promise()] that will execute the `expr` using [future::future()].
#' @inheritParams future::future
#' @param ... extra parameters provided to future
#' @param queue A queue that is used to schedule work to be done using [future::future()].  See the details for more information.
#' @return a [promise()] object that will return the result of the calculated `expr`.
#' @examples
#' \donttest{local({
#'   ## Setup ##
#'
#'   # Reset future plan on exit
#'   old_plan <- future::plan()
#'   on.exit({future::plan(old_plan)}, add = TRUE)
#'
#'   # Set up a plan with 2 future workers
#'   future::plan(future::multisession(workers = 2))
#'
#'   # `cat()` to file so that the console does not display confusing results
#'   log <- tempfile()
#'   on.exit({unlink(log)}, add = TRUE)
#'   start <- Sys.time()
#'   cat_ex <- function(...) {
#'     msg <- paste0(..., "; ", sprintf("%.2f", difftime(Sys.time(), start, units = "secs")))
#'     message(msg)
#'     cat(msg, "\n", sep = "", file = log, append = TRUE)
#'   }
#'
#'   # This function will print every `delay` seconds
#'   # The thing to notice is that the function will not execute unless the main R session is free
#'   # We should expect to see `print_every()` statements interleaved with `future_promise()` calls
#'   # We should NOT expect to see `print_every()` statements
#'   #   while `future::future()` is blocking the main R session
#'   print_every <- function(i = 0, max = 5 / delay, delay = 0.25) {
#'     if (i > max) return()
#'     cat_ex("print_every(): ", i, "/", max)
#'     # Print again, later
#'     later::later(function() { print_every(i + 1, max = max, delay = delay) }, delay = delay)
#'   }
#'
#'   # Act as if the main R session is free for the next `timeout` seconds
#'   run_ex_now <- function() {
#'     while (!later::loop_empty()) {
#'       later::run_now()
#'       Sys.sleep(0.01)
#'     }
#'     cat("\nlog:\n", paste0(readLines(log), collapse = "\n"), "\n", sep = "")
#'     cat_ex("done with run_ex_now()")
#'   }
#'
#'   # Consistent printing method for the example below
#'   print_ex <- function(x) {
#'     promise_all(.list = x) %...>%
#'       {
#'         cat("\n")
#'         print(.);
#'       }
#'   }
#'
#'
#'   ## Example ##
#'
#'   # Execute 10 `future_promise()` calls.
#'   # In this example, we expect
#'   # * all _creating_ statements should appear immediately
#'   # * the first two `future_promise()` calls should start immediately
#'   # * two `future_promise()` calls should execute simultaneously throughout the process
#'   # * the log should have `print_every()` statements interleaved with `future_promise()` statements
#'   {
#'     log <- tempfile(); start <- Sys.time()
#'     print_every()
#'     lapply(1:10, function(i) {
#'       cat_ex("future_promise(): ", i, " creating")
#'       future_promise({
#'         cat_ex("future_promise(): ", i, " starting")
#'         Sys.sleep(1)
#'         cat_ex("future_promise(): ", i, " finished")
#'         i
#'       })
#'     }) %>%
#'       print_ex()
#'     cat_ex("done with lapply()")
#'
#'     run_ex_now() # ~5s; max(5s for `future_promise()`, 5s for `print_every()`)
#'   }
#'
#'   ## Unexpected behavior - Blocking the main R session ##
#'
#'   # Execute 10 `future::future()` calls.
#'   # In this example, we expect
#'   # * the two _creating_ statements should appear immediately.
#'   #     The remaining _creating_ statements will appear after prior work has completed
#'   # * the first two `future::future()` calls should start immediately
#'   # * two `future::future()` calls should execute simultaneously throughout the process
#'   # * the log will NOT have `print_every()` statements mixed with `future::future()` statements.
#'   #     This means the main R session is blocked by `future` waiting for a worker to become free
#'   {
#'     log <- tempfile(); start <- Sys.time()
#'     print_every()
#'     lapply(1:10, function(i) {
#'       cat_ex("future::future(): ", i, " creating")
#'       future::future({
#'         cat_ex("future::future(): ", i, " starting")
#'         Sys.sleep(1)
#'         cat_ex("future::future(): ", i, " finished")
#'         i
#'       })
#'     }) %>%
#'       print_ex()
#'     cat_ex("done with lapply()")
#'
#'     run_ex_now() # ~10s; 5s for `future::future()` + 5s for `print_every()`
#'   }
#' })}
#' @export
future_promise <- function(
  expr = NULL,
  envir = parent.frame(),
  substitute = TRUE,
  globals = TRUE,
  packages = NULL,
  ...,
  queue = future_promise_queue()
) {

  # make sure queue is the right structure
  stopifnot(is.function(queue$schedule_work) && length(formals(queue$schedule_work)) >= 1)

  if (substitute) expr <- substitute(expr)

  # Force all variables to curb values changing before execution
  # Does NOT fix R environment values changing
  force(envir)
  force(substitute)
  force(globals)
  force(packages)
  force(list(...))

  ## Record globals
  gp <- future::getGlobalsAndPackages(expr, envir = envir, globals = globals)
  force(gp)

  promise(function(resolve, reject) {
    # add to queue
    queue$schedule_work(function() {
      ## TODO - barret - should the worker function be taken at creation time or submission time?
      ### Current behavior is submission time to allow
      exec_future <- future::plan("next")

      # execute the future and return a promise so the schedule knows exactly when it is done
      future_job <- exec_future(
        gp$expr,
        envir = envir,
        substitute = FALSE,
        globals = gp$globals,
        packages = unique(c(packages, gp$packages)),
        ...
      )

      # When the future job is complete, resolve it
      # Return a promise so that more promises can be added to it
      then(future_job, function(job_value) {
        resolve(job_value)
        job_value
      })
    })
  })
}



if (FALSE) {

  # ConstDelay <- R6::R6Class("ConstDelay",
  #   inherit = Delay,
  #   private = list(
  #     const = 0.1,
  #     random = TRUE
  #   ),
  #   public = list(
  #     initialize = function(const = 0.1, random = TRUE) {
  #       stopifnot(length(const) == 1 && is.numeric(const) && const >= 0)
  #       private$const <- const
  #       private$random <- isTRUE(random)

  #       self
  #     },
  #     delay = function() {
  #       if (private$random) {
  #         runif(n = 1, max = private$const)
  #       } else {
  #         private$const
  #       }
  #     }
  #   )
  # )
  # LinearDelay <- R6::R6Class("LinearDelay",
  #   inherit = Delay,
  #   private = list(
  #     delta = 0.05,
  #     random = TRUE
  #   ),
  #   public = list(
  #     initialize = function(delta = 0.03, random = TRUE) {
  #       stopifnot(length(delta) == 1 && is.numeric(delta) && delta >= 0)
  #       private$delta <- delta
  #       private$random <- isTRUE(random)

  #       self
  #     },

  #     delay = function() {
  #       delta_delay <- private$delay_count * private$delta
  #       if (private$random) {
  #         runif(n = 1, max = delta_delay)
  #       } else {
  #         delta_delay
  #       }
  #     }
  #   )
  # )


  # dev_load <- pkgload::load_all

  # ## test
  # dev_load(); print_i(); start <- Sys.time(); promise_all(.list = lapply(1:10, function(x) { future_promise({ Sys.sleep(1); print(paste0(x)) })})) %...>% { print(Sys.time() - start) };

  # ## block workers mid job
  # dev_load(); print_i(); start <- Sys.time(); promise_all(.list = lapply(1:10, function(x) { future_promise({ Sys.sleep(1); print(paste0(x)) })})) %...>% { print(Sys.time() - start) }; lapply(1:2, function(i) { later::later(function() { message("*************** adding blockage", i); fj <- future::future({ Sys.sleep(4); message("*************** blockage done", i); i}); then(fj, function(x) { print(paste0("block - ", i))}); }, delay = 0.5 + i) }) -> ignore;

  # ## block main worker mid job
  # dev_load(); print_i(); start <- Sys.time(); promise_all(.list = lapply(1:10, function(x) { future_promise({ Sys.sleep(1); print(paste0(x)) })})) %...>% { print(Sys.time() - start) }; lapply(1:4, function(i) { later::later(function() { message("*************** adding blockage", i); fj <- future::future({ Sys.sleep(4); message("*************** blockage done", i); i}); then(fj, function(x) { print(paste0("block - ", i))}); }, delay = 0.5 + i/4) }) -> ignore;


  # ## block workers pre job
  # dev_load(); print_i(); lapply(1:2, function(i) { message("*************** adding blockage", i); future::future({ Sys.sleep(4); message("*************** blockage done", i); i}) }) -> future_jobs; lapply(future_jobs, function(fj) { as.promise(fj) %...>% { print(.) } }); start <- Sys.time(); promise_all(.list = lapply(1:10, function(x) { future_promise({ Sys.sleep(1); print(paste0(x)) })})) %...>% { print(Sys.time() - start) };

  # ## block main worker workers pre job
  # dev_load(); print_i(); start <- Sys.time(); promise_all(.list = lapply(1:10, function(x) { future_promise({ Sys.sleep(1); print(paste0(x)) })})) %...>% { print(Sys.time() - start) }; lapply(1:4, function(i) { later::later(function() { message("*************** adding blockage", i); fj <- future::future({ Sys.sleep(4); message("*************** blockage done", i); i}); then(fj, function(x) { print(paste0("block - ", i))}); }, delay = 0.5 + i/4) }) -> ignore;

  future::plan(future::multisession(workers = 2))

  debug_msg_can_print <- TRUE

  print_i <- function(i = 0) { if (i <= 50) { print(i); later::later(function() { print_i(i + 1) }, delay = 0.1) } }

  slow_calc <- function(n) {
    Sys.sleep(n)
    "slow!"
  }
  n <- 2
  prom <- future_promise
  # prom <- future::future
  a1 <- prom({
    print(paste0("start 1 - ", Sys.time()))
    print(slow_calc(n))
  })
  a2 <- prom({
    print(paste0("start 2 - ", Sys.time()))
    print(slow_calc(n))
  })
  a3 <- prom({
    print(paste0("start 3 - ", Sys.time()))
    print(slow_calc(n))
  })
  a4 <- prom({
    print(paste0("start 4 - ", Sys.time()))
    print(slow_calc(n))
  })

  print("done assignement!")

  a1 %...>% { message("end 1 - ", format(Sys.time())) }
  a2 %...>% { message("end 2 - ", format(Sys.time())) }
  a3 %...>% { message("end 3 - ", format(Sys.time())) }
  a4 %...>% { message("end 4 - ", format(Sys.time())) }

}
