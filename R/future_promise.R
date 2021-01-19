
fp_message_can_print <- FALSE
fp_message <- function(...) {
  if (fp_message_can_print) {
    message(...)
  }
}

assert_future_version <- local({
  val <- NULL
  function() {
    if (!is.null(val)) return()
    if (utils::packageVersion("future") < "1.21.0") {
      stop("`future` version >= 1.21.0 is required")
    }
    val <<- TRUE
    return()
  }
})
future_worker_is_free <- function() {
  assert_future_version()
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

ConstDelay <- R6::R6Class("ConstDelay",
  inherit = Delay,
  private = list(
    const = 0.1,
    random = TRUE
  ),
  public = list(
    initialize = function(const = 0.1, random = TRUE) {
      stopifnot(length(const) == 1 && is.numeric(const) && const >= 0)
      private$const <- const
      private$random <- isTRUE(random)

      self
    },
    delay = function() {
      if (private$random) {
        runif(n = 1, max = private$const)
      } else {
        private$const
      }
    }
  )
)
LinearDelay <- R6::R6Class("LinearDelay",
  inherit = Delay,
  private = list(
    delta = 0.05,
    random = TRUE
  ),
  public = list(
    initialize = function(delta = 0.03, random = TRUE) {
      stopifnot(length(delta) == 1 && is.numeric(delta) && delta >= 0)
      private$delta <- delta
      private$random <- isTRUE(random)

      self
    },

    delay = function() {
      delta_delay <- private$delay_count * private$delta
      if (private$random) {
        runif(n = 1, max = delta_delay)
      } else {
        delta_delay
      }
    }
  )
)

# Situations
# √ No future workers are busy. All future calls are `future_promises()`
#  * Can be accomplished using followup promise to `$start_work()`
# √ All future workers are busy with other tasks (but will become available).
#  * Require using delay
# √ While processing the first batch, existing future workers are taken over
#  * Require delay

# FIFO queue of workers
WorkQueue <- R6::R6Class("WorkQueue",
  private = list(
    queue = "fastmap::fastqueue()",
    can_proceed_fn = "future_worker_is_free()",
    loop = "later::current_loop()",
    delay = "ExpoDelay$new()",

    cancel_delay_fn = NULL,

    increase_delay = function() {
      fp_message("increase_delay()...", private$delay$count)
      # Increment delay and try again later
      private$delay$increase()
    },

    reset_delay = function() {
      fp_message("reset_delay()")
      private$delay$reset()
      if (is.function(private$cancel_delay_fn)) {
        private$cancel_delay_fn()
      }
      private$cancel_delay_fn <- NULL
    },

    can_proceed = function() {
      isTRUE(private$can_proceed_fn())
    },

    start_work = function(can_check_delay = FALSE) {
      fp_message('start_work()')

      # If we are not waiting on someone else, we can do work now
      while ((private$queue$size() > 0) && private$can_proceed()) {

        private$reset_delay()

        # Do work right away
        private$do_work()
      }

      # if there are still items to be processed, but we can not proceed...
      if (private$queue$size() > 0 && ! private$can_proceed()) {
        # if we are allowed to delay (default FALSE), or nothing is currently delaying
        if (can_check_delay || is.null(private$cancel_delay_fn)) {
          # bump up the delay
          private$increase_delay()

          # try again later
          private$cancel_delay_fn <-
            later::later(
              loop = private$loop,
              delay = private$delay$delay(),
              function() {
                private$start_work(can_check_delay = TRUE)
              }
            )
        }
      }
    },

    do_work = function() {
      fp_message("do_work()")

      # Get first item in queue
      work_fn <- private$queue$remove()

      # safety check...
      # If nothing is returned, no work to be done. Return early
      if (!is.function(work_fn)) return()

      # Do scheduled work
      fp_message("execute work")
      future_job <- work_fn()
      # make sure a promise like object was returned
      stopifnot(is.promising(future_job))

      # If there is more work to do, try to do work once future job has finished
      future_job %...>% {
        fp_message("finished work. queue size: ", private$queue$size())
        private$start_work()
      }

      return()
    }
  ),
  public = list(
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
    schedule_work = function(fn) {
      fp_message("schedule_work()")
      stopifnot(is.function(fn))
      private$queue$add(fn)

      private$start_work()

      invisible(self)
    }
  )
)


future_promise_queue <- local({
  future_promise_queue_ <- NULL
  function() {
    if (is.null(future_promise_queue_)) {
      future_promise_queue_ <<- WorkQueue$new()
    }
    future_promise_queue_
  }
})



future_promise <- function(
  expr = NULL,
  envir = parent.frame(),
  substitute = TRUE,
  globals = TRUE,
  packages = NULL,
  ...,
  queue = future_promise_queue()
) {

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

  promises::promise(function(resolve, reject) {
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

      # When the future job is complete, resolve it~
      # Return the future job so that more promises can be added to it
      then(future_job, resolve)
    })
  })
}



if (FALSE) {
  library(future)
  future::plan(future::multisession(workers = 2))

  # library("future.callr")
  # plan(callr(workers = 2))

  print_i <- function(i = 0) { if (i <= 50) { print(i); later::later(function() { print_i(i + 1) }, delay = 0.1) } }

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
