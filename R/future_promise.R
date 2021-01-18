assert_future_version <- local({
  val <- NULL
  function() {
    if (!is.null(val)) return()
    if (packageVersion("future") < "1.21.0") {
      stop("`future` version >= 1.21.0 is required")
    }
    val <<- TRUE
    return()
  }
})
worker_is_free <- function() {
  assert_future_version()
  future::nbrOfFreeWorkers() > 0
}

# future_stack <- function(max_workers) {
#   stack <- list()

#   return (future_promise)
# }

# Know how to wait until a condition is met to execute scheduled work
#
# WorkQueue$constructor(can_proceed_func, polling_interval)
# WorkQueue$schedule_work(func)

# FIFO queue of workers
WorkQueue <- R6::R6Class("WorkQueue",
  private = list(
    queue = "fastmap::fastqueue()",
    can_proceed_fn = "worker_is_free()",
    loop = "later::current_loop()",

    attempts = 0,

    reset_attempts = function() {
      private$attempts <- 0
    },
    increment_attempts = function() {
      private$attempts <- private$attempts + 1
    },

    # return number of milliseconds until next attempt
    # will randomly backoff to avoid extra work on long poll times
    time_to_next_attempt = function() {
      floor(runif(n = 1, min = 0, max = (2 ^ private$attempts) - 1)) / 100
    },

    can_proceed = function() {
      isTRUE(private$can_proceed_fn())
    },

    check_queue = function() {
      # check if we can actually proceed with submitting `future` work
      if (!private$can_proceed()) {
        # Can not do work
        # Increment attempts and try again later
        private$increment_attempts()
        later::later(
          loop = private$loop,
          delay = private$time_to_next_attempt(),
          function() {
            private$check_queue()
          }
        )
        return()
      }

      # Can do work!
      private$reset_attempts()

      work_fn <- private$queue$remove()
      # no work to be done. return
      if (is.null(work_fn)) return()

      # do work
      work_fn()

      if (private$queue$size() > 0) {
        private$check_queue()
      }

      return()
    }
  ),
  public = list(
    initialize = function(
      # defaults to a plan agnostic function
      can_proceed_fn = worker_is_free,
      loop = later::current_loop()
    ) {
      private$attempts <- 0
      private$can_proceed_fn <- can_proceed_fn
      private$queue <- fastmap::fastqueue()
      private$loop <- loop
      self
    },

    # add to schedule only
    schedule_work = function(fn) {
      stopifnot(is.function(fn))
      private$queue$add(fn)

      invisible(self)
    },

    # try to attempt to do work
    attempt_work = function() {
      # If the number of check attempts is 0, start a worker
      if (private$attempts == 0) {
        # Attempt to do work right away
        private$check_queue()

        TRUE
      } else {
        FALSE
      }


    }
  )
)


future_promise_queue <- local({
  future_promise_queue_ <- NULL
  function() {
    if (is.null(future_promise_queue_)) {
      future_promise_queue_ <<- WorkQueue$new(
        can_proceed_fn = worker_is_free,
        loop = later::current_loop()
      )
    }
    future_promise_queue_
  }
})



counter <- 0
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

  counter <<- counter + 1
  counter_ <- force(counter)

  message(counter_, " - received expr")

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

  message(counter_, " - making promise")
  promises::promise(function(resolve, reject) {
    message(counter_, " - scheduling work")
    # add to queue
    queue$schedule_work(function() {
      message(counter_, " - submitting job")
      # worker available!
      ## TODO - barret - should the worker function be taken at creation time or submission time?
      ### Current behavior is submission time to allow
      make_future <- future::plan("next")

      resolve(
        make_future(
          gp$expr,
          envir = envir,
          substitute = FALSE,
          globals = gp$globals,
          packages = unique(c(packages, gp$packages)),
          ...
        )
      )
    })
    queue$attempt_work()

  }) %...>%
  {
    message(counter_, " - finish job!")
    .
  }
}



if (FALSE) {
  library(future)
  future::plan(future::multisession(workers = 2))

  # library("future.callr")
  # plan(callr(workers = 2))

  # source("plumber/calc.R")
  slow_calc <- function(n) {
    Sys.sleep(n)
    "slow!"
  }
  n <- 5
  a1 <- future_promise({
    print(paste0("start 1 - ", Sys.time()))
    print(slow_calc(n))
  })
  a2 <- future_promise({
    print(paste0("start 2 - ", Sys.time()))
    print(slow_calc(n))
  })
  a3 <- future_promise({
    print(paste0("start 3 - ", Sys.time()))
    print(slow_calc(n))
  })
  a4 <- future_promise({
    print(paste0("start 4 - ", Sys.time()))
    print(slow_calc(n))
  })

  print("done assignement!")

  a1 %...>% { message("end 1 - ", format(Sys.time())) }
  a2 %...>% { message("end 2 - ", format(Sys.time())) }
  a3 %...>% { message("end 3 - ", format(Sys.time())) }
  a4 %...>% { message("end 4 - ", format(Sys.time())) }

}
