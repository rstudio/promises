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

# WorkQueue$schedule_work(func)
    increase = function() {
      private$delay_count <- private$delay_count + 1
    },

    delay = function() {
      stop("$delay() not implemented")
    }
  )
)

ExpoDelay <- R6::R6Class("ExpoDelay",
  inherit = Delay,
  private = list(
    base = 1 / 100,
    min_seconds = 0.01,
    max_seconds = 30
  ),
  public = list(
    initialize = function(
      base = 1 / 100,
      min_seconds = 0.01,
      max_seconds = 30
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
      random_val <- runif(n = 1, min = private$min_seconds, max = min(private$max_seconds, expo_val))
      message(random_val)
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
      message(delta_delay)
      if (private$random) {
        runif(n = 1, max = delta_delay)
      } else {
        delta_delay
      }
    }
  )
)

# FIFO queue of workers
WorkQueue <- R6::R6Class("WorkQueue",
  private = list(
    queue = "fastmap::fastqueue()",
    can_proceed_fn = "future_worker_is_free()",
    loop = "later::current_loop()",
    delay = "ExpoDelay$new()",

    can_proceed = function() {
      isTRUE(private$can_proceed_fn())
    },

    attempt_work = function() {

      # check if we can actually proceed with submitting `future` work
      if (!private$can_proceed()) {
        # Can not do work

        # Increment delay and try again later
        private$delay$increase()
        later::later(
          loop = private$loop,
          delay = private$delay$delay(),
          function() {
            private$attempt_work()
          }
        )
        return()
      }

      # Can do work!

      # Reset the delay
      private$delay$reset()

      # Get first item in queue
      work_fn <- private$queue$remove()
      # no work to be done. return

      # safety check...
      # If nothing is returned, no work to be done. Return early
      if (!is.function(work_fn)) return()

      # Do scheduled work
      message("doing work")
      work_fn()

      if (private$queue$size() > 0) {
        message("attempting more work")
        private$attempt_work()
      }

      return()
    }
  ),
  public = list(
    initialize = function(
      # defaults to a plan agnostic function
      can_proceed = future_worker_is_free,
      queue = fastmap::fastqueue(), # FIFO
      loop = later::current_loop(),
      delay = ExpoDelay$new()
    ) {
      stopifnot(is.function(can_proceed))
      stopifnot(
        is.function(queue$add) &&
        is.function(queue$remove) &&
        is.function(queue$size)
      )
      stopifnot(inherits(loop, "event_loop"))
      stopifnot(inherits(delay, "Delay"))

      private$can_proceed_fn <- can_proceed
      private$queue <- queue
      private$loop <- loop
      private$delay <- delay

      # make sure delay is reset
      private$delay$reset()

      self
    },

    # add to schedule only
    schedule_work = function(fn) {
      stopifnot(is.function(fn))
      message("added to queue")
      private$queue$add(fn)

      # If we are not waiting on someone else, we can do work now
      ## fn was added above, so an _empty_ queue is of size 1
      if (private$queue$size() == 1) {
      # if (private$can_proceed()) {
        message("attempting new work")
        # Do work right away
        private$attempt_work()
      }

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
