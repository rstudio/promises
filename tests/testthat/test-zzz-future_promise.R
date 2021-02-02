skip_on_cran()
skip_on_os("windows") # timing is not consistent on Windows GHA

skip_if_not_installed("future", "1.21.0")
skip_if_not_installed("fastmap", "1.1.0")

source(test_path("common.R"))

local({
  ## Setup ##

  # Set up a plan with 2 future workers
  with_test_workers <- function(code) {
    # (Can not use a variable for workers if in a local({}))
    old_plan <- future::plan(future::multisession(workers = 2))
    on.exit({future::plan(old_plan)}, add = TRUE)

    force(code)
  }


  start <- Sys.time()
  time_diffs <- c()

  reset_baselines <- function() {
    start <<- Sys.time()
    time_diffs <<- c()
  }

  time_diff <- function() {
    difftime(Sys.time(), start, units = "secs")
  }

  # This function will print every `delay` seconds
  # The thing to notes is that the function will not execute unless the main R session is free
  # We should expect to see `run_every()` statements interleaved with `future_promise()` calls
  # We should NOT expect to see `run_every()` statements while `future::future()` is blocking the main R session
  run_every <- function(i = 0, max = 2 / delay, delay = 0.1) {
    if (i > max) return()
    time_diffs <<- c(time_diffs, time_diff())
    # Do it again, later
    later::later(function() { run_every(i + 1, max = max, delay = delay) }, delay = delay)
  }

  worker_jobs <- 8
  worker_job_time <- 1
  expected_total_time <- worker_jobs * worker_job_time / 2 # 2 workers

  do_future_test <- function(
    prom_fn = future_promise,
    # Introduce extra future workers mid execution
    block_mid_session = FALSE,
    # expect that the average finish lag time is less than 2 * n_time
    expect_reasonable_exec_lag_time = TRUE,
    # expect the lapply to finish in less than 1s
    expect_immediate_lapply = TRUE,
    # expect `run_every()` delay to be < 1s (Expected 0.1s)
    expect_no_main_blocking = TRUE
  ) {

    with_test_workers({
      # prep future sessions
      f1 <- future::future({1})
      f2 <- future::future({2})
      c(future::value(future::resolve(f1)), future::value(future::resolve(f2)))

      expect_true(future_worker_is_free())
      expect_equal(future::nbrOfWorkers(), 2)
      expect_equal(future::nbrOfFreeWorkers(), 2)

      reset_baselines()
      run_every()

      future_exec_times <- c()
      if (block_mid_session) {
        # Have `future` block the main R session 1 second into execution
        lapply(1:8, function(i) {
          later::later(
            function() {
              future::future({
                Sys.sleep(1)
                time_diff()
              }) %...>% {
                future_exec_times <<- c(future_exec_times, .)
              }
            },
            delay = 1
          )
        })
      }


      exec_times <- NA
      lapply(seq_len(worker_jobs), function(i) {
        prom_fn({
          Sys.sleep(worker_job_time)
          time_diff()
        })
      }) %>%
        promise_all(.list = .) %...>% {
          exec_times <<- unlist(.)
        }
      post_lapply_time_diff <- time_diff()

      wait_for_it()

      # expect that the average time is less than the expected total time
      expect_equal(median(exec_times) < expected_total_time, !block_mid_session)

      # expect prom_fn to take a reasonable amount of time to finish
      exec_times_lag <- exec_times[-1] - exec_times[-length(exec_times)]
      expect_equal(all(exec_times_lag < (2 * worker_job_time)), expect_reasonable_exec_lag_time)

      # post_lapply_time_diff should be ~ 0s
      expect_equal(post_lapply_time_diff < worker_job_time, expect_immediate_lapply)

      # time_diffs should never grow by more than 1s; (Expected 0.1)
      time_diffs_lag <- time_diffs[-1] - time_diffs[-length(time_diffs)]
      expect_equal(all(time_diffs_lag < worker_job_time), expect_no_main_blocking)
    })

  }


  test_that("future_promise() allows the main thread to keep the main R process open", {

    do_future_test(
      prom_fn = future_promise,
      # expect that the average finish lag time is less than 2 * n_time
      expect_reasonable_exec_lag_time = TRUE,
      # expect the lapply to finish in less than 1s
      expect_immediate_lapply = TRUE,
      # expect `run_every()` delay to be < 1s (Expected 0.1s)
      expect_no_main_blocking = TRUE
    )
  })


  test_that("future::future() does not keep the main process open when all workers are busy", {
    do_future_test(
      prom_fn = future::future,
      # expect that the average finish lag time is less than 2 * n_time
      expect_reasonable_exec_lag_time = TRUE,
      # expect the lapply to finish after 1s
      expect_immediate_lapply = FALSE,
      # expect one `run_every()` delay to be >= 1s (Expected 0.1s)
      expect_no_main_blocking = FALSE
    )
  })


  test_that("future_promise() recovers from losing all future workers", {

    do_future_test(
      prom_fn = future_promise,
      block_mid_session = TRUE,
      # expect that the average finish lag time is less than 2 * n_time
      # b/c `future::future()` blocks the main R session, expect >=1 unreasonable lag time
      expect_reasonable_exec_lag_time = FALSE,
      # expect the lapply to finish in less than 1s
      expect_immediate_lapply = TRUE,
      # expect one `run_every()` delay to be >= 1s (Expected 0.1s)
      # b/c `future::future()` blocks the main R session, expect >=1 unreasonable lag time
      expect_no_main_blocking = FALSE
    )
  })

})
