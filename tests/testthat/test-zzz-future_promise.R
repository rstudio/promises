skip_on_cran()
skip_if_not_installed("future", "1.21.0")

source(test_path("common.R"))


local({
  ## Setup ##

  # Reset future plan on exit
  old_plan <- future::plan()
  on.exit({future::plan(old_plan)}, add = TRUE)

  # Set up a plan with 2 future workers
  future::plan(future::multisession(workers = 2))

  start <- Sys.time()

  time_diff <- function() {
    difftime(Sys.time(), start, units = "secs")
  }

  time_diffs <- c()

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

  reset_baselines <- function() {
    start <<- Sys.time()
    time_diffs <<- c()
  }

  n <- 8
  n_time <- 1
  expected_total_time <- n * n_time

  test_that("future_promise() allows the main thread to keep the main R process open", {
    expect_true(future_worker_is_free())

    reset_baselines()
    run_every()

    exec_times <- NA
    lapply(seq_len(n), function(i) {
      future_promise({
        Sys.sleep(n_time)
        time_diff()
      })
    }) %>%
      promise_all(.list = .) %...>% {
        exec_times <<- unlist(.)

      }
    post_lapply_time_diff <- time_diff()

    wait_for_it()
    # expect that the average time is less than the expected total time
    expect_true(mean(exec_times) < expected_total_time)

    # expect a future_promise to take more than a reasonable amount of time to begin
    exec_times_lag <- exec_times[-1] - exec_times[-length(exec_times)]
    expect_true(all(exec_times_lag < (2 * n_time)))

    # post_lapply_time_diff should be ~ 0s
    expect_true(post_lapply_time_diff < 1)

    # time_diffs never grew by more than 1s; (Expected 0.1)
    time_diffs_lag <- time_diffs[-1] - time_diffs[-length(time_diffs)]
    expect_true(
      all(time_diffs_lag < 1)
    )

  })


  test_that("future::future() does not keep the main process open when all workers are busy", {
    expect_true(future_worker_is_free())

    reset_baselines()
    run_every()

    exec_times <- NA

    lapply(seq_len(n), function(i) {
      future::future({
        Sys.sleep(n_time)
        time_diff()
      })
    }) %>%
      promise_all(.list = .) %...>% {
        exec_times <<- unlist(.)
      }
    post_lapply_time_diff <- time_diff()

    wait_for_it()

    # expect that the average time is less than the expected total time
    expect_true(mean(exec_times) < expected_total_time)

    # expect a future_promise to take more than a reasonable amount of time to begin
    exec_times_lag <- exec_times[-1] - exec_times[-length(exec_times)]
    expect_true(all(exec_times_lag < (2 * n_time)))

    # post_lapply_time_diff should be after the expected total time
    # b/c future blocks the main R session
    # ~ 3s (free at 3s while working on last two jobs)
    expect_false(post_lapply_time_diff < 1)

    # >=1 time_diffs should grew by more than 1s; (Expected 0.1)
    time_diffs_lag <- time_diffs[-1] - time_diffs[-length(time_diffs)]
    expect_false(
      all(time_diffs_lag < 1)
    )
  })


  test_that("future_promise() recovers from losing all future workers", {
    expect_true(future_worker_is_free())

    reset_baselines()
    run_every()

    # Have `future` block the main R session 1 second into execution
    future_time_diffs <- c()
    ignore <- lapply(1:6, function(i) {
      later::later(
        function() {
          then(
            future::future({
              Sys.sleep(1);
              time_diff()
            }),
            function(val) {
              future_time_diffs <<- c(future_time_diffs, val)
            }
          )
        },
        delay = 1
      )
    });

    # Run many `future_promise()` calls
    exec_times <- NA
    lapply(seq_len(n), function(i) {
      future_promise({
        Sys.sleep(n_time)
        time_diff()
      })
    }) %>%
      promise_all(.list = .) %...>% {
        exec_times <<- unlist(.)
      }
    post_lapply_time_diff <- time_diff()

    # Throws after 30s
    wait_for_it()

    # expect that the average time is reasonable
    expect_true(median(exec_times) < expected_total_time + (1 * 6 / 2))

    # b/c `future::future()` blocks the main R session, expect >=1 unreasonable lag time
    exec_times_lag <- exec_times[-1] - exec_times[-length(exec_times)]
    expect_false(all(exec_times_lag < (2 * n_time)))

    # post_lapply_time_diff should roughly immediate
    # ~0s
    expect_true(post_lapply_time_diff < 1)

    # >=1 time_diffs_lag show that the main session was blocked.; (Expected 0.1)
    time_diffs_lag <- time_diffs[-1] - time_diffs[-length(time_diffs)]
    expect_false(
      all(time_diffs_lag < 1)
    )
  })

})
