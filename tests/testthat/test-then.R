test_that("complex promise chains with mixed success/failure", {
  counter <- 0
  promise_resolve(1) |>
    then(\(x) {
      counter <<- counter + 1
      stop("error1")
    }) |>
    catch(\(err) {
      counter <<- counter + 1
      "recovered"
    }) |>
    then(\(x) {
      counter <<- counter + 1
      x
    }) |>
    catch(\(err) {
      counter <<- counter + 1
      "should not reach"
    }) |>
    then(\(x) {
      counter <<- counter + 1
      expect_equal(x, "recovered")
    }) |>
    wait_for_it()
  expect_equal(counter, 4)
})

test_that("then() handles NULL callbacks correctly", {
  # NULL onFulfilled should pass through value
  expect_assertions(1)
  promise_resolve(42) |>
    then(NULL, \(err) "error") |>
    extract() |>
    expect_equal(42)
})

test_that("catch() handles NULL callbacks correctly", {
  # NULL onRejected should pass through error
  expect_assertions(1)
  promise_resolve(stop("boom")) |>
    then(\(x) x, NULL) |>
    catch(\(err) expect_match(err$message, "boom")) |>
    wait_for_it()
})

test_that("then() validates arguments properly", {
  expect_error(
    then(promise_resolve(1), onFulfilled = "not a function"),
    "function"
  )

  expect_error(
    catch(promise_resolve(1), "not a function"),
    "function"
  )
})

test_that("tee types are handled correctly", {
  expect_silent({
    p <- promise_resolve(1) |> then(tee = TRUE)
    p2 <- promise_resolve(1) |> catch(\(err) {}, tee = TRUE)
  })

  expect_snapshot(
    then(promise_resolve(1), tee = 4),
    error = TRUE
  )

  expect_snapshot(
    promise_resolve(1) |> catch(\(err) {}, tee = "4"),
    error = TRUE
  )
})

test_that("then(tee = FALSE) functions are normalized", {
  counter <- 0
  promise_resolve(1) |>
    then(
      \() {
        counter <<- counter + 1
        2
      }
    ) |>
    then(
      \(value) {
        expect_equal(value, 2)
        counter <<- counter + 1
        invisible(3)
      }
    ) |>
    then(
      \(value, .visible) {
        expect_equal(value, 3)
        expect_false(.visible)
        counter <<- counter + 1
        stop("boom")
      }
    ) |>
    then(
      onRejected = \() {
        counter <<- counter + 1
        stop("blast")
      }
    ) |>
    then(
      onRejected = \(err) {
        expect_true(grepl("blast", err$message))
        counter <<- counter + 1
      }
    )
  wait_for_it()
  expect_equal(counter, 5)
})
test_that("catch(tee = FALSE) functions are normalized", {
  counter <- 0
  promise_resolve(stop("boom")) |>
    catch(
      \() {
        counter <<- counter + 1
        stop("blast")
      }
    ) |>
    catch(
      \(err) {
        expect_true(grepl("blast", err$message))
        counter <<- counter + 1
      }
    ) |>
    catch(\(err) {})
  wait_for_it()
  expect_equal(counter, 2)
})

test_that("then(tee=TRUE) functions are normalized", {
  then_counter <- 0
  error_counter <- 0
  promise_resolve(1) |>
    then(
      tee = TRUE,
      \() {
        then_counter <<- then_counter + 1
      }
    ) |>
    then(
      tee = TRUE,
      \(value) {
        expect_equal(value, 1)
        then_counter <<- then_counter + 1
        3
      }
    ) |>
    then(
      tee = TRUE,
      \(value, .visible) {
        expect_equal(value, 1)
        expect_true(.visible)
        then_counter <<- then_counter + 1

        stop("boom")
      }
    ) |>
    then(
      tee = TRUE,
      onRejected = \() {
        error_counter <<- error_counter + 1
      }
    ) |>
    then(
      tee = TRUE,
      onRejected = \(err) {
        expect_true(grepl("boom", err$message))
        error_counter <<- error_counter + 1
      }
    ) |>
    then(onRejected = \(err) {
      expect_true(grepl("boom", err$message))
      error_counter <<- error_counter + 1
    }) |>
    wait_for_it()

  expect_equal(then_counter, 3)
  expect_equal(error_counter, 3)
})

test_that("catch(tee=TRUE) functions are normalized", {
  error_counter <- 0
  promise_resolve(stop("boom")) |>
    catch(
      tee = TRUE,
      \() {
        error_counter <<- error_counter + 1
      }
    ) |>
    catch(
      tee = TRUE,
      \(err) {
        expect_true(grepl("boom", err$message))
        error_counter <<- error_counter + 1
      }
    ) |>
    catch(\() {
      error_counter <<- error_counter + 1
    }) |>
    wait_for_it()
  expect_equal(error_counter, 3)
})

# hybrid_then() tests --------------------------------------------------------

test_that("hybrid_then() handles synchronous values with on_success", {
  result <- hybrid_then(
    42,
    on_success = \(x) x + 8
  )
  expect_equal(result, 50)
})

test_that("hybrid_then() handles synchronous values without callbacks", {
  result <- hybrid_then(42)
  expect_equal(result, 42)
})

test_that("hybrid_then() handles synchronous errors with on_failure", {
  expect_assertions(2)
  result <- hybrid_then(
    stop("sync error"),
    on_failure = \(err) {
      expect_match(err$message, "sync error")
      "recovered"
    }
  )
  expect_equal(result, "recovered")
})

test_that("hybrid_then() re-throws synchronous errors without on_failure", {
  expect_error(
    hybrid_then(stop("sync error")),
    "sync error"
  )
})

test_that("hybrid_then() handles asynchronous values with on_success", {
  expect_assertions(2)

  result <- hybrid_then(
    promise_resolve(42),
    on_success = \(x) x + 8
  )
  expect_true(is.promise(result))
  result |> extract() |> expect_equal(50)
})

test_that("hybrid_then() handles asynchronous values without callbacks", {
  expect_assertions(2)

  result <- hybrid_then(promise_resolve(42))
  expect_true(is.promise(result))
  result |> extract() |> expect_equal(42)
})

test_that("hybrid_then() handles asynchronous errors with on_failure", {
  expect_assertions(3)

  result <- hybrid_then(
    promise_resolve(stop("async error")),
    on_failure = \(err) {
      expect_match(err$message, "async error")
      "recovered"
    }
  )
  expect_true(is.promise(result))
  result |> extract() |> expect_equal("recovered")
})

test_that("hybrid_then() propagates asynchronous errors without on_failure", {
  expect_assertions(2)

  result <- hybrid_then(promise_resolve(stop("async error")))
  expect_true(is.promise(result))
  result |>
    catch(\(err) {
      expect_match(err$message, "async error")
    }) |>
    wait_for_it()
})

test_that("hybrid_then() with tee=TRUE preserves synchronous values", {
  counter <- 0
  result <- hybrid_then(
    42,
    on_success = \(x) {
      counter <<- counter + 1
      99
    },
    tee = TRUE
  )
  expect_equal(result, 42)
  expect_equal(counter, 1)
})

test_that("hybrid_then() with tee=TRUE preserves synchronous errors", {
  counter <- 0
  expect_error(
    hybrid_then(
      stop("sync error"),
      on_failure = \(err) {
        counter <<- counter + 1
        "recovered"
      },
      tee = TRUE
    ),
    "sync error"
  )
  expect_equal(counter, 1)
})


test_that("hybrid_then() with tee=TRUE preserves asynchronous values", {
  expect_assertions(3)

  counter <- 0
  result <- hybrid_then(
    promise_resolve(42),
    on_success = \(x) {
      counter <<- counter + 1
      99
    },
    tee = TRUE
  )
  expect_true(is.promise(result))
  result |> extract() |> expect_equal(42)
  expect_equal(counter, 1)
})

test_that("hybrid_then() with tee=TRUE preserves asynchronous errors", {
  expect_assertions(3)

  counter <- 0
  result <- hybrid_then(
    promise_resolve(stop("async error")),
    on_failure = \(err) {
      counter <<- counter + 1
      "recovered"
    },
    tee = TRUE
  )
  expect_true(is.promise(result))
  result |>
    catch(\(err) {
      expect_match(err$message, "async error")
    }) |>
    wait_for_it()
  expect_equal(counter, 1)
})

test_that("hybrid_then() handles both callbacks with synchronous success", {
  success_called <- FALSE
  failure_called <- FALSE

  result <- hybrid_then(
    42,
    on_success = \(x) {
      success_called <<- TRUE
      x * 2
    },
    on_failure = \(err) {
      failure_called <<- TRUE
      "error"
    }
  )

  expect_equal(result, 84)
  expect_true(success_called)
  expect_false(failure_called)
})

test_that("hybrid_then() handles both callbacks with synchronous failure", {
  success_called <- FALSE
  failure_called <- FALSE

  result <- hybrid_then(
    stop("error"),
    on_success = \(x) {
      success_called <<- TRUE
      x * 2
    },
    on_failure = \(err) {
      failure_called <<- TRUE
      "recovered"
    }
  )

  expect_equal(result, "recovered")
  expect_false(success_called)
  expect_true(failure_called)
})

test_that("hybrid_then() handles both callbacks with asynchronous success", {
  success_called <- FALSE
  failure_called <- FALSE

  result <- hybrid_then(
    promise_resolve(42),
    on_success = \(x) {
      success_called <<- TRUE
      x * 2
    },
    on_failure = \(err) {
      failure_called <<- TRUE
      "error"
    }
  )

  expect_true(is.promise(result))
  result |> extract() |> expect_equal(84)
  expect_true(success_called)
  expect_false(failure_called)
})

test_that("hybrid_then() handles both callbacks with asynchronous failure", {
  success_called <- FALSE
  failure_called <- FALSE

  result <- hybrid_then(
    promise_resolve(stop("error")),
    on_success = \(x) {
      success_called <<- TRUE
      x * 2
    },
    on_failure = \(err) {
      failure_called <<- TRUE
      "recovered"
    }
  )

  expect_true(is.promise(result))
  result |> extract() |> expect_equal("recovered")
  expect_false(success_called)
  expect_true(failure_called)
})

test_that("hybrid_then() can chain synchronous and asynchronous operations", {
  # Start with sync, end with async
  result1 <- hybrid_then(
    42,
    on_success = \(x) promise_resolve(x + 8)
  )
  expect_true(is.promise(result1))
  result1 |> extract() |> expect_equal(50)

  # Start with async, callback returns sync
  result2 <- hybrid_then(
    promise_resolve(42),
    on_success = \(x) x + 8
  )
  expect_true(is.promise(result2))
  result2 |> extract() |> expect_equal(50)
})

test_that("hybrid_then() validates tee parameter", {
  expect_snapshot(
    hybrid_then(42, tee = "invalid"),
    error = TRUE
  )

  expect_snapshot(
    hybrid_then(42, tee = 1),
    error = TRUE
  )
})

test_that("hybrid_then() validates dots parameter", {
  expect_error(
    hybrid_then(42, on_success = \(x) x, extra_arg = "invalid"),
    "must be empty"
  )
})

test_that("hybrid_then() example from documentation works", {
  add_to <- function(x, k) {
    hybrid_then(x, on_success = function(value) {
      value + k
    })
  }

  # Synchronous
  result_sync <- 42 |> add_to(100)
  expect_equal(result_sync, 142)

  # Asynchronous
  async_called <- FALSE
  result_async <- promise_resolve(42) |> add_to(8)
  expect_true(is.promise(result_async))
  result_async |>
    then(\(x) {
      async_called <<- TRUE
      expect_equal(x, 50)
    }) |>
    wait_for_it()
  expect_true(async_called)
})

test_that("hybrid_then() on_success callback can throw errors synchronously", {
  expect_error(
    hybrid_then(
      42,
      on_success = \(x) stop("callback error")
    ),
    "callback error"
  )
})

test_that("hybrid_then() on_success callback can throw errors asynchronously", {
  expect_assertions(2)

  result <- hybrid_then(
    promise_resolve(42),
    on_success = \(x) stop("callback error")
  )
  expect_true(is.promise(result))
  result |>
    catch(\(err) {
      expect_match(err$message, "callback error")
    }) |>
    wait_for_it()
})

test_that("hybrid_then() on_failure callback can throw errors synchronously", {
  expect_error(
    hybrid_then(
      stop("original error"),
      on_failure = \(err) stop("callback error")
    ),
    "callback error"
  )
})

test_that("hybrid_then() on_failure callback can throw errors asynchronously", {
  expect_assertions(2)

  result <- hybrid_then(
    promise_resolve(stop("original error")),
    on_failure = \(err) stop("callback error")
  )
  expect_true(is.promise(result))
  result |>
    catch(\(err) {
      expect_match(err$message, "callback error")
    }) |>
    wait_for_it()
})

test_that("hybrid_then() with tee=TRUE and on_success that throws (sync)", {
  counter <- 0
  expect_error(
    hybrid_then(
      42,
      on_success = \(x) {
        counter <<- counter + 1
        stop("callback error")
      },
      tee = TRUE
    ),
    "callback error"
  )
  expect_equal(counter, 1)
})

test_that("hybrid_then() with tee=TRUE and on_success that throws (async)", {
  expect_assertions(3)

  counter <- 0
  result <- hybrid_then(
    promise_resolve(42),
    on_success = \(x) {
      counter <<- counter + 1
      stop("callback error")
    },
    tee = TRUE
  )
  expect_true(is.promise(result))
  result |>
    catch(\(err) {
      expect_match(err$message, "callback error")
    }) |>
    wait_for_it()
  expect_equal(counter, 1)
})

test_that("hybrid_then() can chain multiple hybrid_then calls synchronously", {
  result <- 1 |>
    hybrid_then(on_success = \(x) x + 1) |>
    hybrid_then(on_success = \(x) x * 2) |>
    hybrid_then(on_success = \(x) x - 1)
  expect_equal(result, 3)
})

test_that("hybrid_then() can chain multiple hybrid_then calls asynchronously", {
  expect_assertions(2)

  result <- promise_resolve(1) |>
    hybrid_then(on_success = \(x) x + 1) |>
    hybrid_then(on_success = \(x) x * 2) |>
    hybrid_then(on_success = \(x) x - 1)
  expect_true(is.promise(result))
  result |> extract() |> expect_equal(3)
})

test_that("hybrid_then() can chain with mix of sync and async results", {
  expect_assertions(2)

  result <- 1 |>
    hybrid_then(on_success = \(x) promise_resolve(x + 1)) |>
    hybrid_then(on_success = \(x) x * 2) |>
    hybrid_then(on_success = \(x) promise_resolve(x - 1))
  expect_true(is.promise(result))
  result |> extract() |> expect_equal(3)
})

test_that("hybrid_then() handles invisible values synchronously", {
  result <- hybrid_then(
    invisible(42),
    on_success = \(x) x + 8
  )
  expect_equal(result, 50)
})

test_that("hybrid_then() handles NULL return from on_success callback", {
  result <- hybrid_then(
    42,
    on_success = \(x) NULL
  )
  expect_null(result)
})

test_that("hybrid_then() handles NULL return from on_failure callback", {
  result <- hybrid_then(
    stop("error"),
    on_failure = \(err) NULL
  )
  expect_null(result)
})

test_that("hybrid_then() handles on_success returning a promise (sync to async)", {
  expect_assertions(2)

  result <- hybrid_then(
    42,
    on_success = \(x) promise_resolve(x * 2)
  )
  expect_true(is.promise(result))
  result |> extract() |> expect_equal(84)
})

test_that("hybrid_then() handles on_failure returning a promise (sync to async)", {
  expect_assertions(2)

  result <- hybrid_then(
    stop("error"),
    on_failure = \(err) promise_resolve("recovered via promise")
  )
  expect_true(is.promise(result))
  result |> extract() |> expect_equal("recovered via promise")
})

test_that("hybrid_then() with only on_failure and synchronous success", {
  result <- hybrid_then(
    42,
    on_failure = \(err) "should not be called"
  )
  expect_equal(result, 42)
})

test_that("hybrid_then() with only on_failure and asynchronous success", {
  expect_assertions(2)

  result <- hybrid_then(
    promise_resolve(42),
    on_failure = \(err) "should not be called"
  )
  expect_true(is.promise(result))
  result |> extract() |> expect_equal(42)
})

test_that("hybrid_then() preserves error class information", {
  expect_assertions(2)

  custom_error <- structure(
    list(message = "custom error"),
    class = c("custom_error", "error", "condition")
  )

  result <- hybrid_then(
    stop(custom_error),
    on_failure = \(err) {
      expect_s3_class(err, "custom_error")
      "recovered"
    }
  )
  expect_equal(result, "recovered")
})

test_that("hybrid_then() works with complex expressions", {
  result <- hybrid_then(
    {
      x <- 10
      y <- 20
      x + y
    },
    on_success = \(val) val * 2
  )
  expect_equal(result, 60)
})
