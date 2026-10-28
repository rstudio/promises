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
    wait_for()
  expect_equal(counter, 4)
})

test_that("then() handles NULL callbacks correctly", {
  # NULL onFulfilled should pass through value
  expect_assertions(n = 1, {
    promise_resolve(42) |>
      then(NULL, \(err) "error") |>
      wait_for() |>
      expect_equal(42)
  })
})

test_that("catch() handles NULL callbacks correctly", {
  # NULL onRejected should pass through error
  expect_assertions(n = 1, {
    promise_resolve(stop("boom")) |>
      then(\(x) x, NULL) |>
      catch(\(err) expect_match(err$message, "boom")) |>
      wait_for()
  })
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
  wait_for_all()
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
  wait_for_all()
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
    wait_for()

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
    wait_for()
  expect_equal(error_counter, 3)
})

test_that("primitive functions receive the value as callbacks", {
  promise_resolve(c(1, 2)) |> then(sum) |> wait_for() |> expect_equal(3)
  promise_resolve(-1) |> then(abs) |> wait_for() |> expect_equal(1)
  promise_reject("boom") |>
    catch(class) |>
    wait_for() |>
    expect_equal(c("simpleError", "error", "condition"))
  hybrid_then(c(1, 2), sum) |> expect_equal(3)
})

test_that("catch(NULL, tee = TRUE) passes the original error through", {
  promise_reject("boom") |>
    catch(NULL, tee = TRUE) |>
    catch(conditionMessage) |>
    wait_for() |>
    expect_equal("boom")
})

test_that("`tee` must be a single TRUE or FALSE", {
  p <- promise_resolve(1)
  for (bad in list(NA, c(TRUE, FALSE), "yes")) {
    expect_error(then(p, identity, tee = bad), "`tee` must be")
    expect_error(catch(p, identity, tee = bad), "`tee` must be")
    expect_error(hybrid_then(1, identity, tee = bad), "`tee` must be")
  }
})

test_that("finally() and tee callbacks keep non-condition rejection reasons", {
  keeps_reason <- function(p) {
    p |>
      catch(\(reason) reason) |>
      wait_for() |>
      expect_identical(42)
  }
  promise_reject(42) |> finally(\() NULL) |> keeps_reason()
  promise_reject(42) |>
    then(onRejected = \(e) NULL, tee = TRUE) |>
    keeps_reason()
  promise_reject(42) |> catch(\(e) NULL, tee = TRUE) |> keeps_reason()
  hybrid_then(promise_reject(42), on_failure = \(e) NULL, tee = TRUE) |>
    keeps_reason()

  # Conditions are passed through as the same object
  err <- simpleError("boom")
  promise_reject(err) |>
    finally(\() NULL) |>
    catch(\(reason) reason) |>
    wait_for() |>
    expect_identical(err)
})

test_that("domain onError sees the reason when a non-condition is re-thrown", {
  seen <- NULL
  d <- new_promise_domain(onError = function(e) seen <<- e)
  with_promise_domain(d, {
    p <- promise_reject(42) |> finally(\() NULL)
  })
  p |> catch(\(reason) reason) |> wait_for() |> expect_identical(42)

  # Same message `stop(42)` gave before non-condition reasons were kept
  expect_s3_class(seen, "simpleError")
  expect_identical(conditionMessage(seen), "42")
})
