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
