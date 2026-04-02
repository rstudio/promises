test_that("wait_for() returns resolved value", {
  p <- promise_resolve(42)
  expect_equal(wait_for(p), 42)
})

test_that("wait_for() works with delayed resolution", {
  p <- promise(\(resolve, reject) {
    later::later(\() resolve("delayed"), delay = 0.1)
  })
  expect_equal(wait_for(p), "delayed")
})

test_that("wait_for() works with already-resolved promise", {
  ep <- ext_promise()
  ep$resolve("already done")
  wait_for_it()
  expect_equal(wait_for(ep$promise), "already done")
})

test_that("wait_for() throws on rejected promise", {
  p <- promise_resolve(stop("boom"))
  squelch_unhandled_promise_error(p)
  expect_error(wait_for(p), "boom")
})

test_that("wait_for() preserves error message from string rejection", {
  p <- promise(\(resolve, reject) reject("something went wrong"))
  squelch_unhandled_promise_error(p)
  expect_error(wait_for(p), "something went wrong")
})

test_that("wait_for() preserves custom condition class", {
  cnd <- simpleError("custom err")
  class(cnd) <- c("my_error", class(cnd))
  p <- promise(\(resolve, reject) reject(cnd))
  squelch_unhandled_promise_error(p)
  expect_error(wait_for(p), class = "my_error")
})

test_that("wait_for() preserves rlang error class", {
  skip_if_not_installed("rlang")
  p <- promise_resolve(rlang::abort(
    "rlang error",
    class = "custom_rlang_error"
  ))
  squelch_unhandled_promise_error(p)
  cnd <- tryCatch(wait_for(p), error = identity)
  expect_s3_class(cnd, "custom_rlang_error")
  expect_match(conditionMessage(cnd), "rlang error")
})

test_that("wait_for() preserves visibility", {
  p_visible <- promise_resolve(42)
  vis <- withVisible(wait_for(p_visible))
  expect_true(vis$visible)
  expect_equal(vis$value, 42)

  p_invisible <- promise(\(resolve, reject) resolve(invisible(10)))
  vis2 <- withVisible(wait_for(p_invisible))
  expect_false(vis2$visible)
  expect_equal(vis2$value, 10)
})

test_that("wait_for() throws on delayed rejection", {
  p <- promise(\(resolve, reject) {
    later::later(\() reject("delayed fail"), delay = 0.1)
  })
  squelch_unhandled_promise_error(p)
  expect_error(wait_for(p), "delayed fail")
})

test_that("wait_for() throws on already-rejected promise", {
  ep <- ext_promise()
  ep$reject("already failed")
  squelch_unhandled_promise_error(ep$promise)
  wait_for_it()
  expect_error(wait_for(ep$promise), "already failed")
})

test_that("wait_for() works on promise chain that resolves", {
  p <- promise_resolve(1) |>
    then(\(x) x + 1) |>
    then(\(x) x * 3)
  expect_equal(wait_for(p), 6)
})

test_that("wait_for() throws on promise chain that rejects", {
  p <- promise_resolve(1) |>
    then(\(x) stop("chain error")) |>
    then(\(x) x + 1)
  squelch_unhandled_promise_error(p)
  expect_error(wait_for(p), "chain error")
})

test_that("wait_for() errors on non-promise input", {
  expect_error(wait_for(42), "wait_for\\(\\) requires a promise object")
  expect_error(wait_for("hello"), "wait_for\\(\\) requires a promise object")
  expect_error(wait_for(NULL), "wait_for\\(\\) requires a promise object")
})

test_that("wait_for() works with mirai promise that resolves", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  p <- as.promise(mirai::mirai(42))
  expect_equal(wait_for(p), 42)
})

test_that("wait_for() works with mirai promise that rejects", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  p <- as.promise(mirai::mirai(stop("there was an error")))
  squelch_unhandled_promise_error(p)
  expect_snapshot(wait_for(p), error = TRUE)
})

test_that("wait_for() works with mirai promise chained via then()", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  p <- as.promise(mirai::mirai(10)) |>
    then(\(x) x + 5) |>
    then(\(x) x * 2)
  expect_equal(wait_for(p), 30)
})

test_that("wait_for() works with mirai promise caught via catch()", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  p <- as.promise(mirai::mirai(stop("oops"))) |>
    catch(\(e) "recovered")
  expect_equal(wait_for(p), "recovered")
})

test_that("wait_for() throws when mirai then() callback errors", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  p <- as.promise(mirai::mirai(1)) |>
    then(\(x) stop("chain error"))
  squelch_unhandled_promise_error(p)
  expect_snapshot(wait_for(p), error = TRUE)
})
