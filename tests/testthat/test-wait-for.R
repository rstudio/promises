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
  wait_for_all()
  expect_equal(wait_for(ep$promise), "already done")
})

test_that("wait_for() throws on rejected promise", {
  p <- promise_resolve(stop("boom"))
  expect_error(wait_for(p), "boom")
})

test_that("wait_for() preserves error message from string rejection", {
  p <- promise(\(resolve, reject) reject("something went wrong"))
  expect_error(wait_for(p), "something went wrong")
})

test_that("wait_for() preserves custom condition class", {
  cnd <- simpleError("custom err")
  class(cnd) <- c("my_error", class(cnd))
  p <- promise(\(resolve, reject) reject(cnd))
  expect_error(wait_for(p), class = "my_error")
})

test_that("wait_for() preserves rlang error class", {
  skip_if_not_installed("rlang")
  p <- promise_resolve(rlang::abort(
    "rlang error",
    class = "custom_rlang_error"
  ))
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
  expect_error(wait_for(p), "delayed fail")
})

test_that("wait_for() throws on already-rejected promise", {
  ep <- ext_promise()
  ep$reject("already failed")
  expect_equal(ep$status(), "rejected")
  expect_error(wait_for(ep$promise), "already failed")
})

test_that("wait_for() does not report the rejection as unhandled", {
  p <- promise_reject(simpleError("boom"))
  msgs <- capture.output(type = "message", {
    expect_error(wait_for(p), "boom")
    wait_for_all()
  })
  expect_equal(msgs, character())
})

test_that("wait_for() throws non-condition rejection reasons", {
  expect_error(wait_for(promise_reject(42)), "42")
  expect_error(wait_for(promise_reject(NULL)), class = "simpleError")
})

test_that("wait_for() runs the given loop", {
  loop <- later::create_loop(parent = NULL)
  on.exit(later::destroy_loop(loop))
  p <- later::with_loop(loop, {
    promise(\(resolve, reject) later::later(\() resolve("private")))
  })
  expect_equal(wait_for(p, loop = loop), "private")
})

test_that("wait_for() requires loop to be named", {
  loop <- later::create_loop(parent = NULL)
  on.exit(later::destroy_loop(loop))
  expect_error(wait_for(promise_resolve(1), loop), class = "rlib_error_dots_nonempty")
})

test_that("wait_for() works inside a later callback", {
  value <- NULL
  later::later(\() value <<- wait_for(resolve_later(1, 0.1)))
  wait_for_all()
  expect_equal(value, 1)
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
  expect_error(wait_for(p), "chain error")
})

test_that("wait_for() errors on non-promise input", {
  expect_error(wait_for(42), "wait_for\\(\\) requires a promise object")
  expect_error(wait_for("hello"), "wait_for\\(\\) requires a promise object")
  expect_error(wait_for(NULL), "wait_for\\(\\) requires a promise object")
})

test_that("wait_for() works with a mirai object", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  p <- mirai::mirai(42)
  expect_equal(wait_for(p), 42)
})

test_that("wait_for() works with mirai promise that rejects", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  p <- mirai::mirai(stop("there was an error"))
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
  expect_snapshot(wait_for(p), error = TRUE)
})
