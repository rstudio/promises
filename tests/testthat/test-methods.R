library(testthat)

describe("then()", {
  it("honors .visible argument", {
    result <- NULL
    p <- promise(\(resolve, reject) resolve(invisible(1))) |>
      then(function(value, .visible) {
        result <<- list(value = value, visible = .visible)
      })
    p |> wait_for_it()
    expect_identical(result$value, 1)
    expect_identical(result$visible, FALSE)

    p <- promise(\(resolve, reject) resolve(1)) |>
      then(function(value, .visible) {
        result <<- list(value = value, visible = .visible)
      })
    p |> wait_for_it()
    expect_identical(result$value, 1)
    expect_identical(result$visible, TRUE)

    # .visible is preserved even with an intermediate then() or catch()
    p <- promise(\(resolve, reject) resolve(invisible(1))) |>
      then() |>
      catch(\(err) "what error?") |>
      then(function(value, .visible) {
        result <<- list(value = value, visible = .visible)
      })
    p |> wait_for_it()
    expect_identical(result$value, 1)
    expect_identical(result$visible, FALSE)
  })
  it("method ignores non-functions or NULL...", {
    p1 <- promise(\(resolve, reject) resolve(1))
    expect_warning(
      {
        p1 <- p1$then(10)
      },
      "`onFulfilled` must be a function or `NULL`",
      fixed = TRUE
    )
    expect_warning(
      {
        p1 <- p1$then(NULL)
      },
      NA
    )
    expect_identical(wait_for(p1), 1)
  })
  it("...but function only ignores NULL, not non-functions", {
    expect_error(promise(\(resolve, reject) resolve(1)) |> then(10))
    expect_error(promise(\(resolve, reject) resolve(1)) |> then(NULL), NA)
  })

  it("honors visibility with no .visible argument", {
    result <- NULL
    p <- promise_resolve(invisible(1))$then(function(value) {
      result <<- withVisible(value)
    })
    p |> wait_for_it()
    expect_identical(result$value, 1)
    expect_identical(result$visible, FALSE)

    result <- NULL
    p <- promise_resolve(2)$then(function(value) {
      result <<- withVisible(value)
    })
    p |> wait_for_it()
    expect_identical(result$value, 2)
    expect_identical(result$visible, TRUE)
  })
})

describe("catch()", {
  it("catches", {
    p <- ext_promise()
    p2 <- p$promise |> catch(\(err) TRUE)
    p$reject("boom")
    expect_identical(wait_for(p2), TRUE)
  })
  it("can throw", {
    p <- promise(\(resolve, reject) stop("foo")) |> catch(\(err) stop("bar"))
    expect_error(wait_for(p), "^bar$")
  })
  it("method ignores non-functions or NULL...", {
    p1 <- promise(\(resolve, reject) resolve(1))
    expect_warning(
      {
        p1 <- p1$catch(10)
      },
      "`onRejected` must be a function or `NULL`",
      fixed = TRUE
    )
    expect_warning(
      {
        p1 <- p1$catch(NULL)
      },
      NA
    )
    expect_identical(wait_for(p1), 1)
  })
  it("...but function only ignores NULL, not non-functions", {
    expect_error(promise(\(resolve, reject) resolve(1)) |> catch(10))
    expect_error(promise(\(resolve, reject) resolve(1)) |> catch(NULL), NA)
  })
})

describe("finally()", {
  it("calls back when a promise is resolved", {
    called <- FALSE
    p <- promise(\(resolve, reject) resolve(10)) |>
      finally(\() {
        called <<- TRUE
      })
    p |> wait_for_it()
    expect_identical(called, TRUE)
    expect_identical(wait_for(p), 10)
  })
  it("calls back when a promise is rejected", {
    called <- FALSE
    p <- promise(\(resolve, reject) reject("foobar"))
    p |>
      finally(\() {
        called <<- TRUE
      }) |>
      squelch_unhandled_promise_error() |>
      wait_for_it()
    expect_identical(called, TRUE)
    expect_error(wait_for(p), "^foobar$")
  })
  it("does not affect the return value of the promise", {
    p1 <- promise(\(resolve, reject) resolve(1)) |>
      finally(\() {
        20
      })
    expect_identical(wait_for(p1), 1)

    p2 <- promise(\(resolve, reject) reject("err")) |>
      finally(\() {
        20
      })
    expect_error(wait_for(p2), "^err$")
  })
  it("errors replace the result of the promise", {
    p1 <- promise(\(resolve, reject) resolve(1)) |>
      finally(\() {
        stop("boom")
      })
    expect_error(wait_for(p1), "^boom$")

    p2 <- promise(\(resolve, reject) reject("foo")) |>
      finally(\() {
        stop("bar")
      })
    expect_error(wait_for(p2), "^bar$")
  })
  it("method ignores non-functions or NULL...", {
    p1 <- promise(\(resolve, reject) resolve(1))$finally(10)$finally(NULL)
    expect_identical(wait_for(p1), 1)
  })
  it("...but function only ignores NULL, not non-functions", {
    expect_error(promise(\(resolve, reject) resolve(1)) |> finally(10))
    expect_error(promise(\(resolve, reject) resolve(1)) |> finally(NULL), NA)
  })
})

describe("future", {
  skip_if_not_installed("future")
  it("is treated as promise when used as resolution", {
    p <- promise_resolve(future::future(1))
    expect_identical(wait_for(p), 1)

    p2 <- promise_resolve(future::future(stop("boom")))
    expect_error(wait_for(p2))
  })

  it("is treated as promise when used as resolution", {
    p <- promise_reject(future::future(1))
    expect_identical(wait_for(p), 1)

    p2 <- promise_reject(future::future(stop("boom")))
    expect_error(wait_for(p2))
  })
})

test_that("unhandled rejection message works for non-condition reasons", {
  out <- capture.output(type = "message", {
    promise_reject(42)
    wait_for_it()
  })
  expect_identical(out, "Unhandled promise error: 42")
})

test_that("promises that resolve to each other reject with a cycle error", {
  a <- ext_promise()
  b <- ext_promise()
  a$resolve(b$promise)
  b$resolve(a$promise)
  for (p in list(a$promise, b$promise)) {
    p |>
      catch(conditionMessage) |>
      wait_for() |>
      expect_match("Chaining cycle detected")
  }
})

test_that("a settled promise drops its link to the promise it followed", {
  following <- function(p) {
    attr(p, "promise_impl", exact = TRUE)$.__enclos_env__$private$following
  }
  inner <- ext_promise()
  outer <- promise(\(resolve, reject) resolve(inner$promise))
  expect_false(is.null(following(outer)))

  inner$resolve(1)
  expect_identical(wait_for(outer), 1)
  expect_null(following(outer))
})

test_that("handlers attached in the same tick prevent the unhandled message", {
  out <- capture.output(type = "message", {
    p <- promise_reject("boom")
    catch(p, \(reason) NULL)
    wait_for_it()
  })
  expect_length(out, 0)
})

test_that("promise_reduce() accepts a function name for .f", {
  promise_reduce(1:3, "+", .init = 0) |> wait_for() |> expect_equal(6)
})

test_that("promise_reduce() without .init starts from the first element", {
  promise_reduce(1:3, `+`) |> wait_for() |> expect_equal(6)
  promise_reduce(list("a"), paste0) |> wait_for() |> expect_equal("a")
  expect_error(promise_reduce(list(), `+`), "`.init`")
})

test_that("promise_race() with no promises stays pending", {
  p <- promise_race()
  wait_for_it()
  expect_identical(attr(p, "promise_impl", exact = TRUE)$status(), "pending")
})
