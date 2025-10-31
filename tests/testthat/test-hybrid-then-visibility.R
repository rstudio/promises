test_that("hybrid_then() preserves visible values from expr (no callback)", {
  result <- hybrid_then(42)
  expect_equal(result, 42)
  # Check visibility by capturing with withVisible
  vis <- withVisible(hybrid_then(42))
  expect_true(vis$visible)
})

test_that("hybrid_then() with invisible expr (no callback)", {
  result <- hybrid_then(invisible(42))
  expect_equal(result, 42)
  vis <- withVisible(hybrid_then(invisible(42)))
  expect_false(vis$visible)
})

test_that("hybrid_then() with visible expr and on_success returning visible", {
  result <- hybrid_then(
    42,
    on_success = \(x) x + 8
  )
  expect_equal(result, 50)
  vis <- withVisible(hybrid_then(42, on_success = \(x) x + 8))
  expect_true(vis$visible)
})

test_that("hybrid_then() with visible expr and on_success returning invisible", {
  result <- hybrid_then(
    42,
    on_success = \(x) invisible(x + 8)
  )
  expect_equal(result, 50)
  vis <- withVisible(hybrid_then(42, on_success = \(x) invisible(x + 8)))
  expect_false(vis$visible)
})

test_that("hybrid_then() with invisible expr and on_success returning visible", {
  result <- hybrid_then(
    invisible(42),
    on_success = \(x) x + 8
  )
  expect_equal(result, 50)
  vis <- withVisible(hybrid_then(invisible(42), on_success = \(x) x + 8))
  expect_true(vis$visible)
})

test_that("hybrid_then() with invisible expr and on_success returning invisible", {
  result <- hybrid_then(
    invisible(42),
    on_success = \(x) invisible(x + 8)
  )
  expect_equal(result, 50)
  vis <- withVisible(hybrid_then(invisible(42), on_success = \(x) {
    invisible(x + 8)
  }))
  expect_false(vis$visible)
})

test_that("hybrid_then() with tee=TRUE preserves expr visibility (visible expr)", {
  result <- hybrid_then(
    42,
    on_success = \(x) invisible(999),
    tee = TRUE
  )
  expect_equal(result, 42)
  vis <- withVisible(hybrid_then(
    42,
    on_success = \(x) invisible(999),
    tee = TRUE
  ))
  expect_true(vis$visible)
})

test_that("hybrid_then() with tee=TRUE preserves expr visibility (invisible expr)", {
  result <- hybrid_then(
    invisible(42),
    on_success = \(x) 999, # callback returns visible
    tee = TRUE
  )
  expect_equal(result, 42)
  vis <- withVisible(hybrid_then(
    invisible(42),
    on_success = \(x) 999,
    tee = TRUE
  ))
  expect_false(vis$visible)
})

test_that("hybrid_then() on_success receives .visible parameter correctly (visible)", {
  visible_captured <- NULL
  result <- hybrid_then(
    42,
    on_success = \(x, .visible) {
      visible_captured <<- .visible
      x * 2
    }
  )
  expect_equal(result, 84)
  expect_true(visible_captured)
})

test_that("hybrid_then() on_success receives .visible parameter (invisible)", {
  visible_captured <- NULL
  result <- hybrid_then(
    invisible(42),
    on_success = \(x, .visible) {
      visible_captured <<- .visible
      x * 2
    }
  )
  expect_equal(result, 84)
  expect_false(visible_captured)
})

test_that("hybrid_then() on_success with .visible conditionally returning", {
  make_conditional <- function(val) {
    hybrid_then(
      val,
      on_success = \(x, .visible) {
        if (.visible) {
          x * 2
        } else {
          invisible(x * 2)
        }
      }
    )
  }

  vis1 <- withVisible(make_conditional(42))
  expect_equal(vis1$value, 84)
  expect_true(vis1$visible)

  vis2 <- withVisible(make_conditional(invisible(42)))
  expect_equal(vis2$value, 84)
  expect_false(vis2$visible)
})

test_that("hybrid_then() async with visible promise and on_success returning visible", {
  expect_assertions(n = 2, {
    result <- hybrid_then(
      promise_resolve(42),
      on_success = \(x) x + 8
    )
    expect_true(is.promise(result))
    result |> extract() |> expect_equal(50)
  })
})

test_that("hybrid_then() async with visible promise and on_success returning invisible", {
  expect_assertions(n = 2, {
    result <- hybrid_then(
      promise_resolve(42),
      on_success = \(x) invisible(x + 8)
    )
    expect_true(is.promise(result))
    result |> extract() |> expect_equal(50)
  })
})

test_that("hybrid_then() async visibility is handled by underlying then()", {
  expect_assertions(n = 3, {
    # The visibility semantics for async are handled by then()
    # Just verify the values work correctly
    visible_captured <- NULL
    result <- hybrid_then(
      promise_resolve(42),
      on_success = \(x, .visible) {
        visible_captured <<- .visible
        x * 2
      }
    )
    expect_true(is.promise(result))
    result |> extract() |> expect_equal(84)
    expect_true(visible_captured)
  })
})

test_that("hybrid_then() with tee=TRUE and visible expr", {
  # With tee=TRUE, expr visibility should be preserved regardless of callback
  vis_test <- function(expr_vis, callback_vis) {
    withVisible(
      hybrid_then(
        if (expr_vis) 42 else invisible(43),
        on_success = if (callback_vis) {
          \(x) x * 2
        } else {
          \(x) invisible(x * 3)
        },
        tee = TRUE
      )
    )
  }

  # Visible expr preserves visibility
  expect_equal(vis_test(TRUE, TRUE)$value, 42)
  expect_true(vis_test(TRUE, TRUE)$visible)

  expect_equal(vis_test(TRUE, FALSE)$value, 42)
  expect_true(vis_test(TRUE, FALSE)$visible)

  expect_equal(vis_test(FALSE, TRUE)$value, 43)
  expect_false(vis_test(FALSE, TRUE)$visible)

  expect_equal(vis_test(FALSE, FALSE)$value, 43)
  expect_false(vis_test(FALSE, FALSE)$visible)
})

test_that("hybrid_then() no callback with visible expr preserves visibility", {
  # Visible expr
  vis1 <- withVisible(hybrid_then(42))
  expect_equal(vis1$value, 42)
  expect_true(vis1$visible)
})

test_that("hybrid_then() no callback with invisible expr", {
  # Invisible expr via invisible()
  vis2 <- withVisible(hybrid_then(invisible(42)))
  expect_equal(vis2$value, 42)
  expect_false(vis2$visible)

  # Expression that returns invisible
  vis3 <- withVisible(hybrid_then({
    x <- 42
    invisible(x)
  }))
  expect_equal(vis3$value, 42)
  expect_false(vis3$visible)
})

test_that("hybrid_then() on_failure with invisible error recovery", {
  result <- hybrid_then(
    stop("error"),
    on_failure = \(err) invisible(0)
  )
  expect_equal(result, 0)
  vis <- withVisible(hybrid_then(stop("error"), on_failure = \(err) {
    invisible(0)
  }))
  expect_false(vis$visible)
})

test_that("hybrid_then() on_failure with visible error recovery", {
  result <- hybrid_then(
    stop("error"),
    on_failure = \(err) 0
  )
  expect_equal(result, 0)
  vis <- withVisible(hybrid_then(stop("error"), on_failure = \(err) 0))
  expect_true(vis$visible)
})
