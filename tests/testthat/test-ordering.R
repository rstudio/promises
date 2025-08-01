test_that("promise resolution order matches expectations", {
  # See discussion in #151 - this is just the current behaviour. There may be
  # grounds for change if we find a legitimate reason for doing so.
  y <- 0L
  p1 <- promise_resolve(1) |> then(\(x) y <<- y + 1) |> then(\(x) y <<- y / 2)
  p2 <- promise_resolve(2) |> then(\(x) y <<- y + 3) |> then(\(x) y <<- y * 2)
  later::run_now()
  expect_equal(y, 4)
  later::run_now()
  expect_equal(y, 4)
})
