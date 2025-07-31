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
