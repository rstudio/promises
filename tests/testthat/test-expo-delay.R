test_that("ExpoDelay caps the backoff at 2 seconds", {
  d <- ExpoDelay$new()
  for (i in 1:30) {
    d$increase()
  }
  delays <- replicate(200, d$delay())
  expect_true(all(delays >= 0 & delays <= 2))
  expect_true(any(delays > 1))
})
