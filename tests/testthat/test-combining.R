library(testthat)

source("common.R")

describe("promise_all", {
  it("preserves element ordering", {
    a <- resolve_later(1, 0.5)
    b <- resolve_later(2, 0.3)
    c <- resolve_later(3, 0.1)

    x <- promise_all(.list = list(a=a, b=b, c=c))
    expect_identical(extract(x), list(a=1, b=2, c=3))
  })
})
