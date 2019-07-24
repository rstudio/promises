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

  it("Handles NULLs correctly", {
    x <- promise_all(promise_resolve(NULL), promise_resolve(NULL),
                     promise_resolve(NULL))
    expect_identical(extract(x), list(NULL, NULL, NULL))

    x <- promise_all(a = promise_resolve(NULL), b = promise_resolve(NULL),
                     c = promise_resolve(NULL))
    expect_identical(extract(x), list(a = NULL, b = NULL, c = NULL))
  })
})
