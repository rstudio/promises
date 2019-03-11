context("Promise domains")

source("common.R")

describe("Promise domains", {

  it("are reentered during handlers", {
    cd <- create_counting_domain(trackFinally = TRUE)
    p <- with_promise_domain(cd, {
      promise_resolve(TRUE) %...>% {
        expect_identical(cd$counts$onFulfilledCalled, 1L)
        expect_identical(cd$counts$onFulfilledActive, 1L)
        10 # sync result
      } %...>% {
        expect_identical(cd$counts$onFulfilledCalled, 2L)
        expect_identical(cd$counts$onFulfilledActive, 1L)
        promise_resolve(20) # async result
      }
    })

    expect_identical(cd$counts$onFulfilledBound, 2L)

    p <- p %...>% {
      expect_identical(cd$counts$onFulfilledCalled, 2L)
      expect_identical(cd$counts$onFulfilledActive, 0L)
    }

    expect_identical(cd$counts$onFulfilledBound, 2L)

    with_promise_domain(cd, {
      p <- p %>% finally(~{
        expect_identical(cd$counts$onFinallyCalled, 1L)
        expect_identical(cd$counts$onFinallyActive, 1L)
      })
      expect_identical(cd$counts$onFinallyBound, 1L)

      expect_identical(cd$counts$onFulfilledBound, 2L)
      expect_identical(cd$counts$onRejectedBound, 0L)

      wait_for_it()
    })

    expect_identical(cd$counts$onFulfilledBound, 2L)
  })
})
