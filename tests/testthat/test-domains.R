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

    with_promise_domain(cd, {
      p <- p %...>% {
        expect_identical(cd$counts$onFulfilledCalled, 3L)
        # This tests if promise domain membership infects subscriptions made
        # in handlers.
        p %...>% {
          expect_true(!is.null(current_promise_domain()))
          expect_identical(cd$counts$onFulfilledCalled, 4L)
        }
      }
    })
    expect_true(is.null(current_promise_domain()))
    expect_identical(cd$counts$onFulfilledCalled, 2L)
    wait_for_it()
  })

  it("pass finally binding to fulfill/reject by default", {
    cd1 <- create_counting_domain(trackFinally = FALSE)

    with_promise_domain(cd1, {
      p1 <- promise_resolve(TRUE) %>%
        finally(~{
          expect_identical(cd1$counts$onFulfilledActive, 1L)
          expect_identical(cd1$counts$onRejectedActive, 0L)
        })
      expect_identical(cd1$counts$onFulfilledBound, 1L)
      expect_identical(cd1$counts$onRejectedBound, 1L)
      wait_for_it()
      expect_identical(cd1$counts$onFulfilledCalled, 1L)
      expect_identical(cd1$counts$onRejectedCalled, 0L)
    })

    cd2 <- create_counting_domain(trackFinally = FALSE)

    with_promise_domain(cd2, {
      p1 <- promise_reject("a problem") %>%
        finally(~{
          expect_identical(cd2$counts$onFulfilledActive, 0L)
          expect_identical(cd2$counts$onRejectedActive, 1L)
        })
      p1
    }) %>% squelch_unhandled_promise_error()

    expect_identical(cd2$counts$onFulfilledBound, 1L)
    expect_identical(cd2$counts$onRejectedBound, 1L)
    wait_for_it()
    expect_identical(cd2$counts$onFulfilledCalled, 0L)
    expect_identical(cd2$counts$onRejectedCalled, 1L)
  })

  it("doesn't intercept fulfill/reject on finally, if finally is explicitly intercepted", {
    cd1 <- create_counting_domain(trackFinally = TRUE)

    with_promise_domain(cd1, {
      p1 <- promise_resolve(TRUE) %>%
        finally(~{
          expect_identical(cd1$counts$onFinallyActive, 1L)
          expect_identical(cd1$counts$onFulfilledActive, 0L)
          expect_identical(cd1$counts$onRejectedActive, 0L)
        })
      expect_identical(cd1$counts$onFinallyBound, 1L)
      expect_identical(cd1$counts$onFulfilledBound, 0L)
      expect_identical(cd1$counts$onRejectedBound, 0L)
      wait_for_it()
      expect_identical(cd1$counts$onFinallyCalled, 1L)
      expect_identical(cd1$counts$onFulfilledCalled, 0L)
      expect_identical(cd1$counts$onRejectedCalled, 0L)
    })

    cd2 <- create_counting_domain(trackFinally = TRUE)

    with_promise_domain(cd2, {
      p2 <- promise_reject(TRUE) %>%
        finally(~{
          expect_identical(cd2$counts$onFinallyActive, 1L)
          expect_identical(cd2$counts$onFulfilledActive, 0L)
          expect_identical(cd2$counts$onRejectedActive, 0L)
        })
      p2
    }) %>% squelch_unhandled_promise_error()

    expect_identical(cd2$counts$onFinallyBound, 1L)
    expect_identical(cd2$counts$onFulfilledBound, 0L)
    expect_identical(cd2$counts$onRejectedBound, 0L)
    wait_for_it()
    expect_identical(cd2$counts$onFinallyCalled, 1L)
    expect_identical(cd2$counts$onFulfilledCalled, 0L)
    expect_identical(cd2$counts$onRejectedCalled, 0L)
  })

})
