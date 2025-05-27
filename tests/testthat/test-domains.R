source("common.R")

async_true <- function() {
  promise_resolve(TRUE)
}

describe("Promise domains", {
  it("are reentered during handlers", {
    cd <- create_counting_domain(trackFinally = TRUE)
    p <- with_promise_domain(cd, {
      async_true() %...>%
        {
          expect_identical(cd$counts$onFulfilledCalled, 1L)
          expect_identical(cd$counts$onFulfilledActive, 1L)
          10 # sync result
        } %...>%
        {
          expect_identical(cd$counts$onFulfilledCalled, 2L)
          expect_identical(cd$counts$onFulfilledActive, 1L)
          promise_resolve(20) # async result
        }
    })

    expect_identical(cd$counts$onFulfilledBound, 2L)

    p <- p %...>%
      {
        expect_identical(cd$counts$onFulfilledCalled, 2L)
        expect_identical(cd$counts$onFulfilledActive, 0L)
      }

    expect_identical(cd$counts$onFulfilledBound, 2L)
    p %>% wait_for_it()

    with_promise_domain(cd, {
      p <- async_true() %>%
        finally(
          ~ {
            expect_identical(cd$counts$onFinallyCalled, 1L)
            expect_identical(cd$counts$onFinallyActive, 1L)
          }
        )
      expect_identical(cd$counts$onFinallyBound, 1L)

      expect_identical(cd$counts$onFulfilledBound, 2L)
      expect_identical(cd$counts$onRejectedBound, 0L)

      p %>% wait_for_it()
    })

    expect_identical(cd$counts$onFulfilledBound, 2L)

    with_promise_domain(cd, {
      p <- async_true() %...>%
        {
          expect_identical(., TRUE)
          expect_identical(cd$counts$onFulfilledCalled, 3L)
          ten <- 10
          # This tests if promise domain membership infects subscriptions made
          # in handlers.
          promise_resolve(invisible(ten)) %...>%
            (function(value, .visible) {
              expect_identical(value, 10)
              expect_false(.visible)
              expect_true(!is.null(current_promise_domain()))
              expect_identical(cd$counts$onFulfilledCalled, 4L)
            })
        }
    })

    expect_true(is.null(current_promise_domain()))
    expect_identical(cd$counts$onFulfilledCalled, 2L)

    p %>% wait_for_it()
  })

  it("pass finally binding to fulfill/reject by default", {
    cd1 <- create_counting_domain(trackFinally = FALSE)

    with_promise_domain(cd1, {
      p1 <- async_true() %>%
        finally(
          ~ {
            expect_identical(cd1$counts$onFulfilledActive, 1L)
            expect_identical(cd1$counts$onRejectedActive, 0L)
          }
        )
      expect_identical(cd1$counts$onFulfilledBound, 1L)
      expect_identical(cd1$counts$onRejectedBound, 1L)
      p1 %>% wait_for_it()
      expect_identical(cd1$counts$onFulfilledCalled, 1L)
      expect_identical(cd1$counts$onRejectedCalled, 0L)
    })

    cd2 <- create_counting_domain(trackFinally = FALSE)

    p2 <- with_promise_domain(cd2, {
      promise_reject("a problem") %>%
        finally(
          ~ {
            expect_identical(cd2$counts$onFulfilledActive, 0L)
            expect_identical(cd2$counts$onRejectedActive, 1L)
          }
        )
    }) %>%
      squelch_unhandled_promise_error()

    expect_identical(cd2$counts$onFulfilledBound, 1L)
    expect_identical(cd2$counts$onRejectedBound, 1L)
    p2 %>% wait_for_it()
    expect_identical(cd2$counts$onFulfilledCalled, 0L)
    expect_identical(cd2$counts$onRejectedCalled, 1L)
  })

  it("doesn't intercept fulfill/reject on finally, if finally is explicitly intercepted", {
    cd1 <- create_counting_domain(trackFinally = TRUE)

    with_promise_domain(cd1, {
      p1 <- async_true() %>%
        finally(
          ~ {
            expect_identical(cd1$counts$onFinallyActive, 1L)
            expect_identical(cd1$counts$onFulfilledActive, 0L)
            expect_identical(cd1$counts$onRejectedActive, 0L)
          }
        )
      expect_identical(cd1$counts$onFinallyBound, 1L)
      expect_identical(cd1$counts$onFulfilledBound, 0L)
      expect_identical(cd1$counts$onRejectedBound, 0L)
      p1 %>% wait_for_it()
      expect_identical(cd1$counts$onFinallyCalled, 1L)
      expect_identical(cd1$counts$onFulfilledCalled, 0L)
      expect_identical(cd1$counts$onRejectedCalled, 0L)
    })

    cd2 <- create_counting_domain(trackFinally = TRUE)

    p2 <- with_promise_domain(cd2, {
      promise_reject(TRUE) %>%
        finally(
          ~ {
            expect_identical(cd2$counts$onFinallyActive, 1L)
            expect_identical(cd2$counts$onFulfilledActive, 0L)
            expect_identical(cd2$counts$onRejectedActive, 0L)
          }
        )
    }) %>%
      squelch_unhandled_promise_error()

    expect_identical(cd2$counts$onFinallyBound, 1L)
    expect_identical(cd2$counts$onFulfilledBound, 0L)
    expect_identical(cd2$counts$onRejectedBound, 0L)
    p2 %>% wait_for_it()
    expect_identical(cd2$counts$onFinallyCalled, 1L)
    expect_identical(cd2$counts$onFulfilledCalled, 0L)
    expect_identical(cd2$counts$onRejectedCalled, 0L)
  })

  it("handles weird edge case relating to symbols", {
    # This test resulted from a bug in wrap_callback_reenter() where do.call()'s
    # default behavior of quote=FALSE would cause a resolved value that happens
    # to be a symbol, to be evaluated. This would only happen when a promise
    # domain was in effect, and the symbol was passed to an onFulfilled. Fixed
    # by using rlang::exec() instead of do.call().
    cd <- create_counting_domain()
    with_promise_domain(cd, {
      promise_resolve(as.symbol("foo")) %...>%
        {
          expect_identical(., as.symbol("foo"))
        } %>%
        wait_for_it()
    })
  })

  it("executes wrap_callback_reenter handlers in the right lexical environment", {
    # No reason this shouldn't work, I haven't seen it fail, just making sure.
    cd <- create_counting_domain()
    x <- NULL
    with_promise_domain(cd, {
      promise_resolve(NULL) %...>%
        {
          x <<- 1L
        } %...>%
        {
          x <<- x + 1L
        } %>%
        wait_for_it()
    })
    expect_identical(x, 2L)
  })

  it("doesn't grow the call stack", {
    # See https://github.com/rstudio/promises/issues/114
    # and also https://github.com/jcheng5/shinychat/issues/16

    recursive_promise <- function(n, .last_callstack_depth = NULL) {
      if (n == 0) {
        return(0)
      } else {
        promise_resolve(TRUE) %...>%
          {
            current_callstack_depth <- length(sys.calls())
            if (!is.null(.last_callstack_depth)) {
              expect_identical(current_callstack_depth, .last_callstack_depth)
            }

            recursive_promise(
              n - 1,
              .last_callstack_depth = current_callstack_depth
            )
          }
      }
    }

    cd <- create_counting_domain()
    with_promise_domain(cd, {
      recursive_promise(5) %...>%
        {
          # 5 (from inside recursive_promise) + 1 (for the current handler)
          expect_identical(cd$counts$onFulfilledCalled, 6L)
        } %>%
        wait_for_it()
    })

    cd2 <- create_counting_domain()
    p <- recursive_promise(5)
    with_promise_domain(cd2, {
      p %...>%
        {
          # This time, none of the ones inside recursive_promise count, since
          # they were bound outside of the influence of cd2 (even though they
          # are resolved within the influence of cd2, thanks to wait_for_it()).
          expect_identical(cd2$counts$onFulfilledCalled, 1L)
        } %>%
        wait_for_it()
    })
  })
})
