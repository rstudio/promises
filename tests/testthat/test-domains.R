async_true <- function() {
  promise_resolve(TRUE)
}

describe("Promise domains", {
  it("are reentered during handlers", {
    cd <- create_counting_domain(trackFinally = TRUE)
    p <- with_promise_domain(cd, {
      async_true() |>
        then(\(x) {
          expect_identical(cd$counts$onFulfilledCalled, 1L)
          expect_identical(cd$counts$onFulfilledActive, 1L)
          10 # sync result
        }) |>
        then(\(x) {
          expect_identical(x, 10)
          expect_identical(cd$counts$onFulfilledCalled, 2L)
          expect_identical(cd$counts$onFulfilledActive, 1L)
          promise_resolve(20) # async result
        })
    })

    expect_identical(cd$counts$onFulfilledBound, 2L)

    p <- p |>
      then(\(x) {
        expect_identical(cd$counts$onFulfilledCalled, 2L)
        expect_identical(cd$counts$onFulfilledActive, 0L)
      })

    expect_identical(cd$counts$onFulfilledBound, 2L)
    p |> wait_for_it()

    with_promise_domain(cd, {
      p <- async_true() |>
        finally(\() {
          expect_identical(cd$counts$onFinallyCalled, 1L)
          expect_identical(cd$counts$onFinallyActive, 1L)
        })
      expect_identical(cd$counts$onFinallyBound, 1L)

      expect_identical(cd$counts$onFulfilledBound, 2L)
      expect_identical(cd$counts$onRejectedBound, 0L)

      p |> wait_for_it()
    })

    expect_identical(cd$counts$onFulfilledBound, 2L)

    with_promise_domain(cd, {
      p <- async_true() |>
        then(\(x) {
          expect_identical(x, TRUE)
          expect_identical(cd$counts$onFulfilledCalled, 3L)
          ten <- 10
          # This tests if promise domain membership infects subscriptions made
          # in handlers.
          promise_resolve(invisible(ten)) |>
            then(\(value, .visible) {
              expect_identical(value, 10)
              expect_false(.visible)
              expect_true(!is.null(current_promise_domain()))
              expect_identical(cd$counts$onFulfilledCalled, 4L)
            })
        })
    })
    expect_true(is.null(current_promise_domain()))
    expect_identical(cd$counts$onFulfilledCalled, 2L)

    p |> wait_for_it()
  })

  it("pass finally binding to fulfill/reject by default", {
    cd1 <- create_counting_domain(trackFinally = FALSE)

    with_promise_domain(cd1, {
      p1 <- async_true() |>
        finally(\() {
          expect_identical(cd1$counts$onFulfilledActive, 1L)
          expect_identical(cd1$counts$onRejectedActive, 0L)
        })

      expect_identical(cd1$counts$onFulfilledBound, 1L)
      expect_identical(cd1$counts$onRejectedBound, 1L)
      p1 |> wait_for_it()
      expect_identical(cd1$counts$onFulfilledCalled, 1L)
      expect_identical(cd1$counts$onRejectedCalled, 0L)
    })

    cd2 <- create_counting_domain(trackFinally = FALSE)

    p2 <- with_promise_domain(cd2, {
      promise_reject("a problem") |>
        finally(\() {
          expect_identical(cd2$counts$onFulfilledActive, 0L)
          expect_identical(cd2$counts$onRejectedActive, 1L)
        })
    }) |>
      squelch_unhandled_promise_error()

    expect_identical(cd2$counts$onFulfilledBound, 1L)
    expect_identical(cd2$counts$onRejectedBound, 1L)
    p2 |> wait_for_it()
    expect_identical(cd2$counts$onFulfilledCalled, 0L)
    expect_identical(cd2$counts$onRejectedCalled, 1L)
  })

  it("doesn't intercept fulfill/reject on finally, if finally is explicitly intercepted", {
    cd1 <- create_counting_domain(trackFinally = TRUE)

    with_promise_domain(cd1, {
      p1 <- async_true() |>
        finally(\() {
          expect_identical(cd1$counts$onFinallyActive, 1L)
          expect_identical(cd1$counts$onFulfilledActive, 0L)
          expect_identical(cd1$counts$onRejectedActive, 0L)
        })

      expect_identical(cd1$counts$onFinallyBound, 1L)
      expect_identical(cd1$counts$onFulfilledBound, 0L)
      expect_identical(cd1$counts$onRejectedBound, 0L)
      p1 |> wait_for_it()
      expect_identical(cd1$counts$onFinallyCalled, 1L)
      expect_identical(cd1$counts$onFulfilledCalled, 0L)
      expect_identical(cd1$counts$onRejectedCalled, 0L)
    })

    cd2 <- create_counting_domain(trackFinally = TRUE)

    p2 <- with_promise_domain(cd2, {
      promise_reject(TRUE) |>
        finally(\() {
          expect_identical(cd2$counts$onFinallyActive, 1L)
          expect_identical(cd2$counts$onFulfilledActive, 0L)
          expect_identical(cd2$counts$onRejectedActive, 0L)
        })
    }) |>
      squelch_unhandled_promise_error()

    expect_identical(cd2$counts$onFinallyBound, 1L)
    expect_identical(cd2$counts$onFulfilledBound, 0L)
    expect_identical(cd2$counts$onRejectedBound, 0L)
    p2 |> wait_for_it()
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
      promise_resolve(as.symbol("foo")) |>
        then(\(x) {
          expect_identical(x, as.symbol("foo"))
        }) |>
        wait_for_it()
    })
  })

  it("executes wrap_callback_reenter handlers in the right lexical environment", {
    # No reason this shouldn't work, I haven't seen it fail, just making sure.
    cd <- create_counting_domain()
    x <- NULL
    with_promise_domain(cd, {
      promise_resolve(NULL) |>
        then(\() {
          x <<- 1L
        }) |>
        then(\() {
          x <<- x + 1L
        }) |>
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
        promise_resolve(TRUE) |>
          then(\() {
            current_callstack_depth <- length(sys.calls())
            if (!is.null(.last_callstack_depth)) {
              expect_identical(current_callstack_depth, .last_callstack_depth)
            }

            recursive_promise(
              n - 1,
              .last_callstack_depth = current_callstack_depth
            )
          })
      }
    }

    cd <- create_counting_domain()
    with_promise_domain(cd, {
      recursive_promise(5) |>
        then(\() {
          # 5 (from inside recursive_promise) + 1 (for the current handler)
          expect_identical(cd$counts$onFulfilledCalled, 6L)
        }) |>
        wait_for_it()
    })

    cd2 <- create_counting_domain()
    p <- recursive_promise(5)
    with_promise_domain(cd2, {
      p |>
        then(\() {
          # This time, none of the ones inside recursive_promise count, since
          # they were bound outside of the influence of cd2 (even though they
          # are resolved within the influence of cd2, thanks to wait_for_it()).
          expect_identical(cd2$counts$onFulfilledCalled, 1L)
        }) |>
        wait_for_it()
    })
  })

  it("apply nested domains in reverse order (innermost domain closest to callback)", {
    # Test that when nesting promise domains, the latest domain to be added
    # (innermost) should be the one that wraps the onFulfilled/onRejected most closely.
    
    # Create a test environment to track domain activation order
    test_env <- new.env()
    test_env$x <- "initial"
    test_env$activations <- character(0)
    
    createVarPromiseDomain <- function(env, name, value) {
      force(env)
      force(name)
      force(value)
      
      new_promise_domain(
        wrapOnFulfilled = function(onFulfilled) {
          function(...) {
            orig <- env[[name]]
            env[[name]] <- value
            env$activations <- c(env$activations, paste0(name, "=", value))
            on.exit({
              env[[name]] <- orig
            })
            
            onFulfilled(...)
          }
        },
        wrapSync = function(expr) {
          orig <- env[[name]]
          env[[name]] <- value
          on.exit(env[[name]] <- orig)
          
          force(expr)
        }
      )
    }
    
    domain1 <- createVarPromiseDomain(test_env, "x", 1)
    domain2 <- createVarPromiseDomain(test_env, "x", 2)
    
    # Test the nested domain behavior
    result <- NULL
    
    with_promise_domain(domain1, {
      # Outer domain should set x=1
      expect_identical(test_env$x, 1)
      
      with_promise_domain(domain2, {
        # Inner domain should set x=2
        expect_identical(test_env$x, 2)
        
        # Create and resolve a promise
        promise_resolve(TRUE) |>
          then(function(value) {
            # The inner domain (domain2) should be closest to this callback,
            # so x should be 2 when this executes
            result <<- test_env$x
            test_env$x
          }) |>
          wait_for_it()
      })
    })
    
    # Verify that the innermost domain took precedence
    expect_identical(result, 2)
    
    # Verify the activation order: domain1 (outer) activates first, 
    # then domain2 (inner) activates and takes precedence
    expect_length(test_env$activations, 2)
    expect_identical(test_env$activations[1], "x=1")
    expect_identical(test_env$activations[2], "x=2")
  })
})
