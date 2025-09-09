skip_on_cran()

skip_if_not_installed("otelsdk")

with_ospan_promise_domain({
  describe("OpenTelemetry integration", {
    describe("create_ospan() and end_ospan()", {
      it("creates spans appropriately", {
        records <- otelsdk::with_otel_record({
          span <- create_ospan("test_span0", tracer = NULL)
          expect_true(inherits(span, "otel_span"))
          end_ospan(span)
        })
        expect_true(!is.null(records$traces[["test_span0"]]))

        expect_null(create_ospan("test_span_null"))
      })

      it("end_ospan handles NULL spans gracefully", {
        expect_invisible(end_ospan(NULL))
      })

      it("end_ospan returns invisibly when not tracing", {
        mock_span <- list(name = "test")
      })
    })

    describe("with_ospan_async()", {
      it("executes synchronous expressions without otel", {
        # When recording is off, should still execute expression
        result <- with_ospan_async("test_span1", {
          42
        })
        expect_equal(result, 42)
      })
      it("executes synchronous expressions", {
        records <- otelsdk::with_otel_record({
          result <- with_ospan_async("test_span1", {
            42
          })
          expect_equal(result, 42)
        })
        expect_true(!is.null(records$traces[["test_span1"]]))
      })
      it("executes asynchronous expressions without otel", {
        # When recording is off, should still execute expression
        result <- with_ospan_async("test_span1", {
          promise_resolve(42)
        })
        expect_true(is.promising(result))
        expect_equal(extract(result), 42)
      })

      it("handles promise results", {
        records <- otelsdk::with_otel_record({
          result <- with_ospan_async("test_span2", {
            promise_resolve(42)
          })

          expect_true(is.promising(result))
          expect_equal(extract(result), 42)
        })
        expect_true(!is.null(records$traces[["test_span2"]]))
      })
    })

    describe("promise domain integration", {
      it("integrates with existing promise domains", {
        # Test domain composition works even when otel is not available
        counting_domain <- create_counting_domain()

        p <- with_promise_domain(counting_domain, {
          with_ospan_async("test_operation", {
            promise_resolve(42) |>
              then(function(x) x * 2)
          })
        })

        result <- extract(p)
        expect_equal(result, 84)

        # Verify the counting domain was active
        expect_gte(counting_domain$counts$onFulfilledBound, 1L)
      })
    })

    describe("error handling", {
      it("handles promise rejections gracefully", {
        # Should handle rejected promises without errors
        records <- otelsdk::with_otel_record({
          p <- with_ospan_async("test_span3", {
            promise_reject("test error") |>
              catch(function(reason) {
                with_ospan_async("catch_span3", {
                  42
                })

                expect_true(inherits(reason, "error"))
                expect_match(as.character(reason), "test error")

                "caught"
              }) |>
              then(function(result) {
                expect_equal(result, "caught")
                with_ospan_async("then_span3", {
                  42
                })
              })
          })
          wait_for_it(p)
          p
        })
        expect_true(is.promising(p))

        # Should be able to catch the error

        test_id <- records$traces[["test_span3"]]$span_id
        expect_equal(records$traces[["catch_span3"]]$parent, test_id)
        expect_equal(records$traces[["then_span3"]]$parent, test_id)
      })

      it("propagates regular errors", {
        # Regular errors should still propagate
        expect_error(
          with_ospan_async("test_span4", {
            stop("regular error")
          }),
          "regular error"
        )
      })
    })

    it("maintains span context across parallel promise chains", {
      # Track execution order and span context
      execution_order <- character(0)

      # Compare execution order after making `recording`
      # Compare span parents using `recording`

      recording <- otelsdk::with_otel_record({
        # Create two parallel promise chains, each with their own span
        chain1 <- with_ospan_async("chain_1", {
          promise_resolve("init1") |>
            then(function(x) {
              spn <- create_ospan("chain1_step1")
              on.exit(end_ospan(spn))
              execution_order <<- c(execution_order, "chain1_step1")
              paste0(x, "_step11")
            }) |>
            then(function(x) {
              spn <- create_ospan("chain1_step2")
              on.exit(end_ospan(spn))
              execution_order <<- c(execution_order, "chain1_step2")
              paste0(x, "_step12")
            }) |>
            then(function(x) {
              spn <- create_ospan("chain1_step3")
              on.exit(end_ospan(spn))
              execution_order <<- c(execution_order, "chain1_step3")
              paste0(x, "_step13")
            }) |>
            then(function(x) {
              spn <- create_ospan("chain1_step4")
              on.exit(end_ospan(spn))
              execution_order <<- c(execution_order, "chain1_step4")
              paste0(x, "_final1")
            })
        })

        chain2 <- with_ospan_async("chain_2", {
          promise_resolve("init2") |>
            then(function(x) {
              spn <- create_ospan("chain2_step1")
              on.exit(end_ospan(spn))
              execution_order <<- c(execution_order, "chain2_step1")
              paste0(x, "_step12")
            }) |>
            then(function(x) {
              spn <- create_ospan("chain2_step2")
              on.exit(end_ospan(spn))
              execution_order <<- c(execution_order, "chain2_step2")
              paste0(x, "_step22")
            }) |>
            then(function(x) {
              spn <- create_ospan("chain2_step3")
              on.exit(end_ospan(spn))
              execution_order <<- c(execution_order, "chain2_step3")
              paste0(x, "_step23")
            }) |>
            then(function(x) {
              spn <- create_ospan("chain2_step4")
              on.exit(end_ospan(spn))
              execution_order <<- c(execution_order, "chain2_step4")
              paste0(x, "_final2")
            })
        })

        # Wait for both chains to complete
        result1 <- extract(chain1)
        result2 <- extract(chain2)

        # Verify final recording
        expect_equal(result1, "init1_step11_step12_step13_final1")
        expect_equal(result2, "init2_step12_step22_step23_final2")

        # Check that execution alternates between chains
        expected_alternating_pattern <- c(
          "chain1_step1",
          "chain2_step1",
          "chain1_step2",
          "chain2_step2",
          "chain1_step3",
          "chain2_step3",
          "chain1_step4",
          "chain2_step4"
        )

        expect_equal(
          execution_order,
          expected_alternating_pattern,
          info = "Execution should alternate between chains"
        )
      })

      # * Verify spans at each step have the same parent, even though the
      #   previous calculation may have been a different promise domain.
      # * All steps in chain1 should have same parent span, all in chain2 should
      #   have same parent span.
      chain1_id <- recording$traces[["chain_1"]]$span_id
      for (name in c(
        "chain1_step1",
        "chain1_step2",
        "chain1_step3",
        "chain1_step4"
      )) {
        expect_equal(recording$traces[[name]]$parent, chain1_id)
      }

      chain2_id <- recording$traces[["chain_2"]]$span_id
      for (name in c(
        "chain2_step1",
        "chain2_step2",
        "chain2_step3",
        "chain2_step4"
      )) {
        expect_equal(recording$traces[[name]]$parent, chain2_id)
      }
    })
  })
})

describe("local_ospan_promise_domain()", {
  it("sets up ospan promise domain for local scope", {
    # Test that the domain is active within the local scope
    original_domain <- current_promise_domain()

    local({
      local_ospan_promise_domain()
      current_domain <- current_promise_domain()

      # Should have an ospan domain set
      expect_false(identical(current_domain, original_domain))
      expect_true(has_ospan_promise_domain(current_domain))
    })

    # Should be restored after exiting the local scope
    expect_identical(current_promise_domain(), original_domain)
  })

  it("restores previous domain when scope exits", {
    counting_domain <- create_counting_domain()

    expect_equal(current_promise_domain(), NULL)

    with_promise_domain(counting_domain, {
      domain_before <- current_promise_domain()

      local({
        local_ospan_promise_domain()
        current_domain <- current_promise_domain()

        # Should be a composed domain
        expect_false(identical(current_domain, domain_before))
        expect_true(has_ospan_promise_domain(current_domain))
      })

      # Should restore the counting domain
      expect_identical(current_promise_domain(), domain_before)
    })

    expect_equal(current_promise_domain(), NULL)
  })

  # it("works with custom environment", {
  #   test_env <- new.env()
  #   original_domain <- current_promise_domain()

  #   # Set up domain in custom environment
  #   local_ospan_promise_domain(envir = test_env)

  #   # Domain should still be active since test_env hasn't been cleaned
  #   current_domain <- current_promise_domain()
  #   expect_false(identical(current_domain, original_domain))
  #   expect_true(has_ospan_promise_domain(current_domain))

  #   # Clean up the environment to trigger restoration
  #   rm(list = ls(test_env), envir = test_env)

  #   # Force garbage collection to ensure cleanup handlers run
  #   gc()

  #   # Domain should be restored (though this timing is implementation dependent)
  #   # So we mainly test that the function doesn't error with custom envir
  #   expect_true(TRUE)
  # })

  it("integrates with promise execution", {
    records <- otelsdk::with_otel_record({
      result <- local({
        local_ospan_promise_domain()

        otel::start_local_active_span("outer_test_span")

        # Create a promise within the ospan domain
        promise_resolve(21) |>
          then(function(x) {
            # This should be executed within the ospan domain
            span <- create_ospan("inner_test_span")
            on.exit(end_ospan(span))
            x * 2
          })
      })

      wait_for_it(result)
      result
    })

    expect_true(is.promising(result))
    expect_equal(extract(result), 42)
    expect_true(!is.null(records$traces[["outer_test_span"]]))
    expect_true(!is.null(records$traces[["inner_test_span"]]))
    expect_equal(
      records$traces[["inner_test_span"]]$parent,
      records$traces[["outer_test_span"]]$span_id
    )
  })

  it("can be nested without issues", {
    # Track how many times promise domain setup occurs
    domain_creation_count <- 0
    original_copd <- create_ospan_promise_domain

    # Mock the domain creation to count calls
    with_mocked_bindings(
      create_ospan_promise_domain = function() {
        domain_creation_count <<- domain_creation_count + 1
        original_copd()
      },
      {
        original_domain <- current_promise_domain()

        local({
          local_ospan_promise_domain()
          expect_equal(domain_creation_count, 1)
          domain1 <- current_promise_domain()

          local({
            # This should not create another domain due to idempotency
            local_ospan_promise_domain()
            expect_equal(domain_creation_count, 1) # Still 1, not 2
            domain2 <- current_promise_domain()

            # Both should have ospan domains and should be identical
            expect_true(has_ospan_promise_domain(domain1))
            expect_true(has_ospan_promise_domain(domain2))
            expect_identical(domain1, domain2) # Should be the same due to idempotency
          })

          # Should still be domain1 after nested scope exits
          expect_identical(current_promise_domain(), domain1)
        })

        # Should restore to original after all scopes exit
        # Note: This may not be immediate due to deferred cleanup
        expect_equal(domain_creation_count, 1) # Only created once total
      }
    )
  })
})

describe("has_ospan_promise_domain()", {
  it("returns FALSE for NULL domain", {
    expect_false(has_ospan_promise_domain(NULL))
  })

  it("returns FALSE for empty list", {
    expect_false(has_ospan_promise_domain(list()))
  })

  it("returns FALSE for regular promise domain", {
    regular_domain <- new_promise_domain(
      wrapOnFulfilled = function(onFulfilled) onFulfilled
    )
    expect_false(has_ospan_promise_domain(regular_domain))
  })

  it("returns TRUE for ospan promise domain", {
    ospan_domain <- create_ospan_promise_domain()
    expect_true(has_ospan_promise_domain(ospan_domain))
  })

  it("returns TRUE for composed domain with ospan", {
    counting_domain <- create_counting_domain()
    ospan_domain <- create_ospan_promise_domain()
    composed_domain <- compose_domains(counting_domain, ospan_domain)

    expect_true(has_ospan_promise_domain(composed_domain))
  })

  it("returns FALSE for composed domain without ospan", {
    counting_domain1 <- create_counting_domain()
    counting_domain2 <- create_counting_domain()
    composed_domain <- compose_domains(counting_domain1, counting_domain2)

    expect_false(has_ospan_promise_domain(composed_domain))
  })

  it("uses current_promise_domain() when no argument provided", {
    # Initially no domain
    expect_false(has_ospan_promise_domain())

    # With ospan domain active
    with_ospan_promise_domain({
      expect_true(has_ospan_promise_domain())
    })

    # Back to no domain
    expect_false(has_ospan_promise_domain())
  })

  it("works with various data types", {
    # # Test edge cases
    # expect_false(has_ospan_promise_domain(character(0)))
    # expect_false(has_ospan_promise_domain(numeric(0)))
    # expect_false(has_ospan_promise_domain("not a domain"))
    # expect_false(has_ospan_promise_domain(42))

    # Test list with wrong flag
    fake_domain <- list(.some_other_flag = TRUE)
    expect_false(has_ospan_promise_domain(fake_domain))

    # Test list with correct flag
    correct_domain <- list(.ospan_promise_domain = TRUE)
    expect_true(has_ospan_promise_domain(correct_domain))

    # Test list with correct flag but FALSE value
    false_domain <- list(.ospan_promise_domain = FALSE)
    expect_false(has_ospan_promise_domain(false_domain))
  })
})

describe("with_ospan_promise_domain() idempotency", {
  it("is idempotent when called multiple times", {
    # Track how many times promise domain setup occurs
    domain_creation_count <- 0
    reset_count <- function() {
      domain_creation_count <<- 0
    }
    original_copd <- create_ospan_promise_domain

    expect_false(has_ospan_promise_domain())

    # Mock the domain creation to count calls
    with_mocked_bindings(
      create_ospan_promise_domain = function() {
        domain_creation_count <<- domain_creation_count + 1
        original_copd()
      },
      {
        # First call should create the domain
        result1 <- with_ospan_promise_domain({
          42
        })
        expect_equal(result1, 42)
        expect_equal(domain_creation_count, 1)

        reset_count()

        result2 <-
          with_ospan_promise_domain({
            expect_equal(domain_creation_count, 1)

            # Nested calls should not create additional domains
            with_ospan_promise_domain({
              with_ospan_promise_domain({
                expect_equal(domain_creation_count, 1)

                84
              })
            })
          })
        expect_equal(result2, 84)
        # Should still be 1, even with two prom domain calls
        expect_equal(domain_creation_count, 1)
      }
    )
  })

  it("allows nested calls without duplicate domain setup", {
    # Test that nested with_ospan_promise_domain calls work correctly
    # and don't interfere with each other

    records <- otelsdk::with_otel_record({
      result <- with_ospan_promise_domain({
        with_ospan_async("outer_span", {
          # This nested call should be idempotent
          with_ospan_promise_domain({
            with_ospan_async("inner_span", {
              promise_resolve(42) |>
                then(function(x) {
                  # Another nested call
                  with_ospan_promise_domain({
                    x * 2
                  })
                })
            })
          })
        })
      })

      wait_for_it(result)
      result
    })

    expect_true(is.promising(result))
    expect_equal(extract(result), 84)

    # Verify spans were created correctly
    expect_true(!is.null(records$traces[["outer_span"]]))
    expect_true(!is.null(records$traces[["inner_span"]]))

    # Verify parent-child relationship
    outer_id <- records$traces[["outer_span"]]$span_id
    expect_equal(records$traces[["inner_span"]]$parent, outer_id)
  })
})
