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

describe("with_ospan_promise_domain() idempotency", {
  it("is idempotent when called multiple times", {
    # Track how many times promise domain setup occurs
    domain_creation_count <- 0
    reset_count <- function() {
      domain_creation_count <<- 0
    }
    original_copd <- create_ospan_promise_domain

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
