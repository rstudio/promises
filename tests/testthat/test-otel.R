skip_on_cran()

skip_if_not_installed("otel")
skip_if_not_installed("otelsdk")

# withr::local_envvar("TESTTHAT" = "true")
local_mocked_bindings(
  is_otel_tracing = function() TRUE
)

ignore <- otelsdk::with_otel_record({
  describe("OpenTelemetry integration", {
    describe("is_otel_tracing()", {
      it("checks otel availability correctly", {
        # Test that the function works regardless of otel status
        result <- is_otel_tracing()
        expect_type(result, "logical")
        expect_length(result, 1)
      })
    })

    describe("create_ospan() and end_ospan()", {
      it("creates spans appropriately", {
        # Test span creation - should work if otel is available
        span <- create_ospan("test_span0")
        expect_s3_class(span, "otel_span")
        end_ospan(span)

        with_mocked_bindings(
          is_otel_tracing = function() FALSE,
          {
            expect_null(create_ospan("test_span_null"))
          }
        )
      })

      it("end_ospan handles NULL spans gracefully", {
        # Should not error when passed NULL
        expect_invisible(end_ospan(NULL))
      })

      it("end_ospan returns invisibly when otel is not available", {
        # Should not error when otel is not available
        with_mocked_bindings(
          is_otel_tracing = function() FALSE,
          {
            mock_span <- list(name = "test")
            expect_invisible(end_ospan(mock_span))
          }
        )
      })
    })

    describe("with_ospan_async()", {
      it("executes synchronous expressions without otel", {
        # When otel is not available, should still execute expression
        result <- with_ospan_async("test_span1", {
          42
        })
        expect_equal(result, 42)
      })

      it("handles promise results", {
        # Should work with promises even when otel is unavailable
        result <- with_ospan_async("test_span2", {
          promise_resolve(42)
        })

        expect_true(is.promising(result))
        expect_equal(extract(result), 42)
      })
    })

    describe("promise domain integration", {
      it("works with promise chains", {
        # Test integration with actual promise chains (without otel)
        p <- with_ospan_async("test_operation", {
          promise_resolve(21) |>
            then(function(x) x * 2)
        })

        result <- extract(p)
        expect_equal(result, 42)
      })

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
        p <- with_ospan_async("test_span3", {
          promise_reject("test error")
        })

        expect_true(is.promising(p))
        # Should be able to catch the error
        p |>
          catch(function(reason) {
            expect_true(inherits(reason, "error"))
            expect_match(as.character(reason), "test error")
            "caught"
          }) |>
          then(function(result) {
            expect_equal(result, "caught")
          }) |>
          wait_for_it()
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

      it("works with complex promise chains", {
        # Test with more complex promise operations
        p <- with_ospan_async("complex_operation", {
          promise_resolve(10) |>
            then(function(x) {
              if (x > 5) {
                x * 2
              } else {
                promise_reject("too small")
              }
            }) |>
            then(function(x) x + 5)
        })

        result <- extract(p)
        expect_equal(result, 25) # (10 * 2) + 5
      })
    })
  })
})


it("maintains span context across parallel promise chains", {
  # Track execution order and span context
  execution_order <- character(0)

  results <- otelsdk::with_otel_record({
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

    # Verify final results
    expect_equal(result1, "init1_step11_step12_step13_final1")
    expect_equal(result2, "init2_step12_step22_step23_final2")

    # Check that execution alternates between chains
    # Expected pattern: chain1_step1, chain2_step1, chain1_step2, chain2_step2, etc.
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

  # Verify spans were captured consistently within each chain
  # (All steps in chain1 should have same parent span, all in chain2 should have same parent span)

  chain1_id <- results$traces[["chain_1"]]$span_id
  for (name in c(
    "chain1_step1",
    "chain1_step2",
    "chain1_step3",
    "chain1_step4"
  )) {
    expect_equal(results$traces[[name]]$parent, chain1_id)
  }

  chain2_id <- results$traces[["chain_2"]]$span_id
  for (name in c(
    "chain2_step1",
    "chain2_step2",
    "chain2_step3",
    "chain2_step4"
  )) {
    expect_equal(results$traces[[name]]$parent, chain2_id)
  }
})
