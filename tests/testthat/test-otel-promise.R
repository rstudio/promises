skip_on_cran()

skip_if_not_installed("otel")
skip_if_not_installed("otelsdk")

test_that("span persists", {
  record <-
    otelsdk::with_otel_record({
      testspan <- NULL
      testspan2 <- NULL

      local({
        testspan <<- otel_start_active_span(
          "test span",
          attributes = list("test.attribute" = "value")
        )

        testspan2 <<- otel_start_active_span(
          "test span 2",
          attributes = list("test.attribute2" = "value2")
        )
      })

      expect_true(testspan$is_recording())
      expect_true(testspan2$is_recording())

      otel::end_span(testspan)
      otel::end_span(testspan2)
      expect_false(testspan$is_recording())
      expect_false(testspan2$is_recording())
    })

  spns <- record[["traces"]]
  expect_equal(names(spns)[1], "test span")
  expect_equal(names(spns)[2], "test span 2")
  expect_equal(spns[[1]]$name, "test span")
  expect_equal(spns[[2]]$name, "test span 2")
  expect_equal(spns[[1]]$parent, "0000000000000000")
  expect_equal(spns[[2]]$parent, spns[[1]]$span_id)
})


test_that("local promise domain can be created", {
  record <-
    otelsdk::with_otel_record({
      spn1 <- otel_create_inactive_span("test span 1")
      spn2 <- otel_create_inactive_span("test span 2")

      # expect_false(spn1$is_recording())
      # expect_false(spn2$is_recording())

      otel::with_active_span(spn1, {
        otel::start_local_active_span("spn1 sub")
        # expect_true(spn1$is_recording())
        # expect_false(spn2$is_recording())
      })

      # expect_false(spn1$is_recording())
      # expect_false(spn2$is_recording())

      otel::with_active_span(spn2, {
        otel::start_local_active_span("spn2 sub")
        # expect_false(spn1$is_recording())
        # expect_true(spn2$is_recording())
      })

      # expect_false(spn1$is_recording())
      # expect_false(spn2$is_recording())
    })

  # str(record)
})

test_that("local promise domain with active span works", {
  prom_names <- c()
  record <-
    otelsdk::with_otel_record({
      tstspn1 <- otel_create_inactive_span(
        "test span 1",
        attributes = list("test.attribute" = "value1")
      )
      tstspn2 <- otel_create_inactive_span(
        "test span 2",
        attributes = list("test.attribute" = "value2")
      )
      defer({
        otel::end_span(tstspn1)
        otel::end_span(tstspn2)
      })

      with_otel_active_span_promise_domain(tstspn1, {
        p1 <-
          promise_resolve("Start1") %...>%
          {
            # message("sub1_span_1")
            prom_names <<- c(prom_names, "sub1_span_1")
            sub_spn <- otel::start_local_active_span("sub1_span_1")
            expect_true(tstspn1$is_recording())
            expect_true(sub_spn$is_recording())
            expect_equal(sub_spn$parent, tstspn1$span_id)
            expect_equal(., "Start1")

            promise_resolve("Middle1")
          } %...>%
          {
            # message("sub1_span_2")
            prom_names <<- c(prom_names, "sub1_span_2")
            sub_spn <- otel::start_local_active_span("sub1_span_2")
            expect_equal(sub_spn$span_id, tstspn1$span_id)
            expect_true(sub_spn$is_recording())
            expect_equal(., "Middle1")

            promise_resolve("End1")
          } %...>%
          {
            # message("sub1_span_3")
            prom_names <<- c(prom_names, "sub1_span_3")
            sub_spn <- otel::start_local_active_span("sub1_span_3")
            expect_true(tstspn1$is_recording())
            expect_true(sub_spn$is_recording())
            expect_equal(sub_spn$parent, tstspn1$span_id)
            expect_equal(., "End1")
          }
      })
      with_otel_active_span_promise_domain(tstspn2, {
        p2 <-
          promise_resolve("Start2") %...>%
          {
            # message("sub2_span_1")
            prom_names <<- c(prom_names, "sub2_span_1")
            sub_spn <- otel::start_local_active_span("sub2_span_1")
            expect_true(tstspn2$is_recording())
            expect_true(sub_spn$is_recording())
            expect_equal(sub_spn$parent, tstspn2$span_id)
            expect_equal(., "Start2")

            promise_resolve("Middle2")
          } %...>%
          {
            # message("sub2_span_2")
            prom_names <<- c(prom_names, "sub2_span_2")
            sub_spn <- otel::start_local_active_span("sub2_span_2")
            expect_equal(sub_spn$span_id, tstspn2$span_id)
            expect_true(sub_spn$is_recording())
            expect_equal(., "Middle2")

            promise_resolve("End2")
          } %...>%
          {
            # message("sub2_span_3")
            prom_names <<- c(prom_names, "sub2_span_3")
            sub_spn <- otel::start_local_active_span("sub2_span_3")
            expect_true(tstspn2$is_recording())
            expect_true(sub_spn$is_recording())
            expect_equal(sub_spn$parent, tstspn2$span_id)
            expect_equal(., "End2")
          }
      })

      # expect_false(tstspn1$is_recording())

      wait_for_it(p1)

      # expect_length(prom_names, 6)
      expect_equal(
        prom_names,
        c(
          "sub1_span_1",
          "sub2_span_1",
          "sub1_span_2",
          "sub2_span_2",
          "sub1_span_3",
          "sub2_span_3"
        )
      )
    })

  spns <- record[["traces"]]
  str(spns)
  spns$sub
  spns$`test span 2`
  # expect_equal(names(spns)[1], "test span")
  # expect_equal(spns[[1]]$name, "test span")
})
