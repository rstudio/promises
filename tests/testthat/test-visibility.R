library(testthat)

source("common.R")

describe("visibility", {

  single_fn <- function(value) {
    info <- withVisible(value)
    if (info$visible) {
      info$value
    } else {
      invisible(info$value)
    }
  }
  double_fn <- function(value, .visible) {
    if (.visible) value else invisible(value)
  }

  # display in block to avoid indent of doom
  for (add_catch in c("false", "single", "double", "expr")) {
  for (add_finally in c("false", "expr")) {
  for (add_then in c("false", "single", "double", "expr")) {

    it(
      paste0(
        "survives ", paste0(c(
          if (add_then != "false") paste0("then-", add_then),
          if (add_catch != "false") paste0("catch-", add_catch),
          if (add_finally != "false") paste0("finally-", add_finally),
          "then"
        ), collapse = ", ")),
      {

        p <- promise_resolve(invisible(1))

        p <-
          switch(add_then,
            "false" = p,
            "single" = p %>% then(single_fn),
            "double" = p %>% then(double_fn),
            "expr" = p %>% then(~ {
              info <- withVisible(.)
              if (info$visible) {
                info$value
              } else {
                invisible(info$value)
              }
            })
          )
        p <-
          switch(add_catch,
            "false" = p,
            "single" = p %>% catch(single_fn),
            "double" = p %>% catch(double_fn),
            "expr" = p %>% catch(~ {})
          )

        finally_val <- NULL
        p <-
          switch(add_finally,
            "false" = p,
            "expr" = p %>% finally(~ {
              finally_val <<- TRUE
            })
          )

        extended_val <-
          p %>%
          then(function(value, .visible) {
            list(value = value, visible = .visible)
          }) %>%
          extract()

        regular_val <-
          p %>%
          then(function(value) {
            withVisible(value)
          }) %>%
          extract()

        if (add_finally != "false") {
          expect_true(finally_val)
        }

        expect_identical(extended_val$value, 1)
        expect_identical(extended_val$visible, FALSE)

        expect_identical(regular_val$value, 1)
        expect_identical(regular_val$visible, FALSE)

      }
    )
  }}}
})
