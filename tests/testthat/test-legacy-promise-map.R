test_that("promise_map handles NULL values", {
  prom2_map <- function(first_res, second_res) {
    f <- function(index) {
      switch(index, first_res, second_res)
    }
    promise_map(1:2, f)
  }

  counter <- 0

  prom2_map("anything", "anything_else") %...>%
    {
      counter <<- counter + 1
      expect_equal(., list("anything", "anything_else"))
    } %>%
    wait_for_it()

  prom2_map(NULL, "anything_else") %...>%
    {
      counter <<- counter + 1
      expect_equal(., list(NULL, "anything_else"))
    } %>%
    wait_for_it()

  prom2_map("anything", NULL) %...>%
    {
      counter <<- counter + 1
      expect_equal(., list("anything", NULL))
    } %>%
    wait_for_it()

  prom2_map(NULL, NULL) %...>%
    {
      counter <<- counter + 1
      expect_equal(., list(NULL, NULL))
    } %>%
    wait_for_it()

  expect_equal(counter, 4)
})
