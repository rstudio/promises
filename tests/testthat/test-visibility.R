library(testthat)

source("common.R")

describe("visibility", {
  it("survives catch, finally", {
    p <- promise_resolve(invisible(1)) %>%
      catch(~{}) %>%
      finally(~{}) %>%
      then(function(x, .visible) {
        .visible
      })

    expect_false(extract(p))

    p <- promise_resolve(1) %>%
      catch(~{}) %>%
      finally(~{}) %>%
      then(function(x, .visible) {
        .visible
      })

    expect_true(extract(p))
  })
})
