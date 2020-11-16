library(testthat)

source("common.R")

describe("then()", {
  it("honors .visible argument", {
    result <- NULL
    p <- promise(~resolve(invisible(1))) %>% then(function(value, .visible) {
      result <<- list(value = value, visible = .visible)
    })
    wait_for_it()
    expect_identical(result$value, 1)
    expect_identical(result$visible, FALSE)

    p <- promise(~resolve(1)) %>% then(function(value, .visible) {
      result <<- list(value = value, visible = .visible)
    })
    wait_for_it()
    expect_identical(result$value, 1)
    expect_identical(result$visible, TRUE)

    # .visible is preserved even with an intermediate then() or catch()
    p <- promise(~resolve(invisible(1))) %>%
      then() %>%
      catch(~"what error?") %>%
      then(function(value, .visible) {
        result <<- list(value = value, visible = .visible)
      })
    wait_for_it()
    expect_identical(result$value, 1)
    expect_identical(result$visible, FALSE)
  })
  it("method ignores non-functions or NULL...", {
    p1 <- promise(~resolve(1))$then(10)$then(NULL)
    expect_identical(extract(p1), 1)
  })
  it("...but function only ignores NULL, not non-functions", {
    expect_error(promise(~resolve(1)) %>% then(10))
    expect_error(promise(~resolve(1)) %>% then(NULL), NA)
  })

  it("honors visibility with no .visible argument", {
    result <- NULL
    p <- promise_resolve(invisible(1))$
      then(function(value) {
        result <<- withVisible(value)
      })
    wait_for_it()
    expect_identical(result$value, 1)
    expect_identical(result$visible, FALSE)

    result <- NULL
    p <- promise_resolve(2)$
      then(function(value) {
        result <<- withVisible(value)
      })
    wait_for_it()
    expect_identical(result$value, 2)
    expect_identical(result$visible, TRUE)
  })
})

describe("catch()", {
  it("catches", {
    p <- ext_promise()
    p2 <- p$promise %>% catch(~TRUE)
    p$reject("boom")
    expect_identical(extract(p2), TRUE)
  })
  it("can throw", {
    p <- promise(~stop("foo")) %>% catch(~stop("bar"))
    expect_error(extract(p), "^bar$")
  })
  it("method ignores non-functions or NULL...", {
    p1 <- promise(~resolve(1))$catch(10)$catch(NULL)
    expect_identical(extract(p1), 1)
  })
  it("...but function only ignores NULL, not non-functions", {
    expect_error(promise(~resolve(1)) %>% catch(10))
    expect_error(promise(~resolve(1)) %>% catch(NULL), NA)
  })
})

describe("finally()", {
  it("calls back when a promise is resolved", {
    called <- FALSE
    p <- promise(~resolve(10)) %>%
      finally(~{
        called <<- TRUE
      })
    wait_for_it()
    expect_identical(called, TRUE)
    expect_identical(extract(p), 10)
  })
  it("calls back when a promise is rejected", {
    called <- FALSE
    p <- promise(~reject("foobar")) %>%
      finally(~{
        called <<- TRUE
      })
    squelch_unhandled_promise_error(p)
    wait_for_it()
    expect_identical(called, TRUE)
    expect_error(extract(p), "^foobar$")
  })
  it("does not affect the return value of the promise", {
    p1 <- promise(~resolve(1)) %>% finally(~20)
    expect_identical(extract(p1), 1)

    p2 <- promise(~reject("err")) %>% finally(~20)
    expect_error(extract(p2), "^err$")
  })
  it("errors replace the result of the promise", {
    p1 <- promise(~resolve(1)) %>% finally(~stop("boom"))
    expect_error(extract(p1), "^boom$")

    p2 <- promise(~reject("foo")) %>% finally(~stop("bar"))
    expect_error(extract(p2), "^bar$")
  })
  it("method ignores non-functions or NULL...", {
    p1 <- promise(~resolve(1))$finally(10)$finally(NULL)
    expect_identical(extract(p1), 1)
  })
  it("...but function only ignores NULL, not non-functions", {
    expect_error(promise(~resolve(1)) %>% finally(10))
    expect_error(promise(~resolve(1)) %>% finally(NULL), NA)
  })
})

describe("future", {
  it("is treated as promise when used as resolution", {
    p <- promise_resolve(future::future(1))
    expect_identical(extract(p), 1)

    p2 <- promise_resolve(future::future(stop("boom")))
    expect_error(extract(p2))
  })

  it("is treated as promise when used as resolution", {
    p <- promise_reject(future::future(1))
    expect_identical(extract(p), 1)

    p2 <- promise_reject(future::future(stop("boom")))
    expect_error(extract(p2))
  })
})
