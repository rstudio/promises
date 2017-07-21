library(testthat)

source("common.R")

describe("2.2. The `then` Method", {
  # A promise must provide a `then` method to access its current or eventual
  # value or reason.
  #
  # A promise’s then method accepts two arguments:
  # promise.then(onFulfilled, onRejected)

  describe("2.2.1. Both onFulfilled and onRejected are optional arguments:", {
    it("2.2.1.1. If onFulfilled is not a function, it must be ignored.", {
      # NOTE: This behavior varies; we throw an error
      # p <- promise(~resolve(10)) %>% then(20)
      # expect_identical(extract(p), 10)
    })
    it("2.2.1.1. If onRejected is not a function, it must be ignored.", {
      # NOTE: This behavior varies; we throw an error
      # p <- promise(~reject("foo")) %>% then(onRejected = "bar")
      # expect_error(extract(p), "foo")
    })
  })

  describe("2.2.2. If onFulfilled is a function:", {
    it("2.2.2.1. it must be called after promise is fulfilled, with promise’s value as its first argument.", {
      x <- NULL
      p <- ext_promise()

      p$promise %>% then(function(value) { x <<- value })
      wait_for_it()
      expect_identical(x, NULL)

      p$resolve(10)
      wait_for_it()
      expect_identical(x, 10)
    })
    it("2.2.2.2. it must not be called before promise is fulfilled.", {

    })
    it("2.2.2.3. it must not be called more than once.", {

    })
  })
  describe("2.2.3. If onRejected is a function,", {
    it("2.2.3.1. it must be called after promise is rejected, with promise’s reason as its first argument.", {
      x <- NULL
      p <- ext_promise()

      p$promise %>% then(onRejected = function(reason) { x <<- reason })
      wait_for_it()
      expect_identical(x, NULL)

      p$reject(simpleError("boom"))
      wait_for_it()
      expect_identical(x, simpleError("boom"))
    })
  })
  describe("2.2.4. onFulfilled or onRejected must not be called until the execution context stack contains only platform code. [3.1].", {
    it(" ", {
      x <- NULL
      promise(~resolve(TRUE)) %>% then(function(value) {x <<- value})
      expect_identical(x, NULL)
      wait_for_it()
      expect_identical(x, TRUE)
    })
  })
  describe("2.2.5. onFulfilled and onRejected must be called as functions (i.e. with no this value). [3.2]", {
    # Not relevant for R.
  })
  describe("2.2.6. `then` may be called multiple times on the same promise.", {
    it("2.2.6.1. If/when promise is fulfilled, all respective onFulfilled callbacks must execute in the order of their originating calls to then.", {
      p <- ext_promise()
      callbacks_called <- 0L
      results <- new.env(parent = emptyenv())

      lapply(1:10, function(i) {
        results[[as.character(i)]] <- p$promise %>%
          then(function(value) {
            callbacks_called <<- callbacks_called + 1L
            expect_identical(callbacks_called, i)
            value
          })
      })

      p$resolve(cars)
      wait_for_it()

      lapply(as.list(results), function(x) {
        expect_identical(extract(x), cars)
      })
    })
  })

  describe("2.2.6.2. If/when promise is rejected, all respective onRejected callbacks must execute in the order of their originating calls to then.", {
    p <- ext_promise()
    callbacks_called <- 0L
    results <- new.env(parent = emptyenv())

    lapply(1:10, function(i) {
      results[[as.character(i)]] <- p$promise %>%
        catch(function(err) {
          callbacks_called <<- callbacks_called + 1L
          expect_identical(callbacks_called, i)
          err
        })
    })

    p$reject(simpleError("an error"))
    wait_for_it()

    lapply(as.list(results), function(x) {
      expect_identical(extract(x), simpleError("an error"))
    })
  })

  describe("2.2.7. `then` must return a promise [3.3].", {
    it(" ", {
      promise(~{}) %>% then() %>% is.promise() %>% expect_true()
    })

    it("2.2.7.1. If either onFulfilled or onRejected returns a value x, run the Promise Resolution Procedure [[Resolve]](promise2, x).", {
      p1 <- promise(~resolve(TRUE)) %>% then(~"foo")
      expect_identical(extract(p1), "foo")

      p2 <- promise(~reject("boom")) %>% catch(~"bar")
      expect_identical(extract(p2), "bar")
    })

    it("2.2.7.2. If either onFulfilled or onRejected throws an exception e, promise2 must be rejected with e as the reason.", {
      p1 <- promise(~resolve(TRUE)) %>% then(~stop("foo"))
      expect_error(extract(p1), "^foo$")

      p2 <- promise(~reject("boom")) %>% catch(~stop("bar"))
      expect_error(extract(p2), "^bar$")
    })

    it("2.2.7.3. If onFulfilled is not a function and promise1 is fulfilled, promise2 must be fulfilled with the same value as promise1.", {
      p <- promise(~resolve("baz")) %>% then()
      expect_identical(extract(p), "baz")
    })

    it("2.2.7.4. If onRejected is not a function and promise1 is rejected, promise2 must be rejected with the same reason as promise1.", {
      p <- promise(~reject("qux")) %>% then()
      expect_error(extract(p), "^qux$")
    })
  })
})
