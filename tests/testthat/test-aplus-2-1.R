library(testthat)

source("common.R")

describe("2.1. Promise States", {
  describe("2.1.1. When pending, a promise:", {
    it("2.1.1.1. may transition to either the fulfilled or rejected state.", {
      a <- ext_promise()
      expect_identical(a$status(), "pending")
      a$resolve(0)
      expect_identical(a$status(), "fulfilled")

      b <- ext_promise()
      expect_identical(b$status(), "pending")
      squelch_unhandled_promise_error(b$promise)
      b$reject("err")
      expect_identical(b$status(), "rejected")
    })
  })

  describe("2.1.2. When fulfilled, a promise:", {
    it("2.1.2.1. must not transition to any other state.", {
      a <- ext_promise()
      a$resolve(TRUE)
      expect_identical(a$status(), "fulfilled")
      a$reject("err")
      expect_identical(a$status(), "fulfilled")
    })
    it("2.1.2.2. must have a value, which must not change.", {
      a <- ext_promise()
      a$resolve(TRUE)
      expect_identical(a$status(), "fulfilled")
      a$resolve(FALSE)
      expect_identical(extract(a$promise), TRUE)
    })
  })

  describe("2.1.3. When rejected, a promise:", {
    it("2.1.3.1. must not transition to any other state.", {
      a <- ext_promise()
      squelch_unhandled_promise_error(a$promise)
      a$reject("err")
      expect_identical(a$status(), "rejected")
      a$resolve(TRUE)
      expect_identical(a$status(), "rejected")
    })
    it("2.1.3.2. must have a reason, which must not change.", {
      a <- ext_promise()
      a$reject("err1")
      expect_identical(a$status(), "rejected")
      a$reject("err2")
      expect_error(extract(a$promise), "err1")
    })
  })
})
