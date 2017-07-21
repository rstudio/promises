library(testthat)

source("common.R")

# 2.3. The Promise Resolution Procedure

describe("2.3.1. If promise and x refer to the same object, reject promise with a TypeError as the reason.", {
  it(" ", {
    p <- ext_promise()
    p$resolve(p$promise)
    expect_error(extract(p$promise), "^Chaining cycle detected for promise$")
  })
})

describe("2.3.2. If x is a promise, adopt its state [3.4]:", {
  it("2.3.2.1. If x is pending, promise must remain pending until x is fulfilled or rejected.", {
    p <- ext_promise()
    x <- ext_promise()
    p$resolve(x$promise)
    wait_for_it()
    expect_identical(x$status(), "pending")
    expect_identical(p$status(), "pending")
  })
  it("2.3.2.2. If/when x is fulfilled, fulfill promise with the same value.", {
    p <- ext_promise()
    x <- ext_promise()
    p$resolve(x$promise)
    wait_for_it()
    expect_identical(x$status(), "pending")
    expect_identical(p$status(), "pending")
    x$resolve(100)
    expect_identical(extract(x$promise), 100)
    expect_identical(extract(p$promise), 100)
  })
  it("2.3.2.3. If/when x is rejected, reject promise with the same reason.", {
    p <- ext_promise()
    x <- ext_promise()
    p$resolve(x$promise)
    wait_for_it()
    expect_identical(x$status(), "pending")
    expect_identical(p$status(), "pending")
    squelch_unhandled_promise_error(p$promise)
    x$reject("bad")
    expect_error(extract(x$promise), "^bad$")
    expect_error(extract(p$promise), "^bad$")
  })
})
# 2.3.3. Otherwise, if x is an object or function,
# 2.3.3.1. Let then be x.then. [3.5]
# 2.3.3.2. If retrieving the property x.then results in a thrown exception e, reject promise with e as the reason.
# 2.3.3.3. If then is a function, call it with x as this, first argument resolvePromise, and second argument rejectPromise, where:
# 2.3.3.3.1. If/when resolvePromise is called with a value y, run [[Resolve]](promise, y).
# 2.3.3.3.2. If/when rejectPromise is called with a reason r, reject promise with r.
# 2.3.3.3.3. If both resolvePromise and rejectPromise are called, or multiple calls to the same argument are made, the first call takes precedence, and any further calls are ignored.
# 2.3.3.3.4. If calling then throws an exception e,
# 2.3.3.3.4.1. If resolvePromise or rejectPromise have been called, ignore it.
# 2.3.3.3.4.2. Otherwise, reject promise with e as the reason.
# 2.3.3.4. If then is not a function, fulfill promise with x.
describe("2.3.4. If x is not an object or function, fulfill promise with x.", {
  it(" ", {
    p <- ext_promise()
    p$resolve(10)
    expect_identical(extract(p$promise), 10)
  })
})
