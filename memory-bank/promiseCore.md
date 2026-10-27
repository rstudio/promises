---
title: "Promise Core"
description: "How a promise object is built, how it moves between states, and how callbacks are scheduled. Read this before changing R/promise.R, the as.promise() methods, or anything that touches resolve/reject timing or visibility."
---

# Promise Core

## Overview

A promise holds the future result (or error) of some work. In this package a
promise has two layers:

- An inner object made by the internal `Promise()` factory. It holds the state,
  the value, and the waiting callbacks.
- An outer object returned by `promise()`. It is a plain list of class
  `"promise"` with three functions (`then`, `catch`, `finally`) and an
  attribute `promise_impl` that points to the inner object.

Promises are classed environments, not R6 objects. NEWS.md for 1.5.0 notes
this change made creation up to 10x faster. R6 is still imported, but only
`R/future_promise.R` uses it (`Delay`, `ExpoDelay`, `WorkQueue`).

All callbacks run later, through `later::later()`, never at the moment you call
`then()`. This matches the Promises/A+ rule that handlers run on a clean stack.

## Key files

- `R/promise.R`
  - `Promise()` - internal factory for the inner object.
  - `normalizeOnFulfilled()`, `normalizeOnRejected()` - make callbacks take a
    fixed set of arguments. `callback_formals()` reads the formals they use.
  - `promise()` - public constructor.
  - `promise_resolve()`, `promise_reject()` - helpers for an already settled
    promise.
  - `is.promise()`, `is.promising()` (S3 generic), `as.promise()` (S3
    generic), with methods for `promise`, `Future`, and `default`.
  - `resolve()`, `reject()` - internal stubs that only exist for docs; they
    always throw.
- `R/methods.R` - `format.promise()` and `print.promise()`.
- `R/domains.R` - package-level `tryCatch()` override and
  `promiseDomain$onThen()`, used by `Promise()` (see `domains.md`).
- Tests: `tests/testthat/test-aplus-2-1.R`, `test-aplus-2-2.R`,
  `test-aplus-2-3.R` (Promises/A+ spec), `test-methods.R`, `test-visibility.R`,
  `test-ordering.R`, `test-zzz-future_promise.R`.
- Test helpers: `tests/testthat/helper.R` (`ext_promise()`, `wait_for_it()`,
  `extract()`, `squelch_unhandled_promise_error()`).

## How it works

### The inner object

`Promise()` makes two environments with `emptyenv()` as parent: `self` and
`private`. `private` holds:

- `state` - `"pending"`, `"fulfilled"`, or `"rejected"`.
- `value` - the result, or the error when rejected.
- `visible` - whether the result was visible (see Visibility).
- `publicResolveRejectCalled` - guards so `resolve`/`reject` work only once.
- `onFulfilled`, `onRejected` - lists of waiting handlers.
- `rejectionHandled` - used for the unhandled error message.
- `following` - the inner object of the promise this one is waiting on, or
  `NULL`. Set by `adopt()` and used to find chaining cycles. Cleared when the
  promise settles, so a settled promise does not keep the chain it followed
  in memory.

`self` gets the public functions `status()`, `resolve()`, `reject()`,
`then()`, `catch()`, `finally()`, and `format()`, plus the internal
`subscribe()` (see below). It also gets
`self$.__enclos_env__ <- list(private = private)` so code that expects the R6
layout can still reach `private`. `private$adopt()` reads it to walk the
`following` chain of other promises. The class is set to `"Promise"`.

### Resolve and reject

`self$resolve(value)` and `self$reject(reason)` both return `invisible()` and
do nothing after the first call to either one.

- `resolve()` passes `value` to `private$doResolve()` without forcing it. This
  is on purpose: `doResolve()` calls `withVisible(value)`, and forcing it
  earlier would lose the visibility. If forcing `value` throws, the error goes
  to `doReject()`.
- `reject()` forces `reason`. A character string becomes `simpleError(reason)`.
  Other values are passed as is.

`doResolve()`:

1. Runs `withVisible(value)`.
2. If the value `is.promising()`, it calls `private$adopt(as.promise(value))`.
3. Otherwise it calls `doResolveFinalValue(value, visible)`.

`doReject()` does the same check: if the reason `is.promising()`, it calls
`adopt()` on it, so the final state comes from that promise. Otherwise it
calls `doRejectFinalReason()`.

`private$adopt(p)` makes this promise follow `p`:

1. It walks the chain of promises that `p` is waiting on, starting at the inner
   object of `p` and moving through each `private$following`. If the walk
   reaches `self`, it rejects with `"Chaining cycle detected for promise"`.
   This finds direct cycles (resolving with yourself) and longer ones (`a`
   waits on `b`, `b` waits on `a`).
2. It sets `private$following` to the inner object of `p`.
3. It subscribes to `p` directly:
   `attr(p, "promise_impl")$subscribe(private$doResolveFinalValue,
   private$doRejectFinalReason)`. A settled promise's value is always final,
   so the `withVisible()`/`is.promising()` re-check in
   `doResolve()`/`doReject()` is skipped, and `subscribe()` never consults
   domains. If `p` has no `promise_impl` attribute (possible for a
   third-party `as.promise()` result; the documented shape is just
   `then`/`catch`/`finally`), it falls back to
   `p$then(private$doResolve, private$doReject)` inside
   `without_promise_domain()`.

### Settling and scheduling

`doResolveFinalValue()` sets `value`, `visible`, and `state`
(`"fulfilled"`) right away. When at least one `onFulfilled` handler is waiting
(the chain is not `NULL`), it schedules one `later()` task that calls every
waiting handler in registration order with `(value, visible)` and clears the
chain. With no handlers it schedules nothing — so `later::loop_empty()` can be
`TRUE` immediately after `promise_resolve(x)`, and a `then()` attached after
settlement schedules its own task. `doRejectFinalReason()` always schedules:
its nested `later()` drives the unhandled-rejection message.

`doRejectFinalReason()` sets `value` and `state` (`"rejected"`), then schedules
one `later()` task that:

1. Calls every waiting `onRejected` handler in order, setting
   `rejectionHandled <- TRUE` before each one.
2. Schedules a second `later()` task. If `rejectionHandled` is still `FALSE`,
   it writes `Unhandled promise error: <message>` to `stderr()` with `cat()`.
   For a condition, `<message>` is `conditionMessage(reason)`. For any other
   value (for example `reject(42)`), it is `paste(format(reason), collapse =
   " ")`. The code comment says `warning()` was not reliable here.

Because state changes at once, `status()` reports `"fulfilled"` right after
`promise_resolve(1)`, even though no handler has run yet.

### then()

`self$then(onFulfilled, onRejected, onFinally)`:

1. If no promise domain is active (the common case), `then_no_domain()`
   takes a fast path: it builds the derived promise directly with
   `new_promise()` (no `promise()` constructor, no `tryCatch`, no
   `promiseDomain$onThen()`), computes each callback's calling convention
   from `callback_formals()` as a 0/1/2-arg code instead of allocating
   `normalizeOn*()` wrappers, and fuses the NULL-domain reentry branch into
   the handle closures. The "cannot combine `onFinally` with
   `onFulfilled`/`onRejected`" check still rejects the derived promise
   asynchronously rather than throwing.
2. With a domain active, it normalizes `onFulfilled` and `onRejected`
   (below). A non-function `onFinally` becomes `NULL`. Then it builds a new
   promise with `promise()`. Inside its action:
   - Calls `promiseDomain$onThen()` to let the current domain wrap the
     callbacks. If `onFinally` is set, it is turned into an `onFulfilled` and
     `onRejected` pair here. See `domains.md`.
   - `handleFulfill(value, visible)` calls `onFulfilled(value, visible)` and
     resolves the new promise with the result. With no `onFulfilled`, it passes
     the value through and keeps its visibility.
   - `handleReject(reason)` calls `onRejected(reason)` and *resolves* the new
     promise with the result. With no `onRejected`, it rejects the new promise
     with the same reason.
3. Both paths end the same way: if pending, both handlers are added to the
   waiting lists; if already settled, one `later()` task is scheduled to call
   the right handler (for an already rejected promise, that task also sets
   `rejectionHandled`); the new promise is returned invisibly.

Since `resolve()` does not force its argument, an error thrown by a callback is
caught inside `resolve()` and rejects the new promise.

### subscribe()

`self$subscribe(onFulfilled, onRejected)` is the internal, low-overhead way
to listen for settlement: it registers the handlers as-is — no
normalization, no domain wrapping, no derived promise. Handlers must use the
internal calling convention, `(value, visible)` on fulfillment and
`(reason)` on rejection, matching what `doResolveFinalValue()`'s settlement
task invokes. If the promise is already settled, it schedules the usual
`later()` task (setting `rejectionHandled` first when rejected, which keeps
the unhandled-rejection message semantics intact). Used by `adopt()` and by
`promise_all()`/`promise_race()` — the latter two through the
`promise_subscribe()` helper, which falls back to a domain-free `then()` for
third-party promises without a `promise_impl` attribute. User callbacks
still go through `then()` and get domain wrapping.

`self$catch(onRejected)` is `then(onRejected = onRejected)`.
`self$finally(onFinally)` is `then(onFinally = onFinally)`.

### Callback normalization

Both functions count formals with `callback_formals(f)`. For most functions
this is `formals(f)`. For a primitive such as `sum` or `abs`, `formals()` is
`NULL`, so it uses `formals(args(f))` instead. This way a primitive is called
with the value, not with no arguments.

`normalizeOnFulfilled(f)` always returns `NULL` or a function of
`(value, .visible)`:

- Not a function: returns `NULL`, and warns if it was not `NULL`.
- Two or more formals and the second is named exactly `.visible`: returned as
  is.
- One or more formals otherwise: wrapped so it is called with one argument,
  `value` or `invisible(value)`.
- Zero formals: wrapped so it is called with no arguments.

`normalizeOnRejected(f)` returns `NULL` (warning if not `NULL`), a wrapper that
calls it with no arguments when it has zero formals, a `function(reason)`
wrapper for a primitive, or the function as is otherwise. Primitives need the
wrapper because domain code copies the callback's formals
(`wrap_with_signature()` in `R/domains.R`), and a primitive has none.

### Visibility

The value's visibility is captured by `withVisible()` in `doResolve()` and
stored in `private$visible`. It is passed to handlers as `.visible`. For a
one-argument callback, the wrapper passes `invisible(value)` as a lazy
argument, so `withVisible(value)` inside the callback still sees
`visible = FALSE`. `tests/testthat/test-visibility.R` checks that invisibility
survives every mix of `then`, `catch`, and `finally`.

### promise(), promise_resolve(), promise_reject()

`promise(action)`:

- Accepts a function with exactly two formals, or a one-sided formula. Anything
  else throws right away.
- A formula is evaluated with `resolve`, `reject`, and a `return` that warns
  `"Can't return a value from a promise, use resolve/reject"`. The formula form
  is marked superseded in the docs but gives no runtime warning.
- The action runs at once, inside the package `tryCatch()`. If it throws while
  the promise is pending, the promise is rejected. If it throws after
  `resolve`/`reject` was called, the error is re-raised as a warning.
- The return value of the action is ignored.

`promise_resolve(value)` and `promise_reject(reason)` are one-liners around
`promise()`. `promise_resolve(stop("x"))` gives a rejected promise because
`value` is forced inside `resolve()`.

### Type checks and coercion

- `is.promise(x)` is `inherits(x, "promise")`.
- `is.promising()` is `TRUE` for `promise` and `Future`, `FALSE` by default.
  Other packages add methods (mirai defines its own `as.promise.mirai()`).
- `as.promise.promise()` returns the input. `as.promise.default()` throws
  `"Don't know how to convert object of class ..."`.
- `as.promise.Future()` first checks `attr(x, "converted_promise")`. If set, it
  returns the cached promise, so one Future has only one polling loop. If not,
  it builds a promise that calls `future::resolved(x, timeout = 0)` and, if not
  done, re-checks with `later(check, 0.1)`. When done it calls
  `future::value(x, signal = TRUE)`. A `FutureError` or any error from
  `value()` rejects the promise. The cache works because a Future is an
  environment, so setting an attribute on the local `x` changes the shared
  object. Comments in the code explain why polling is still needed.

### Printing

`format.promise()` reads the `promise_impl` attribute and calls its `format()`.
It gives `<Promise [pending]>` or `<Promise [<state>: <class of value>]>`, for
example `<Promise [fulfilled: numeric]>`. `print.promise()` prints that line.

## Pitfalls

- **Handler lists grow by prepending cells.** Registration uses
  `private$onFulfilled <- list(f, private$onFulfilled)` (and the `onRejected`
  twin): O(1) per registration, with handlers stored newest-first.
  `collect_handlers()` turns the chain into a plain list at settlement and the
  loops iterate `rev()` to restore registration order. The obvious alternatives
  lose: `c(chain, list(f))` copies the list every time (quadratic for fan-out —
  1000 registrations measured ~8MB), and index assignment
  (`x[[length(x) + 1L]] <- f`) is O(1) only for local lists — on an
  environment-bound list it duplicates on every write (measured 2x slower than
  `c()` in place). An empty chain is `NULL`, not `list()`.
- **Callbacks always run later.** Even for a settled promise, `then()` schedules
  the handler with `later()`. Tests must run the event loop (`wait_for_it()` or
  `later::run_now()`). `tests/testthat/test-ordering.R` pins the exact order
  for two parallel chains.
- **Adoption skips a settlement tick.** Resolving a promise with another
  promise used to route through a throwaway derived promise (an extra
  `later()` task); now `adopt()` subscribes to it directly, so the adopting
  promise's handlers run one event-loop tick earlier. The relative order of
  `then()` callbacks is unchanged.
- **Rejecting with a promise follows that promise.** `R/promise.R`
  `private$doReject()` checks `is.promising(reason)`. So
  `promise_reject(promise_resolve(5))` *fulfills* with `5`. This differs from
  JavaScript. `tests/testthat/test-methods.R` ("future" block) pins it with a
  Future as the reason: `promise_reject(future::future(1))` fulfills with `1`.
- **Cycle checks only follow `adopt()` links.** `adopt()` walks
  `private$following`, which is set only when a promise is resolved with
  another promise. A link made by `then()` is not tracked. So
  `p2 <- then(p1, \(x) p3); p3 <- then(p2, identity)` stays pending forever
  with no error (checked by running). Resolving with yourself and promises
  that resolve to each other are found. Pinned by
  `tests/testthat/test-aplus-2-3.R` and "promises that resolve to each other
  reject with a cycle error" in `tests/testthat/test-methods.R`.
- **The unhandled error message can fire too early.** It is checked two
  `later()` ticks after rejection. A `catch()` added after that still runs, but
  the message is already printed. Attach handlers in the same tick (the
  "Unhandled errors" section of the `then()` docs says this). A `then()` with
  only `onFulfilled` counts as handling the parent; the error moves to the
  child promise, which then reports it if nothing handles it.
- **The `.visible` name must be exact.** Only a second formal named `.visible`
  gets the flag. `function(value, visible)` is called with one argument and
  fails with "argument "visible" is missing".
- **The method and the function treat bad callbacks differently.**
  `p$then(10)` warns and ignores the value (`normalizeOnFulfilled()`), while
  `then(p, 10)` errors via `rlang::as_function()`. Pinned by
  `tests/testthat/test-methods.R`.
- **Callbacks that pass a rejection on must use `rethrow_reason()`.**
  `stop(reason)` turns a non-condition reason such as `42` into a
  `simpleError`. `rethrow_reason()` wraps it in a `promises_rethrow`
  condition, and the error handler in `self$resolve()` unwraps it, so the
  promise rejects with the original value.
- **There is no public way to read the state.** Code that needs it must use `attr(p, "promise_impl")$status()`, as
  `ext_promise()` in `tests/testthat/helper.R` does. There is no exported
  status function.
- **`tryCatch` here is not base `tryCatch`.** `R/domains.R` redefines
  `tryCatch()` for the whole package so every error passes through
  `promiseDomain$onError()` first. Errors in `promise()` actions,
  `resolve()`, and `as.promise.Future()` all notify the current domain.
