---
title: "Promise Domains"
description: "How promise domains wrap callbacks, nest, and get re-entered when callbacks run. Read this before changing R/domains.R, the then() method in R/promise.R, or the otel domain in R/otel.R."
---

# Promise Domains

## Overview

A promise domain is a set of hooks that change how callbacks behave. When a
domain is active and you call `then()`, the domain can wrap the callbacks.
When those callbacks later run, the same domain is made active again, so any
`then()` calls made inside them also belong to the domain. Shiny uses this for
reactive context and graphics devices. This package uses it for OpenTelemetry
span hand-off (`R/otel.R`).

The active domain is one global value, `globals$domain`. It is either `NULL`,
an environment made by `new_promise_domain()`, or a list made by
`compose_domains()` when domains are nested.

## Key files

- `R/domains.R`
  - `tryCatch()` - package-wide override of base `tryCatch()`.
  - `spliceOnFinally()`, `finallyToFulfilled()`, `finallyToRejected()` - turn
    one `onFinally` into an `onFulfilled`/`onRejected` pair.
  - `promiseDomain` - internal list with `onThen()` and `onError()`.
  - `wrap_callback_reenter()`, `wrap_with_signature()` - re-enter the domain
    when a callback runs, keeping the callback's formals.
  - `globals`, `current_promise_domain()`.
  - `with_promise_domain()` (exported), `reenter_promise_domain()`,
    `without_promise_domain()`.
  - `new_promise_domain()` (exported), `compose_domains()`, `wrap_finally()`.
- `R/promise.R` - `self$then()` inside `Promise()` calls
  `promiseDomain$onThen()`; `doResolve()`/`doReject()` use
  `without_promise_domain()`.
- `R/otel.R` - `local_promise_domain()`, `has_otel_promise_domain()`,
  `create_otel_promise_domain()`, `with_otel_promise_domain()`.
- Tests: `tests/testthat/test-domains.R`, `test-legacy-domains.R` (same tests
  written with the `%...>%` promise pipes), `test-otel.R` (composition and the `.otel_promise_domain`
  flag). `create_counting_domain()` in `tests/testthat/helper.R` is the main
  test domain.

## How it works

### Making a domain

`new_promise_domain(wrapOnFulfilled = identity, wrapOnRejected = identity,
wrapSync = force, onError = force, ..., wrapOnFinally = NULL)` returns an
environment (parent `emptyenv()`) holding those fields plus any named values
in `...`.

- `wrapOnFulfilled(f)` / `wrapOnRejected(f)` get a callback and return a
  replacement. They run once, at `then()` time. The returned function runs at
  callback time.
- `wrapOnFinally(f)` is optional. If `NULL`, a `finally()` callback is wrapped
  by `wrapOnFulfilled` and `wrapOnRejected` instead (as the spliced pair).
- `wrapSync(expr)` wraps the synchronous body of `with_promise_domain()`. It
  must force `expr`.
- `onError(e)` is called for errors seen by the package `tryCatch()`. It acts
  like a calling handler: it does not stop the error.

### Activating a domain

`with_promise_domain(domain, expr, replace = FALSE)`:

1. Saves the current domain.
2. Sets `globals$domain` to `domain` if `replace = TRUE`, or to
   `compose_domains(old, domain)` if not.
3. Restores the old domain with `on.exit()`.
4. Evaluates `domain$wrapSync(expr)` (or just `expr` if `domain` is `NULL`).
   Only the new domain's `wrapSync` runs; outer domains already ran theirs.

`reenter_promise_domain()` is the same without `wrapSync`. It is used when
callbacks run. `without_promise_domain(expr)` is
`with_promise_domain(NULL, expr, replace = TRUE)`; `Promise()` uses it so its
internal "follow another promise" `then()` is not seen by any domain.

`local_promise_domain()` in `R/otel.R` sets the domain until a frame exits
(using `defer()` from `R/import-standalone-defer.R`). It does not call
`wrapSync`.

### What happens at then() time

`self$then()` in `R/promise.R` first normalizes the callbacks, then, inside the
action of the new promise (which runs right away), calls
`promiseDomain$onThen(onFulfilled, onRejected, onFinally)`:

1. Errors if `onFinally` is given with either other callback.
2. Reads `current_promise_domain()`. **This is the moment the domain is
   captured.**
3. If `onFinally` is set and the domain has `wrapOnFinally`, it wraps
   `onFinally` with it.
4. If `onFinally` is set, it is spliced into an `onFulfilled`/`onRejected`
   pair (`spliceOnFinally()`).
5. If there is a domain and `wrapOnFinally` was *not* used, it wraps
   `onFulfilled` with `wrapOnFulfilled` and `onRejected` with
   `wrapOnRejected`.
6. Drops `NULL` entries and wraps each remaining callback with
   `wrap_callback_reenter(callback, domain)`.

`wrap_callback_reenter()` returns a function that runs
`reenter_promise_domain(domain, callback(...), replace = TRUE)`. This is done
even if the domain is `NULL`, so a callback bound outside any domain runs with
no domain, no matter what is active when the event loop calls it. The wrapper
is rebuilt with `wrap_with_signature()` so it has the same formals as the
callback; other code checks `formals()` for `.visible`. It uses
`rlang::call2()` and `rlang::new_function()` instead of `do.call()` so values
that are symbols are not evaluated (see "handles weird edge case relating to
symbols" in `tests/testthat/test-domains.R`).

The final call order when a callback runs is:

```
reenter wrapper (sets globals$domain)
  -> base domain wrapper (outermost)
    -> new domain wrapper (innermost)
      -> your callback
```

### Captured at then() time, not at run time

The domain is chosen when `then()` is called. The domain that is active when
the event loop runs the callback does not matter. `tests/testthat/test-domains.R`
"doesn't grow the call stack" shows this: callbacks bound outside `cd2` are not
counted by `cd2`, even though they run while `cd2` is active.

Because callbacks re-enter their domain, any `then()` call made inside a
callback joins that domain too. "are reentered during handlers" in
`tests/testthat/test-domains.R` checks this.

### Composition (nesting)

`compose_domains(base, new)` returns `new` if `base` is `NULL`, and `base` if
`new` is `NULL`. So `with_promise_domain(NULL, expr)` inside another domain
keeps the outer domain active. Otherwise it returns a plain list with:

- `wrapOnFulfilled` / `wrapOnRejected` that call `new$wrap...` first and then
  `base$wrap...` on the result. The inner (newer) domain sits closest to the
  callback. Pinned by "apply nested domains in reverse order" in
  `tests/testthat/test-domains.R`.
- `wrapOnFinally`, only if either side has one. It calls
  `wrap_finally(base, wrap_finally(new, onFinally))`, so again `new` is
  innermost. `wrap_finally(domain, f)` uses the domain's `wrapOnFinally`, or
  its `wrapOnFulfilled` if it has none (the finally callback runs once either
  way). Since the composed domain then has a `wrapOnFinally`, `onThen()` does
  not also wrap the spliced pair with `wrapOnFulfilled`/`wrapOnRejected`. If
  neither side has one, the field is `NULL` and the normal pair wrapping is
  used.
- `wrapSync = new$wrapSync`.
- `onError` that calls `base$onError(e)` then `new$onError(e)`.
- `.otel_promise_domain` set if either side has it, so
  `has_otel_promise_domain()` still works after nesting.

### Errors and onError

`R/domains.R` defines a package-level `tryCatch()` that wraps `expr` in
`withCallingHandlers(error = function(e) promiseDomain$onError(e))`.
`promiseDomain$onError()` calls `onError` of the current domain, if any. Since
every `tryCatch()` call in the package uses this version, the domain sees
errors in `promise()` actions, in `resolve()` (which is where callback errors
surface), in `hybrid_then()`, and in `as.promise.Future()`.

## Pitfalls

- **Wrappers must accept the same arguments as the callback.** By the time
  `wrapOnFulfilled` sees a callback, it has been normalized to
  `function(value, .visible)` (`self$then()` in `R/promise.R`), and it is
  called with two arguments. A wrapper that returns `function(value)` fails
  with "unused argument". Return `function(...)` and pass `...` on, as
  `create_counting_domain()` and `create_otel_promise_domain()` do.
- **In a nested stack, a domain without `wrapOnFinally` sees `finally()`
  through `wrapOnFulfilled` only.** Once any domain in the stack has
  `wrapOnFinally`, `wrap_finally()` wraps the finally callback with
  `wrapOnFulfilled` for the domains that lack it, and their `wrapOnRejected` is
  not used for it. Pinned by "a nested domain without wrapOnFinally still sees
  finally" in `tests/testthat/test-domains.R`.
- **Composed domains drop extra fields.** Named values passed in `...` to
  `new_promise_domain()` are not copied by `compose_domains()`, except the
  `.otel_promise_domain` flag. Code that reads custom fields from
  `current_promise_domain()` will not find them once domains are nested.
- **`with_promise_domain(NULL, expr)` does not clear the domain.** With
  `replace = FALSE`, `compose_domains(old, NULL)` returns `old`, so the outer
  domain stays active. To run with no domain, use `replace = TRUE` (as
  `without_promise_domain()` does).
- **Re-throw with `rethrow_reason()`, not `stop()`.** `finallyToRejected()`
  passes the rejection on with `rethrow_reason()` (in `R/promise.R`) so a
  non-condition reason such as `42` reaches the next handler unchanged. See
  `thenAndHybrid.md`.
- **The domain is captured when `then()` is called.** Wrapping the event loop
  (for example `with_promise_domain(d, later::run_now())`) has no effect on
  callbacks that were bound earlier.
- **`onError` sees handled errors too.** It runs as a calling handler, before
  any `catch()` or `on_failure` gets the error. It is a notice, not a catch.
- **`onThen()` errors become rejections.** `promiseDomain$onThen()` runs inside
  the action of the new promise, so an error from it, or from a domain's
  `wrapOn*` function, rejects the returned promise instead of being thrown
  from `then()`.
- **`local_promise_domain()` skips `wrapSync`.** It is only used for the otel
  domain, which has no `wrapSync`, but a general domain would behave
  differently from `with_promise_domain()`.
