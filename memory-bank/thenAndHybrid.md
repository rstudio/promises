---
title: "then(), hybrid_then(), and Pipes"
description: "The user-facing functions for chaining work onto promises: then/catch/finally, the tee option, callback conversion, hybrid_then() for code that may be sync or async, the promise pipe operators, and deprecated functions. Read this before changing R/then.R, R/hybrid-then.R, or R/pipe.R."
---

# then(), hybrid_then(), and Pipes

## Overview

`then()`, `catch()`, and `finally()` are the exported functions most users
call. They are thin wrappers around the `then`/`catch`/`finally` functions
stored on each promise object (see `promiseCore.md`). The wrappers add:

- Coercion of the input with `as.promise()`, so a `Future` works directly.
- Conversion of non-function callbacks with `rlang::as_function()`.
- The `tee` option, which runs a callback for side effects and keeps the
  original value or error.

`hybrid_then()` is for code that may get a plain value or a promise. With a
plain value it runs callbacks right away, on the same tick, and returns a plain
value. With a promise it hands off to `then()`.

The pipe operators (`%...>%` and friends) are older magrittr-style helpers.
They are marked superseded by `|>` plus `then()`.

## Key files

- `R/then.R` - `then()`, `catch()`, `finally()`, `check_dots()`,
  `check_tee()`.
- `R/hybrid-then.R` - `hybrid_then()`, `check_hybrid_callback()`,
  `return_with_visible()`.
- `R/pipe.R` - `%...>%`, `%...T>%`, `%...!%`, `%...T!%`, `has.visible()`,
  `pipeify_rhs()`, and re-exports of magrittr `%>%` and `%T>%`.
- `R/first_type.R` - `is_first()`, `prepare_first()` (magrittr "first
  argument" rule for pipes).
- `R/is_something.R` - `is_parenthesized()`, `is_funexpr()`, `is_colexpr()`,
  `is_function()`, `is_lambda()` (shape checks on pipe right-hand sides).
- `R/deprecated.R` - `with_ospan_async()`, `with_ospan_promise_domain()`,
  `local_ospan_promise_domain()`.
- `R/promise.R` - `normalizeOnFulfilled()`, `normalizeOnRejected()` (used by
  both `then()` and `hybrid_then()`).
- Tests: `tests/testthat/test-then.R`, `test-methods.R`, `test-visibility.R`,
  `test-hybrid-then.R`, `test-hybrid-then-visibility.R`,
  `test-legacy-aplus-2-2.R` (formula-callback version of the A+ tests),
  `test-legacy-domains.R` and `test-legacy-promise-map.R` (the `%...>%`
  promise pipes), `test-otel.R` ("Deprecated functions" block). Snapshots for `tee` errors live
  in `tests/testthat/_snaps/`.

## How it works

### then()

`then(promise, onFulfilled = NULL, onRejected = NULL, ..., tee = FALSE)`:

1. `check_dots(...)` errors if anything is passed in `...` (via
   `rlang::check_dots_empty()`).
2. `check_tee(tee)` errors with "`tee` must be `TRUE` or `FALSE`" unless
   `rlang::is_bool(tee)` is `TRUE`. So `NA` and `c(TRUE, FALSE)` are rejected.
3. `promise <- as.promise(promise)`. A non-promise, non-Future value errors
   at once with "Don't know how to convert object of class ...".
4. Any non-`NULL`, non-function callback goes through `rlang::as_function()`.
   This accepts a formula (`~ .x + 1`) or a string naming a function. Numbers
   and lists are not accepted (that is `purrr::as_mapper()`, not
   `rlang::as_function()`), and `then()` errors at once.
5. With `tee = FALSE`, it calls `promise$then(onFulfilled, onRejected)`.
6. With `tee = TRUE`, it normalizes the callbacks first and wraps them:
   - fulfilled: call the callback, then return the original value with its
     original visibility;
   - rejected: call the callback, then `rethrow_reason(reason)` to pass the
     original reason on.
   If the callback itself throws, that new error wins.
7. Returns the new promise invisibly.

The formula form is marked superseded in the roxygen docs (use `\(x) fn(x)`).
There is no runtime deprecation warning.

### catch() and finally()

`catch(promise, onRejected, ..., tee = FALSE)` works like `then()` with only
`onRejected`. Note that `onRejected` has no default. The `tee = TRUE` path
wraps the callback the same way as in `then()`. A `NULL` `onRejected` always
takes the plain `promise$catch(NULL)` path, so `catch(p, NULL, tee = TRUE)`
passes the original error through.

`finally(promise, onFinally)` has no `tee` and no `...`. It converts
`onFinally` with `rlang::as_function()` if needed and calls
`promise$finally()`. Inside, the domain code turns `onFinally` into a pair of
handlers (`finallyToFulfilled()`, `finallyToRejected()` in `R/domains.R`):
the callback runs with no arguments, its return value is dropped, and the
original value (with its visibility) or error is passed on. If `onFinally`
throws, that error replaces the result.

### hybrid_then()

`hybrid_then(expr, on_success = NULL, on_failure = NULL, ..., tee = FALSE)`:

1. Checks `...` and `tee` like `then()`. `check_hybrid_callback()` requires
   each callback to be a function or `NULL`. No `rlang::as_function()`
   conversion here; a formula errors.
2. Normalizes callbacks with `normalizeOnFulfilled()` and
   `normalizeOnRejected()`, so `on_success` may take `.visible`.
3. Evaluates `expr` inside `withVisible(tryCatch(...))`. Note this is the
   package `tryCatch()` from `R/domains.R`, so the current domain's `onError`
   sees the error.

Then one of these paths runs:

- **Sync error, no `on_failure`**: the error is re-thrown at once.
- **Sync error, with `on_failure`**: `on_failure(e)` runs now. With
  `tee = FALSE` its result (with visibility) is returned. With `tee = TRUE`
  the original error is re-thrown after `on_failure` returns.
- **Value is promising** (`is.promising()`): returns
  `then(value, on_success, on_failure, tee = tee)`. Everything after this is
  async and follows `then()` rules.
- **Plain value, no `on_success`**: the value is returned as is, keeping
  visibility.
- **Plain value, with `on_success`**: `on_success(value, visible)` runs now.
  Returns its result, or the original value when `tee = TRUE`.

If a callback returns a promise on a sync path with `tee = FALSE`, that promise
is returned, so the result turns async. With `tee = TRUE` the callback's return
value is dropped, even if it is a promise, so a sync input gives a sync result.
The roxygen docs say this. `return_with_visible()` rebuilds visibility from a
`withVisible()` list.

### Pipe operators

All four pipes capture the right-hand side unevaluated with `match.call()`,
build a function with `pipeify_rhs()`, and then:

- `lhs %...>% rhs` is `then(lhs, func)`.
- `lhs %...!% rhs` is `catch(lhs, func)`.
- `lhs %...T!% rhs` is `catch(lhs, func, tee = TRUE)`.
- `lhs %...T>% rhs` is `lhs %>% then(func) %>% then(function(value) lhs)`. It
  waits for `func`, then resolves to the original `lhs` promise. Errors from
  `func` still flow through.

`pipeify_rhs(rhs, env)` builds `function(., .visible) <body>`:

- `(expr)` is evaluated first, so `(\(x) x + 1)` becomes a function value.
- `{ ... }` is used as the body as is, with `.` bound to the value.
- An unparenthesized `function(...)` or `\(x)` errors with "Anonymous functions
  must be parenthesized".
- A symbol, function, or `pkg::fn` becomes `fn(.)`, or `fn(., .visible =
  .visible)` if the function has a `.visible` formal (`has.visible()`).
- A call with no `.` argument gets `.` added as the first argument
  (`is_first()` / `prepare_first()`), so `head(5)` becomes `head(., 5)`.
- Any other call is used as the body as is.

The magrittr feature of starting a pipe with `.` to make a function is not
supported. `%>%` and `%T>%` are re-exported from magrittr.

### Deprecated functions

`R/deprecated.R` holds three wrappers that warn with
`lifecycle::deprecate_warn("1.5.0", ...)` and then call the new name:

- `with_ospan_async()` -> `with_otel_span()`
- `with_ospan_promise_domain()` -> `with_otel_promise_domain()`
- `local_ospan_promise_domain()` -> `local_otel_promise_domain()`

They are documented under the internal `promises-deprecated` topic and tested
in `tests/testthat/test-otel.R`.

## Pitfalls

- **`then(p, 10)` errors, `p$then(10)` warns.** `then()` in `R/then.R` sends
  non-functions to `rlang::as_function()`, which errors. The method uses
  `normalizeOnFulfilled()`, which warns and ignores. Pinned by
  `tests/testthat/test-methods.R`.
- **Primitive callbacks get the value.** `callback_formals()` in
  `R/promise.R` reads a primitive's formals with `formals(args(f))`, so
  `then(p, sum)` and `hybrid_then(c(1, 2), sum)` work as expected. Pinned by
  "primitive functions receive the value as callbacks" in
  `tests/testthat/test-then.R`.
- **`.visible` must be the exact name of the second formal.** A callback
  written as `function(value, visible)` fails with a missing argument. The
  "Visibility" section in `R/then.R` names `.visible`.
- **Pass rejections on with `rethrow_reason()`, not `stop(reason)`.**
  `finallyToRejected()` in `R/domains.R` and the rejected `tee` wrappers in
  `then()` and `catch()` use it, so a non-condition reason such as `42` reaches
  the next handler unchanged. `stop(reason)` would turn it into a
  `simpleError`. Pinned in `tests/testthat/test-then.R`.
- **`hybrid_then()` does not send `on_success` errors to `on_failure`.** This
  is expected: `on_failure` handles a failure of `expr` only. On the sync path
  an error in `on_success` is thrown straight out; on the async path the
  returned promise rejects with it. This matches `then()`, where `onRejected`
  does not see errors from `onFulfilled` in the same call. Pinned by
  "on_failure does not handle on_success errors" in
  `tests/testthat/test-hybrid-then.R`.
- **`hybrid_then(tee = TRUE)` ignores promises returned by callbacks.**
  `on_success` or `on_failure` may return a promise; with `tee = TRUE` it is
  not waited on or followed, on both the sync and async paths. A sync input
  gives back the plain value right away (or re-throws the error). This is on
  purpose, matches `then(tee = TRUE)`, and is documented in `R/hybrid-then.R`.
  Pinned by "tee = TRUE ignores a promise returned by the callback" in
  `tests/testthat/test-hybrid-then.R`, using a promise that never settles and
  one that rejects.
- **`hybrid_then()` sync errors still reach domain `onError`.** Because the
  package `tryCatch()` runs `promiseDomain$onError()` as a calling handler, a
  domain sees the error even when `on_failure` recovers from it.
- **`%...T>%` is not the same as `then(tee = TRUE)`.** It resolves to the
  `lhs` promise through a second `then()`, so it settles later than
  `then(lhs, func, tee = TRUE)` would, and `.visible` is passed to `func` only
  if `func` names it.
- **`finally()` cannot be mixed with other callbacks in one call.**
  `promiseDomain$onThen()` in `R/domains.R` errors if `onFinally` is given with
  `onFulfilled` or `onRejected`. The exported functions never do this, but
  `p$then(f, onFinally = g)` does; the error is caught by `promise()` and
  becomes a rejection, not a thrown error.
