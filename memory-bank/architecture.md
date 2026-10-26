---
title: "Architecture"
description: "Map of how promises fits together: the promise object, chaining, domains, combinators, future and mirai, OpenTelemetry, and package layout. Start here, then follow the table to the doc for the file you are changing."
---

# Architecture

## Core Promise Implementation

**Promise Class (`R/promise.R`):**
- Promises are simple classed environments (not R6) created by the `Promise()` factory function; this reduces creation overhead by up to 10x (#191)
  - Internal state and methods live in a `private` environment inside the closure; the public promise object is a list of `$then()`, `$catch()`, and `$finally()`; `$status()` lives only on the inner object
  - Internals are reachable via `attr(p, "promise_impl", exact = TRUE)` (used by tests and `as.promise()` methods)
- States: "pending", "fulfilled", "rejected"
- Private methods `doResolve`/`doReject` handle state transitions
- Public API through `resolve()`, `reject()`, and `then()` methods
- Chaining cycle detection (`private$adopt()` walks `private$following`) so promises that wait on each other reject instead of hanging
- Uses `later::later()` to schedule callbacks for fulfillment/rejection
- `normalizeOnFulfilled()`/`normalizeOnRejected()` - Normalize callbacks to handle 0, 1, or 2 arguments (`callback_formals()` lets primitives like `sum` get the value)
  - `onFulfilled` can take `(value, .visible)` for visibility detection
  - Callbacks with 0 args are wrapped to ignore arguments
  - Used by both `then()` and `hybrid_then()` for consistent callback handling

**Promise Domains (`R/domains.R`):**
- Context management system for promise execution
- Custom error handling and domain-specific behavior
- Functions: `new_promise_domain()`, `with_promise_domain()`
- Domain composition allows layering of contexts
- Critical for maintaining execution context across async boundaries (e.g., graphics devices, reactive contexts)

**Core Operations (`R/then.R`):**
- `then()` - Primary interface for promise chaining and result handling
  - Supports both fulfillment (`onFulfilled`) and rejection (`onRejected`) callbacks
  - Callbacks support `rlang::as_function()` conversions (functions, formulas, and function-name strings)
  - Formula syntax `~ fn(.)` is deprecated in favor of function shorthand `\(x) fn(x)`
  - Returns new promise that chains from the input promise
- `catch()` and `finally()` - Convenience functions (equivalent to `then()` with specific callbacks)
- `tee` parameter - Execute callbacks for side-effects while preserving original values
  - With `tee=TRUE`, callback result is discarded and original value/error is propagated
  - Useful for logging, debugging, or other side-effect operations
- `hybrid_then()` - Hybrid synchronous/asynchronous execution
  - If `expr` evaluates to a promise, delegates to `then()` for async handling
  - If `expr` evaluates to a value, executes `on_success`/`on_failure` callbacks synchronously (same tick)
  - Useful for functions that need to work with both sync and async inputs (e.g., plumber2)

**Combining Operations (`R/utils.R`):**
- `promise_all()` - Wait for all promises to complete
- `promise_race()` - Wait for first promise to complete
- `promise_map()` - Apply function over list of promises
- `promise_reduce()` - Reduce promises sequentially

**Future Integration (`R/future_promise.R`):**
- `future_promise()` - Run an expression in a future once a worker is free, and return a promise for the result
- `future_promise_queue()` - The default `WorkQueue` used by `future_promise()`
- Work queue system (`WorkQueue` R6 class) that holds work until a future worker is free
- Exponential backoff (`ExpoDelay`) when workers unavailable
- Polling mechanism to check `future::resolved()` status

**OpenTelemetry Integration (`R/otel.R`):**
- Integration with `otel` package (>= 0.2.0, in Imports) for distributed tracing; `otelsdk` (>= 0.2.0) in Suggests for span collection in tests
- `with_otel_span()` - Create a span, execute an expression within it, end it on completion (handles both sync values and promises); marks the span status `"error"` on a sync error or a rejection
- `with_otel_promise_domain()` - Handoff promise domain that restores the active span from when `then()` was called
- `local_otel_promise_domain()` - Add the span promise domain to the local scope (for `{coro}` where `with_*()` wrappers are not allowed)
- Earlier `with_ospan_async()`/`with_ospan_promise_domain()`/`local_ospan_promise_domain()` names are deprecated (#198)
- `with_otel_span()` needs a `tracer`; promises keeps no tracer cache of its own

## Integration Points

**Future Package:**
- `as.promise.Future()` (in `R/promise.R`) converts Future objects to promises and caches the promise on the Future
- Uses polling loop with 0.1s intervals to check `future::resolved()`
- `future_promise()` creates lazy futures and schedules work via WorkQueue

**Mirai Package:**
- Mirai package provides its own `as.promise.mirai()` method
- Newer generation event-driven promises
- No polling needed - mirai handles completions on a thread pool, and calls into the later event loop directly

## Package Structure
Standard R package layout:
- `R/` - Core R source code (15 files; includes `hybrid-then.R`, `deprecated.R`, `pipe.R`, `first_type.R`, `is_something.R`, `promises-package.R`, and standalone imports in `staticimports.R` / `import-standalone-defer.R`)
- `tests/testthat/` - Unit tests following testthat 3.0 conventions; includes Promises/A+ spec tests (`test-aplus-*.R`), legacy behavior tests (`test-legacy-*.R`), and `_snaps/` snapshots
- `vignettes/` - Comprehensive documentation with 9 vignettes covering motivation, usage patterns, and case studies
- `inst/` - Only `WORDLIST` for spelling; the C++ `promise_task.cpp` template / interface test was moved to the `later` package (#204)

## Key Design Patterns
- **Classed Environments:** Promises are environments with closures (not R6) for fast construction; R6 remains only for `WorkQueue`/`Delay`/`ExpoDelay` in `future_promise.R`
- **Callback Chaining:** JavaScript-style `.then()` method chaining
- **Domain System:** Context isolation and custom error handling via promise domains; nested domains invoke in reverse (innermost-first) order
- **Lazy Evaluation:** `withVisible()` to preserve visibility of promise results
- **Work Queue Pattern:** FIFO queue with exponential backoff for resource-constrained async operations

## Where to read next

| R file(s) | Doc |
|---|---|
| `R/promise.R`, `R/methods.R` | `promiseCore.md` |
| `R/then.R`, `R/hybrid-then.R`, `R/pipe.R`, `R/deprecated.R` | `thenAndHybrid.md` |
| `R/domains.R` | `domains.md` |
| `R/utils.R` | `combining.md` |
| `R/future_promise.R` | `futurePromise.md` |
| `R/otel.R` | `otel.md` |
| `tests/testthat/` | `testing.md` |
| `.github/`, `DESCRIPTION`, `pkgdown/`, `NEWS.md`, vignettes | `ciAndTooling.md` |
