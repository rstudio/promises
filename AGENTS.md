# AGENTS.md

Project context and guidance for AI coding assistants working in this repository.

## Project Overview

`promises` implements JavaScript-style promises in R for asynchronous programming. It is an R package following standard R package development practices (devtools, roxygen2, testthat, pkgdown).

## Development Commands

### Testing
- `devtools::check()` - Run full R CMD check (standard R package check)
- `devtools::test()` - Run unit tests using testthat
- `devtools::test(filter = "then")` - Run tests matching pattern (e.g., all tests in test-then.R)
- `testthat::test_file("tests/testthat/test-*.R")` - Run a single test file
- `spelling::spell_check_test(vignettes = TRUE, error = TRUE, skip_on_cran = TRUE)` - Check spelling

**Test Helpers** (`tests/testthat/helper.R`):
- `wait_for_it(p)` - Block until all pending later tasks execute; optionally wait for promise `p` to resolve/reject
- `extract(promise)` - Block until promise resolves and return value (or throw error if rejected)
- `ext_promise()` - Create externally-controllable promise for testing (returns list with `promise`, `resolve`, `reject`, `status`)
- `expect_assertions(code, n_expected)` - Run `code` and assert it produces exactly `n_expected` testthat expectations (useful with async tests)
- `resolve_later(value, delaySecs)` - Create a promise that resolves with `value` after a delay
- `squelch_unhandled_promise_error(promise)` - Suppress "Unhandled promise error" warnings (but rethrow test failures)
- `create_counting_domain(trackFinally)` - Promise domain that counts callback bind/call/active events for domain tests

### Package Development
- `devtools::load_all()` - Load package for development
- `devtools::document()` - Generate documentation from roxygen2 comments
- `devtools::build()` - Build source package
- `devtools::install()` - Install package locally
- `pkgdown::build_site()` - Build package documentation website

### GitHub Actions
The package uses RStudio's shiny-workflows for CI/CD:
- Automated R CMD check on push/PR
- Website deployment via pkgdown
- Code formatting checks

## Architecture Overview

### Core Promise Implementation

**Promise Class (`R/promise.R`):**
- Promises are simple classed environments (not R6) created by the `Promise()` factory function; this reduces creation overhead by up to 10x (#191)
  - Internal state and methods live in a `private` environment inside the closure; the public promise object exposes `$then()`, `$catch()`, `$finally()`, and `$status`
  - Internals are reachable via `attr(p, "promise_impl", exact = TRUE)` (used by tests and `as.promise()` methods)
- States: "pending", "fulfilled", "rejected"
- Private methods `doResolve`/`doReject` handle state transitions
- Public API through `resolve()`, `reject()`, and `then()` methods
- Chaining cycle detection to prevent infinite loops
- Uses `later::later()` to schedule callbacks for fulfillment/rejection
- `normalizeOnFulfilled()`/`normalizeOnRejected()` - Normalize callbacks to handle 0, 1, or 2 arguments
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
  - Callbacks support `rlang::as_function()` conversions (formulas, strings, integers, lists for extraction)
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

**Combining Operations (`R/methods.R`):**
- `promise_all()` - Wait for all promises to complete
- `promise_race()` - Wait for first promise to complete
- `promise_map()` - Apply function over list of promises
- `promise_reduce()` - Reduce promises sequentially

**Future Integration (`R/future_promise.R`):**
- `future_promise()` - Convert future objects to promises
- Work queue system (`WorkQueue` R6 class) for managing concurrent operations
- Exponential backoff (`ExpoDelay`) when workers unavailable
- Polling mechanism to check `future::resolved()` status

**OpenTelemetry Integration (`R/otel.R`):**
- Integration with `otel` package (>= 0.2.0, in Imports) for distributed tracing; `otelsdk` (>= 0.2.0) in Suggests for span collection in tests
- `with_otel_span()` - Create a span, execute an expression within it, end it on completion (handles both sync values and promises)
- `with_otel_promise_domain()` - Handoff promise domain that restores the active span from when `then()` was called
- `local_otel_promise_domain()` - Add the span promise domain to the local scope (for `{coro}` where `with_*()` wrappers are not allowed)
- Earlier `with_ospan_async()`/`with_ospan_promise_domain()`/`local_ospan_promise_domain()` names are deprecated (#198)
- Uses cached tracer via `promises_otel_tracer()` for performance

### Integration Points

**Future Package:**
- `as.promise.Future()` method converts Future objects to promises (R/promise.R:494)
- Uses polling loop with 0.1s intervals to check `future::resolved()`
- `future_promise()` creates lazy futures and schedules work via WorkQueue

**Mirai Package:**
- Mirai package provides its own `as.promise.mirai()` method
- Newer generation event-driven promises
- No polling needed - mirai handles completions on a thread pool, and calls into the later event loop directly

### Package Structure
Standard R package layout:
- `R/` - Core R source code (15 files; includes `hybrid-then.R`, `deprecated.R`, `pipe.R`, `first_type.R`, `is_something.R`, `promises-package.R`, and standalone imports in `staticimports.R` / `import-standalone-defer.R`)
- `tests/testthat/` - Unit tests following testthat 3.0 conventions; includes Promises/A+ spec tests (`test-aplus-*.R`), legacy behavior tests (`test-legacy-*.R`), and `_snaps/` snapshots
- `vignettes/` - Comprehensive documentation with 9 vignettes covering motivation, usage patterns, and case studies
- `inst/` - Only `WORDLIST` for spelling; the C++ `promise_task.cpp` template / interface test was moved to the `later` package (#204)

### Dependencies
- **Core (Imports):** `fastmap` (>= 1.1.0), `later`, `lifecycle`, `magrittr` (>= 1.5), `otel` (>= 0.2.0), `R6`, `rlang`; requires R >= 4.1.0
- **Integration (Suggests):** `future` (>= 1.21.0), `mirai`, `otelsdk` (>= 0.2.0)
- **Development:** `testthat` (>= 3.0.0), `knitr`, `rmarkdown`, `spelling`, `purrr`, `vembedr`; roxygen2 8.0.0 with markdown enabled

### Key Design Patterns
- **Classed Environments:** Promises are environments with closures (not R6) for fast construction; R6 remains only for `WorkQueue`/`ExpoDelay` in `future_promise.R`
- **Callback Chaining:** JavaScript-style `.then()` method chaining
- **Domain System:** Context isolation and custom error handling via promise domains; nested domains invoke in reverse (innermost-first) order
- **Lazy Evaluation:** `withVisible()` to preserve visibility of promise results
- **Work Queue Pattern:** FIFO queue with exponential backoff for resource-constrained async operations
