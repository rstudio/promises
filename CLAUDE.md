# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

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
- `expect_assertions(n)` - Declare expected number of assertions in a test (useful with async tests)

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
The promises package implements JavaScript-style promises in R for asynchronous programming. Key architectural components:

**Promise Class (`R/promise.R`):**
- R6 class-based implementation with states: "pending", "fulfilled", "rejected"
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
- Bridges promises with the `future` package for parallel computing
- `future_promise()` - Convert future objects to promises
- Work queue system (`WorkQueue` R6 class) for managing concurrent operations
- Exponential backoff (`ExpoDelay`) when workers unavailable
- Polling mechanism to check `future::resolved()` status
- Requires `future >= 1.21.0` and `fastmap >= 1.1.0`

**OpenTelemetry Integration (`R/otel.R`):**
- Experimental integration with `otel` package for distributed tracing
- `with_otel_span()` - Create/manage spans for async operations
- `with_otel_promise_domain()` - Handoff promise domain for span context propagation
- Critical for observability in complex async workflows
- Uses cached tracer via `promises_otel_tracer()` for performance

### Integration Points

**Future Package:**
- `as.promise.Future()` method converts Future objects to promises (R/promise.R:487-543)
- Caches converted promise as attribute to avoid duplicate polling
- Uses polling loop with 0.1s intervals to check `future::resolved()`
- `future_promise()` creates lazy futures and schedules work via WorkQueue

**Mirai Package:**
- Promises implements `is.promising.Future` for Future objects
- Mirai package provides its own `as.promise.mirai()` method
- No polling needed - mirai has background process that interrupts main session

### Package Structure
Standard R package layout:
- `R/` - Core R source code (11 main files)
- `tests/testthat/` - Unit tests following testthat 3.0 conventions
- `vignettes/` - Comprehensive documentation with 9 vignettes covering motivation, usage patterns, and case studies
- `inst/` - Contains C++ template code (`promise_task.cpp`) for background tasks using `later::BackgroundTask`

### Dependencies
- **Core:** `fastmap`, `later`, `R6`, `rlang`, `magrittr`, `lifecycle`, `otel`
- **Integration:** `future` (optional), `mirai` (optional)
- **Development:** `testthat`, `knitr`, `rmarkdown`, `spelling`

### Key Design Patterns
- **R6 Classes:** Object-oriented design for promise state management
- **Callback Chaining:** JavaScript-style `.then()` method chaining
- **Domain System:** Context isolation and custom error handling via promise domains
- **Lazy Evaluation:** `withVisible()` to preserve visibility of promise results
- **Work Queue Pattern:** FIFO queue with exponential backoff for resource-constrained async operations

The package follows R package development best practices with comprehensive documentation, extensive testing, and CI/CD automation.
