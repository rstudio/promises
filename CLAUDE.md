# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Testing
- `devtools::check()` - Run full R CMD check (standard R package check)
- `devtools::test()` - Run unit tests using testthat
- `spelling::spell_check_test(vignettes = TRUE, error = TRUE, skip_on_cran = TRUE)` - Check spelling

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

**Promise Domains (`R/domains.R`):**
- Context management system for promise execution
- Custom error handling and domain-specific behavior
- Functions: `new_promise_domain()`, `with_promise_domain()`

**Core Operations (`R/then.R`):**
- `then()` - Primary interface for promise chaining and result handling
- Supports both fulfillment (`onFulfilled`) and rejection (`onRejected`) callbacks
- Formula syntax is deprecated in favor of function shorthand `\(x) fn(x)`

**Combining Operations (`R/methods.R`):**
- `promise_all()` - Wait for all promises to complete
- `promise_race()` - Wait for first promise to complete
- `promise_map()` - Apply function over list of promises
- `promise_reduce()` - Reduce promises sequentially

**Future Integration (`R/future_promise.R`):**
- Bridges promises with the `future` package for parallel computing
- `future_promise()` - Convert future objects to promises
- Work queue system for managing concurrent operations
- Requires `future >= 1.21.0` and `fastmap >= 1.1.0`

### Package Structure
Standard R package layout:
- `R/` - Core R source code (7 main files)
- `src/` - C++ code using Rcpp for performance-critical operations
- `tests/testthat/` - Unit tests following testthat 3.0 conventions
- `vignettes/` - Comprehensive documentation with 8 vignettes covering motivation, usage patterns, and case studies
- `inst/` - Contains C++ template code (`promise_task.cpp`) for background tasks

### Dependencies
- **Core:** `fastmap`, `later`, `R6`, `rlang`, `magrittr`
- **Integration:** `future` (optional), `mirai` (optional)
- **Development:** `testthat`, `knitr`, `rmarkdown`, `spelling`

### Key Design Patterns
- **R6 Classes:** Object-oriented design for promise state management
- **Callback Chaining:** JavaScript-style `.then()` method chaining
- **Domain System:** Context isolation and custom error handling

The package follows R package development best practices with comprehensive documentation, extensive testing, and CI/CD automation.
