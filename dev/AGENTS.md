# AGENTS.md

This file provides guidance to coding agents when working with code in
this repository.

## Project Overview

`promises` implements JavaScript-style promises in R for asynchronous
programming. It is an R package following standard R package development
practices (devtools, roxygen2, testthat, pkgdown).

## Development Commands

### Testing

- `devtools::check()` - Run full R CMD check (standard R package check)
- `devtools::test()` - Run unit tests using testthat
- `devtools::test(filter = "then")` - Run tests matching pattern (e.g.,
  all tests in test-then.R)
- `testthat::test_file("tests/testthat/test-*.R")` - Run a single test
  file
- `spelling::spell_check_test(vignettes = TRUE, error = TRUE, skip_on_cran = TRUE)` -
  Check spelling

To get a promise’s value in a test, use the package’s `wait_for(p)`; it
blocks until `p` settles and returns the value or throws the rejection.

**Test Helpers** (`tests/testthat/helper.R`): - `wait_for_all(p)` -
Block until all pending later tasks execute; optionally wait for promise
`p` to resolve/reject - `ext_promise()` - Create externally-controllable
promise for testing (returns list with `promise`, `resolve`, `reject`,
`status`) - `expect_assertions(code, n_expected)` - Run `code` and
assert it produces exactly `n_expected` testthat expectations (useful
with async tests) - `resolve_later(value, delaySecs)` - Create a promise
that resolves with `value` after a delay -
`squelch_unhandled_promise_error(promise)` - Suppress “Unhandled promise
error” warnings (but rethrow test failures) -
`create_counting_domain(trackFinally)` - Promise domain that counts
callback bind/call/active events for domain tests

### Package Development

- `devtools::load_all()` - Load package for development
- `devtools::document()` - Generate documentation from roxygen2 comments
- `devtools::build()` - Build source package
- `devtools::install()` - Install package locally
- [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html) -
  Build package documentation website

### GitHub Actions

The package uses RStudio’s shiny-workflows for CI/CD: - Automated R CMD
check on push/PR - Website deployment via pkgdown - Code formatting
checks

## Memory Bank

The `memory-bank/` directory holds contributor documentation with YAML
frontmatter (`title`, `description`). **Before exploring the codebase**,
invoke the `memory-bank` skill to find and read the relevant docs. Start
with `memory-bank/architecture.md`; it explains how the pieces fit and
which doc covers each `R/` file. Update or add docs when you learn
something future contributors will need (see the skill for conventions).

Contributor skills live in `.agents/skills/`; `.claude/skills` is a
symlink to it. Only allowlisted skills are tracked, so when adding or
removing one, update the `!/.agents/skills/<name>/` lines in
`.gitignore` to match.
