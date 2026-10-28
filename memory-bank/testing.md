---
title: "Testing promises"
description: "How the testthat suite is laid out, what the helpers in helper.R do, how tests drive the later event loop, and which tests skip where. Read before writing or debugging any test in tests/testthat/."
---

# Testing promises

## Overview

Tests use testthat 3rd edition (`Config/testthat/edition: 3` in
`DESCRIPTION`). Promise callbacks never run right away; they are scheduled
with `later::later()`. So almost every test has to *run the event loop*
before it can check a result. The helpers in `tests/testthat/helper.R` do
that.

Many tests use `describe()` / `it()` blocks, often named after the
Promises/A+ spec sections.

## Key files

- `tests/testthat.R` - standard `test_check("promises")`.
- `tests/spelling.R` - runs `spelling::spell_check_test(vignettes = TRUE, error = TRUE, skip_on_cran = TRUE)`
  during `R CMD check`.
- `tests/testthat/helper.R` - shared helpers (below). Calls `library(later)`.
- `tests/testthat/test-aplus-2-1.R`, `test-aplus-2-2.R`, `test-aplus-2-3.R` -
  Promises/A+ spec sections 2.1 (states), 2.2 (`then`), 2.3 (resolution).
- `tests/testthat/test-legacy-*.R` - older copies that use the magrittr-style
  promise pipes (`%...>%`, `%...!%`) and formula callbacks (`~ { ... }`).
- `tests/testthat/test-zzz-future_promise.R` - slow timing tests for
  `future_promise()`.
- `tests/testthat/test-otel.R` - OpenTelemetry; see `otel.md`.
- `tests/testthat/_snaps/` - `hybrid-then.md`, `otel.md`, `then.md`, `wait-for.md`.
- Other files: `test-combining.R`, `test-domains.R`, `test-hybrid-then.R`,
  `test-expo-delay.R` (the 2 second `ExpoDelay` cap),
  `test-hybrid-then-visibility.R`, `test-methods.R`, `test-ordering.R`,
  `test-promise-map.R`, `test-then.R`, `test-visibility.R`, `test-wait-for.R`.

## Helpers in helper.R

| Helper | What it does |
|---|---|
| `on_ci` | `TRUE` when env var `CI` is true. Used for longer timeouts. |
| `ext_promise()` | Makes a pending promise and returns `list(promise, resolve, reject, status)`, so the test can settle it from outside. `status` is the internal `status()` function, read from `attr(p, "promise_impl")`. |
| `wait_for_it(p = NULL, timeout = if (on_ci) 60 else 30)` | Runs `later::run_now(0.1)` until `later::loop_empty()` is `TRUE`. Stops with "Waited too long" after `timeout` seconds. If `p` is given and rejects, re-throws the reason after the loop is empty, inside `withRestarts(..., continue_test = )`. |
| `resolve_later(value, delaySecs)` | Promise that resolves to `value` after `delaySecs` using `later::later()`. |
| `squelch_unhandled_promise_error(promise)` | Adds a `catch()` so a rejection does not print "Unhandled promise error". Re-throws testthat `failure` conditions so failed expectations still fail. |
| `create_counting_domain(trackFinally = FALSE)` | A promise domain that counts, in `pd$counts`, how many callbacks were bound (`*Bound`), called (`*Called`), and are running now (`*Active`). With `trackFinally = TRUE` it also sets `wrapOnFinally` and `onFinally*` counters; `onFinallyActive` goes up while the callback runs and back down after. |
| `expect_assertions(code, n_expected)` | Counts `expectation_success` and `expectation_failure` signals while running `code`, then checks the count. Proves that expectations inside callbacks actually ran. |
| `.GlobalEnv$.Last` | Set to call `wait_for_it()` when R exits, so stray unhandled rejections still print. |

## Driving the event loop

- To get a promise's value, use the package's own `wait_for(p)`. It stops as
  soon as `p` settles, so other callbacks may still be queued. Use
  `wait_for_it()` when the test needs the whole loop drained.
- `wait_for_it()` only runs the **global** later loop. `wait_for()` runs
  `later::current_loop()` unless given `loop =`.
- A single `later::run_now()` runs everything that is ready, including
  callbacks scheduled by those callbacks with no delay. `test-ordering.R`
  depends on this: two chains of two `then()` calls each finish in one
  `run_now()`, in interleaved order. Its comment says this is current
  behavior, not a promise
  ([rstudio/promises#151](https://github.com/rstudio/promises/issues/151)).
- Expectations inside callbacks run on a later tick. If the loop is never
  run, the expectation never runs and the test passes silently. Use
  `wait_for()`, `wait_for_it(p)`, or `expect_assertions()` to make sure they
  run.
- Unhandled rejections print "Unhandled promise error: ..." to `stderr` two
  ticks after rejection (see `doRejectFinalReason()` in `R/promise.R`). Tests
  for this use `capture.output(type = "message", ...)`
  (`test-methods.R`, `test-wait-for.R`, `test-zzz-future_promise.R`).

## Promises/A+ and legacy tests

- `test-aplus-*` follow the Promises/A+ spec text, with the spec quoted in
  comments. A few `it()` blocks are empty on purpose because R behaves
  differently (for example, a non-function `onFulfilled` is an error, not
  ignored). testthat reports these as "empty test" skips: five in
  `test-aplus-2-2.R` and five in `test-legacy-aplus-2-2.R`.
- `test-legacy-*` were kept when the package moved to `|>` and `\(x)`
  (commit "Promote usage of `|>` ..." [rstudio/promises#149](https://github.com/rstudio/promises/pull/149)).
  They still test the superseded pipes and formula callbacks. Do not
  "modernize" them; that would remove the coverage.

## future_promise tests (test-zzz-future_promise.R)

- testthat runs files in alphabetical order, so `zzz` makes this file run
  last. The file has been named this way since `future_promise()` was added
  ([rstudio/promises#62](https://github.com/rstudio/promises/pull/62)). The
  reason is not written down, but the file is the slowest (several seconds of
  `Sys.sleep()` per test), starts `multisession` worker processes, and changes
  `future::plan()` (restored with `on.exit()`).
- Skips: `skip_on_cran()`, `skip_on_os("mac")` (a GitHub Actions problem with
  parallelly, see the comment link), `skip_on_os("windows")` (timing),
  `skip_if_not_installed("future", "1.21.0")`, and `fastmap`.
- `worker_job_time` is 4 seconds on CI and 1 second locally.
- The checks are timing based: total time, lag between finishes, and whether
  a `run_every()` ticker kept running every 0.1 s (main session not
  blocked).

## Skip conditions

- `test-otel.R`: `skip_on_cran()`, `skip_if_not_installed("otelsdk")`, and one
  `skip_if()` on `OTEL_TRACES_EXPORTER`.
- `test-zzz-future_promise.R`: listed above. On macOS the whole file skips.
- `test-methods.R`: `describe("future", ...)` uses
  `skip_if_not_installed("future")`. `future` is only in `Suggests`, so a
  plain dev setup may skip these.

## Snapshots

`expect_snapshot()` is used for error and deprecation messages:
`tee` validation (`then.md`), `hybrid_then()` argument checks
(`hybrid-then.md`), and deprecated `ospan` functions (`otel.md`). Snapshot
section names come from the `describe()` / `it()` path, e.g.
`# OpenTelemetry integration / Deprecated functions / with_ospan_async() is deprecated`.
Update with `testthat::snapshot_accept()` after checking the diff.

## Commands

```r
devtools::test()                                   # all tests
devtools::test(filter = "then")                    # files matching "then"
devtools::test(filter = "aplus")                   # all A+ files
testthat::test_file("tests/testthat/test-otel.R")  # one file
devtools::test(reporter = "summary")               # shows skip reasons
devtools::check()                                  # full R CMD check
```

`filter` is a regular expression on the file name after `test-`, so
`filter = "then"` also runs `test-hybrid-then*.R`.

## Pitfalls

- **`expect_assertions()` takes `(code, n_expected)`.** Tests call it as
  `expect_assertions(n = 2, { ... })`, which works only through partial
  argument matching of `n` to `n_expected`.
- **`ext_promise()$status` depends on an internal detail**: the
  `"promise_impl"` attribute on the public promise object
  (`promise()` in `R/promise.R`). If that changes, this helper breaks.
- **`wait_for_it(p)` re-throws only after the loop is empty**, and only for
  `p`. Other rejected promises just print "Unhandled promise error" and do not
  fail the test.
- **`squelch_unhandled_promise_error()` must not hide test failures.** It
  re-throws conditions of class `failure`; keep that if you edit it.
- **Spans and `with_otel_record()`**: settle promises inside the record block,
  or the spans are missing (see `otel.md`).
- **Running only `test-zzz-future_promise.R` on macOS does nothing**: the
  whole file is skipped there.
