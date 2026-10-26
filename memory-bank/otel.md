---
title: "OpenTelemetry integration"
description: "How with_otel_span(), with_otel_promise_domain(), and local_otel_promise_domain() keep the active OpenTelemetry span alive across promise callbacks, and how the tests record spans with otelsdk. Read before changing R/otel.R, R/deprecated.R, or tests/testthat/test-otel.R."
---

# OpenTelemetry integration

## Overview

promises imports the `otel` package (`>= 0.2.0`). The goal is simple: when a
`then()` callback runs on a later tick, the OpenTelemetry span that was active
when `then()` was *called* should be active again. Without help, the callback
runs with no active span, and child spans lose their parent.

The fix is a promise domain (see `R/domains.R`) that, at `then()` time,
captures the active span and, at callback time, re-activates it. promises calls
this a *handoff* domain: it only restores the single span that was active,
not a stack of spans. That keeps restore cost O(1) per callback, even when
many `with_otel_span()` calls are nested (the roxygen "Complexity" section in
`R/otel.R` explains the O(n) vs O(n^2) reasoning).

Users of the feature are mostly other packages (shiny, chromote, ellmer). They
pass their own tracer.

## Key files

- `R/otel.R`
  - `with_otel_span(name, expr, ..., tracer, attributes = NULL)` - exported.
  - `with_otel_promise_domain(expr)` - exported.
  - `local_otel_promise_domain(envir = parent.frame())` - exported.
  - `local_promise_domain(domain, envir, replace)` - internal; like
    `with_promise_domain()` but undone with `defer()` when `envir` exits. It
    does not call `wrapSync`.
  - `has_otel_promise_domain(domain = current_promise_domain())` - checks the
    `.otel_promise_domain` flag.
  - `create_otel_promise_domain()` - builds the handoff domain.
  - `promises_otel_tracer()` - `otel::get_tracer()`, cached once it is
    enabled; used by tests.
  - `otel_tracer_name` - `"co.posit.r-package.promises"`. `otel` looks up this
    variable name to pick the default tracer name for the package.
  - `testthat__is_testing()` - `Sys.getenv("TESTTHAT") == "true"`.
- `R/domains.R` - `compose_domains()` carries the `.otel_promise_domain` flag
  through composition. It is the only extra field it carries.
- `R/deprecated.R` - `with_ospan_async()`, `with_ospan_promise_domain()`,
  `local_ospan_promise_domain()`. Each calls `lifecycle::deprecate_warn("1.5.0", ...)`
  and forwards to the new name.
- `R/import-standalone-defer.R` - `defer()` used by `local_promise_domain()`.
- `tests/testthat/test-otel.R` and `tests/testthat/_snaps/otel.md`.
- `pkgdown/_pkgdown.yml` - reference section "OpenTelemetry" lists only
  `with_otel_span` (the `otel` Rd page holds all three functions via
  `@describeIn otel`).

## How it works

### create_otel_promise_domain()

Returns `new_promise_domain(.otel_promise_domain = TRUE, wrapOnFulfilled = ..., wrapOnRejected = ...)`.

Each wrapper runs when `then()` is called (inside `promiseDomain$onThen()`):

1. `span <- get_active_span()`.
2. If `!.subset2(span, "is_recording")()`, return the callback unchanged
   (cheap path when nothing is recording).
3. Otherwise return `function(...) with_active_span(span, callback(...))`.

It calls `otel::get_active_span()` instead of a tracer method on purpose. It
is slower, but it follows changes to the tracer provider at run time (see the
comments in the function). There is no `wrapOnFinally`, so `finally()`
callbacks are wrapped by both wrappers, as with any domain. If it is nested
with a domain that has `wrapOnFinally`, `compose_domains()` wraps the finally
callback with this domain's `wrapOnFulfilled` instead (see `domains.md`).

### with_otel_promise_domain() / local_otel_promise_domain()

Both are idempotent. If `has_otel_promise_domain()` is already `TRUE`, they
just evaluate `expr` (or return). Otherwise they add a new handoff domain
with `with_promise_domain()` / `local_promise_domain()`. Only one handoff
domain is ever in the stack; the tests use `with_mocked_bindings()` on
`create_otel_promise_domain` to count this.

`local_otel_promise_domain()` exists for `coro`, where you cannot wrap code in
a `with_*()` call.

Shiny adds the domain itself. Packages that use promises outside Shiny must
add it (the roxygen text names chromote as an example).

### with_otel_span()

1. `if (!.subset2(tracer, "is_enabled")()) return(force(expr))`. This is the
   fast path; the code comment has the benchmark for why it uses `.subset2()`
   instead of `otel::is_tracing_enabled()`.
2. `span <- start_span(name, ..., tracer = tracer, attributes = attributes)`.
3. Registers `on.exit(if (needs_cleanup) end_span(span))`.
4. Defines `set_error(e)`, which calls `span$set_status("error",
   description)`. `description` is `conditionMessage(e)` for a condition and
   `NULL` for any other value (a promise may reject with, say, `42`).
5. `result <- withCallingHandlers(with_active_span(span, expr), error =
   set_error)`. Any `then()` called inside `expr` (with the handoff domain
   present) captures `span`. A sync error marks the span as an error and keeps
   going up.
6. Sync result: `on.exit` ends the span when the function returns or errors.
7. Promise result (`is.promising(result)`): sets `needs_cleanup <- FALSE` and
   returns `result |> catch(set_error, tee = TRUE) |> finally(cleanup)`. A
   rejection marks the span as an error, and the span ends when the promise
   settles.

### Timeline example

From the roxygen docs in `R/otel.R`, in short:

```r
with_otel_promise_domain({
  p <- with_otel_span("async_op", tracer = tr, {
    start() |> then(step1)   # then() captures "async_op"
  })
  p |> then(step2)           # no active span here, nothing captured
})
```

`step1` runs with `async_op` active. `async_op` ends after `step1`'s promise
settles. `step2` runs with no active span.

### promises_otel_tracer()

Caches the first *enabled* `otel::get_tracer()` result in a closure. A no-op
tracer (`is_enabled()` is `FALSE`) is returned but not cached, so a tracer
provider set up later is still picked up. When `TESTTHAT=true` it never
caches, because `otelsdk::with_otel_record()` swaps the tracer provider for
each recording. No package code calls it; only
tests do. `with_otel_span()` has no default tracer, so callers must pass one.

## Tests

`tests/testthat/test-otel.R`:

- `skip_on_cran()` and `skip_if_not_installed("otelsdk")` at the top.
- Most of the file is wrapped in `with_otel_promise_domain({ describe(...) })`
  so the handoff domain is present for those tests. The later `describe()`
  blocks for `local_otel_promise_domain()` and `has_otel_promise_domain()`
  are outside it and check `current_promise_domain()` is `NULL`.
- Spans are collected with `records <- otelsdk::with_otel_record({ ... })`,
  then checked by name: `records$traces[["name"]]`, `$span_id`, `$parent`.
  Promises must be settled inside the `with_otel_record()` block (call
  `wait_for_it(p)` or `extract(p)` there), or the spans are not recorded.
- Parent checks such as
  `expect_equal(records$traces[["child"]]$parent, records$traces[["outer"]]$span_id)`
  are the main way to prove the handoff works.
- Error status is checked with `expect_identical(span$status, "error")` for a
  sync error, a rejected promise, and a non-condition rejection.
- Deprecated names are covered by `expect_snapshot()`; the warning text lives
  in `tests/testthat/_snaps/otel.md`.
- One test skips if `OTEL_TRACES_EXPORTER` is set to something other than
  `""` or `"none"`, because then `otel::start_span()` is not a no-op.

Run: `devtools::test(filter = "otel")`.

## Pitfalls

- **No handoff domain, no parent.** `with_otel_span()` alone does not carry
  the span into `then()` callbacks. Checked by running: without
  `with_otel_promise_domain()`, a child span started in a `then()` callback
  had parent `0000000000000000`; with it, the parent was the outer span.
- **Capture happens at `then()` call time**, not at resolve time. A `then()`
  added after `with_otel_span()` returns sees no active span, even though the
  span is still open (see `step2` above).
- **`with_otel_span()` returns a new promise** (from `catch()` and
  `finally()`), not the one `expr` returned. Do not compare by identity.
- **Non-condition rejections pass through unchanged.** If `expr` rejects
  with `42`, the span status is `"error"` with no description, and the
  returned promise still rejects with `42`.
- **`tracer` is required and has no default.** A missing tracer fails with
  "argument "tracer" is missing, with no default". A `NULL` tracer fails too,
  since `.subset2(NULL, "is_enabled")` is `NULL`.
- **`promises_otel_tracer()` caches an enabled tracer outside tests.** Once
  an enabled tracer is cached, a later tracer provider swap (as
  `otelsdk::with_otel_record()` does) is not seen. This is why tests set no
  cache when `TESTTHAT=true`. Do not "simplify" that branch away. Pinned by
  "does not cache a no-op tracer" in `tests/testthat/test-otel.R`.
- **`compose_domains()` only copies `.otel_promise_domain`.** Any other
  `...` field given to `new_promise_domain()` is lost when domains are
  composed. If you add a new flag to the otel domain, update
  `compose_domains()` too.
- **`local_promise_domain()` skips `wrapSync`.** That is fine for the otel
  domain (its `wrapSync` is `force`) but not for a general domain.
- **Deprecation version is "1.5.0"** in `R/deprecated.R`. If you change the
  warning text, re-run the snapshot tests and review `_snaps/otel.md`.
