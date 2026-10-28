---
title: "Combining Promises"
description: "How promise_all(), promise_race(), promise_map(), and promise_reduce() work: result order, names, errors, and empty input. Read this before changing R/utils.R or when a combined promise does something surprising."
---

# Combining Promises

## Overview

`R/utils.R` has four exported helpers that work on many promises or values at
once:

| Function | Runs | Resolves to | Rejects when |
|---|---|---|---|
| `promise_all()` | all inputs at once (they are already started) | list of values, in input order | the first input to reject |
| `promise_race()` | all inputs at once | the first input to settle | the first input to settle is a rejection |
| `promise_map()` | one element at a time | list of `.f` results, in input order | `.f` throws or returns a rejected promise |
| `promise_reduce()` | one element at a time | the last `.f` result | `.f` throws or returns a rejected promise |

All four return a promise. All are built only from `promise()`,
`promise_resolve()`, and `then()` — except that `promise_all()` and
`promise_race()` attach to their inputs with the internal
`promise_subscribe()` (no derived promise per input; see
`promiseCore.md`#subscribe). The core rules in `promiseCore.md`
apply: callbacks run later, and the first `resolve`/`reject` wins.

## Key files

- `R/utils.R` - `promise_all()`, `promise_race()`, `promise_map()`,
  `promise_reduce()`. Also `is_false()` (used by `R/future_promise.R`) and an
  unused function that mentions `purrr::reduce` so R CMD check accepts the
  `purrr` link in the docs.
- Tests: `tests/testthat/test-combining.R` (`promise_all` order, `NULL`
  values, duplicate names), `test-promise-map.R` and
  `test-legacy-promise-map.R` (`NULL` results), `test-aplus-2-2.R` (uses
  `promise_all(.list = ...)` to wait on many promises), `test-methods.R`
  (`promise_reduce()` with a function name or no `.init`, empty
  `promise_race()`).

## How it works

### promise_all(..., .list = NULL)

1. If `.list` is missing, it uses `list(...)`. If `.list` is given, `...` is
   ignored.
2. Empty input returns `promise_resolve(list())`.
3. Names must be all set or all empty. A mix throws right away (a real error,
   not a rejected promise): "promise_all expects promise arguments (or list) to
   be either all named or all unnamed".
4. Inside `promise()`, it coerces each input with `as.promise()` and
   subscribes to it, in order (the callbacks are pure bookkeeping, so they
   skip domain wrapping):
   - On success, it stores the value with `results[idx] <<- list(value)` (this
     keeps `NULL` values) and counts down. When the count reaches zero it
     resolves with `results`.
   - On failure, it rejects with that reason.
5. `results` starts as a list of `NA`, named like the input when the input is
   named, so the output keeps input order and names, not finish order.
   Duplicate names are kept.

Once one input rejects, later results are ignored because `resolve`/`reject`
only work once. Other inputs keep running; there is no cancel (see the `TODO`
in the code). Since every input has an `onRejected` handler attached, later
rejections do not print "Unhandled promise error".

### promise_race(..., .list = NULL)

Uses `.list` or `list(...)` the same way, then inside `promise()` subscribes
to every input with `function(value, visible) resolve(if (visible) value
else invisible(value))` and `reject` (equivalent to `then(p, resolve,
reject)`, without the derived promise). The first input to settle wins. When
several inputs are already settled, their handlers are queued in input
order, so the first one in the list wins.

### promise_map(.x, .f, ...)

1. `.f <- match.fun(.f)`, so a function name as a string works.
2. If `.x` is not a plain vector, or has a class, it becomes `as.list(.x)`
   (for example a factor becomes a list of one-level factors).
3. `do_next(pos)` calls `.f(.x[[pos]], ...)`, wraps the result with
   `promise_resolve()`, and chains `then()` that saves the value with
   `results[pos] <<- list(value)` and calls `do_next(pos + 1)`. When `pos`
   passes the end it returns `stats::setNames(results, names(.x))`.
4. The outer promise is `promise(function(resolve, reject) resolve(do_next(1)))`.
   Each step returns a promise, so the outer promise follows the whole chain.

Work is serial: `.f` for element 2 is not called until the result for element
1 has resolved. Results keep input order and names. Empty input resolves to
`list()`.

### promise_reduce(.x, .f, ..., .init)

1. `.f <- match.fun(.f)`, so a function name such as `"+"` works.
2. If `.init` is missing, it uses `.x[[1]]` as `.init` and drops it from `.x`,
   like `Reduce()` and `purrr::reduce()`. If `.x` is also empty, it throws
   "`.init` must be supplied when `.x` is empty" right away (a real error, not
   a rejected promise).
3. `p <- promise_resolve(.init)`.
4. For each item, in order, `p <- then(p, function(x) .f(x, item, ...))`.
5. Returns `p`.

The whole chain is built at once, at call time, but each `.f` runs only after
the previous step resolves. If `.f` returns a promise, the next step waits for
it. Empty `.x` with `.init` resolves to `.init`. If any step fails, the later
steps have no `onRejected`, so the error passes straight to the returned
promise.

## Pitfalls

- **Inputs must be promises.** `promise_all()` and `promise_race()` call
  `as.promise()` on each element. A plain value like `1`
  makes the result reject with "Don't know how to convert object of class
  numeric into a promise". Wrap plain values in `promise_resolve()`.
- **Mixed names throw instead of rejecting.** The name check in
  `promise_all()` runs before `promise()`, so it is a normal R error at call
  time. `promise_race()` has no name check and ignores names.
- **Named empty input loses its names.** `promise_all(.list = setNames(list(),
  character()))` resolves to plain `list()`.
- **`promise_race()` with no inputs never settles.** There is no special case
  for empty input, so the returned promise stays pending forever. This matches
  JavaScript and is now in the docs; pinned by "promise_race() with no
  promises stays pending" in `tests/testthat/test-methods.R`.
- **`promise_map()` calls `.f` for the first element right away.** `do_next(1)`
  runs inside the `promise()` action, which runs at once. Only the later
  elements wait for the event loop. An error from that first call becomes a
  rejection, not a thrown error.
- **`promise_reduce()` with empty `.x` and no `.init` throws.** The check runs
  before any promise is made, so it is a normal R error at call time.
- **`promise_map()` and `promise_reduce()` take values, not promises, as
  `.x`.** A promise passed as `.x` is a classed list, so `promise_map()` turns
  it into a list of its three functions. If `.x` holds promises, `.f` gets the
  promise objects, not their values.
- **No limit on parallel work.** `promise_all()` only waits; whatever started
  the input promises decides how much runs at once. Use `promise_map()` for
  one-at-a-time work.
