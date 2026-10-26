---
title: "future_promise() and the WorkQueue"
description: "How future_promise() holds future work in a queue until a future worker is free, how the WorkQueue backs off, and how Future objects become promises. Read before changing R/future_promise.R, as.promise.Future(), or anything that touches future or mirai."
---

# future_promise() and the WorkQueue

## Overview

`future::future()` blocks the main R session when every future worker is busy.
`future_promise()` avoids this. It returns a promise right away and puts the
work in a queue. The queue only starts the future when a worker is free.

Three pieces work together:

- `future_promise()` makes a lazy future and a promise, and hands a "start
  this future" function to a queue.
- `WorkQueue` (an R6 class) runs queued functions while `can_proceed()` says
  yes, and retries later with a random, growing delay when it says no.
- `as.promise.Future()` (in `R/promise.R`) turns a `Future` into a promise by
  polling `future::resolved()` every 0.1 seconds.

`future` is only in `Suggests`. `fastmap` is in `Imports` (see the 1.2.1 entry
in `NEWS.md`), but the code still checks both versions at run time.

## Key files

- `R/future_promise.R`
  - `future_promise(expr, envir, ..., substitute, queue)` - exported.
  - `future_promise_queue()` - exported; returns one `WorkQueue` cached for the
    whole R session.
  - `WorkQueue` - R6 class, documented but `@keywords internal`. Public:
    `initialize(can_proceed, queue, loop)`, `schedule_work(fn)`. Private:
    `can_proceed()`, `attempt_work(can_delay)`, `do_work()`, `reset_delay()`,
    `increase_delay()`.
  - `Delay` (base R6 class) and `ExpoDelay` (exponential backoff with random
    jitter). Not exported.
  - `future_worker_is_free()` - `future::nbrOfFreeWorkers() > 0`.
  - `assert_work_queue_pkgs()` - checks `future >= 1.21.0` and
    `fastmap >= 1.1.0`; remembers success so it only checks once.
  - `debug_msg()` / `debug_msg_can_print` - set the flag to `TRUE` by hand to
    print queue tracing messages.
- `R/promise.R`
  - `is.promising.Future()` returns `TRUE`.
  - `as.promise.Future()` - polling conversion, cached on the future.
- `R/promises-package.R` - `ignore_unused_imports()` keeps `fastmap` "used";
  `release_bullets()` lists two future-related items to undo before a release.
- `tests/testthat/test-zzz-future_promise.R` - timing tests with two
  `multisession` workers.
- `tests/testthat/test-methods.R` - `describe("future", ...)` checks that a
  `Future` passed to `promise_resolve()` / `promise_reject()` is followed.
- `vignettes/promises_05b_future_promise.Rmd` - user-facing story (plumber
  example). `vignettes/promises_05a_futures.Rmd` covers plain `future`.
- `vignettes/promises_04_mirai.Rmd` - the mirai guide.

## How it works

### future_promise()

1. Checks that `queue$schedule_work` is a function with at least one argument.
2. If `substitute = TRUE`, captures `expr` with `substitute()`.
3. Forces `envir`, `substitute`, and `...` so later changes to those
   arguments do not leak in. It does not freeze values inside `envir`.
4. Calls `future::future(expr, envir = envir, substitute = FALSE, ..., lazy = TRUE)`.
   The future object is made now (so globals are found now), but it does not
   start running.
5. Returns `promise(function(resolve, reject) { queue$schedule_work(work) })`.
   The `work` function calls `resolve(future_job)` and then returns
   `future_job`.

When the queue runs `work`:

- `resolve(future_job)` sees a promising value, so `doResolve()` calls
  `as.promise(future_job)`. That calls `future::resolved(x, timeout = 0)`,
  which launches the lazy future. Under `plan(sequential)` the whole job runs
  right here, inside the queue call.
- The returned `future_job` goes back to `WorkQueue$do_work()`, which calls
  `then(future_job, onFulfilled = continue_work, onRejected = continue_work)`.
  `then()` calls `as.promise()` again, which returns the cached promise. So
  there is only one polling loop per future.

### WorkQueue

`schedule_work(fn)` adds `fn` to the queue (default `fastmap::fastqueue()`,
first in, first out) and calls `attempt_work()`.

`attempt_work(can_delay = FALSE)`:

1. Returns if the queue is empty.
2. While the queue has items and `can_proceed()` is `TRUE`: reset the delay
   (which also cancels any pending delayed retry) and call `do_work()`.
3. If items remain and we cannot proceed: schedule one delayed retry with
   `later(delay = private$delay$delay(), loop = private$loop)`. Only one
   delayed retry is kept at a time. The handle in
   `cancel_delayed_attempt_work` works as a lock. The delayed retry itself
   calls `attempt_work(can_delay = TRUE)`, which is allowed to schedule the
   next retry.

`do_work()` removes one function, runs it, and when the returned promise
settles (either way) calls `attempt_work()` again. It uses `then()` with both
callbacks instead of `finally()`, so a rejected job does not create a second
"Unhandled promise error" message
([rstudio/promises#86](https://github.com/rstudio/promises/issues/86)).

### can_proceed() cache

Asking future how many workers are free is slow. In `can_proceed()`, once the
answer is `FALSE`, it stores `FALSE` in `can_proceed_cache_val` and schedules
`later(delay = 0)` on the queue's loop to clear it. So inside one run of the
event loop, many `future_promise()` calls ask future only once
([rstudio/promises#78](https://github.com/rstudio/promises/pull/78)). A `TRUE`
answer is never cached.

Checked by running: five `schedule_work()` calls in one tick with a
`can_proceed` that returns `FALSE` called `can_proceed` once.

### ExpoDelay

`ExpoDelay$delay()` returns seconds (not milliseconds, despite the code
comment):

```
expo_val   <- base * (2^count - 1)              # base = 0.01
random_val <- runif(1, max = min(max_seconds, expo_val))
max(min_seconds, random_val)                    # min 0.01, max 2
```

`increase()` runs before `delay()`, so the first retry waits about 0.01 s.
The upper bound doubles each time and reaches the 2 second cap at about the
8th retry. Any successful start calls `reset_delay()` and the count goes back
to 0.

### as.promise.Future()

- Looks for a cached promise in `attr(x, "converted_promise")`. A `Future` is
  an environment, so setting the attribute changes the shared object and the
  cache works for every reference (checked by running: two `as.promise()`
  calls return the identical promise).
- Otherwise makes a promise whose `check()` calls
  `future::resolved(x, timeout = 0)`. `timeout = 0` matters; the default waits
  200 ms. If not resolved, `later(check, 0.1)`.
- When resolved, `future::value(x, signal = TRUE)` is resolved or rejected.
  `FutureError` from either step rejects the promise.
- Comments explain why polling is still needed: future does not push results
  back to the main session.

### mirai

promises has no mirai code. The `R/promise.R` docs for `as.promise` say mirai
objects get their `as.promise` method from the mirai package. The mirai
vignette (`promises_04_mirai.Rmd`) says mirai promises are event driven,
while future promises poll every 100 ms. There is no `mirai_promise()` or
queue here; `WorkQueue` and `future_promise()` are only for future.

## Pitfalls

- **The roxygen docs for `WorkQueue` say the backoff caps at 10 seconds.** The
  code in `ExpoDelay` caps at `max_seconds = 2`. The comment on
  `ExpoDelay$delay()` says "milliseconds"; it is seconds. Trust the code.
- **The work function must return something promising.** `do_work()` calls
  `then()` on whatever the work function returns. A custom queue or custom
  `schedule_work()` caller that returns a plain value gets "Don't know how to
  convert object of class numeric into a promise" (checked by running).
- **`WorkQueue`'s `loop` is only used for the retry timer and the cache
  reset.** `then()` in `do_work()` and the polling in `as.promise.Future()`
  use whatever loop is current. The roxygen section "Global event loop" says
  private loops are not really supported.
- **Globals are captured when `future_promise()` is called, not when the
  work starts.** That is the point of `lazy = TRUE` (see 1.3.3 in
  `NEWS.md`, [rstudio/promises#123](https://github.com/rstudio/promises/pull/123)).
  But values inside `envir` can still change before launch if they are
  environments or are captured by reference.
- **`future_promise_queue()` is created once per session** and uses the
  `can_proceed` default `future_worker_is_free()`, which reads the *current*
  `future::plan()` each time. Changing the plan is fine; replacing the queue
  is not possible without a custom `queue =` argument.
- **Under `plan(sequential)`, `nbrOfFreeWorkers()` is 1**, so work starts on
  the next `attempt_work()` and runs synchronously inside it. The promise
  still settles on a later tick, so callers must run the event loop.
- **`as.promise.Future()` uses the package's own `tryCatch()`**, defined in
  `R/domains.R`. It wraps `base::tryCatch()` with `withCallingHandlers()` that
  calls the current promise domain's `onError` first. Keep that in mind when
  reading error paths here.
- **The `future_promise()` example is `\dontrun{}`** on purpose for now.
  `release_bullets()` in `R/promises-package.R` says to switch it back to
  `\donttest{}` and to drop the macOS skip in the test file before a release.
- **Masking during development:** `resolve()` is internal (not exported), but
  `devtools::load_all()` exposes it. Calling `library(future)` afterwards
  prints "The following object is masked from 'package:promises': resolve".
  Package code and tests are not affected; ad hoc scripts may be.
