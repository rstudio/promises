# Future promise work queue

Future promise work queue

Future promise work queue

## Details

An R6 class to help with scheduling work to be completed. `WorkQueue`
will only execute work if the `can_proceed()` returns `TRUE`. For the
use case of `future`, `can_proceed()` defaults to
`future::nbrOfFreeWorkers() > 0` which will not allow for work to be
executed if a future worker is not available.

`WorkQueue` will constantly try to start new work once prior work item
finishes. However, if `can_proceed()` returns `FALSE` (no future workers
are available) and there is more work to be done, then work is attempted
later a random amount of time later using exponential backoff. The
exponential backoff will cap out at 10 seconds to prevent unnecessarily
large wait times.

Each time `WorkQueue` tries to start more work, it will repeat until
`can_proceed()` returns `FALSE` or there is no more work in the `queue`.

## Global event loop

The global loop is used by default as the internal `WorkQueue` "delayed
check" uses a single delay check for the whole queue, rather than having
each item in the queue attempt to process. This behavior might change in
the future, but we are not exactly sure how at this point.

If a private `later` loop wants to become synchronous by running until
all jobs are completed but is waiting on a
[`future_promise()`](https://rstudio.github.io/promises/dev/reference/future_promise.md),
the private loop will not complete unless the global loop is allowed to
move forward.

However, it is possible to use a private loop inside a user-defined
`WorkQueue` may work which can be provided directly to
`future_promise(queue=custom_queue)`. Having a concrete example (or
need) will help us understand the problem better. If you have an
example, please reach out .

## See also

[`future_promise_queue()`](https://rstudio.github.io/promises/dev/reference/future_promise.md)
which returns a `WorkQueue` which is cached per R session.

## Methods

### Public methods

- [`WorkQueue$new()`](#method-WorkQueue-new)

- [`WorkQueue$schedule_work()`](#method-WorkQueue-schedule_work)

- [`WorkQueue$clone()`](#method-WorkQueue-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `WorkQueue`

#### Usage

    WorkQueue$new(
      can_proceed = future_worker_is_free,
      queue = fastmap::fastqueue(),
      loop = later::global_loop()
    )

#### Arguments

- `can_proceed`:

  Function that should return a logical value. If `TRUE` is returned,
  then the next scheduled work will be executed. By default, this
  function checks if
  [`future::nbrOfFreeWorkers()`](https://future.futureverse.org/reference/nbrOfWorkers.html)` > 0`

- `queue`:

  Queue object to use to store the scheduled work. By default, this is a
  "First In, First Out" queue using
  [`fastmap::fastqueue()`](https://r-lib.github.io/fastmap/reference/fastqueue.html).
  If using your own queue, it should have the methods `$add(x)`,
  `$remove()`, `$size()`.

- `loop`:

  later loop to use for calculating the next delayed check. Defaults to
  [`later::global_loop()`](https://later.r-lib.org/reference/create_loop.html).
  Schedule work

------------------------------------------------------------------------

### Method `schedule_work()`

#### Usage

    WorkQueue$schedule_work(fn)

#### Arguments

- `fn`:

  function to execute when `can_proceed()` returns `TRUE`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    WorkQueue$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
