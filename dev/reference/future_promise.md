# future promise

When submitting future work, future (by design) will block the main R
session until a worker becomes available. This occurs when there is more
submitted future work than there are available future workers. To
counter this situation, we can create a promise to execute work using
future (using `future_promise()`) and only begin the work if a future
worker is available.

## Usage

``` r
future_promise_queue()

future_promise(
  expr = NULL,
  envir = parent.frame(),
  ...,
  substitute = TRUE,
  queue = future_promise_queue()
)
```

## Arguments

- expr:

  An R expression. While the `expr` is eventually sent to
  [`future::future()`](https://future.futureverse.org/reference/future.html),
  please use the same precautions that you would use with regular
  [`promises::promise()`](https://rstudio.github.io/promises/dev/reference/promise.md)
  expressions. `future_promise()` may have to hold the `expr` in a
  [`promise()`](https://rstudio.github.io/promises/dev/reference/promise.md)
  while waiting for a future worker to become available.

- envir:

  The [environment](https://rdrr.io/r/base/environment.html) from where
  global objects should be identified.

- ...:

  extra parameters provided to
  [`future::future()`](https://future.futureverse.org/reference/future.html)

- substitute:

  If TRUE, argument `expr` is
  [`substitute()`](https://rdrr.io/r/base/substitute.html):ed, otherwise
  not.

- queue:

  A queue that is used to schedule work to be done using
  [`future::future()`](https://future.futureverse.org/reference/future.html).
  This queue defaults to `future_promise_queue()` and requires that
  method `queue$schedule_work(fn)` exist. This method should take in a
  function that will execute the promised future work.

## Value

Unlike
[`future::future()`](https://future.futureverse.org/reference/future.html),
`future_promise()` returns a
[`promise()`](https://rstudio.github.io/promises/dev/reference/promise.md)
object that will eventually resolve the future `expr`.

## Details

Using `future_promise()` is recommended whenever a continuous runtime is
used, such as with plumber or shiny.

For more details and examples, please see the
[`vignette("future_promise", "promises")`](https://rstudio.github.io/promises/articles/promises_05b_future_promise.html)
vignette.

## Functions

- `future_promise_queue()`: Default `future_promise()` work queue to
  use. This function returns a
  [`WorkQueue`](https://rstudio.github.io/promises/dev/reference/WorkQueue.md)
  that is cached per R session.

- `future_promise()`: Creates a
  [`promise()`](https://rstudio.github.io/promises/dev/reference/promise.md)
  that will execute the `expr` using
  [`future::future()`](https://future.futureverse.org/reference/future.html).

## See also

[`WorkQueue`](https://rstudio.github.io/promises/dev/reference/WorkQueue.md)

## Examples

``` r
if (FALSE) # Relative start time
start <- Sys.time()
# Helper to force two `future` workers
with_two_workers <- function(expr) {
  if (!require("future")) {
    message("`future` not installed")
    return()
  }
  old_plan <- future::plan(future::multisession, workers = 2)
  on.exit({future::plan(old_plan)}, add = TRUE)
  start <<- Sys.time()
  force(expr)
  while(!later::loop_empty()) {Sys.sleep(0.1); later::run_now()}
  invisible()
}
# Print a status message. Ex: `"PID: XXX; 2.5s promise done"`
print_msg <- function(pid, msg) {
  message(
    "PID: ", pid, "; ",
    round(difftime(Sys.time(), start, units = "secs"), digits = 1), "s " ,
    msg
  )
}

# `"promise done"` will appear after four workers are done and the main R session is not blocked
# The important thing to note is the first four times will be roughly the same
with_two_workers({
  promise_resolve(Sys.getpid()) |>
    then(\(x) {print_msg("promise done")})
  for (i in 1:6) {
    future::future({Sys.sleep(1); Sys.getpid()}) |>
      then(\(x) {print_msg("future done")})
  }
})
#> Loading required package: future
#> Error in with_two_workers({    then(promise_resolve(Sys.getpid()), function(x) {        print_msg("promise done")    })    for (i in 1:6) {        then(future::future({            Sys.sleep(1)            Sys.getpid()        }), function(x) {            print_msg("future done")        })    }}): cannot change value of locked binding for 'start'
{
#> PID: XXX; 2.5s promise done
#> PID: YYY; 2.6s future done
#> PID: ZZZ; 2.6s future done
#> PID: YYY; 2.6s future done
#> PID: ZZZ; 2.6s future done
#> PID: YYY; 3.4s future done
#> PID: ZZZ; 3.6s future done
}
#> NULL

# `"promise done"` will almost immediately, before any workers have completed
# The first two `"future done"` comments appear earlier the example above
with_two_workers({
  promise_resolve(Sys.getpid()) |>
    then(\(x) {print_msg("promise")})
  for (i in 1:6) {
    future_promise({Sys.sleep(1); Sys.getpid()}) |>
      then(\(x) {print_msg("future done")})
  }
})
#> Error in start <<- Sys.time(): cannot change value of locked binding for 'start'
{
#> PID: XXX; 0.2s promise done
#> PID: YYY; 1.3s future done
#> PID: ZZZ; 1.4s future done
#> PID: YYY; 2.5s future done
#> PID: ZZZ; 2.6s future done
#> PID: YYY; 3.4s future done
#> PID: ZZZ; 3.6s future done
} # \dontrun{}
#> NULL
```
