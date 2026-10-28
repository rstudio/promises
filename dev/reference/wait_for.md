# Wait for a Promise

Synchronizes a promise by waiting for it to complete and then retrieving
its value.

## Usage

``` r
wait_for(promise, ..., timeout = Inf, loop = later::current_loop())
```

## Arguments

- promise:

  A promise object, or an object that can be converted to one with
  [`as.promise()`](https://rstudio.github.io/promises/dev/reference/is.promise.md)
  (such as a future or a mirai).

- ...:

  Ignored.

- timeout:

  The most seconds to wait. If the promise has not settled by then, an
  error of class `promises_timeout` is thrown and the promise is left
  pending. Defaults to `Inf` (wait forever). With `0`, only an already
  settled promise returns.

- loop:

  the later loop on which the promise has been scheduled. Defaults to
  [`later::current_loop()`](https://later.r-lib.org/reference/create_loop.html).

## Value

The value of the resolved promise. An error will be thrown if the
promise is rejected.

## Event loop

`wait_for()` runs a single event loop, given by `loop`. This must be the
loop that runs the promise's callbacks. If it is not, `wait_for()` waits
until `timeout`, which is forever by default. The default,
[`later::current_loop()`](https://later.r-lib.org/reference/create_loop.html),
is the right choice whenever you did not create a custom loop yourself.
Do not call `wait_for()` from inside a `later` callback to wait on work
that depends on that callback finishing.

## Interrupts

An interrupt (Esc in RStudio, Ctrl+C in a terminal) means "stop
waiting". It exits `wait_for()` and leaves the promise pending.
Callbacks that have not started yet stay on the loop and run the next
time the loop runs. If the interrupt lands while a callback is running,
that callback stops part way and is not run again, so promises that
depend on it may never settle. `wait_for()` does not cancel the
underlying work, because only the creator of the work knows how to
cancel it.

## Examples

``` r
# Returns a promise for the sum of e1 + e2, with a 1 sec delay
slowly_add <- function(e1, e2) {
  promise(\(resolve, reject) {
    later::later(\() resolve(e1 + e2), delay = 1)
  })
}

# Returns 3 after 1 second
res <- wait_for(slowly_add(1, 2))
#> Failure
#> Something went wrong
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
#> promise_race:
#>  num 1
#> [1] 0.08075014
#> promise_all:
#> List of 2
#>  $ : num 1
#>  $ : num 2
res
#> [1] 3
```
