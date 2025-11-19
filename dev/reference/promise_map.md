# Promise-aware lapply/map

Similar to [`base::lapply()`](https://rdrr.io/r/base/lapply.html) or
[`purrr::map`](https://purrr.tidyverse.org/reference/map.html), but
promise-aware: the `.f` function is permitted to return promises, and
while `lapply` returns a list, `promise_map` returns a promise that
resolves to a similar list (of resolved values only, no promises).

## Usage

``` r
promise_map(.x, .f, ...)
```

## Arguments

- .x:

  A vector (atomic or list) or an expression object (but not a promise).
  Other objects (including classed objects) will be coerced by
  base::as.list.

- .f:

  The function to be applied to each element of `.x`. The function is
  permitted, but not required, to return a promise.

- ...:

  Optional arguments to `.f`.

## Value

A promise that resolves to a list (of values, not promises).

## Details

`promise_map` processes elements of `.x` serially; that is, if
`.f(.x[[1]])` returns a promise, then `.f(.x[[2]])` will not be invoked
until that promise is resolved. If any such promise rejects (errors),
then the promise returned by `promise_map` immediately rejects with that
err.

## Examples

``` r
# Waits x seconds, then returns x*10
wait_this_long <- function(x) {
  promise(\(resolve, reject) {
    later::later(\() resolve(x*10), delay = x)
  })
}

promise_map(
  list(A=1, B=2, C=3),
  wait_this_long
) |>
  then(print)
```
