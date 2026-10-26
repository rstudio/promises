# Promise-aware version of Reduce

Similar to
[`purrr::reduce`](https://purrr.tidyverse.org/reference/reduce.html)
(left fold), but the function `.f` is permitted to return a promise.
`promise_reduce` will wait for any returned promise to resolve before
invoking `.f` with the next element; in other words, execution is
serial. `.f` can return a promise as output but should never encounter a
promise as input (unless `.x` itself is a list of promises to begin
with, in which case the second parameter would be a promise).

## Usage

``` r
promise_reduce(.x, .f, ..., .init)
```

## Arguments

- .x:

  A vector or list to reduce. (Not a promise.)

- .f:

  A function that takes two parameters. The first parameter will be the
  "result" (initially `.init`, and then set to the result of the most
  recent call to `func`), and the second parameter will be an element of
  `.x`.

- ...:

  Other arguments to pass to `.f`

- .init:

  The initial result value of the fold, passed into `.f` when it is
  first executed.

## Value

A promise that will resolve to the result of calling `.f` on the last
element (or `.init` if `.x` had no elements). If any invocation of `.f`
results in an error or a rejected promise, then the overall
`promise_reduce` promise will immediately reject with that error.

## Examples

``` r
# Returns a promise for the sum of e1 + e2, with a 0.5 sec delay
slowly_add <- function(e1, e2) {
  promise(\(resolve, reject) {
    later::later(\() resolve(e1 + e2), delay = 0.5)
  })
}

# Prints 55 after a little over 5 seconds
promise_reduce(1:10, slowly_add, .init = 0) |>
  then(print)
```
