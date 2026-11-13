# Combine multiple promise objects

Use `promise_all` to wait for multiple promise objects to all be
successfully fulfilled. Use `promise_race` to wait for the first of
multiple promise objects to be either fulfilled or rejected.

## Usage

``` r
promise_all(..., .list = NULL)

promise_race(..., .list = NULL)
```

## Arguments

- ...:

  Promise objects. Either all arguments must be named, or all arguments
  must be unnamed. If `.list` is provided, then these arguments are
  ignored.

- .list:

  A list of promise objects–an alternative to `...`.

## Value

A promise.

For `promise_all`, if all of the promises were successful, the returned
promise will resolve to a list of the promises' values; if any promise
fails, the first error to be encountered will be used to reject the
returned promise.

For `promise_race`, the first of the promises to either fulfill or
reject will be passed through to the returned promise.

## Examples

``` r
p1 <- promise(\(resolve, reject) later::later(\() resolve(1), delay = 1))
p2 <- promise(\(resolve, reject) later::later(\() resolve(2), delay = 2))

# Resolves after 1 second, to the value: 1
promise_race(p1, p2) |>
  then(\(x) {
    cat("promise_race:\n")
    str(x)
  })

# Resolves after 2 seconds, to the value: list(1, 2)
promise_all(p1, p2) |>
  then(\(x) {
    cat("promise_all:\n")
    str(x)
  })
```
