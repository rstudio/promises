# Asynchronous or synchronous `then()`

This is a helper function that behaves like `then`, however if
[`is.promising()`](https://rstudio.github.io/promises/dev/reference/is.promise.md)
returns `FALSE` then the handlers will be executed immediately.

## Usage

``` r
hybrid_then(expr, on_success = NULL, on_failure = NULL, ..., tee = FALSE)
```

## Arguments

- expr:

  An expression that evaluates to either a promise or a non-promise
  value.

- on_success:

  A function to be called when no error occurs synchronously or
  asynchronously. When invoked, the function will be called with a
  single argument: the resolved value. Optionally, the function can take
  a second parameter `.visible` if you care whether the promise was
  resolved with a visible or invisible value. Can return a value or a
  promise.

- on_failure:

  A function to be called if an error occurs synchronously or
  asynchronously. Takes one argument: the error object. Can return a
  value or a promise to recover from the error, or throw a new error. If
  `on_failure` is provided and doesn't throw an error (or return a
  promise that fails) then this is the async equivalent of catching an
  error.

- ...:

  Reserved for future use. Currently must be empty.

- tee:

  If `TRUE`, ignore the return value of the callback, and use the
  original value of `expr` as the result. For `on_failure` with
  `tee = TRUE`, the callback executes but the original error is
  re-thrown afterward.

## Value

- If `expr` evaluates to a promise, a promise with a single followup
  promise to handle the `on_success` or `on_failure` callbacks.

- If `expr` evaluates to a non-promise value, the result of the
  synchronous operation after being processed by `on_success` or
  `on_failure`.

- If a callback returns a promise, the result is always a promise.

## Details

Execution paths:

- If `expr` evaluates to a promise (`p`), it will call
  `p |> then(on_success, on_failure)`.

- If `expr` evaluates to a non-promise value (`x`), it will call
  `on_success(x)`.

- If `expr` throws an error (`e`) during calculation, it will call
  `on_failure(e)`.

In all cases, the `on_success` and `on_failure` callbacks are executed
(when provided).

## Utility

This function is useful for writing functions that need to execute
followup behavior *now* or within a promise. This is different behavior
than
[`then()`](https://rstudio.github.io/promises/dev/reference/then.md)
where *everything* is made into a promise.

`hybrid_then()` allows authors to keep synchronous execution on the same
*tick* without requiring the use of a followup promise. This is
particularly appealing for situations where the author does not control
the execution flow for items that may be either synchronous or
asynchronous, such as within `{plumber2}`.

## Error Handling

If no `on_failure` callback is provided and an error occurs, the error
is re-thrown immediately (for synchronous errors) or propagated through
the returned promise (for asynchronous errors).

If an `on_failure` callback is provided but it throws an error, that new
error replaces the original error. With `tee = TRUE`, even if
`on_failure` executes successfully, the original error is still
re-thrown.

## Callback Return Values

Callbacks can return any value, including promises. If a callback
returns a promise, the entire `hybrid_then()` call will return a
promise, even if the input was synchronous. This allows seamless
transitions between synchronous and asynchronous execution.

## See also

[`then()`](https://rstudio.github.io/promises/dev/reference/then.md),
[`is.promising()`](https://rstudio.github.io/promises/dev/reference/is.promise.md),
[`promise_resolve()`](https://rstudio.github.io/promises/dev/reference/promise_resolve.md)

## Examples

``` r
# Basic usage - works with both sync and async values
add_to <- function(x, k) {
  hybrid_then(
    x,
    on_success = function(value) {
      value + k
    },
    on_failure = function(err) {
      message("Error: ", err$message)
      NA_real_
    }
  )
}

# Synchronous
42 |> add_to(100)
#> [1] 142
#> [1] 142

# Synchronous error
add_to({stop("Bad input!")}, 8)
#> Error: Bad input!
#> [1] NA
#> Error: Bad input!
#> [1] NA

if (FALSE) { # \dontrun{
# Asynchronous
promise_resolve(42) |>
  add_to(8) |>
  then(print)
# When resolved...
#> [1] 50

# Error handling - asynchronous
promise_resolve(stop("Bad async input!")) |>
  add_to(8) |>
  then(print)
# When resolved...
#> Error: Bad async input!
#> [1] NA

# Chaining multiple operations
# (Move the `promise_resolve()` around to see sync vs async behavior)
1 |>
  hybrid_then(on_success = \(x) x + 1) |>
  hybrid_then(on_success = \(x) promise_resolve(x * 2)) |>
  hybrid_then(on_success = \(x) x - 1) |>
  hybrid_then(print)
# When resolved...
#> [1] 3
} # }
```
