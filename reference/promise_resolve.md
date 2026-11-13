# Create a resolved or rejected promise

Helper functions to conveniently create a promise that is resolved to
the given value (or rejected with the given reason).

## Usage

``` r
promise_resolve(value)

promise_reject(reason)
```

## Arguments

- value:

  A value, or promise, that the new promise should be resolved to. This
  expression will be lazily evaluated, and if evaluating the expression
  raises an error, then the new promise will be rejected with that error
  as the reason.

- reason:

  An error message string, or error object.

## Examples

``` r
promise_resolve(mtcars) |>
  then(head) |>
  then(print)

promise_reject("Something went wrong") |>
  catch(tee = TRUE, \(e) message(conditionMessage(e)))
```
