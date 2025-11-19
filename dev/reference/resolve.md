# Fulfill a promise

Use these functions to satisfy a promise with either success (`resolve`)
or failure (`reject`). These functions are not exported, but rather, are
passed as arguments to the `action` function you pass to a
[promise](https://rstudio.github.io/promises/dev/reference/promise.md)
constructor.

## Usage

``` r
resolve(value = NULL)

reject(reason)
```

## Arguments

- value:

  The result from a successful calculation.

- reason:

  An error or string that explains why the operation failed.
