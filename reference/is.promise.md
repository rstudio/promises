# Coerce to a promise

Use `is.promise` to determine whether an R object is a promise. Use
`as.promise` (an S3 generic method) to attempt to coerce an R object to
a promise, and `is.promising` (another S3 generic method) to test
whether `as.promise` is supported.
[mirai::mirai](https://mirai.r-lib.org/reference/mirai.html) objects
have an `as.promise` method defined in the mirai package, and this
package provides one for converting
[future::Future](https://future.futureverse.org/reference/Future-class.html)
objects into promises.

## Usage

``` r
is.promise(x)

is.promising(x)

as.promise(x)
```

## Arguments

- x:

  An R object to test or coerce.

## Value

`as.promise` returns a promise object, or throws an error if the object
cannot be converted.

`is.promise` returns `TRUE` if the given value is a promise object, and
`FALSE` otherwise.

`is.promising` returns `TRUE` if the given value is a promise object or
if it can be converted to a promise object using `as.promise`, and
`FALSE` otherwise.
