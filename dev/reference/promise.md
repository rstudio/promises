# Create a new promise object

`promise()` creates a new promise. A promise is a placeholder object for
the eventual result (or error) of an asynchronous operation. This
function is not generally needed to carry out asynchronous programming
tasks; instead, it is intended to be used mostly by package authors who
want to write asynchronous functions that return promises.

## Usage

``` r
promise(action)
```

## Arguments

- action:

  A function with signature `function(resolve, reject)`.

## Value

A promise object (see
[`then`](https://rstudio.github.io/promises/dev/reference/then.md)).

## Details

The `action` function should be a piece of code that returns quickly,
but initiates a potentially long-running, asynchronous task. If/when the
task successfully completes, call `resolve(value)` where `value` is the
result of the computation (like the return value). If the task fails,
call `reject(reason)`, where `reason` is either an error object, or a
character string.

It's important that asynchronous tasks kicked off from `action` be coded
very carefully–in particular, all errors must be caught and passed to
[`reject()`](https://rstudio.github.io/promises/dev/reference/resolve.md).
Failure to do so will cause those errors to be lost, at best; and the
caller of the asynchronous task will never receive a response (the
asynchronous equivalent of a function call that never returns, i.e.
hangs).

The return value of `action` will be ignored.

## `action=` formulas

**\[superseded\]**

With `{promises}` depending on R \>= 4.1, the shorthand of a formula,
`~ fn(.)` for `action` is no longer recommended by the `{promises}`
package or tidyverse (for example,
[`{purrr}`](https://github.com/tidyverse/purrr/commit/670c3ed9920f15da0d4175068ecddc41f0f1f335#diff-c4dcc43795da5c7f6bf5a94d957b5507ce795fedd6d3eb092ccad03678c4f76dR15))
as we now have access to the function shorthand, `\(x) fn(x)`. Please
update your `action` code to use the new function shorthand syntax
`\(resolve, reject) resolve(arg1, arg2)` instead of
`~ { resolve(arg1, arg2) }`. The magically created `resolve`/`reject`
functions can be confusing when chained with other methods.

## Examples

``` r
# Create a promise that resolves to a random value after 2 secs
p1 <- promise(\(resolve, reject) {
  later::later(\() resolve(runif(1)), delay = 2)
})

p1 |> then(print)

# Create a promise that errors immediately
p2 <- promise(\(resolve, reject) {
  reject("An error has occurred")
})
then(p2,
  onFulfilled = \(value) message("Success"),
  onRejected = \(err) message("Failure")
)
```
