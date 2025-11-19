# Promise domains

Promise domains are used to temporarily set up custom environments that
intercept and influence the registration of callbacks. Create new
promise domain objects using `new_promise_domain`, and temporarily
activate a promise domain object (for the duration of evaluating a given
expression) using `with_promise_domain`.

## Usage

``` r
with_promise_domain(domain, expr, replace = FALSE)

new_promise_domain(
  wrapOnFulfilled = identity,
  wrapOnRejected = identity,
  wrapSync = force,
  onError = force,
  ...,
  wrapOnFinally = NULL
)
```

## Arguments

- domain:

  A promise domain object to install while `expr` is evaluated.

- expr:

  Any R expression, to be evaluated under the influence of `domain`.

- replace:

  If `FALSE`, then the effect of the `domain` will be added to the
  effect of any currently active promise domain(s). If `TRUE`, then the
  current promise domain(s) will be ignored for the duration of the
  `with_promise_domain` call.

- wrapOnFulfilled:

  A function that takes a single argument: a function that was passed as
  an `onFulfilled` argument to
  [`then()`](https://rstudio.github.io/promises/dev/reference/then.md).
  The `wrapOnFulfilled` function should return a function that is
  suitable for `onFulfilled` duty.

- wrapOnRejected:

  A function that takes a single argument: a function that was passed as
  an `onRejected` argument to
  [`then()`](https://rstudio.github.io/promises/dev/reference/then.md).
  The `wrapOnRejected` function should return a function that is
  suitable for `onRejected` duty.

- wrapSync:

  A function that takes a single argument: a (lazily evaluated)
  expression that the function should
  [`force()`](https://rdrr.io/r/base/force.html). This expression
  represents the `expr` argument passed to `with_promise_domain()`;
  `wrapSync` allows the domain to manipulate the environment
  before/after `expr` is evaluated.

- onError:

  A function that takes a single argument: an error. `onError` will be
  called whenever an exception occurs in a domain (that isn't caught by
  a `tryCatch`). Providing an `onError` callback doesn't cause errors to
  be caught, necessarily; instead, `onError` callbacks behave like
  calling handlers.

- ...:

  Arbitrary named values that will become elements of the promise domain
  object, and can be accessed as items in an environment (i.e. using
  `[[` or `$`).

- wrapOnFinally:

  A function that takes a single argument: a function that was passed as
  an `onFinally` argument to
  [`then()`](https://rstudio.github.io/promises/dev/reference/then.md).
  The `wrapOnFinally` function should return a function that is suitable
  for `onFinally` duty. If `wrapOnFinally` is `NULL` (the default), then
  the domain will use both `wrapOnFulfilled` and `wrapOnRejected` to
  wrap the `onFinally`. If it's important to distinguish between normal
  fulfillment/rejection handlers and finally handlers, then be sure to
  provide `wrapOnFinally`, even if it's just
  [`base::identity()`](https://rdrr.io/r/base/identity.html).

## Details

While `with_promise_domain` is on the call stack, any calls to
[`then()`](https://rstudio.github.io/promises/dev/reference/then.md) (or
higher level functions or operators, like
[`catch()`](https://rstudio.github.io/promises/dev/reference/then.md))
will belong to the promise domain. In addition, when a `then` callback
that belongs to a promise domain is invoked, then any new calls to
`then` will also belong to that promise domain. In other words, a
promise domain "infects" not only the immediate calls to `then`, but
also to "nested" calls to `then`.

For more background, read the [original design
doc](https://gist.github.com/jcheng5/b1c87bb416f6153643cd0470ac756231).

For examples, see the source code of the Shiny package, which uses
promise domains extensively to manage graphics devices and reactivity.
