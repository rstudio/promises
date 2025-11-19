# OpenTelemetry integration

otel provides tools for integrating with OpenTelemetry, a framework for
observability and tracing in distributed systems.

These methods are intended to enhance the framework to be used with the
promises package, not as a generic replacement.

Developer note - Barret 2025/09: This otel span handoff promise domain
topic is complex and has been discussed over many hours. Many advanced
Shiny/R developers are not even aware of promise domains (very
reasonable!), therefore this topic requires more in-depth documentation
and examples.

## Usage

``` r
with_otel_span(name, expr, ..., tracer, attributes = NULL)

with_otel_promise_domain(expr)

local_otel_promise_domain(envir = parent.frame())
```

## Arguments

- name:

  Character string. The name of the otel span.

- expr:

  An expression to evaluate within the otel span context.

- ...:

  Additional arguments passed to
  [`otel::start_span()`](https://otel.r-lib.org/reference/start_span.html).

- tracer:

  (Required) An `{otel}` tracer. It is required to provide your own
  tracer from your own package. See
  [`otel::get_tracer()`](https://otel.r-lib.org/reference/get_tracer.html)
  for more details.

- attributes:

  Attributes passed through
  [`otel::as_attributes()`](https://otel.r-lib.org/reference/as_attributes.html)
  (when not `NULL`)

- envir:

  The "local" environment in which to add the promise domain. When the
  environment is exited, the promise domain is removed.

## Functions

- `with_otel_span()`: Creates an OpenTelemetry span, executes the given
  expression within it, and ends the span.

  This method **requires** the use of `with_otel_promise_domain()` to be
  within the execution stack.

  This function is designed to handle both synchronous and asynchronous
  (promise-based) operations. For promises, the span is automatically
  ended when the promise resolves or rejects.

  Returns the result of evaluating `expr`. If `expr` returns a promise,
  the span will be automatically ended when the promise completes.

  This function differs from synchronous otel span operations in that it
  installs a promise domain and properly handles asynchronous
  operations. In addition, the internal span will be ended either when
  the function exits (for synchronous operations) or when a returned
  promise completes (for asynchronous operations).

  If OpenTelemetry is not enabled, the expression will be evaluated
  without any tracing context.

- `with_otel_promise_domain()`: Adds an idempotent handoff Active
  OpenTelemetry span promise domain.

  Package authors are required to use this function to have otel span
  context persist across asynchronous boundaries. This method is only
  needed once per promise domain stack. If you are unsure, feel free to
  call `with_otel_promise_domain()` as the underlying promise domain
  will only be added if not found within the current promise domain
  stack. If your package **only** works within Shiny apps, Shiny will
  have already added the domain so no need to add it yourself. If your
  package works outside of Shiny and you use `{promises}` (i.e.
  `{chromote}`), then you'll need to use this wrapper method.

  This method adds a *handoff* Active OpenTelemetry span promise domain
  to the expression evaluation. This *handoff* promise domain will only
  run once on reactivation. This is critical if there are many layered
  `with_otel_span()` calls, such as within Shiny reactivity. For
  example, if we nested many `with_otel_span()` calls of which each call
  added a promise domain that reactivated each otel span on restore,
  we'd reactivate `k` otel span objects (`O(k)`) when we only need to
  activate the **last** span (`O(1)`).

  Returns the result of evaluating `expr` within the otel span promise
  domain.

- `local_otel_promise_domain()`: Local OpenTelemetry span promise domain

  Adds an OpenTelemetry span promise domain to the local scope. This is
  useful for `{coro}` operations where encapsulating the coro operations
  inside a `with_*()` methods is not allowed.

  When not using `{coro}`, please prefer to use `with_otel_span()` or
  `with_otel_promise_domain()`.

## Definitions

- Promise domain: An environment in which has setup/teardown methods.
  These environments can be composed together to facilitate execution
  context for promises. In normal R execution, this can be achieved with
  scope / stack. But for complex situations, such as the currently open
  graphics device, async operations require promise domains to
  setup/teardown these contexts to function properly. Otherwise a
  multi-stage promise that adds to the graphics device at each stage
  will only ever print to the most recently created graphics device, not
  the associated graphics device. These promise domains are not
  automatically created, they must be manually added to the execution
  stack, for example `with_otel_promise_domain()` does this for
  OpenTelemetry spans.

- Promise domain restoration: When switching from one promise chain to
  another, the execution context is torn down and then re-established.
  This re-establishment is called "promise domain restoration". During
  this process, the promise domains are restored in their previously
  established combination order.

- Promise chain: A set of promise objects to execute over multiple async
  ticks.

- Async tick: the number of times an event loop must run to move
  computation forward. (Similar to a JavaScript event loop tick.)

- [`then()`](https://rstudio.github.io/promises/dev/reference/then.md)
  promise domain capture: When
  [`then()`](https://rstudio.github.io/promises/dev/reference/then.md)
  is called, it will capture the current promise domain. This promise
  domain is restored (only if needed) when evaluating the given
  `onFulfilled` and `onRejected` callbacks. This captured promise domain
  does not go into any downstream promise chain objects. The only way
  the promise domain is captured is exactly when the
  [`then()`](https://rstudio.github.io/promises/dev/reference/then.md)
  method is called.

`with_otel_promise_domain()` creates a promise domain that restores the
currently active OpenTelemetry span from when a call to
[`promises::then()`](https://rstudio.github.io/promises/dev/reference/then.md)
is executed. Given the special circumstance where only the current otel
span is needed to continue recording (not a full ancestry tree of otel
spans), we can capture *just* the current otel span and reactivate that
otel span during promise domain restoration.

## When promise domains are captured

Asynchronous operation

- Creates `async_op` otel span

- Automatically ends the otel span (`async_op`) when the promise (p)
  resolves or rejects

The code below illustrates an example of when the promise domain are
created/captured/restored and when otel span objects are
created/activated/reactivated/ended.

    # t0.0
    p2 <- with_otel_promise_domain({
      # t0.1
      p <- with_otel_span("async_op", {
        # ... return a promise ...
        init_async_work() |> # t0.2
          then( # t0.3
            some_async_work # t1.0
          )
      }) # t0.4, t1.0, t2.0
      p |>
        then( # t0.5
          more_async_work # t3.0
        )
    }) # t0.6

    p_final <-
      p2 |> then( # t0.7
        final_work # t4.0
      )

An in-depth explanation of the execution timeline is below.

- At the first initial tick, `t0.*`:

  - `t0.0`: The code is wrapped in `with_otel_promise_domain()`

  - `t0.1`: The `async_op` otel span is created and activated

  - `t0.2`: Some async work is initiated

  - `t0.3`:
    [`then()`](https://rstudio.github.io/promises/dev/reference/then.md)
    is called, capturing the active `async_op` otel span (as it is
    called within `with_otel_promise_domain()`)

  - `t0.4`: The `with_otel_span()` call exits, but the `async_op` otel
    span is not ended as the promise is still pending. The returned
    promise has a
    [`finally()`](https://rstudio.github.io/promises/dev/reference/then.md)
    step added to it that will end the otel span `async_op` when `p` is
    resolved.

  - `t0.5`: Another
    [`then()`](https://rstudio.github.io/promises/dev/reference/then.md)
    is called, but there is no active otel span to capture

  - `t0.6`: The otel span promise domain call exits

  - `t0.7`: Another
    [`then()`](https://rstudio.github.io/promises/dev/reference/then.md)
    is called. No otel span will be captured as there is no active otel
    span / promise domain

- At the first followup tick, `t1.0`:

  - The active `async_op` otel span is reactivated during promise domain
    restoration for the duration of the `then` callback

  - The `some_async_work` function is called

- At tick, `t2.0`:

  - `some_async_work` has resolved

  - A hidden
    [`finally()`](https://rstudio.github.io/promises/dev/reference/then.md)
    step closes the otel span, `async_op`

  - `p` is now resolved

- At tick, `t3.0`:

  - There is no active otel span at `t0.5`, so no otel span is
    reactivated during promise domain restoration

  - The `more_async_work` function is executed

- At tick, `t4.0`:

  - `more_async_work` has resolved, therefore `p2` is now resolved

  - There was no otel span promise domain at `t0.7`, so no attempt is
    made to reactivate any otel span

  - The `final_work` function is executed

- At tick, `t5.0`:

  - `p_final` has resolved

## Complexity

When reactivating the `k`th step in a promise chain, the currently
active otel span (during the call to
[`then()`](https://rstudio.github.io/promises/dev/reference/then.md))
will be reactivated during promise domain restoration (`O(1)`). To
restore a chain of promises, the active otel span will be restored at
each step (`O(n)`) due to the **`n`** calls to wrapping each
`onFulfilled` and `onRejected` callbacks inside
[`then()`](https://rstudio.github.io/promises/dev/reference/then.md).

If we did NOT have a handoff promise domain for otel span restoration, a
regular promise domain approach would be needed at each step to restore
the active otel span. Each step would call `with_active_span()` `k`
times (`O(k)`, where as handoff domain computes in `O(1)`). Taking a
step back, to restore each otel span at for every step in a promise
chain would then take `O(n^2)` time, not `O(n)`. The standard, naive
promise domain approach does not scale for multiple similar promise
domain restorations.

## Execution model for `with_otel_promise_domain()`

1.  `with_otel_promise_domain(expr)` is called.

    - The following steps all occur within `expr`.

2.  Create an otel span object using
    [`otel::start_span()`](https://otel.r-lib.org/reference/start_span.html).

    - We need the otel span to be active during the a followup async
      operation. Therefore,
      [`otel::start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.html)
      is not appropriate as the otel span would be ended when the
      function exits, not when the promise chain resolves.

3.  Be sure your otel span is activated before calling
    [`promises::then()`](https://rstudio.github.io/promises/dev/reference/then.md).

    - Activate it using `with_otel_span(name, expr)` (which also
      creates/ends the otel span) or
      `otel::with_active_span(span, expr)`.

4.  Call
    [`promises::then()`](https://rstudio.github.io/promises/dev/reference/then.md)

- When
  [`promises::then()`](https://rstudio.github.io/promises/dev/reference/then.md)
  is called, the two methods (`onFulfilled` and `onRejected`) capture
  the currently active spans. (Performed by the initial
  `with_otel_promise_domain()`)

1.  During reactivation of the promise chain step, the previously
    captured otel span is reactivated via `with_active_span()`.
    (Performed by the initial `with_otel_promise_domain()`)

## OpenTelemetry span compatibility

For otel span objects to exist over may async ticks, the otel span must
be created using
[`otel::start_span()`](https://otel.r-lib.org/reference/start_span.html)
and later ended using
[`otel::end_span()`](https://otel.r-lib.org/reference/end_span.html).
Ending the otel span must occur **after** any promise chain work has
completed.

If we were to instead use
[`otel::start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.html),
the otel span would be ended when the function exits, not when the
promise chain completes. Even though the local otel span is created,
activated, and eventually ended, the otel span will not exist during
reactivation of the otel span promise domain.

`with_otel_span()` is a convenience method that creates, activates, and
ends the otel span only after the returned promise (if any) resolves. It
also properly handles both synchronous (ending the otel span within
[`on.exit()`](https://rdrr.io/r/base/on.exit.html)) and asynchronous
operations (ending the otel span within
[`promises::finally()`](https://rstudio.github.io/promises/dev/reference/then.md)).

## See also

[`otel::start_span()`](https://otel.r-lib.org/reference/start_span.html),
[`otel::with_active_span()`](https://otel.r-lib.org/reference/with_active_span.html),
[`otel::end_span()`](https://otel.r-lib.org/reference/end_span.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Common usage:
with_otel_promise_domain({
  # ... deep inside some code execution ...

  # Many calls to `with_otel_span()` within `with_otel_promise_domain()`
  with_otel_span("my_operation", {
    # ... do some work ...
  })
})
} # }
if (FALSE) { # \dontrun{
with_otel_promise_domain({
  # ... deep inside some code execution ...

  # Synchronous operation
  # * Creates `my_operation` span
  result <- with_otel_span("my_operation", {
    # ... do some work ...
    print(otel::get_active_span()$name) # "my_operation"

    # Nest (many) more spans
    prom_nested <- with_otel_span("my_nested_operation", {
      # ... do some more work ...
      promise_resolve(42) |>
        then(\(value) {
          print(otel::get_active_span()$name) # "my_nested_operation"
          print(value) # 42
        })
    })

    # Since `then()` is called during the active `my_operation` span,
    # the `my_operation` span will be reactivated in the `then()` callback.
    prom_nested |> then(\(value) {
      print(otel::get_active_span()$name) # "my_operation"
      value
    })
  })

  # Since `then()` is called where there is no active span,
  # there is no _active_ span in the `then()` callback.
  result |> then(\(value) {
    stopifnot(inherits(otel::get_active_span(), "otel_span_noop"))
    print(value) # 42
  })
})
} # }
```
