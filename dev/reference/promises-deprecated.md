# **\[deprecated\]**: Deprecated functions in promises

These functions have been deprecated and will be removed in a future
version. Please use the suggested alternatives instead.

## Usage

``` r
with_ospan_async(name, expr, ..., tracer, attributes = NULL)

with_ospan_promise_domain(expr)

local_ospan_promise_domain(envir = parent.frame())
```

## Functions

- `with_ospan_async()`: Use
  [`with_otel_span()`](https://rstudio.github.io/promises/dev/reference/otel.md)
  instead

- `with_ospan_promise_domain()`: Use
  [`with_otel_promise_domain()`](https://rstudio.github.io/promises/dev/reference/otel.md)
  instead

- `local_ospan_promise_domain()`: Use
  [`local_otel_promise_domain()`](https://rstudio.github.io/promises/dev/reference/otel.md)
  instead
