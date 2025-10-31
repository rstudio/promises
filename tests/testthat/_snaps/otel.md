# Deprecated functions: with_ospan_async() is deprecated

    Code
      with_ospan_async("test_span_deprecated", {
        42
      }, tracer = promises_otel_tracer())
    Condition
      Warning:
      `with_ospan_async()` was deprecated in promises 1.5.0.
      i Please use `with_otel_span()` instead.
    Output
      [1] 42

# Deprecated functions: with_ospan_promise_domain() is deprecated

    Code
      with_ospan_promise_domain({
        42
      })
    Condition
      Warning:
      `with_ospan_promise_domain()` was deprecated in promises 1.5.0.
      i Please use `with_otel_promise_domain()` instead.
    Output
      [1] 42

# Deprecated functions: local_ospan_promise_domain() is deprecated

    Code
      local({
        local_ospan_promise_domain()
        42
      })
    Condition
      Warning:
      `local_ospan_promise_domain()` was deprecated in promises 1.5.0.
      i Please use `local_otel_promise_domain()` instead.
    Output
      [1] 42

