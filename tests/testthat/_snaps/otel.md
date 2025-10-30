# with_ospan_async(): is deprecated

    Code
      with_ospan_async("test_span_deprecated", {
        42
      }, tracer = get_tracer())
    Condition
      Warning:
      `with_ospan_async()` was deprecated in promises 1.5.0.
      i Please use `with_hybrid_otel_span()` instead.
    Output
      [1] 42

