# Remove special case optimation of a custom tracer and leaning to "least surprise" for tracing is enabled or not.
# If these are to be restored, the default value of `tracer` should be `promise_otel_tracer()` within `otel.R`
# 10micro seconds is still fairly fast given the consistent result.

# The commented code below would be used to optimize the tracer lookup and is kept for knowledge purposes.

# --------------------------------------------------------------

# #' @importFrom otel
# #'   get_tracer
# NULL

# PROMISES_OTEL_TRACER <- NULL

# on_load({
#   PROMISES_OTEL_TRACER <- get_tracer(otel_tracer_name)
# })

# #' Promises OpenTelemetry tracer
# #'
# #' Placeholder tracer to speed up default methods. No need to look up the tracer
# #' when we can define it here.
# #'
# #' ```
# #' bench::mark(
# #'   otel::is_tracing_enabled(),
# #'   otel::is_tracing_enabled(promises_otel_tracer()),
# #'   promises_otel_tracer()$is_enabled()
# #' )
# #' #> # A tibble: 3 × 13
# #' #>   expression       min  median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory     time       gc
# #' #>   <bch:expr>   <bch:t> <bch:t>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>     <list>     <list>
# #' #> 1 otel::is_tr… 11.11µs 12.55µs    73955.        0B     22.2  9997     3    135.2ms <lgl>  <Rprofmem> <bench_tm> <tibble>
# #' #> 2 otel::is_tr…   3.9µs  4.39µs   220877.        0B     22.1  9999     1     45.3ms <lgl>  <Rprofmem> <bench_tm> <tibble>
# #' #> 3 promises_ot…  1.39µs   1.6µs   595965.        0B      0   10000     0     16.8ms <lgl>  <Rprofmem> <bench_tm> <tibble>
# #' ```
# #'
# #' While we could use `promises_otel_tracer()$is_enabled()`, there is no tryCatch safety during execution.
# #' @noRd
# promises_otel_tracer <- function() {
#   if (Sys.getenv("TESTTHAT") == "true") {
#     # Allow for dynamic values during testing
#     return(get_tracer(otel_tracer_name))
#   }
#
#   PROMISES_OTEL_TRACER
# }
