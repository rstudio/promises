#' @importFrom otel
#'   as_attributes
#'   end_span
#'   get_active_span
#'   get_tracer
#'   is_tracing_enabled
#'   start_span
#'   with_active_span
NULL

# Just in case we don't use `promises_otel_tracer()`, hopefully it is found using this variable for `otel::default_tracer_name()`
otel_tracer_name <- "co.posit.r-package.shiny"
PROMISES_OTEL_TRACER <- NULL

on_load({
  PROMISES_OTEL_TRACER <- get_tracer(otel_tracer_name)
})


#' Promises OpenTelemetry tracer
#'
#' Placeholder tracer to speed up default methods. No need to look up the tracer
#' when we can define it here.
#'
#' ```
#' bench::mark(
#'   otel::is_tracing_enabled(),
#'   otel::is_tracing_enabled(promises_otel_tracer()),
#'   promises_otel_tracer()$is_enabled()
#' )
#' #> # A tibble: 3 × 13
#' #>   expression       min  median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory     time       gc
#' #>   <bch:expr>   <bch:t> <bch:t>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>     <list>     <list>
#' #> 1 otel::is_tr… 11.11µs 12.55µs    73955.        0B     22.2  9997     3    135.2ms <lgl>  <Rprofmem> <bench_tm> <tibble>
#' #> 2 otel::is_tr…   3.9µs  4.39µs   220877.        0B     22.1  9999     1     45.3ms <lgl>  <Rprofmem> <bench_tm> <tibble>
#' #> 3 promises_ot…  1.39µs   1.6µs   595965.        0B      0   10000     0     16.8ms <lgl>  <Rprofmem> <bench_tm> <tibble>
#' ```
#'
#' While we could use `promises_otel_tracer()$is_enabled()`, there is no tryCatch safety during execution.
#' @noRd
promises_otel_tracer <- function() {
  if (Sys.getenv("TESTTHAT") == "true") {
    # Allow for dynamic values during testing
    return(get_tracer(otel_tracer_name))
  }

  PROMISES_OTEL_TRACER
}

#' `r lifecycle::badge("experimental")` OpenTelemetry integration
#'
#' @description
#' \pkg{otel} provides tools for integrating with OpenTelemetry, a framework for
#' observability and tracing in distributed systems.
#'
#' These methods are intended to enhance the framework to be used with the
#' \pkg{promises} package, not as a generic replacement.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Synchronous operation
#' result <- with_ospan_async("my_operation", {
#'   # ... do some work ...
#'   42
#' })
#'
#' # Asynchronous operation
#' p <- with_ospan_async("async_operation", {
#'   # ... return a promise ...
#'   some_async_function()
#' })
#' }
#' \dontrun{
#' # Set up a promise domain to maintain the active otel span
#' with_ospan_promise_domain({
#'   # ... do some promise work ...
#' })
#' }
#'
#' @describeIn otel `r lifecycle::badge("experimental")`
#'
#' Creates an OpenTelemetry span, executes the given expression within it, and
#' ends the span.
#'
#' This function is designed to handle both synchronous and asynchronous
#' (promise-based) operations. For promises, the span is automatically ended
#' when the promise resolves or rejects.
#'
#' Returns the result of evaluating `expr`. If `expr` returns a promise,
#'   the span will be automatically ended when the promise completes.
#'
#' This function differs from synchronous otel span operations in that it
#' installs a promise domain and properly handles asynchronous operations. In
#' addition, the internal span will be ended either when the function exits (for
#' synchronous operations) or when a returned promise completes (for
#' asynchronous operations).
#'
#' If OpenTelemetry is not enabled, the expression will be evaluated without any
#' tracing context.
#' @param name Character string. The name of the span.
#' @param expr An expression to evaluate within the span context.
#' @param ... Additional arguments passed to [`otel::start_span()`].
#' @param tracer An `{otel}` tracer. The default value is set to promises
#'   package (which it would eventually calculate). It is strongly recommended
#'   to provide your own tracer from your own package. See
#'   [`otel::get_tracer()`] for more details.
#' @param attributes Attributes passed through [`otel::as_attributes()`] (when
#'   not `NULL`)
#' @export
with_ospan_async <- function(
  name,
  expr,
  ...,
  tracer = promises_otel_tracer(),
  attributes = NULL
) {
  if (!is_tracing_enabled(tracer)) {
    return(force(expr))
  }

  span <- create_ospan(name, ..., tracer = tracer, attributes = attributes)

  needs_cleanup <- TRUE
  cleanup <- function() {
    end_ospan(span, tracer = tracer)
  }
  on.exit(if (needs_cleanup) cleanup(), add = TRUE)

  result <- with_active_span(span, expr)

  if (is.promising(result)) {
    needs_cleanup <- FALSE

    result <- finally(result, cleanup)
  }

  result
}

#' @describeIn otel `r lifecycle::badge("experimental")`
#'
#' Adds a handoff "Active OpenTelemetry promise domain" for the expression.
#'
#' Package authors are required to use this function to have otel span context
#' persist across asynchronous boundaries. It is important to leverage this
#' function only once within the execution domain, not recursively / many times.
#'
#' This method adds a _handoff_ "Active OpenTelemetry promise domain" to the
#' expression evaluation. This _handoff_ promise domain will only run once on
#' reactivation. This is critical if there are many layered `with_ospan_async()`
#' calls, such as within Shiny reactivity. For example, if we nested many
#' `with_ospan_async()` that added a domain which reactivated each ospan on
#' restore, we'd reactive `k` ospan objects (`O(k)`) when we only need to
#' activate the **last** span (`O(1)`).
#'
#' The currently active span (during the call to `then()`) will be reactivated
#' during promise domain restoration.
#' @export
with_ospan_promise_domain <- function(expr) {
  act_span_pd <- create_otel_ospan_handoff_promise_domain()
  with_promise_domain(act_span_pd, expr)
}


#' @describeIn otel `r lifecycle::badge("experimental")`
#'
#' Creates an OpenTelemetry span for discontiguous operations where you need
#' manual control over when the span ends.
#'
#' Note, the created span is not activated. Please use [`with_ospan_async()`] to
#' activate it for the supplied expression.
#'
#' Returns an \pkg{otel} span object.
#' @export
create_ospan <- function(
  name,
  ...,
  tracer = promises_otel_tracer(),
  attributes = NULL
) {
  if (!is_tracing_enabled(tracer)) {
    return(NULL)
  }

  span <-
    start_span(
      name,
      ...,
      tracer = tracer,
      ## Use when otel v0.3.0 is released: https://github.com/r-lib/otel/commit/43e59c45a7de50cfd8af95f73d45f9899f957b44
      # attributes = as_attributes(attributes)
      attributes = if (!is.null(attributes)) {
        as_attributes(attributes)
      }
    )
  span
}


#' @describeIn otel `r lifecycle::badge("experimental")`
#'
#' Ends an created OpenTelemetry span for discontiguous operations.
#' @export
end_ospan <- function(span, tracer = promises_otel_tracer()) {
  if (is.null(span) || !is_tracing_enabled(tracer)) {
    return(invisible())
  }

  end_span(span)
}


# # TODO: Set attributes on the current active span
# # 5. Set attributes on the current active span
# set_ospan_attrs(status = 200L)

# -- Helpers --------------------------------------------------------------

#' Creates a promise domain that activates the given OpenTelemetry span.
#'
#' @param span An OpenTelemetry span object.
#' @noRd
create_otel_ospan_handoff_promise_domain <- function() {
  new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      force(onFulfilled)

      span <- get_active_span()
      if (!span$is_recording()) {
        return(onFulfilled)
      }

      function(...) {
        with_active_span(span, {
          onFulfilled(...)
        })
      }
    },
    wrapOnRejected = function(onRejected) {
      force(onRejected)

      span <- get_active_span()
      if (!span$is_recording()) {
        return(onRejected)
      }

      function(...) {
        with_active_span(span, {
          onRejected(...)
        })
      }
    }
  )
}
