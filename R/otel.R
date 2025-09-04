#' @importFrom otel
#'   as_attributes
#'   end_span
#'   get_active_span
#'   is_tracing_enabled
#'   start_span
#'   with_active_span
NULL


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
