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
#' @describeIn otel `r lifecycle::badge("experimental")`
#'
#' Check if OpenTelemetry tracing is enabled
#' @export
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
#' # Start a span for discontiguous work (uncommon; use with caution)
#' spn <- create_ospan("my_operation")
#' with_otel_span_async(spn, {
#'   # ... do some work ...
#' })
#' # ...
#' # End span when ready
#' end_ospan(spn)
#' }
#'
is_otel_tracing <- function() {
  is_installed("otel") && otel::is_tracing_enabled()
}


#' @param name Character string. The name of the span.
#' @param ... Additional arguments passed to [`otel::start_span()`].
#' @param attributes Attributes passed through [`otel::as_attributes()`] (when
#' not `NULL`)
#'
#' For `with_otel_span_async()`, they must be empty.
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
  attributes = NULL
) {
  if (!is_otel_tracing()) {
    return(NULL)
  }

  span <-
    otel::start_span(
      name,
      ...,
      attributes = if (!is.null(attributes)) {
        otel::as_attributes(attributes)
      }
    )
  span
}


#' @describeIn otel `r lifecycle::badge("experimental")`
#'
#' Ends an created OpenTelemetry span for discontiguous operations.
#' @export
end_ospan <- function(span) {
  if (!is_otel_tracing() || is.null(span)) {
    return(invisible())
  }

  otel::end_span(span)
}


#' Execute code within an OpenTelemetry span for asynchronous operations
#'
#' @param expr An expression to evaluate within the span context.
#' @describeIn otel `r lifecycle::badge("experimental")`
#'
#' Creates an OpenTelemetry span, executes the given expression within it, and ends the span.
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
#' @export
with_ospan_async <- function(
  name,
  expr,
  ...,
  attributes = NULL
) {
  if (!is_otel_tracing()) {
    return(force(expr))
  }

  span <- create_ospan(name, ..., attributes = attributes)

  with_otel_span_async(span, expr, auto_end_span = TRUE)
}

#' @describeIn otel `r lifecycle::badge("experimental")`
#'
#' Executes an expression within the context of an active OpenTelemetry span.
#'
#' Adds an "Active OpenTelemetry promise domain" to the expression evaluation.
#' This domain activates the span during promise domain restoration.
#'
#' If `auto_end_span = TRUE`, the `span` will be automatically ended either when
#' the function exits (for synchronous operations) or when a returned promise
#' completes (for asynchronous operations).
#' @param auto_end_span Logical, \[`FALSE`\]; If `TRUE`, the span will
#' be ended either when the function exits (for synchronous operations) or when
#' a returned promise completes (for asynchronous operations).
#' @export
with_otel_span_async <- function(span, expr, ..., auto_end_span = FALSE) {
  check_dots(...)

  # Early return if we just need the promise domain
  if (!isTRUE(auto_end_span)) {
    return(
      with_ospan_promise_domain(span, expr)
    )
  }

  needs_cleanup <- TRUE
  cleanup <- function() {
    end_ospan(span)
  }
  on.exit(if (needs_cleanup) cleanup(), add = TRUE)

  with_ospan_promise_domain(span, {
    result <- force(expr)

    if (is.promising(result)) {
      needs_cleanup <- FALSE

      result <- finally(result, cleanup)
    }

    result
  })
}


# # TODO: Set attributes on the current active span
# # 5. Set attributes on the current active span
# set_ospan_attrs(status = 200L)

# -- Helpers --------------------------------------------------------------

# Executes an expression within the context of an active OpenTelemetry span.
with_ospan_promise_domain <- function(span, expr) {
  act_span_pd <- create_otel_active_span_promise_domain(span)

  new_domain <- compose_domains(
    current_promise_domain(),
    act_span_pd
  )
  with_promise_domain(new_domain, expr, replace = TRUE)
}

#' Creates a promise domain that activates the given OpenTelemetry span.
#'
#' @param span An OpenTelemetry span object.
#' @noRd
create_otel_active_span_promise_domain <- function(span) {
  force(span)

  new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      force(onFulfilled)
      function(...) {
        otel::with_active_span(span, {
          onFulfilled(...)
        })
      }
    },
    wrapOnRejected = function(onRejected) {
      force(onRejected)
      function(...) {
        otel::with_active_span(span, {
          onRejected(...)
        })
      }
    },
    wrapSync = function(expr) {
      otel::with_active_span(span, {
        force(expr)
      })
    }
  )
}
