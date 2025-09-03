# Set during `.onLoad()`
IS_OTEL_TRACING <- NULL


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
#' # Start a span for discontiguous work (uncommon; use with caution)
#' spn <- create_ospan("my_operation")
#' with_ospan_promise_domain(spn, {
#'   # ... do some work ...
#' })
#' # ...
#' # End span when ready
#' end_ospan(spn)
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
#' @param attributes Attributes passed through [`otel::as_attributes()`] (when
#' not `NULL`)
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

  needs_cleanup <- TRUE
  cleanup <- function() {
    end_ospan(span)
  }
  on.exit(if (needs_cleanup) cleanup(), add = TRUE)

  result <- with_ospan_promise_domain(span, expr)

  if (is.promising(result)) {
    needs_cleanup <- FALSE

    result <- finally(result, cleanup)
  }

  result
}

#' @describeIn otel `r lifecycle::badge("experimental")`
#'
#' Check if OpenTelemetry tracing is enabled.
#'
#' Note: The returned value is precalculated during `.onLoad()`. If `{otel}` is
#' installed after the `{promises}` package has been loaded, please restart your
#' R session to retrieve an updated value.
#' @export
is_otel_tracing <- function() {
  IS_OTEL_TRACING
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
  attributes = NULL
) {
  if (!is_otel_tracing()) {
    return(NULL)
  }

  span <-
    otel::start_span(
      name,
      ...,
      ## Use when otel v0.3.0 is released: https://github.com/r-lib/otel/commit/43e59c45a7de50cfd8af95f73d45f9899f957b44
      # attributes = otel::as_attributes(attributes)
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


#' @describeIn otel `r lifecycle::badge("experimental")`
#'
#' Executes an expression within the context of an active OpenTelemetry span.
#'
#' Adds an "Active OpenTelemetry promise domain" to the expression evaluation.
#' This span will be reactivated during promise domain restoration.
#' @param span An OpenTelemetry span object.
#' @export
with_ospan_promise_domain <- function(span, expr) {
  if (!inherits(span, "otel_span")) {
    stop("`span=` must be an {otel} span object")
  }

  act_span_pd <- create_otel_active_span_promise_domain(span)
  with_promise_domain(act_span_pd, expr)
}


# # TODO: Set attributes on the current active span
# # 5. Set attributes on the current active span
# set_ospan_attrs(status = 200L)

# -- Helpers --------------------------------------------------------------

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
