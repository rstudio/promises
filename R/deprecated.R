#' `r lifecycle::badge("deprecated")`: Deprecated functions in promises
#'
#' These functions have been deprecated and will be removed in a future version.
#' Please use the suggested alternatives instead.
#'
#' @name promises-deprecated
#' @keywords internal
NULL

#' @describeIn promises-deprecated Use `with_otel_span()` instead
#' @export
with_ospan_async <- function(
  name,
  expr,
  ...,
  tracer,
  attributes = NULL
) {
  lifecycle::deprecate_warn(
    "1.5.0",
    "with_ospan_async()",
    "with_otel_span()"
  )
  with_otel_span(
    name = name,
    expr = expr,
    ...,
    tracer = tracer,
    attributes = attributes
  )
}

#' @describeIn promises-deprecated Use `with_otel_promise_domain()` instead
#' @export
with_ospan_promise_domain <- function(expr) {
  lifecycle::deprecate_warn(
    "1.5.0",
    "with_ospan_promise_domain()",
    "with_otel_promise_domain()"
  )
  with_otel_promise_domain(expr)
}

#' @describeIn promises-deprecated Use `local_otel_promise_domain()` instead
#' @export
local_ospan_promise_domain <- function(envir = parent.frame()) {
  lifecycle::deprecate_warn(
    "1.5.0",
    "local_ospan_promise_domain()",
    "local_otel_promise_domain()"
  )
  local_otel_promise_domain(envir = envir)
}
