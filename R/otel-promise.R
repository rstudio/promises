# --- withr-style functions ---------------------------------------------

#' Manage promise domains given an active OpenTelemetry span.
#'
#' @keywords internal
#' @seealso
#' * [`create_otel_active_span_promise_domain()`]
#' * [`local_promise_domain()`]
#' * [`with_promise_domain()`]
#' @example inst/ex-roxygen/otel_create_inactive_span.R
#' @describeIn otel Create (but do not start) an OpenTelemetry span that will be used
#'   within a promise domain. The span will need to be ended manually (`otel::end_span(spn)`).
#'   The returned object is intended to be used with `otel::local_active_span(spn)` or
#'   `otel::with_active_span(spn, expr)`.
#' @param name The name of the span.
#' @param attributes A list of attributes to add to the span. This value will be passed into `otel::as_attributes()`.
#' @param links A list of links to add to the span. Default is `NULL`.
#' @param option Additional options for the span. Default is `NULL`.
#' @param ... Additional arguments passed through or not used, depending on the function.
otel_create_inactive_span <- function(
  name,
  attributes = list(),
  links = NULL,
  option = NULL,
  ...
) {
  otel::start_span(
    name = name,
    attributes = otel::as_attributes(attributes),
    links = links,
    option = option,
    ...
  )
}

#' @keywords internal
#' @param activation_scope The environment in which the span will be activated.
#' @param end_on_exit If `TRUE`, the span will be ended when the activation scope is exited. Default is `FALSE`. When `FALSE`, the span must be ended manually using `otel::end_span(spn)`.
#' @describeIn otel Create and start an active OpenTelemetry span. This span will need to be ended manually (`otel::end_span(spn)`).
otel_start_active_span <- function(
  name,
  ...,
  attributes = list(),
  activation_scope = parent.frame(),
  end_on_exit = FALSE
) {
  otel::start_local_active_span(
    name = name,
    ...,
    activation_scope = activation_scope,
    attributes = otel::as_attributes(attributes),
    end_on_exit = end_on_exit
  )
}


#' @keywords internal
#' @param active_span An active span object.
#' @param ... Additional arguments (not used).
#' @param replace If `TRUE`, replaces the current promise domain with the new one.
#' @param .envir The environment in which to create the local promise domain.
#' @describeIn otel Local promise domain for an active OpenTelemetry span. The
#'   promise domain will be removed when the `.envir` is removed from the stack.
local_otel_active_span_promise_domain <- function(
  active_span,
  ...,
  replace = FALSE,
  .envir = parent.frame()
) {
  stopifnot(length(list(...)) == 0L)

  act_span_pd <- create_otel_active_span_promise_domain(active_span)

  local_promise_domain(
    act_span_pd,
    replace = replace,
    local_envir = .envir
  )

  invisible()
}


#' @keywords internal
#' @param expr An expression to evaluate within the promise domain.
#' @describeIn otel Local promise domain for an active OpenTelemetry span. The
#'   promise domain will be removed when the `.envir` is removed from the stack.
with_otel_active_span_promise_domain <- function(
  active_span,
  expr,
  ...,
  replace = FALSE
) {
  stopifnot(length(list(...)) == 0L)

  act_span_pd <- create_otel_active_span_promise_domain(active_span)

  with_promise_domain(act_span_pd, expr, replace = replace)
}


# --- Helpers ---------------------------------------------

#' Create and manage otel promise domain / spans
#'
#' @param active_span An otel active span object. This span will must be
#'   previously created and must be closed manually.
#' @keywords internal
#' @seealso
#' * [`local_otel_active_span_promise_domain()`]
#' * [`with_otel_active_span_promise_domain()`]
#' @describeIn otel-helpers Creates a promise domain for an active OpenTelemetry span.
create_otel_active_span_promise_domain <- function(
  active_span
) {
  force(active_span)

  new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      # During binding ("then()")
      force(onFulfilled)

      function(...) {
        # During runtime ("resolve()")
        otel::with_active_span(active_span, {
          onFulfilled(...)
        })
      }
    },
    wrapOnRejected = function(onRejected) {
      force(onRejected)

      function(...) {
        otel::with_active_span(active_span, {
          onRejected(...)
        })
      }
    },
    ## TODO: Is this needed?
    # wrapSync = function(expr) {
    #   otel::with_active_span(active_span, {
    #     force(expr)
    #   })
    # },
    "_local_promise_domain" = TRUE
  )
}


#' @keywords internal
#' @param domain A promise domain object.
#' @describeIn otel-helpers Local promise domain for an active OpenTelemetry
#'   span. The promise domain will be removed when the `.envir` is removed from
#'   the stack. This method is currently only compatible with
#'   `create_otel_active_span_promise_domain(active_span)` as we can guarantee
#'   that the domain's `wrapSync` is `base::force`.
local_promise_domain <- function(
  domain,
  ...,
  replace = FALSE,
  local_envir = parent.frame()
) {
  stopifnot(length(list(...)) == 0L)

  if (!identical(domain[["_local_promise_domain"]], TRUE)) {
    if (!identical(domain$wrapSync, base::force)) {
      # If the domain's `wrapSync` is not `base::force`, then we assume it is a
      # custom domain a custom domain that has been created by the otel package.
      # This is to ensure that the domain's `wrapSync` is used correctly.
      stop(
        "The domain's `wrapSync` must be `base::force` to be used within `local_promise_domain()`."
      )
    }
    stop(
      "local_promise_domain() is only compatible with `create_otel_active_span_promise_domain(active_span)`.",
      call. = FALSE
    )
  }

  oldval <- current_promise_domain()
  if (replace) {
    globals$domain <- domain
  } else {
    new_domain <- compose_domains(globals$domain, domain)

    globals$domain <- new_domain
  }

  defer(
    {
      globals$domain <- oldval
    },
    envir = local_envir
  )
}
