#' @importFrom otel
#'   as_attributes
#'   end_span
#'   get_active_span
#'   is_tracing_enabled
#'   start_span
#'   with_active_span
NULL

# For `otel::default_tracer_name()`
otel_tracer_name <- "co.posit.r-package.shiny"


#' `r lifecycle::badge("experimental")` OpenTelemetry integration
#'
#' @description
#' \pkg{otel} provides tools for integrating with OpenTelemetry, a framework for
#' observability and tracing in distributed systems.
#'
#' These methods are intended to enhance the framework to be used with the
#' \pkg{promises} package, not as a generic replacement.
#'
#' Dev note - Barret 2025/09: This ospan handoff promise domain topic is complex
#' and has been discussed over many hours. As even advanced Shiny/R developers
#' are not even aware of promise domains, this topic requires more in-depth
#' documentation and examples.
#'
#' @section Execution model for with_ospan_promise_domain():
#'
#' ### Definitions
#' * Promise domain: An environment in which has setup/teardown methods. These
#'   environments can be composed together to facilitate execution context for
#'   promises. In normal R execution, this can be achieved with scope / stack.
#'   But for complex situations, such as the currently open graphics device,
#'   async operations require promise domains to setup/teardown these contexts
#'   to function properly. Otherwise a multi-stage promise that adds to the
#'   graphics device at each stage will only ever print to the most recently
#'   created graphics device, not the associated graphics device. These promise
#'   domains are not automatically created, they must be manually added to the
#'   execution stack, for example `with_ospan_promise_domain()` does this for
#'   OpenTelemetry spans ("ospan").
#' * Promise domain restoration: When switching from one promise chain to
#'   another, the execution context is torn down and then re-established. This
#'   re-establishment is called "promise domain restoration". During this
#'   process, the promise domains are restored in their previously established
#'   combination order.
#' * Promise chain: A set of promise objects to execute over multiple async
#'   ticks.
#' * Async tick: the number of times an event loop must run to move computation
#'   forward. (Similar to a Javascript event loop tick.)
#' * `then()` promise domain capture: When `then()` is called, it will capture
#'   the current promise domain. This promise domain is restored (only if
#'   needed) when evaluating the given `onFulfilled` and `onRejected` callbacks.
#'   This captured promise domain does not go into any downstream promise chain
#'   objects. The only way the promise domain is captured is exactly when the
#'   `then()` method is called.
#'
#' `with_ospan_promise_domain()` creates a promise domain that restores the
#' currently active OpenTelemetry span from when a call to `promises::then()` is
#' executed. Given the special circumstance where only the current ospan is
#' needed to continue recording (not a full ancestry tree of ospans), we can
#' capture _just_ the current ospan and reactivate that ospan during promise
#' domain restoration.
#'
#' ### Complexity
#'
#' When reactivating the `k`th step in a promise chain, the currently active
#' ospan (during the call to `then()`) will be reactivated during promise domain
#' restoration (`O(1)`). To restore a chain of promises, the active ospan will
#' be restored at each step (`O(n)`) due to the **`n`** calls to wrapping each
#' `onFulfilled` and `onRejected` callbacks inside `then()`.
#'
#' If we did NOT have a handoff promise domain for ospan restoration, a regular
#' promise domain approach would be needed at each step to restore the active
#' ospan. Each step would call `with_active_span()` `k` times (`O(k)`, where as
#' handoff domain computes in `O(1)`). Taking a step back, to restore each ospan
#' at for every step in a promise chain would then take `O(n^2)` time, not
#' `O(n)`. The standard, naive promise domain approach does not scale for
#' multiple similar promise domain restorations.
#'
#'
#' ### Execution model
#'
#' 1. `with_ospan_promise_domain(expr)` is called.
#'    * The following steps all occur within `expr`.
#' 2. Create an ospan object using `create_ospan()` or `otel::start_span()`.
#'    * We need the ospan to be active during the a followup async operation.
#'      Therefore, `otel::start_local_active_span()` is not appropriate as the
#'      ospan would be ended when the function exits, not when the promise chain
#'      resolves.
#' 2. Be sure your ospan is activated before calling `promises::then()`.
#'    * Activate it using `with_ospan_async(name, expr)` (which also
#'      creates/ends the ospan) or `otel::with_active_span(span, expr)`.
#' 3. Call `promises::then()`
#'   * When `promises::then()` is called, the two methods (`onFulfilled` and
#'     `onRejected`) capture the currently active spans. (Performed by the
#'     initial `with_ospan_promise_domain()`)
#' 4. During reactivation of the promise chain step, the previously captured
#'    ospan is reactivated via `with_active_span()`. (Performed by the initial
#'    `with_ospan_promise_domain()`)
#'
#' @section OpenTelemetry span compatibility:
#'
#' For ospan objects to exist over may async ticks, the ospan must be created
#' using `create_ospan()` (or `otel::start_span()`) and later ended using
#' `end_ospan()` (or `otel::end_span()`). Ending the ospan must occur **after**
#' any promise chain work has completed.
#'
#' If we were to instead use `otel::start_local_active_span()`, the ospan would
#' be ended when the function exits, not when the promise chain completes. Even
#' though the local ospan is created, activated, and eventually ended, the ospan
#' will not exist during reactivation of the ospan promise domain.
#'
#' `with_ospan_async()` is a convenience method that creates, activates, and
#' ends the ospan only after the returned promise (if any) resolves. It also
#' properly handles both synchronous (ending the ospan within `on.exit()`) and
#' asynchronous operations (ending the ospan within `promises::finally()`).
#'
#'
#'
#'
#' @describeIn otel `r lifecycle::badge("experimental")`
#'
#' Creates an OpenTelemetry span, executes the given expression within it, and
#' ends the span. This method requires the use of `with_ospan_promise_domain()`
#' to be within the execution stack.
#'
#' This function is designed to handle both synchronous and asynchronous
#' (promise-based) operations. For promises, the span is automatically ended
#' when the promise resolves or rejects.
#'
#' Returns the result of evaluating `expr`. If `expr` returns a promise, the
#' span will be automatically ended when the promise completes.
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
#' @param tracer An `{otel}` tracer. If not provided, code will be executed
#'   under a `{promises}` package tracer. It is strongly recommended to provide
#'   your own tracer from your own package. See [`otel::get_tracer()`] for more
#'   details.
#' @param attributes Attributes passed through [`otel::as_attributes()`] (when
#'   not `NULL`)
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Set up promise domain to be able to restore active otel span
#' # * Makes a small promise domain that will restore the active span from when
#' #   `promises::then()` is called
#' # * Does not activate any spans, the author must do that themselves
#' #   (or use `with_ospan_async()`)
#' with_ospan_promise_domain({
#'
#'   # ... deep inside some code execution ...
#'
#'   # Synchronous operation
#'   # * Creates `my_operation` span
#'   result <- with_ospan_async("my_operation", {
#'     # ... do some work ...
#'     42
#'   })
#'
#'   # Asynchronous operation
#'   # * Creates `async_op` ospan
#'   # * Automatically ends the ospan (`async_op`) when the promise (p)
#'   #   resolves or rejects
#'
#'   # t0.0
#'   with_ospan_async("async_op", {
#'     # ... return a promise ...
#'     p <- # t0.1
#'       init_async_work() |> # t0.2
#'       then(some_async_work) # t1.0
#'   }) # t0.3
#'   p2 <- p |> # t0.4
#'     then(more_async_work) # t2.0
#' })
#' }
with_ospan_async <- function(
  name,
  expr,
  ...,
  tracer = NULL,
  attributes = NULL
) {
  if (!is_tracing_enabled(tracer)) {
    return(force(expr))
  }

  span <- create_ospan(name, ..., tracer = tracer, attributes = attributes)

  needs_cleanup <- TRUE
  cleanup <- function() {
    end_ospan(span) #, tracer = tracer)
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
  tracer = NULL,
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
end_ospan <- function(span) {
  if (
    is.null(span)
    #  || !is_tracing_enabled(tracer)
  ) {
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

      ## Motivation on why we can't use
      ## `promises_otel_tracer()$get_active_span()` here to save 10 microseconds:
      ## https://github.com/r-lib/otel/issues/31#issuecomment-3250669840
      #> ❯❯ bench::mark(promises_otel_tracer()$get_active_span(), get_active_span())
      #> # A tibble: 2 × 13
      #>   expression            min  median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result     memory     time
      #>   <bch:expr>         <bch:> <bch:t>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>     <list>     <list>
      #> 1 promises_otel_tra…  4.8µs  6.52µs   144270.        0B     14.4  9999     1     69.3ms <otl_spn_> <Rprofmem> <bench_tm>
      #> 2 get_active_span()  14.3µs 15.13µs    64876.    7.02KB     38.9  9994     6      154ms <otl_spn_> <Rprofmem> <bench_tm>
      # ℹ 1 more variable: gc <list>
      ## tl/dr: Speed comes at the cost of safety. Just use this for now as it is only a single cost on restore.

      # Instead, use `get_active_span()` directly so that the recording status
      # can be changed over time for testing and within the terminal. The 10
      # microseconds is not worth the inconsistent behavior or the time to debug
      # the inconsistent behavior.

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
