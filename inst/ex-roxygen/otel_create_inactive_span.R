\dontrun{
# Example usage:
library(promises)
library(otel)

# Create an active span
inactive_span <- otel_create_inactive_span("example_span")
# Use the local promise domain
local_otel_active_span_promise_domain(inactive_span)

# Create a promise within the active span domain
p <- promise_resolve(TRUE) %...>%
  {
    # This will run within the active span domain
    # Run user code here
    Sys.sleep(1)
    "Done"
  } %...>%
  {
    # This will run within the active span domain
    print(.)
  }

p <- finally(p, function() {
  # This will run after the promise is resolved
  otel::end_span(inactive_span)
})
}
