# Using local scope avoids an environment object lookup on each call.
# Inspired by httr2:::get_tracer(). Taken from Shiny
# Benchmark: https://github.com/rstudio/shiny/pull/4269/files#diff-0cc4a76032b57fcc125d41dfa3fb0f0c39976bb00a1d84bb56a0b77c331ce2d1R42

testthat__is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

get_tracer <- local({
  tracer <- NULL
  function() {
    if (!is.null(tracer)) {
      return(tracer)
    }
    if (testthat__is_testing()) {
      # Don't cache the tracer in unit tests. It interferes with tracer provider
      # injection in otelsdk::with_otel_record().
      return(otel::get_tracer())
    }
    tracer <<- otel::get_tracer()
    tracer
  }
})
