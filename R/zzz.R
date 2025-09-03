.onLoad <- function(libname, pkgname) {
  # Inspriation: https://github.com/r-lib/mirai/blob/7716d1653991caffc6f38d52f44ec1bcf5e9cd90/R/mirai-package.R#L48C3-L48C90
  # If `is_installed(otel)` is called, it will still touch disk every time until the package is loaded: expensive.
  # If `otel::is_tracing_enabled()` is called, it is also expensive to call repeatedly (35μs). Better to just use a singular value between packages.
  #
  # For testing, we can use a local mock to allow for testing (given otel/otelsdk exist)
  # ```r
  # skip_if_not_installed("otel")
  # skip_if_not_installed("otelsdk")
  #
  # local_mocked_bindings(
  #   is_otel_tracing = function() TRUE
  # )
  #
  # # TESTING CODE!
  # ```
  IS_OTEL_TRACING <<-
    requireNamespace("otel", quietly = TRUE) && otel::is_tracing_enabled()
}
