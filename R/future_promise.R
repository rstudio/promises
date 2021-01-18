assert_future_version <- local({
  val <- NULL
  function() {
    if (!is.null(val)) return()
    if (packageVersion("future") < "1.21.0") {
      stop("`future` version >= 1.21.0 is required")
    }
    val <<- TRUE
    return()
  }
})
