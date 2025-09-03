.on_load_exprs <- list()
# Register an expression to be evaluated when the package is loaded (in the
# .onLoad function).
on_load <- function(expr) {
  .on_load_exprs[[length(.on_load_exprs) + 1]] <<- substitute(expr)
}

.onLoad <- function(libname, pkgname) {
  for (expr in .on_load_exprs) {
    eval(expr, envir = environment(.onLoad))
  }
}
