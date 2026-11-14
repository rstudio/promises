#!/usr/bin/env Rscript
# Step 0 profiles for the two headline scenarios (see the performance plan):
#   scenario 2 - 1000-link then() chain on a resolved promise
#   scenario 4 - 1000-link adoption chain (callbacks return promise_resolve)
#
# Run from the package root:
#   Rscript dev/bench/01-profile.R
#
# Saves Rprof files to dev/bench/profiles/ for before/after comparison.

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the promises package root.", call. = FALSE)
}

pkgload::load_all(quiet = TRUE)

dir.create("dev/bench/profiles", showWarnings = FALSE, recursive = TRUE)

N_CHAIN <- 1000L
REPS <- 20L

drain <- function() {
  while (!later::loop_empty()) {
    later::run_now()
  }
}

build_chain <- function(p, n, f) {
  for (i in seq_len(n)) {
    p <- promises::then(p, f)
  }
  p
}

profile <- function(name, expr) {
  path <- file.path("dev/bench/profiles", sprintf("%s.Rprof", name))
  utils::Rprof(
    path,
    interval = 0.001,
    memory.profiling = TRUE,
    gc.profiling = TRUE,
    line.profiling = TRUE
  )
  for (i in seq_len(REPS)) {
    force(expr)
  }
  utils::Rprof(NULL)
  cat("Wrote", path, "\n")
  invisible(path)
}

# Scenario 2: resolved chain
profile("scenario2-resolved-chain", {
  p <- build_chain(promises::promise_resolve(1), N_CHAIN, function(x) x)
  drain()
})

# Scenario 4: adoption chain
profile("scenario4-adoption-chain", {
  p <- build_chain(
    promises::promise_resolve(1),
    N_CHAIN,
    function(x) promises::promise_resolve(x)
  )
  drain()
})
