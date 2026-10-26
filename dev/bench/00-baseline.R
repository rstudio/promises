#!/usr/bin/env Rscript
# Step 0 baseline benchmark harness (see the performance plan).
#
# Run from the package root:
#   Rscript dev/bench/00-baseline.R
#
# Measures the 9 scenarios from the plan (time, mem_alloc, n_gc) against the
# dev build (pkgload::load_all) and saves results to dev/bench/results/.
# Re-run after each optimization step; keep the comparison table in
# dev/bench/README.md up to date.

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the promises package root.", call. = FALSE)
}

pkgload::load_all(quiet = TRUE)
library(bench)

dir.create("dev/bench/results", showWarnings = FALSE, recursive = TRUE)

N_CHAIN <- 1000L # links in then() chains
N_FAN <- 1000L # callbacks in settlement fan-out
N_COMB <- 100L # promises in promise_all / promise_race

drain <- function() {
  while (!later::loop_empty()) {
    later::run_now()
  }
}

# Externally-resolvable promise (mirrors tests/testthat/helper.R ext_promise)
new_ext <- function() {
  e <- new.env()
  p <- promises::promise(function(resolve, reject) {
    e$resolve <- resolve
    e$reject <- reject
  })
  list(promise = p, resolve = e$resolve, reject = e$reject)
}

build_chain <- function(p, n, f) {
  for (i in seq_len(n)) {
    p <- promises::then(p, f)
  }
  p
}

pass_through <- function(x) x
adopt <- function(x) promises::promise_resolve(x)

mark <- function(..., min_iterations = 10, min_time = 0.3) {
  bench::mark(
    ...,
    check = FALSE,
    memory = TRUE,
    filter_gc = FALSE,
    min_iterations = min_iterations,
    min_time = min_time
  )
}

# -- Scenario 1: promise creation ------------------------------------------
# `promise_resolve(1)` schedules a settlement task even with no listeners
# (plan finding 8), so drain it. The bare constructor stays pending and
# schedules nothing.
s1 <- mark(
  create_resolve = {
    promises::promise_resolve(1)
    drain()
  },
  create_pending = {
    promises::promise(function(resolve, reject) NULL)
  }
)

# -- Scenario 2: chain throughput, resolved (headline) ----------------------
s2 <- mark(
  resolved_chain = {
    p <- build_chain(promises::promise_resolve(1), N_CHAIN, pass_through)
    drain()
    p
  }
)

# -- Scenario 3: chain throughput, pending (Shiny-typical) ------------------
# Reported separately: registration = pending_register; settlement =
# pending_register_settle - pending_register.
s3 <- mark(
  pending_register = {
    ext <- new_ext()
    build_chain(ext$promise, N_CHAIN, pass_through)
  },
  pending_register_settle = {
    ext <- new_ext()
    p <- build_chain(ext$promise, N_CHAIN, pass_through)
    ext$resolve(1)
    drain()
    p
  }
)

# -- Scenario 4: adoption chain (plan finding 7) ----------------------------
s4 <- mark(
  adoption_chain = {
    p <- build_chain(promises::promise_resolve(1), N_CHAIN, adopt)
    drain()
    p
  }
)

# -- Scenario 5: settlement fan-out (plan finding 5) ------------------------
s5 <- mark(
  fanout_register = {
    ext <- new_ext()
    for (i in seq_len(N_FAN)) {
      promises::then(ext$promise, pass_through)
    }
  },
  fanout_register_settle = {
    ext <- new_ext()
    for (i in seq_len(N_FAN)) {
      promises::then(ext$promise, pass_through)
    }
    ext$resolve(1)
    drain()
  }
)

# -- Scenario 6: promise_all / promise_race ---------------------------------
# Pre-built resolved promises; registration onto settled sources is the
# steady-state hot path.
ps <- lapply(seq_len(N_COMB), promises::promise_resolve)
drain()
s6 <- mark(
  promise_all = {
    promises::promise_all(.list = ps)
    drain()
  },
  promise_race = {
    promises::promise_race(.list = ps)
    drain()
  }
)

# -- Scenario 7: then() with an active promise domain -----------------------
# Non-identity wrappers (Shiny-style) so the full domain path is exercised;
# must not regress (target: within 5%).
wrap <- function(f) {
  force(f)
  function(...) f(...)
}
dom <- promises::new_promise_domain(
  wrapOnFulfilled = wrap,
  wrapOnRejected = wrap
)
s7 <- mark(
  domain_chain = {
    p <- promises::with_promise_domain(
      dom,
      build_chain(promises::promise_resolve(1), N_CHAIN, pass_through)
    )
    drain()
    p
  }
)

# -- Scenario 8: hybrid_then sync path --------------------------------------
s8 <- mark(
  hybrid_then_sync = promises::hybrid_then(1L, pass_through),
  min_iterations = 1000
)

# -- Scenario 9: later floor ------------------------------------------------
# Not a target; used to compute the promises-side share (a 1000-link chain
# has ~1001 tasks promises cannot remove).
s9 <- mark(
  later_floor = {
    for (i in seq_len(N_CHAIN)) {
      later::later(function() NULL)
    }
    drain()
  }
)

# -- Metadata and output ----------------------------------------------------
meta <- list(
  date = Sys.time(),
  r_version = R.version.string,
  later_version = as.character(utils::packageVersion("later")),
  bench_version = as.character(utils::packageVersion("bench")),
  # Bytecode-compile status matters: several planned gains (plain closures vs
  # rlang::new_function output) are compiler-dependent. Under load_all the
  # package functions are NOT byte-compiled.
  then_byte_compiled = typeof(body(promises::then)) == "bytecode",
  promise_impl_byte_compiled = typeof(body(promises:::Promise)) == "bytecode"
)

scenarios <- list(
  s1_creation = s1,
  s2_resolved_chain = s2,
  s3_pending_chain = s3,
  s4_adoption_chain = s4,
  s5_fanout = s5,
  s6_combinators = s6,
  s7_domain_chain = s7,
  s8_hybrid_then = s8,
  s9_later_floor = s9
)
results <- do.call(
  rbind,
  Map(cbind, scenario = names(scenarios), scenarios)
)

out_path <- file.path(
  "dev/bench/results",
  sprintf("baseline-%s.rds", format(Sys.Date(), "%Y%m%d"))
)
saveRDS(list(meta = meta, results = results), out_path)

cat("\n== Metadata ==\n")
str(meta)
cat("\n== Results (median in seconds; mem_alloc, n_gc per bench summary) ==\n")
summary_df <- data.frame(
  scenario = results$scenario,
  expression = vapply(
    results$expression,
    \(e) paste(deparse(e), collapse = " "),
    character(1)
  ),
  median_s = signif(as.numeric(results$median), 4),
  mem_alloc = as.character(results$mem_alloc),
  n_gc = results$n_gc,
  n_itr = results$n_itr,
  row.names = NULL
)
print(summary_df, right = FALSE)
cat("\nSaved to", out_path, "\n")
