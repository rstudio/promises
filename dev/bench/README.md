# Benchmarks

Step 0 harness for the promises performance plan. Not part of the package build
(`^dev$` is in `.Rbuildignore`).

## Running

From the package root:

```sh
Rscript dev/bench/00-baseline.R   # scenarios 1-9, saves dev/bench/results/baseline-<date>.rds
Rscript dev/bench/01-profile.R    # Rprof of scenarios 2 and 4, saves dev/bench/profiles/*.Rprof
```

Both benchmark the dev build via `pkgload::load_all()`. View a profile with
`profvis::profvis(prof_input = "dev/bench/profiles/scenario2-resolved-chain.Rprof")`.

`dev/bench/results/` and `dev/bench/profiles/` are gitignored: the `.rds` and
`.Rprof` artifacts are local-only and are not committed. The canonical record
is the results table in this file — re-run after each optimization step and
update the before/after numbers (time, `mem_alloc`, `n_gc`) below.

## Scenarios

1. Promise creation (`promise_resolve(1)`, bare pending `promise()`)
2. Chain throughput, resolved — 1000-link `then()` chain on a resolved promise, drained (headline)
3. Chain throughput, pending — register 1000-link chain on a pending promise, then resolve + drain (registration and settlement reported separately; settlement = combined − registration)
4. Adoption chain — 1000-link chain, every callback returns `promise_resolve(x)`
5. Settlement fan-out — 1000 callbacks on one promise, then resolve + drain
6. `promise_all` / `promise_race` over 100 resolved promises
7. 1000-link chain registered under an active promise domain (non-identity wrappers)
8. `hybrid_then` sync path
9. `later` floor — 1000× `later(function() NULL)` + drain (reference, not a target)

## Baseline (2026-10-26)

Environment: R 4.6.1 (2026-06-24), later 1.4.8, bench 1.1.4, macOS arm64.
Package loaded via `pkgload::load_all()` — **not byte-compiled**; several
planned gains (plain closures vs `rlang::new_function` output) are
compiler-dependent, so installed-package numbers will differ.

| Scenario | Median | mem_alloc (per itr) | n_gc |
|---|---|---|---|
| 1 create `promise_resolve(1)` + drain | 31.8µs | 3.98MB (8779 itr) | 13 |
| 1 create pending `promise()` | 9.2µs | 1.06MB (10000 itr) | 6 |
| 2 resolved chain ×1000 | 66.8ms (~67µs/link) | 744KB | 41 |
| 3 pending register ×1000 | 36.2ms (~36µs/link) | 548KB | 18 |
| 3 pending register + settle ×1000 | 75.6ms (settle ≈ 39.5ms) | 576KB | 30 |
| 4 adoption chain ×1000 | 188ms (~188µs/link) | 1.61MB | 75 |
| 5 fan-out register ×1000 | 54.4ms | 8.26MB | 19 |
| 5 fan-out register + settle | 72.6ms | 8.27MB | 31 |
| 6 `promise_all` ×100 | 6.74ms | 56KB | 8 |
| 6 `promise_race` ×100 | 6.60ms | 56KB | 11 |
| 7 domain chain ×1000 | 85.6ms (~86µs/link) | 559KB | 28 |
| 8 `hybrid_then` sync | 19.6µs | 0B | 7 |
| 9 `later` floor ×1000 | 8.73ms (~8.7µs/task) | 0B | 8 |

Notes:

- The scenario 9 floor implies ~8.7ms of scenario 2's 66.8ms (~1001 tasks) is
  `later` overhead the package cannot remove; targets are net of this floor.
- Scenario 5's 8.26MB registration allocation matches plan finding 5
  (quadratic list copies on `private$onFulfilled`).
- `mem_alloc` for scenarios 1 and 9 is dominated by per-iteration noise /
  non-R allocation (`later()` tasks are C++-side); treat with care.
- Scenario 3 settlement (~39.5ms) is derived by subtracting registration from
  the combined expression.

## Final results (2026-10-27)

Same environment and caveats as the baseline (R 4.6.1, later 1.4.8, bench
1.1.4, macOS arm64, `pkgload::load_all()` — not byte-compiled). This session's
`later` floor measured 4.31ms vs 8.73ms in the baseline session (~2x
machine-state difference), so raw cross-session ratios are approximate; the
per-step interleaved A/B measurements in the step notes below are the
reliable per-change evidence.

| Scenario | Baseline (2026-10-26) | Final | Change |
|---|---|---|---|
| 1 create `promise_resolve(1)` + drain | 31.8µs | 11.9µs | −63% |
| 1 create pending `promise()` | 9.2µs | 8.7µs | −6% |
| 2 resolved chain ×1000 (headline) | 66.8ms | 34.9ms | −48% (1.92x); n_gc 41 → 20 |
| 3 pending register ×1000 | 36.2ms | 19.7ms | −46% |
| 3 register + settle ×1000 | 75.6ms | 38.9ms | −49% (settle 39.5 → 19.3ms) |
| 4 adoption chain ×1000 | 188ms | 64.6ms | −66% (2.91x); mem 1.61 → 1.07MB; n_gc 75 → 41 |
| 5 fan-out register ×1000 | 54.4ms / 8.26MB | 19.8ms / 548KB | −64% / −93% alloc |
| 5 fan-out register + settle | 72.6ms | 21.0ms | −71% |
| 6 `promise_all` ×100 | 6.74ms | 0.82ms | −88% (8.2x); mem 56KB → 1.4KB |
| 6 `promise_race` ×100 | 6.60ms | 0.78ms | −88% (8.5x); mem 56KB → 1.4KB |
| 7 domain chain ×1000 | 85.6ms | 53.4ms | −38% (no regression — improved) |
| 8 `hybrid_then` sync | 19.6µs | 9.3µs | −52% |
| 9 `later` floor ×1000 | 8.73ms | 4.31ms | machine-state delta, not package code |

Targets assessment:

- **≥2x on scenario 2: essentially met.** Raw 1.92x; net of each session's own
  `later` floor (58.1 → 30.6ms promises-side) also 1.90x; compounding the
  per-step same-session A/B ratios (steps 1, 2, 3, 5, 7) gives ≈2.1x. The
  cross-session machine drift (~2x) dominates the residual uncertainty.
- **≥2x on scenario 4: met** (2.91x), with `mem_alloc` −34% and `n_gc` −45%.
- **Scenario 2 allocation:** `n_gc` halved (41 → 20); `bench` `mem_alloc`
  reads 744KB → 881KB, but that instrument proved unreliable for these
  scenarios (step-2 notes: Rprofmem A/B measured +1.7% per link at step 2,
  with reductions expected from steps 3–5; step 5 then removed the fan-out
  copies). Treat the GC halving as the trustworthy signal.
- **Scenario 7 (domain path): no regression** — it improved ~1.6x.

Release-check profile note: scenario 2 re-profiled after all steps — `Promise()`
self-time share is ~0% (not in the top rows). Dominant residuals: later's
C++ `execCallbacks`/`execLater` (~23%, includes GC triggered there), `<GC>`
(~21%), `new.env` (~9%), byte-compiler rows for per-link closures (~9%).
The ~10% criterion for reviving the fewer-closures-per-`Promise()` item is
not met; it stays deferred.

## After each step

| Step | Scenario 2 | Scenario 4 | Scenario 7 | Notes |
|---|---|---|---|---|
| Baseline | 66.8ms | 188ms | 85.6ms | |
| 1 | 51.1ms | 167.7ms | 70.8ms | −23% / −11% / −17% vs baseline; `wrap_callback_reenter` now returns fixed-shape byte-compiled closures with an explicit side, `wrap_with_signature` deleted, `NULL`-domain call-time branch added |
| 2 | 42.1ms | 122ms | 58.0ms | −37% / −35% / −32% vs baseline; median of 3 same-session runs. Domain-free fast path in `self$then` (no `promise()`/`tryCatch`/`onThen`, fused reenter branch, calling convention as data); `promise_resolve`/`promise_reject` skip the constructor. See step-2 notes below |

### Step 2 measurement notes (2026-10-26)

- **Floor shift across sessions.** This session's `later` floor measured
  4.4–5.0ms vs 8.73ms in the baseline session (~1.9x machine-speed
  difference), so raw cross-session comparisons flatter the step. Using each
  session's own floor, the net promises-side share of scenario 2 is
  58.1 → 42.4 (step 1) → 37.4ms (step 2), i.e. −36% vs baseline; scenario 4 is
  179.3 → 159.0 → 117.3ms, i.e. −35% vs baseline. Same-session back-to-back
  A/B (step 1 → step 2, machine under load, floor drifting 11.1 → 15.1ms):
  scenario 2 −24%, scenario 4 −41%, scenario 7 −24%.
- **`bench` `mem_alloc` is unreliable here.** It reported scenario 7 at
  736KB vs the baseline 559KB, but an Rprofmem A/B of the identical scenario-7
  expression shows the domain path's allocation is unchanged (step 1:
  6,091,016 B; step 2: 6,098,464 B, +0.12%). Scenario 2 by Rprofmem A/B:
  6,078,344 → 6,183,888 B (+1.7%, ~105B/link — slightly larger retained
  closure frame, offset by two fewer wrapper closures; allocation reductions
  are expected from steps 3–5 instead).
- Scenario 1 creation: `promise_resolve(1)` + drain 31.8 → 27.8µs;
  bare `promise()` 9.2 → 9.1µs (unchanged, as expected — public `promise()`
  keeps full validation and its `tryCatch`).

| 3 | 39.6ms | 82.0ms | 60.3ms | −41% / −56% / −29% vs baseline; scenario 4 −33% vs step 2 despite a ~1.6x slower `later` floor this session (7.44ms vs 4.4–5.0ms). Scenario 6: `promise_all`/`promise_race` ×100 went 6.74/6.60 → 0.85/0.84ms (−87%), mem 56KB → 1.4KB. Adoption and combinators now subscribe to settlement directly — no throwaway derived promise. See step-3 notes below |
| 4 | 47.3ms | — | — | Target was scenario 1: `promise_resolve(1)` + drain 26.0–26.5 → 14.9–15.2µs (−42%; skips the settlement `later()` task when nobody is listening). Scenario 2 unchanged (delta is machine drift). See step-4 notes below |
| 5 | 35.8ms | 77.7ms | 56.9ms | Target was scenario 5: register 24.6–25.4 → 17.6–20.3ms (−25%), `mem_alloc` 8.27MB → 558KB (−93%). Mechanism differs from the plan — linked-cell prepend, not index assignment; see step-5 notes. All numbers interleaved same-round A/B vs HEAD |
| 6 | — | — | 62.5ms | Target was scenario 7 (no regression >5%): ≈63.5 → 62.5ms vs HEAD, interleaved 6-round A/B, both orders (noise ±5ms; directionally a small win). Pass-through (identity-wrapped) domain chain ≈ −4%. See step-6 notes below |
| 7 | 29ms | — | — | Target was the settlement path: scenario 1 `promise_resolve(1)` + drain 13.3 → 12.5µs (−6%); scenario 2 min-of-3 × 8 alternating rounds 30–31 → 29ms. `is.promising()` S3 dispatch gated behind `is.object()`. See step-7 notes |

### Step 7 measurement notes (2026-10-27)

- `doResolve()`/`doReject()` now run `is.object(value) && is.promising(value)`:
  unclassed values (the common case) skip `UseMethod` dispatch. Mechanism
  micro-benchmark (10k+ itr): `is.promising(1)` ~570–740ns vs
  `is.object(1) && is.promising(1)` ~0–41ns.
- Interleaved A/B vs HEAD (both builds as sibling git worktrees).
  Scenario 1 (`bench::mark`, 10k itr, 11 paired rounds over 2 batches):
  HEAD 13.24–13.49µs, modified 12.46–12.69µs — non-overlapping, ~−0.8µs
  (−6%). Scenario 2: fine-grained protocol (short Rscript per round, warmup +
  min-of-3 `system.time`, 8 alternating rounds): HEAD min 30–31ms, modified
  29ms — modified faster in every round, ≈ −1.5ms, matching the predicted
  1000 × ~0.6–1µs. `mem_alloc`/`n_gc` unchanged, as expected.
- **Measurement caution (matters for step 8):** two `bench::mark` A/B batches
  on this machine produced tightly-clustered but mutually contradictory
  scenario 2/3 results (modified +7ms on s2, −5ms on s3, both label-locked
  across alternating rounds — mechanistically impossible for this change).
  Long `bench::mark` processes (~40s, `memory = TRUE` profiling) appear to
  alias with periodic system load. The short-process, warmup + min-of-N
  protocol above agreed with the mechanism and the scenario 1 result; prefer
  it for sub-5% effects on this machine.
- Compat note drafted for the PR description (also in NEWS): behaviour changes
  only for hypothetical `is.promising()` methods on implicit classes (e.g.
  `is.promising.numeric`); none exist in the ecosystem — mirai/future methods
  are on classed objects, which still dispatch normally.
- Full test suite: 791 passed, 0 failed.

### Step 6 measurement notes (2026-10-27)

- `promiseDomain$onThen()` streamlined: direct per-callback handling replaces
  the `results` list + `vapply(is.null)` filter (the list is now built by
  conditional assignment), and `domain$wrapOnFulfilled`/`wrapOnRejected` are
  skipped when `identical()` to `base::identity` (the `new_promise_domain()`
  default — pass-through domains).
- Benchmark: `bench7.R`-style interleaved A/B (HEAD worktree vs modified),
  3 rounds each run order, 6 total. Scenario 7 (1000-link chain, non-identity
  wrappers): HEAD medians 57.8–66.4ms, modified 58.9–64.3ms — overlapping
  ranges, no regression. Pass-through domain (`new_promise_domain()`
  defaults): ≈ −4%. `resolved_chain` control (untouched fast path) moved with
  run order, not build — machine drift, confirming only same-round comparisons
  are meaningful. `mem_alloc` 5.99 → 5.97MB, `n_gc` 35 → 34 (marginal; the
  per-link saving is one small list + one `vapply`).
- Implementation pitfall found (fixed before commit): domain wrappers are not
  required to force their argument, so `onThen` must never rebind the symbols
  it passes to them (`onFulfilled`/`onRejected`/`onFinally`) — the wrapper's
  lazy reference would see its own output and recurse infinitely. Caught by
  `test-domains.R` (the counting domains don't force); regression test for
  the identity-skip path added ("pass-through (identity-wrapped) domains
  still reenter and propagate").
- Full test suite: 785 passed, 0 failed, identical to HEAD.

### Step 3 measurement notes (2026-10-27)

- **Floor shift again.** This session's `later` floor measured 7.44ms vs
  4.4–5.0ms in the step-2 session, so raw step-2 → step-3 comparisons are
  conservative. Scenario 4 removes the throwaway derived promise per link —
  one fewer `later()` task per link (~4 tasks/link before, ~3 now, so ~1000
  fewer tasks per chain) plus its ~44µs registration and ~31µs settlement
  cost. Raw: 122 → 82.0ms (−33%); using each session's own floor, the
  promises-side share drops from ~103ms to ~60ms (about −40%).
- Scenario 4 `mem_alloc` 1.61 → 1.07MB and `n_gc` 75 → 45 vs the baseline
  session; scenario 2 (39.6ms) and 7 (60.3ms) are untouched paths, their
  differences vs step 2 are session noise from the slower floor.
- Scenario 6 collapses because registration onto settled sources now
  schedules one bare task per input instead of constructing and settling a
  derived promise per input: `promise_all` ×100 6.74 → 0.845ms,
  `promise_race` ×100 6.60 → 0.840ms, `mem_alloc` 56KB → 1.4KB.

### Step 4 measurement notes (2026-10-27)

- Gate the settlement `later()` in `doResolveFinalValue()` on `onFulfilled`
  handlers being present; `doRejectFinalReason()` is unchanged — its nested
  `later()` drives the unhandled-rejection message and must always run.
- Interleaved Rscript A/B vs the pre-step-4 build, 3 rounds: scenario 1
  `promise_resolve(1)` + drain 26.0–26.5 → 14.9–15.2µs (−42%; one fewer
  event-loop task per listener-less settlement). Scenario 2: 50.8–54.6 →
  47.0–48.0ms — the gate adds one `is.null` check per settlement, so treat
  the delta as machine drift. (Absolute numbers this session are ~40% slower
  than the step-5 session for identical code; only same-round comparisons are
  meaningful.)
- NEWS bullet added: `later::loop_empty()` may be `TRUE` immediately after
  `promise_resolve(x)`. Full test suite passes (787 tests).

### Step 5 measurement notes (2026-10-27)

- **The plan's prescribed mechanism was measured and rejected.** The plan
  called for index assignment (`x[[length(x) + 1L]] <- f`), which is amortized
  O(1) only for *local* lists (259µs/186KB per 1000 appends vs 2.0ms/3.88MB
  for `c()`). The handler lists live in the `private` environment, and
  env-bound lists are duplicated on every `[[<-`: measured in place the idiom
  cost 4.4ms/7.9MB per 1000 paired appends — 2x *worse* than `c()` — and
  scenario 5 registration rose to 24.9ms/17.4MB (from 23.5ms/9.4MB).
- **Shipped instead: linked-cell prepend.** Registration stores
  `list(handler, rest)` cells (O(1); ~240µs/46KB per 1000 paired
  registrations). Settlement collects the chain and iterates `rev()` to
  restore registration order (A+ 2.2.6), with a single-handler fast path that
  skips collect/`rev()` for the common one-callback case. An empty chain is
  now `NULL`, not `list()` — step 4's settlement gate is expressed as
  `!is.null(private$onFulfilled)`.
- Interleaved Rscript A/B vs HEAD (step 3), medians of 2–3 rounds: scenario 2
  39.4–46.7 → 35.3–36.3ms; scenario 4 79.6–82.8 → 77.5–77.9ms; scenario 7
  58.8–58.9 → 56.4–57.4ms (no domain-path regression); scenario 5 as in the
  table above. Scenario 2's small win is incidental: `NULL` init saves two
  `list()` allocations per `Promise()`, and the fast path skips `lapply`.
  Full test suite passes (787 tests).

Targets (net of the `later` floor): ≥2x on scenario 2, ≥2x on scenario 4,
meaningfully lower `mem_alloc` and `n_gc` on both; no regression >5% on
scenario 7.
