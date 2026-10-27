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

| 3 | | | | |
| 4 | | | | |
| 5 | | | | |
| 6 | | | | |
| 7 | | | | |

Targets (net of the `later` floor): ≥2x on scenario 2, ≥2x on scenario 4,
meaningfully lower `mem_alloc` and `n_gc` on both; no regression >5% on
scenario 7.
