---
title: "CI and tooling"
description: "What runs in GitHub Actions, how docs, formatting, spelling, the pkgdown site, vignettes, NEWS, revdeps, and CRAN notes are handled. Read before changing workflows, DESCRIPTION Config fields, build ignores, or preparing a release."
---

# CI and tooling

## Overview

CI uses the Shiny team's reusable workflows from
[rstudio/shiny-workflows](https://github.com/rstudio/shiny-workflows) at tag
`@v1`. One workflow file in this repo calls four jobs. The "routine" job can
push fix-up commits (formatting, docs) back to a PR branch.

## Key files

- `.github/workflows/R-CMD-check.yaml` - the main workflow ("Package checks").
- `.github/workflows/rhub.yaml` - R-hub, manual only (`workflow_dispatch`).
- `.github/CODE_OF_CONDUCT.md`.
- `AGENTS.md`, `memory-bank/`, `.agents/skills/` - agent and contributor docs; all are in `.Rbuildignore`.
- `DESCRIPTION` - `Config/*` fields, `VignetteBuilder`, `Language`.
- `.Rbuildignore` - what `R CMD build` leaves out.
- `pkgdown/_pkgdown.yml`, `pkgdown/_brand.yml`, `pkgdown/extra.js`,
  `pkgdown/favicon/`, `index.md` (site home page).
- `inst/WORDLIST`, `tests/spelling.R`.
- `R/staticimports.R` (generated), `R/utils.R` (`# @staticimports` header).
- `NEWS.md`, `cran-comments.md`, `revdep/`.
- `vignettes/*.Rmd`, `vignettes/future_promise/`, `vignettes/*.png`.
- `R/promises-package.R` - `release_bullets()` (extra items for
  `usethis::use_release_issue()`).

## How it works

### GitHub Actions (R-CMD-check.yaml)

Triggers: push to `main` and `rc-**`, every pull request, every Monday 08:00
UTC, and manual.

| Job | Reusable workflow | Inputs |
|---|---|---|
| `website` | `website.yaml@v1` | `check-title: false` |
| `routine` | `routine.yaml@v1` | `format-r-code: true` |
| `R-CMD-check` | `R-CMD-check.yaml@v1` | `cache-version: "regular-1"` |
| `R-CMD-check-with-dont-test` | `R-CMD-check.yaml@v1` | `extra-check-args: '"--run-donttest"'`, `cache-version: "donttest-1"` |

What the shared workflows do (read from shiny-workflows `v1`):

- **routine**, in order: format R code with air; on `rc-v*` branches run
  `urlchecker::url_update()` and set the version from the branch name;
  `usethis::use_tidy_description()`; `devtools::document()`; build
  `README.Rmd` if present (there is none here); optimize PNGs in
  `man/figures/` and `vignettes/` with `oxipng`; check `staticimports` is up
  to date (warning only). Each step commits its changes. On a same-repo PR the
  commits are pushed back. On a push event or a fork PR, any new commit makes
  the job fail with "verify no new commits".
- **R-CMD-check**: a matrix across OSes and R versions (release and older
  releases). The second job adds `--run-donttest`.
- **website**: builds pkgdown; deploys with
  `pkgdown::deploy_to_branch()` to `gh-pages` when it should deploy, else only
  verifies. `check-title: false` sets
  `options(rmarkdown.html_vignette.check_title = FALSE)`.

### Formatting (air)

The `format-r-code` action installs the latest
[air](https://github.com/posit-dev/air) and runs `air format .`, then commits
changed `.R` files as "`air format` (GitHub Actions)". There is no
`air.toml` in the repo, so air defaults apply. The whole code base was
formatted with air in
[rstudio/promises#133](https://github.com/rstudio/promises/pull/133). Locally:
`air format .` (or `air format --check .`; it passes on the current tree).

`.vscode/settings.json` only has C/C++ formatter settings, left from when the
package had C++ code.

### Docs: roxygen2

- `Config/roxygen2/markdown: TRUE`, `Config/roxygen2/version: 8.0.0`.
- Run `devtools::document()` after any roxygen change. It rewrites
  `NAMESPACE` and `man/`. Never edit those by hand.
- CI also runs `devtools::document()` and commits the result, so a PR that
  forgets will get a bot commit (or a failure on fork PRs).
- `@keywords internal` is used for `WorkQueue`, the package doc, and
  `promises-deprecated`.

### staticimports

`R/utils.R` starts with `# @staticimports pkg:staticimports` and lists the
helpers it wants. `staticimports::import()` regenerates
`R/staticimports.R` ("do not edit by hand"). The routine job warns when it is
out of date; `release_bullets()` also reminds you.

### pkgdown site

- Config lives in `pkgdown/_pkgdown.yml` (not the repo root). Whole `pkgdown/`
  folder is in `.Rbuildignore`.
- Bootstrap 5, light/dark switch, colors and fonts from
  `pkgdown/_brand.yml` via `template: bslib: brand:`.
- `pkgdown/extra.js` swaps the navbar logo to
  `reference/figures/logo-dark.svg` in dark mode.
- `development: mode: auto`.
- `reference:` groups every exported topic. A new exported function needs a
  new entry here or pkgdown fails with "topics missing from index".
- `navbar:` lists vignettes by file name; `redirects:` maps old article URLs
  to the numbered `promises_0N_*.html` names. Renaming a vignette needs both.
- `index.md` is the site home page; `README.md` is for GitHub.
- `Config/Needs/website: brand.yml, rsconnect, tidyverse/tidytemplate` tells
  the website job what extra packages to install.

### Spelling

`Language: en-US` in `DESCRIPTION`. `tests/spelling.R` runs the spell check on
docs and vignettes as part of `R CMD check` (skipped on CRAN, fails on
error). Add real words to `inst/WORDLIST`, one per line; the list is not
sorted. Check with `spelling::spell_check_package()` (clean today).

### Vignettes

- Nine `.Rmd` files, all `output: rmarkdown::html_vignette` and
  `%\VignetteEngine{knitr::rmarkdown}`; `VignetteBuilder: knitr, rmarkdown`.
- Almost no code is run: only `promises_05b_future_promise.Rmd` has R chunks,
  two `vembedr::embed_vimeo()` calls with
  `eval = requireNamespace("vembedr", quietly = TRUE)`. All other code is in
  plain `r` blocks. So vignette builds do not need future, mirai, or shiny.
- `promises_05b_future_promise.Rmd` lists `future_promise/*.png` in
  `resource_files`. `vignettes/future_promise/*.key` (Keynote source) and
  `*.R` (`plots.R`) are in `.Rbuildignore`.
- Number prefixes (`01`...`08`, `05a`/`05b`) set the order on the site.

### NEWS.md

Style (from the current file):

- Top heading `# promises (development version)`; released versions are
  `# promises X.Y.Z`. Two blank lines between versions.
- Optional `##` groups such as `## Breaking changes`, `## New features`,
  `## Minor improvements and fixes`, `## Life cycle changes`, or a topic
  (`## OpenTelemetry`).
- Bullets start with `* `, name functions in backticks with `()`, and end
  with the PR number: `(#191)`. Thanks look like
  `(Thank you, @RLesur! #47)`.
- Newer entries wrap at about 80 columns; older ones are one long line.
- The `release-update-news` skill can polish bullets.

### Release files

- `cran-comments.md` - notes for CRAN, last written for 1.5.0 (dated
  2025-10-31). Has R CMD check and revdep summary.
- `revdep/` - output of `revdepcheck` cloud runs (`README.md`, `cran.md`,
  `failures.md`, `problems.md`). `revdep/.gitignore` drops `data.sqlite` and
  `*.noindex`. Last run: 49 revdeps, 0 new problems; GSVA and Prostar failed
  to install in both versions.
- `release_bullets()` in `R/promises-package.R`: update staticimports, and
  revert two temporary future workarounds (example back to `\donttest{}`,
  remove the macOS test skip).
- `rc-v*` branches trigger version and URL updates in the routine job.

## Pitfalls

- **`.Rbuildignore` does not list `.context`.** Checked by running
  `R CMD build` in a temp folder: the tarball contained
  `promises/.context/...`. In a git worktree it also contains the `.git`
  *file*. Both would show up as hidden files in `R CMD check`. Add
  `^\.context$` (and similar) before release builds from a worktree.
- **Some `.Rbuildignore` patterns have unescaped dots** (`^.claude$`,
  `^.posit$`, `^.vscode$`). They still work but match more than meant.
- **Installed roxygen2 may be newer than 8.0.0** (8.1.1 here). Running
  `devtools::document()` then rewrites `Config/roxygen2/version` in
  `DESCRIPTION`. Do not commit that bump by accident.
- **The routine job commits for you.** If you push to `main` directly and the
  job would change files (formatting, docs, DESCRIPTION order), the job fails
  instead. Run `air format .` and `devtools::document()` before pushing.
- **`--run-donttest` job**: `\donttest{}` examples run in CI. The
  `future_promise()` example is `\dontrun{}` for now, so it does not.
- **pkgdown reference index must list every exported topic.** The deprecated
  `ospan` functions are on the `promises-deprecated` page, which is
  `@keywords internal`, so they do not need an entry.
- **Vignette chunks must stay non-evaluated or guarded.** `mirai`, `future`,
  and `vembedr` are only `Suggests`; an unguarded chunk breaks CRAN builds.
- **`NEWS.md` points users to `otel::get_current_span()`**, which does not
  exist; the function is `otel::get_active_span()`.
