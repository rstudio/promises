# GitHub Copilot Instructions for promises

The promises package provides fundamental abstractions for doing
asynchronous programming in R using promises. This R package follows
standard R package development practices using the devtools ecosystem.

**CRITICAL: Always follow these instructions first and only fallback to
additional search and context gathering if the information in these
instructions is incomplete or found to be in error.**

## Working Effectively

### Essential Setup Commands

Install required R and development dependencies:

``` bash
# Install R if not available (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install -y r-base r-base-dev build-essential libcurl4-openssl-dev libssl-dev libxml2-dev

# Install core R packages via apt (faster than CRAN for basic packages)
sudo apt-get install -y r-cran-fastmap r-cran-later r-cran-r6 r-cran-rlang r-cran-lifecycle r-cran-magrittr r-cran-testthat

# Install additional development packages if available via apt
sudo apt-get install -y r-cran-devtools r-cran-knitr r-cran-rmarkdown r-cran-spelling

# If packages not available via apt, install via CRAN (may fail if network restricted)
sudo R -e "install.packages(c('devtools', 'pkgdown'), repos='https://cloud.r-project.org/')"
```

### Build and Development Commands

Always run these commands from the package root directory
(`/home/runner/work/promises/promises`):

``` bash
# Install package from source (basic development workflow)
# TIMING: ~3-5 seconds
sudo R -e "install.packages('.', type = 'source', repos = NULL)"

# Generate documentation from roxygen2 comments (if devtools available)
R -e "devtools::document()"

# Build source package without vignettes (fastest option)
# TIMING: ~0.3 seconds - VERY FAST
R CMD build --no-build-vignettes .

# Basic R CMD check (without tests/vignettes to avoid missing dependencies)
# TIMING: ~12-15 seconds - NEVER CANCEL, Set timeout to 30+ seconds
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-vignettes --no-tests promises_*.tar.gz

# Full R CMD check with devtools (if available)
# NEVER CANCEL: Takes 5-15 minutes with all dependencies. Set timeout to 20+ minutes.
R -e "devtools::check()"

# Run unit tests directly from source
# TIMING: ~7-10 seconds - NEVER CANCEL, Set timeout to 30+ seconds  
R -e "library(testthat); library(promises); test_dir('tests/testthat')"

# Run unit tests using devtools (if available)
R -e "devtools::test()"
```

### Testing Commands

``` bash
# Run full test suite from source directory
# TIMING: ~7-10 seconds - NEVER CANCEL, Set timeout to 30+ seconds
R -e "library(testthat); library(promises); test_dir('tests/testthat')"

# Run spelling checks (if spelling package available)
R -e "spelling::spell_check_test(vignettes = TRUE, error = TRUE, skip_on_cran = TRUE)"

# Run single test file
R -e "library(testthat); library(promises); test_file('tests/testthat/test-methods.R')"

# Check if package loads correctly 
R -e "library(promises); promise_resolve('test') %>% then(function(x) cat('OK:', x, '\n'))"
```

### Documentation Commands

``` bash
# Build package documentation website (if pkgdown available)
# NEVER CANCEL: Takes 3-8 minutes. Set timeout to 15+ minutes.
R -e "pkgdown::build_site()"

# Render specific vignette (if knitr/rmarkdown available)
R -e "rmarkdown::render('vignettes/promises_01_motivation.Rmd')"
```

## Validation Requirements

### Always Test These Scenarios After Making Changes:

1.  **Basic Package Loading**: Verify the package loads without errors

    ``` r
    library(promises)
    # Should load successfully with all dependencies
    ```

2.  **Basic Promise Functionality**: Test core promise operations

    ``` r
    library(promises)
    promise_resolve("hello") %>%
      then(function(x) paste(x, "world")) %>%
      then(function(x) cat("Result:", x, "\n"))
    ```

3.  **Promise Creation**: Test manual promise creation

    ``` r
    p <- promise(function(resolve, reject) resolve(42))
    # Promise should be created successfully
    ```

4.  **Integration Testing**: Verify core dependencies work together

    ``` r
    library(later)
    library(promises) 
    library(fastmap)
    library(R6)
    # All should load without conflicts
    ```

### Mandatory Pre-Commit Checks:

**CRITICAL**: Run these validation steps before committing any changes:

``` bash
# 1. Build package to check for syntax/dependency errors
# TIMING: ~0.3 seconds - VERY FAST
R CMD build --no-build-vignettes .

# 2. Install package to verify it works
# TIMING: ~3-5 seconds
sudo R -e "install.packages('.', type = 'source', repos = NULL)"

# 3. Test package loading and basic functionality  
# TIMING: ~1-2 seconds
R -e "library(promises); promise_resolve('test')"

# 4. Run test suite if testthat is available
# TIMING: ~7-10 seconds - NEVER CANCEL
R -e "library(testthat); library(promises); test_dir('tests/testthat')"

# 5. Full check if time permits (optional but recommended)
# TIMING: ~12-15 seconds - NEVER CANCEL, Set timeout to 30+ seconds
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-vignettes --no-tests promises_*.tar.gz
```

**Expected timing summary**: - Basic build: ~0.3 seconds - **INSTANT** -
Package install: ~3-5 seconds - **VERY FAST**  
- Test suite: ~7-10 seconds - **NEVER CANCEL, timeout 30+ seconds** -
Basic check: ~12-15 seconds - **NEVER CANCEL, timeout 30+ seconds** -
Full devtools::check(): 5-15 minutes - **NEVER CANCEL, timeout 20+
minutes**

## Repository Structure

### Core Development Files:

- `R/` - Main R source code (11 files including promise.R, then.R,
  methods.R)
- `tests/testthat/` - Unit tests using testthat framework
- `vignettes/` - 8 comprehensive documentation vignettes
- `inst/` - Template C++ code (`promise_task.cpp`) for background tasks
- `man/` - Generated documentation (do not edit manually)

### Key Architecture Components:

- **Promise Class** (`R/promise.R`): R6-based promise implementation
  with states
- **Core Operations** (`R/then.R`): Primary promise chaining interface  
- **Combining Operations** (`R/methods.R`): promise_all(),
  promise_race(), etc.
- **Domain System** (`R/domains.R`): Context management for promise
  execution
- **Future Integration** (`R/future_promise.R`): Bridge with future
  package

### Dependencies (Auto-installed via devtools):

- **Core**: fastmap, later, R6, rlang, magrittr, lifecycle
- **Optional**: future (\>= 1.21.0), mirai
- **Development**: testthat, knitr, rmarkdown, spelling

## GitHub Actions / CI Information

The package uses RStudio’s shiny-workflows for CI/CD
(`.github/workflows/R-CMD-check.yaml`): - Automated R CMD check on
push/PR  
- Website deployment via pkgdown - Code formatting and routine checks -
Runs on multiple R versions and platforms

**Local validation should match CI requirements**: Always run
`devtools::check()` locally before pushing.

## Common Development Tasks

### Adding New Functions:

1.  Add function to appropriate R file in `R/` directory
2.  Document with roxygen2 comments (use `@export` if public)
3.  Run `devtools::document()` to update NAMESPACE and man pages
4.  Add tests in `tests/testthat/test-[function-area].R`
5.  Run `devtools::test()` to verify tests pass
6.  Run `devtools::check()` for full validation

### Working with Vignettes:

``` bash
# Build specific vignette
R -e "rmarkdown::render('vignettes/promises_01_motivation.Rmd')"

# Build all vignettes (part of pkgdown::build_site)
R -e "devtools::build_vignettes()"
```

### Performance Testing:

The package includes performance tests in
`tests/testthat/test-zzz-future_promise.R` that validate timing
expectations. These tests run longer than typical unit tests.

## Troubleshooting

### Common Issues:

- **Missing Dependencies**: Install core packages via apt first, then
  try CRAN for others

  ``` bash
  sudo apt-get install -y r-cran-fastmap r-cran-later r-cran-r6 r-cran-rlang r-cran-testthat
  ```

- **Package Won’t Load**: Reinstall from source:
  `sudo R -e "install.packages('.', type = 'source', repos = NULL)"`

- **devtools Not Available**: Use R CMD directly for basic operations

- **Test Failures**: Ensure package is installed: tests need the package
  loaded

- **Build Failures**: Check DESCRIPTION file dependencies match actual
  imports

### Alternative Commands When devtools Unavailable:

``` bash
# Use R CMD instead of devtools equivalents:
R CMD build --no-build-vignettes .                    # instead of devtools::build()
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-vignettes --no-tests *.tar.gz  # instead of devtools::check()
R -e "library(testthat); test_dir('tests/testthat')"  # instead of devtools::test()
sudo R -e "install.packages('.', type='source', repos=NULL)"  # instead of devtools::install()
```

### Network/CRAN Issues:

If CRAN mirrors are unavailable, use apt packages or local installation:

``` bash
# Prefer apt packages over CRAN when possible
sudo apt-cache search r-cran- | grep <package_name>

# Force local installation without network
sudo R -e "install.packages('.', type='source', repos=NULL)"
```

### Performance Notes:

- **Test file `test-zzz-future_promise.R`**: Contains performance/timing
  tests that may run longer than typical unit tests
- **Vignettes**: Building vignettes requires knitr/rmarkdown and may
  take several minutes
- **PDF manuals**: Require LaTeX installation (texlive-latex-base) to
  build without errors

## Important Files Reference

### Repository Structure:

- **Root**: `.Rbuildignore`, `.github/`, `CLAUDE.md`, `DESCRIPTION`,
  `LICENSE`, `NAMESPACE`, `NEWS.md`, `R/`, `README.md`, `_pkgdown.yml`,
  `inst/`, `man/`, `promises.Rproj`, `tests/`, `vignettes/`

### R Source Files (`R/` directory):

- `domains.R`, `first_type.R`, `future_promise.R`, `is_something.R`,
  `methods.R`, `pipe.R`, `promise.R`, `promises-package.R`,
  `staticimports.R`, `then.R`, `utils.R`

### Test Files (`tests/testthat/` directory):

- `helper.R`, `test-aplus-2-1.R`, `test-aplus-2-2.R`,
  `test-aplus-2-3.R`, `test-combining.R`, `test-cpp.R`,
  `test-domains.R`, `test-legacy-*.R`, `test-methods.R`,
  `test-ordering.R`, `test-promise-map.R`, `test-then.R`,
  `test-visibility.R`, `test-zzz-future_promise.R`

**Remember**: This is an R package focused on asynchronous programming.
Always consider promise semantics, error handling, and integration with
the later package when making changes.
