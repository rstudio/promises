# promises R Package

The promises package provides fundamental abstractions for asynchronous programming in R using promises. It enables a single R process to orchestrate multiple tasks in the background while attending to other work. This is an R package with standard structure using devtools workflow.

Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.

## Working Effectively

### Environment Setup
- Install R and essential development tools:
  - `sudo apt-get update && sudo apt-get install -y r-base-core r-base-dev`
  - `sudo apt-get install -y r-cran-devtools r-cran-testthat r-cran-knitr r-cran-rmarkdown r-cran-roxygen2`
  - `sudo apt-get install -y r-cran-rlang r-cran-r6 r-cran-magrittr r-cran-later r-cran-fastmap r-cran-lifecycle r-cran-purrr r-cran-lintr`

### Package Development Workflow
- Load package for development: `R --slave -e "library(devtools); load_all()"`
- Build package: `R CMD build --no-build-vignettes .` -- takes <1 second. Safe to use default timeout.
- Install package: `sudo R CMD INSTALL promises_*.tar.gz` -- takes 3 seconds. Safe to use default timeout.
- Run tests: `R --slave -e "library(devtools); test()"` -- takes 88 seconds. NEVER CANCEL. Set timeout to 120+ seconds.
- Check package: `_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-build-vignettes --no-manual promises_*.tar.gz` -- takes 30 seconds. NEVER CANCEL. Set timeout to 60+ seconds.

### Testing
- NEVER use `library(testthat); test_dir('tests/testthat')` directly - tests will fail due to missing library(promises) context
- Always use `library(devtools); test()` which properly loads the package context
- Test suite contains 372 tests that all pass
- Duration: 88 seconds. NEVER CANCEL. Set timeout to 120+ seconds minimum.

## Validation

### Manual Testing Scenarios
Always manually validate any new promises functionality:
```r
library(devtools); load_all()
# Test basic promise creation and resolution
p1 <- promise_resolve(123)
p2 <- then(p1, function(x) x * 2)
later::run_now()  # Run event loop to resolve promises
print(p2)  # Should show <Promise [fulfilled: numeric]>
```

### Build and Test Validation
- Always run the complete test suite after making changes: `library(devtools); test()`
- Always check R CMD check status: `_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-build-vignettes --no-manual promises_*.tar.gz`
- Always run linting before committing: `R --slave -e "library(lintr); lint_package()"`

## Known Issues and Limitations

### Vignettes
- Some vignettes fail to build due to missing `vembedr` package dependency
- Use `--no-build-vignettes` flag for R CMD build and R CMD check
- Vignette `promises_05b_future_promise.Rmd` requires `vembedr` which is not available in standard repositories

### Package Dependencies
- Some suggested packages (`mirai`, `vembedr`) are not available in standard Ubuntu repositories
- Use `_R_CHECK_FORCE_SUGGESTS_=false` environment variable to bypass suggests checking
- Install available dependencies via apt: `r-cran-*` packages

### Manual Generation
- PDF manual generation fails due to missing `pdflatex`
- Use `--no-manual` flag for R CMD check
- HTML documentation can be generated but pkgdown requires `tidytemplate` which is not available

## Code Style and Linting
- Run linting with: `R --slave -e "library(lintr); lint_package()"`
- Linting takes ~8 seconds and shows style violations (line length, naming conventions, etc.)
- Current codebase has existing style issues - focus only on new code style compliance

## Key Projects and Structure

### Core Components
- `R/promise.R` - Core promise implementation
- `R/then.R` - Promise chaining and transformation methods  
- `R/domains.R` - Promise domain system for execution context
- `R/future_promise.R` - Integration with future package for async execution
- `tests/testthat/` - Comprehensive test suite (372 tests)
- `vignettes/` - 8 vignettes documenting package usage

### Important Files
- `DESCRIPTION` - Package metadata and dependencies
- `NAMESPACE` - Exported functions and imports
- `promises.Rproj` - RStudio project configuration with devtools setup
- `_pkgdown.yml` - Website generation configuration
- `.github/workflows/R-CMD-check.yaml` - CI/CD pipeline

## Common Development Tasks

### Repository Structure
```
promises/
├── DESCRIPTION          # Package metadata
├── NAMESPACE           # Exports and imports
├── R/                  # Source code
├── man/                # Documentation
├── tests/              # Test suite
├── vignettes/          # Package vignettes
├── inst/               # Additional files
└── .github/            # GitHub workflows
```

### Quick Reference Commands
```bash
# Load package for interactive development
R --slave -e "library(devtools); load_all()"

# Run tests (takes 88 seconds - NEVER CANCEL)
R --slave -e "library(devtools); test()"

# Build package (takes <1 second)
R CMD build --no-build-vignettes .

# Check package (takes 30 seconds - NEVER CANCEL)  
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-build-vignettes --no-manual promises_*.tar.gz

# Lint code (takes 8 seconds)
R --slave -e "library(lintr); lint_package()"
```

### Working with Promises
- Promises require event loop execution to resolve: use `later::run_now()` in interactive sessions
- Test promise chains by creating simple resolve/then patterns
- Always validate async functionality with realistic scenarios, not just simple tests
- Remember that promises are lazy - they need explicit resolution or event loop execution

## CI/CD Pipeline
- Uses GitHub Actions with R-CMD-check workflow
- Workflow file: `.github/workflows/R-CMD-check.yaml`
- Includes website building, routine checks, and R CMD check with different configurations
- Tests run automatically on push/PR to main branch and scheduled weekly