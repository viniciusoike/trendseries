## Resubmission
This is a resubmission addressing the NOTEs from the initial win-builder check.

### Changes made:
* Fixed 'tibble' being flagged as potentially misspelled by adding single quotes in DESCRIPTION
* Added non-standard files to .Rbuildignore (submit-cran.R, NEXT-STEPS.md, CRAN-SUBMISSION.md)
* The previously reported broken URL (getting-started.html) was already fixed in the current version

## Test environments
* local macOS install, R 4.5.1
* win-builder (devel and release)
* R-hub (ubuntu-latest, windows-latest, macos-latest)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Comments

This package provides methods for extracting trends from economic time series data. It focuses on established econometric methods (Hodrick-Prescott, Baxter-King, Christiano-Fitzgerald, etc.) and provides a modern, pipe-friendly API for R users.

Key features:
- 19 trend extraction methods covering econometric filters, moving averages, and smoothing techniques
- Two-function API: augment_trends() for data.frame workflows, extract_trends() for ts objects
- Smart defaults optimized for monthly and quarterly economic data
- Comprehensive test suite with 258 passing tests
- Three vignettes demonstrating common use cases

All examples run in < 5 seconds. All URLs in DESCRIPTION and documentation are valid and accessible.
