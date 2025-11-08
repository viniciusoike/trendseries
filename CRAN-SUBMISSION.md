# CRAN Submission Guide for trendseries 1.1.0

## Pre-submission Checklist

### ✅ Package Checks
- [x] R CMD check --as-cran: **0 errors, 0 warnings, 0 notes**
- [x] Local macOS R 4.5.1: PASS
- [x] win-builder (devel): Submitted, awaiting results
- [x] win-builder (release): Submitted, awaiting results
- [ ] R-hub: To be submitted

### ✅ Quality Checks
- [x] All URLs validated and accessible
- [x] LICENSE files present and correct (MIT + file LICENSE)
- [x] DESCRIPTION uses modern Authors@R format
- [x] Examples run successfully (< 5 seconds each)
- [x] All tests pass (258 tests)
- [x] Three vignettes build successfully

### ✅ Documentation
- [x] All exported functions documented
- [x] README.md up to date
- [x] NEWS.md describes changes
- [x] cran-comments.md prepared

## Submission Type

**NEW SUBMISSION** - This is the first CRAN release of trendseries.

## Package Description

trendseries provides methods for extracting trends from economic time series data with a focus on monthly and quarterly data. It offers 19 different trend extraction methods including:

- Econometric filters (HP, Baxter-King, Christiano-Fitzgerald, Hamilton, etc.)
- Moving averages (Simple, Weighted, Exponential, Median, Gaussian)
- Smoothing methods (STL, Loess, Splines, Polynomial)
- Signal processing (Kernel smoothing, Kalman filtering)

The package provides a modern, pipe-friendly API with two main functions:
- `augment_trends()` for data.frame/tibble workflows
- `extract_trends()` for ts/xts/zoo objects

## Key Features

1. **Unified Parameter System**: Consistent interface across all 19 methods
2. **Smart Defaults**: Optimized for economic frequencies (quarterly/monthly)
3. **Comprehensive Testing**: 258 passing tests across multiple files
4. **Well Documented**: Three vignettes + extensive function documentation
5. **Modern R Practices**: Native pipe, cli messaging, tidy evaluation

## Dependencies

### Imports (10 packages)
- cli: User messaging
- dlm: Kalman filtering
- glue: String formatting
- hpfilter: HP filter implementation
- lubridate: Date handling
- mFilter: Economic filters (BK, CF)
- RcppRoll: Fast rolling statistics
- stats: Base R statistics
- tibble: Data frame construction
- tsbox: Time series conversion

### Suggests (6 packages)
- dplyr: Data manipulation (examples)
- ggplot2: Plotting (vignettes)
- knitr: Vignette building
- rmarkdown: Documentation
- testthat: Testing
- tidyr: Data reshaping (examples)
- xts: Extended time series (examples)

## Test Environments

- Local: macOS Sequoia 15.6.1, R 4.5.1
- win-builder: R-devel and R-release (submitted)
- R-hub: Multiple platforms (pending)

## R CMD check Results

**Duration**: 27.6s
**Result**: 0 errors ✔ | 0 warnings ✔ | 0 notes ✔

All checks passed successfully with no issues.

## Downstream Dependencies

None - this is a new package with no reverse dependencies.

## Submission Steps

### Before Submitting

1. **Wait for win-builder results** (15-30 mins after submission)
   - Check email for results from both devel and release
   - Address any issues found

2. **Run R-hub checks** (optional but recommended)
   ```r
   rhub::check_for_cran()
   ```

3. **Final spell check**
   ```r
   devtools::spell_check()
   ```
   Most flagged words are technical terms or proper nouns (expected)

4. **Final URL check**
   ```r
   urlchecker::url_check()
   ```
   Currently all URLs valid ✓

### Submission Command

When ready to submit:

```r
devtools::submit_cran()
```

This will:
1. Build the package
2. Upload to CRAN's submission server
3. Send confirmation email

### After Submission

1. **Respond promptly** to any CRAN maintainer questions
2. **Be prepared** to make quick fixes if requested
3. **Monitor** the CRAN incoming dashboard
4. **Typical timeline**: 1-3 days for initial review

## CRAN Policy Compliance

### Package Name
- "trendseries" is descriptive and not used on CRAN
- All lowercase, no special characters

### Title and Description
- Title in title case: "Extract Trends from Time Series"
- Description does not start with package name
- No redundant "this package" language

### License
- MIT + file LICENSE (standard approved license)
- LICENSE file includes YEAR and COPYRIGHT HOLDER
- LICENSE.md includes full MIT license text

### Code Quality
- No calls to `set.seed()` in package code
- No `cat()` or `print()` in functions (uses `cli` instead)
- Proper error handling with informative messages
- No hard-coded paths
- No modification of user's options or par()

### Examples
- All examples run successfully
- Execution time < 5 seconds per example
- No interactive functions in examples
- All examples wrapped in functions (not bare code)

### Tests
- Comprehensive test suite (258 tests)
- Tests in tests/testthat/
- Uses testthat edition 3
- All tests pass in check

### Vignettes
- Three vignettes provided
- All build successfully
- Reasonable file sizes
- Clear, informative content

## Common CRAN Issues - Status

- [ ] Package name conflicts: NONE (checked CRAN)
- [x] Author/Maintainer format: UPDATED to Authors@R
- [x] URLs accessible: ALL VERIFIED
- [x] Examples timeout: ALL < 5s
- [x] Tests pass: 258/258 PASSING
- [x] R CMD check: 0/0/0 (errors/warnings/notes)
- [x] License: MIT COMPLIANT
- [x] Non-ASCII characters: NONE
- [x] Spelling: Only technical terms flagged

## Post-Acceptance Tasks

1. Add CRAN badge to README
2. Update installation instructions
3. Announce release (Twitter, LinkedIn, etc.)
4. Monitor for user feedback
5. Prepare maintenance plan

## Contact Information

**Maintainer**: Vinicius Oike <viniciusoike@gmail.com>
**Repository**: https://github.com/viniciusoike/trendseries
**Website**: https://viniciusoike.github.io/trendseries/

## Notes

- This is version 1.1.0 (not 0.x.x) because the package has been in development
  and testing for some time. It represents a mature, production-ready release.
- The package removes some signal processing methods (Butterworth, Savitzky-Golay)
  and exponential smoothing methods to focus on core econometric approaches.
- All dependencies are well-established CRAN packages.
- The package has been thoroughly tested with Brazilian economic data.
