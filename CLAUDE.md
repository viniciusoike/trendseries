# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Before Writing Code

**Always read `claude/coding_guidelines.md`** first. It covers modern
tidyverse patterns, rlang metaprogramming, and the coding style required
for this project.

## Build & Development Commands

``` bash
# Run all tests
devtools::test()

# Run a single test file (regex matched against filename without test- prefix and .R extension)
devtools::test(filter = "extract_trends")
devtools::test(filter = "filters-ma")

# Regenerate documentation (roxygen2 -> man/ and NAMESPACE)
devtools::document()

# Full package check (CRAN-like, run before committing)
devtools::check()

# Load package for interactive testing
devtools::load_all()

# Build vignettes
devtools::build_vignettes()
```

## Package Architecture

**Two-function API** for trend extraction from economic time series
(monthly/quarterly focus):

- [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  — pipe-friendly, takes data.frames/tibbles, adds `trend_{method}`
  columns. Supports grouped operations via dplyr.
- [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
  — takes ts/xts/zoo objects, returns ts objects (single method) or
  named list (multiple methods).

Both functions share a **unified parameter system**: `window`,
`smoothing`, `band`, `align`, `params` (list for method-specific
options).

### Source File Organization

| File | Purpose |
|----|----|
| `R/augment_trends.R` | Data.frame interface, grouping logic, naming conflict resolution |
| `R/extract_trends.R` | Time series interface, method dispatch, input validation |
| `R/filters_econometric.R` | HP, BK, CF, Hamilton, Beveridge-Nelson, UCM (6 methods) |
| `R/filters_ma.R` | SMA, WMA, EWMA, Triangular, Median, Gaussian, Spencer (7 methods) |
| `R/filters_smoothing.R` | STL, LOESS, Splines, Polynomial (4 methods) |
| `R/filters_signal.R` | Kernel smoother, Kalman smoother (2 methods) |
| `R/converters.R` | [`df_to_ts()`](https://viniciusoike.github.io/trendseries/reference/df_to_ts.md), [`ts_to_df()`](https://viniciusoike.github.io/trendseries/reference/ts_to_df.md), frequency detection, trend-to-df merging |
| `R/utils.R` | Unified parameter mapping (routes `window`/`smoothing`/`band` to method-specific params) |
| `R/data.R` | Documentation for 10 bundled economic datasets |

### Data Flow

1.  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md):
    data.frame →
    [`df_to_ts()`](https://viniciusoike.github.io/trendseries/reference/df_to_ts.md)
    → ts object →
    [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
    → ts result → `.trends_to_df()` → `.safe_merge()` back to original
    data
2.  [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md):
    ts object → `.process_unified_params()` maps generic params to
    method-specific → dispatches to `filters_*.R` → returns ts

### Frequency-Aware Defaults

Smart defaults change based on detected frequency: - Monthly (12): HP
λ=14400, MA window=12 - Quarterly (4): HP λ=1600, MA window=4 - General:
λ = 1600 × (freq/4)^4 (Ravn & Uhlig 2002)

### Exported Functions

`augment_trends`, `extract_trends`, `df_to_ts`, `ts_to_df`,
`list_datasets`

## Coding Standards

- **Native pipe `|>`** only (never `%>%`)
- **`<-` for assignment** (never `=`)
- **Explicit [`return()`](https://rdrr.io/r/base/function.html)** in all
  functions
- **`cli` package** for all user-facing messages (`cli_abort`,
  `cli_warn`, `cli_inform`). Never use `cat`, `print`, `message`,
  `stop`, `warning` directly.
- Pipe chains max 4-5 operations; break into smaller functions beyond
  that
- Internal helper functions prefixed with `.` (e.g.,
  `.detect_frequency()`)
- In vignettes/examples: split data processing and plotting into
  separate code chunks

## Test Organization

Tests in `tests/testthat/` mirror the source structure: -
`test-augment_trends.R` — data.frame API, grouping, naming conflicts -
`test-extract_trends.R` — ts API, all methods, parameter validation -
`test-filters-econometric.R` — HP, BK, CF, Hamilton, UCM specifics -
`test-filters-ma.R` — all moving average variants, alignment, window
validation - `test-params-stl.R` — STL parameter combinations -
`test-edge-cases.R` — short series, missing values, frequency
detection - `test-utils.R` — frequency detection, parameter mapping

## Key Dependencies

- `tsbox` — format conversion between ts/xts/zoo/data.frame
- `mFilter` — Baxter-King and Christiano-Fitzgerald bandpass filters
- `hpfilter` — one-sided (`hp1`) and two-sided (`hp2`) HP filter
- `RcppRoll` — C++ optimized rolling mean/median
- `dlm` — Kalman smoother via dynamic linear models
- `dplyr` — Suggested (not required), enables grouped operations in
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)

## Current Status

Version 1.1.0. 19 trend methods. CRAN submission preparation phase. See
`claude/TODO.md` for completed work and future ideas.
