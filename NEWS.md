# trendseries 1.4.0

This release combines the 1.3.0 development series, which was never published
on CRAN, with the 1.4.0 changes.

## New Features

* `decompose_series()` is now exported and available for use. This
  pipe-friendly function decomposes a time series into trend, seasonal, and
  remainder components, adding `trend_*`, `seasonal_*`, and `remainder_*`
  columns to the input data frame. Five methods are available: `"stl"`
  (default), `"regression"`, `"classic"` (classical decomposition via centred
  moving averages, `stats::decompose()`), `"bsm"` (Basic Structural state-space
  Model estimated by the Kalman smoother, `stats::StructTS()`), and `"seats"`
  (X-13ARIMA-SEATS via the optional **`seasonal`** package, a Suggested
  dependency only required for this method). It supports grouped decomposition
  via `group_cols` and guarantees the exact identity
  `value = trend + seasonal + remainder`. See the new *Decomposing Series*
  vignette. Additional conveniences:
  - `methods` accepts a vector (e.g. `c("stl", "classic")`), adding each
    method's components as separate columns for side-by-side comparison.
  - `transform = "log"` provides a uniform multiplicative decomposition across
    every method (decompose on the log scale, exponentiate back), so
    `value = trend * seasonal * remainder` holds exactly.
  - `seasadj = TRUE` adds a `seasadj_{method}` column with the seasonally
    adjusted series.

* `deseason_series()` is a new convenience wrapper around `decompose_series()`
  focused on seasonal adjustment. It adds a `seasadj_{method}` column with the
  deseasoned series (methods `"stl"` or `"seats"`), and optionally the full
  trend/seasonal/remainder decomposition via `components = TRUE`.

* `detrend_series()` is a new convenience wrapper around `augment_trends()`
  focused on detrending. It adds a `detrend_{method}` column holding the
  detrended series — the deviation from trend, the *cycle* in economics — for
  any of the 20 trend methods, defaulting to the Hodrick-Prescott filter.
  `transform = "log"` returns the log deviation from trend (approximately the
  percentage deviation, the output-gap convention), and `components = TRUE`
  also keeps the fitted `trend_{method}` columns. The exact identity
  `value = trend + detrend` holds (`value = trend * exp(detrend)` with
  `transform = "log"`).

## Bug Fixes

* The Hamilton filter now uses frequency-aware default parameters, as
  documented. The defaults were hardcoded to the quarterly values
  (`h = 8`, `p = 4`) regardless of frequency, so monthly series were filtered
  with a two-quarter horizon instead of the recommended two-year one. Monthly
  data now defaults to `h = 24`, `p = 12` (Hamilton 2018); quarterly
  behaviour is unchanged. Because the monthly defaults are larger, monthly
  series now require at least 37 observations (`h + p + 1`) and the first 35
  trend values are `NA` (previously 13 and 11). Pass
  `params = list(hamilton_h = , hamilton_p = )` to reproduce old results.

## Internal Improvements

* Removed the **`glue`** dependency. The two remaining `glue::glue()` calls
  were replaced by the interpolation `cli` already provides.

* Removed dead internal code left over from earlier refactors: the unused
  `.ensure_odd_window()` and `.check_deprecated_params()` helpers, leftover
  `zlema` references, and stale `HoltWinters`/`roll_median` namespace imports.

* The list of valid methods is now defined in a single internal registry,
  ensuring `augment_trends()` and `extract_trends()` can never drift out of
  sync. The valid decomposition methods for `decompose_series()` are defined
  there as well.

* The unified parameter validation (`window`, `smoothing`, `band`, `align`,
  `params`) shared by `augment_trends()` and `extract_trends()` now lives in a
  single internal helper, so the two functions can no longer drift apart.

## Documentation

* Added a *Trend Extraction Methods* vignette cataloguing all 20 trend methods
  by family — when to use each one and which parameters it takes.

* Added a *Detrending Series* vignette covering `detrend_series()`: the
  deseason-then-detrend workflow for seasonal data, percentage deviations from
  trend via `transform = "log"`, method comparison (HP vs Hamilton), and
  grouped detrending.

* Removed outdated references to the **`TTR`** package from the `augment_trends()`
  and `extract_trends()` documentation. The EWMA `window` parameter is now
  documented by what it does: it sets `alpha = 2 / (window + 1)`.

---

# trendseries 1.2.0

**Release Date**: 2026-05-02

## Breaking Changes

* The `group_vars` argument in `augment_trends()` is deprecated in favour of
  `group_cols`. A deprecation warning is now issued when `group_vars` is used.
  `group_vars` will be removed in a future release.

## New Features

* `augment_trends()` now accepts multiple value columns via a character vector
  in `value_col`. Trends are extracted for each column and named
  `trend_{method}_{col}` (e.g. `trend_stl_consumption`).

* Improved UCM (Unobserved Components Model) trend extraction. The model now
  uses fixed variance components with signal-to-noise ratios derived from
  Hodrick-Prescott filter lambdas, producing smoother, economically meaningful
  trends by default. The `smoothing` parameter can be used to override the
  default.

* Added London Underground transit datasets: `transit_london_monthly` and
  `transit_london_avgs`.

## Bug Fixes and Improvements

* Fixed typos, grammar, and prose across vignettes.
* Updated vignettes to use `group_cols` instead of deprecated `group_vars`.
* Fixed mislabeled y-axis in vignette plots.
* Removed stale ZLEMA reference from moving average documentation.

---

# trendseries 1.1.0

**Release Date**: November 2025

## Breaking Changes

* **Removed Butterworth filter**: The Butterworth low-pass filter has been removed to focus the package on core econometric methods. The `signal` package dependency has been removed.

* **Removed Savitzky-Golay filter**: The Savitzky-Golay polynomial smoothing filter has been removed to streamline the package. The `signal` package dependency has been removed.

* **Removed exponential smoothing methods**: Simple and double exponential smoothing (`exp_simple`, `exp_double`) have been removed. Users can continue using EWMA for exponential smoothing. The `forecast` package dependency has been removed.

## Note

* **EWMA retained**: The Exponentially Weighted Moving Average (EWMA) method has been kept as it is categorized as a moving average method and is widely used in economic analysis.

# trendseries 1.0.1

**Release Date**: January 2025

## Breaking Changes

* **Removed ZLEMA filter**: The Zero-Lag Exponential Moving Average (ZLEMA) has been removed from the package. This method was not commonly used in economic analysis and added unnecessary complexity. Users needing zero-lag smoothing can use EWMA with appropriate alpha values.

## New Features

* **Added Spencer filter**: Classic 15-term Spencer moving average filter for trend extraction. The Spencer filter uses symmetric weights designed to preserve cubic polynomial trends while providing smooth results. Implementation uses linear extrapolation at endpoints for simplicity and computational efficiency.

## Bug Fixes and Improvements

### Moving Average Enhancements

* **Implemented econometrically correct 2xN MA for centered even-window moving averages**:
  - When using centered alignment with even windows (e.g., 12 for monthly data, 4 for quarterly), the simple moving average now automatically applies the proper 2xN double-smoothing technique
  - This matches the X-13ARIMA-SEATS standard for seasonal adjustment
  - Example: `window=12, align="center"` now correctly applies a 2x12 MA instead of naive centering
  - Non-centered alignments (right/left) and odd windows continue to use regular single MA

* **Fixed misleading "2x" notation in messages**:
  - Previous versions displayed "2x12" in messages but didn't actually implement double smoothing
  - Now the "2x" notation only appears when the 2xN algorithm is actually used
  - Messages clearly indicate: "Computing 2x12-period MA (auto-adjusted for even-window centering)" vs "Computing 12-period MA with right alignment"

* **Added comprehensive tests for 2xN MA**:
  - 5 new test cases validating correct behavior for monthly and quarterly data
  - Tests confirm 2xN MA differs from simple MA for even-window centered cases
  - All 106 MA filter tests passing

### Technical Changes

* Added `glue` package to Imports for message formatting
* Added `.ma_2x()` internal function implementing proper double-smoothing
* Added `.ensure_odd_window()` utility function for future use
* Updated test expectations to account for new 2xN behavior

### Impact

This is an important correctness fix for users doing seasonal adjustment or business cycle analysis with monthly/quarterly data. The new implementation ensures that centered moving averages with even windows produce econometrically sound results.

---

# trendseries 1.0.0

**Release Date**: January 2025

## First Production Release

This is the first production release of trendseries, providing a modern, pipe-friendly interface for extracting trends from economic time series data.

### Key Features

* **21 Trend Extraction Methods**:
  - **Econometric filters**: HP filter (one-sided and two-sided), Baxter-King, Christiano-Fitzgerald, Hamilton filter, Beveridge-Nelson decomposition, Unobserved Components Model (UCM)
  - **Moving averages**: Simple (SMA), Weighted (WMA), Exponential (EWMA), Zero-lag (ZLEMA), Triangular, Median, Gaussian-weighted
  - **Smoothing methods**: STL decomposition, Loess, Splines, Polynomial trends, Simple/Double exponential smoothing
  - **Signal processing**: Kalman filter/smoother, Savitzky-Golay, Butterworth, Kernel smoother

* **Two-Function API**:
  - `augment_trends()`: Pipe-friendly function for tibble/data.frame workflows with grouped operations
  - `extract_trends()`: Direct time series analysis for ts/xts/zoo objects

* **Unified Parameter System**: Consistent interface with `window`, `smoothing`, `band`, `align`, and `params` parameters across all methods

* **Smart Economic Defaults**:
  - HP filter: λ=1600 (quarterly), λ=14400 (monthly)
  - Moving averages: Frequency-appropriate windows (4 quarters, 12 months)
  - Bandpass filters: 6-32 quarter business cycle range

* **Performance Optimizations**:
  - C++ implementations via RcppRoll for fast rolling statistics
  - Optimized exponential smoothing with automatic parameter selection
  - Efficient signal processing filters

### Major Improvements

* **Mathematical Correctness**: All 21 methods validated for theoretical accuracy and proper implementation
* **EWMA Dual Interface**: Support for both window-based (TTR optimization) and alpha-based (traditional formula) approaches
* **One-sided HP Filter**: Real-time analysis support with `hp_onesided=TRUE` parameter for nowcasting and policy analysis
* **Align Parameter**: Flexible positioning for moving averages (left/center/right) enabling causal and anti-causal filters
* **Modern R Patterns**: Native pipe `|>`, cli messaging, comprehensive error handling
* **Scale Invariance**: Kernel smoother with theoretically sound bandwidth selection
* **Robust Error Handling**: Informative messages with actionable suggestions using cli package

### Quality Metrics

* **R CMD check**: 0 errors | 0 warnings | 0 notes (perfect score)
* **Test suite**: 317 passing tests across 9 test files
* **Documentation**: All examples verified working
* **Code quality**: No duplicates, modern patterns, clean dependencies

### Included Datasets

The package includes 10 economic datasets for examples and testing:

* **Brazilian data (BCB)**: `gdp_construction`, `ibcbr`, `vehicles`, `oil_derivatives`, `electric`
* **UK data (ONS)**: `retail_households`, `retail_autofuel`
* **Coffee prices (CEPEA)**: `coffee_arabica`, `coffee_robusta` (daily data)
* **Metadata**: `series_metadata`

### Package Scope

Optimized for monthly (frequency=12) and quarterly (frequency=4) economic data, with smart defaults tailored for business cycle analysis. Methods like STL and moving averages also support daily and other frequencies.

### Technical Details

* **Minimum R version**: 4.1.0
* **Dependencies**: mFilter, hpfilter, RcppRoll, forecast, dlm, signal, tsbox, cli, lubridate, tibble
* **License**: MIT
* **Repository**: https://github.com/viniciusoike/trendseries
* **Website**: https://viniciusoike.github.io/trendseries/

### Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("viniciusoike/trendseries")
```

### Acknowledgments

This package builds upon excellent work from the R community: mFilter (economic filters), hpfilter (one-sided HP filter), RcppRoll (fast C++ rolling statistics), forecast (exponential smoothing), dlm (Kalman filtering), signal (signal processing), tsbox (time series conversions).
