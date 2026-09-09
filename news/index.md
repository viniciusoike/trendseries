# Changelog

## trendseries 1.6.0

- [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md),
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md),
  and
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
  now handle date-column names that overlap generated column names,
  preserving the date column and applying the usual numeric suffix to
  the generated column.

- [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
  and
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  now apply the first window to methods such as WMA in mixed
  vector-window requests, with a warning, instead of silently using the
  default window.

- [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
  and
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  now honor Kalman smoothing as the measurement-to-process noise ratio
  and preserve individually supplied noise variances. Explicitly
  supplying both variances takes precedence over the ratio.

- [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
  and
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  now report STL and UCM estimator fallbacks even with `.quiet = TRUE`.
  Quiet augmentation also consolidates warnings and identifies affected
  groups.

- [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md),
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md),
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md),
  and
  [`index_series()`](https://viniciusoike.github.io/trendseries/reference/index_series.md)
  now keep groups distinct when their labels contain periods or combine
  missing values with the literal string `"NA"`.

- [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  and
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
  now reject interior missing values in daily and weekly series instead
  of silently removing those observations before estimation. Leading and
  trailing missing values and irregular trading calendars remain
  supported.

- [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  now warns about every group whose year-to-date accumulation starts
  mid-year.

- [`index_series()`](https://viniciusoike.github.io/trendseries/reference/index_series.md)
  now detects frequency independently within each group, including
  groups with different dating conventions or frequencies.

- Centered even-window moving averages now return padded `NA` values
  when the series cannot support the N+1 filter weights.

- [`roll_series()`](https://viniciusoike.github.io/trendseries/reference/roll_series.md)
  and
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  now honor `na_rm = TRUE` for centered even-window means by
  renormalizing the observed weights while retaining boundary padding.

- Fixed
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md),
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md),
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md),
  [`deseason_series()`](https://viniciusoike.github.io/trendseries/reference/deseason_series.md),
  and
  [`detrend_series()`](https://viniciusoike.github.io/trendseries/reference/detrend_series.md)
  returning rows in join or group order rather than preserving the
  caller’s input order.

- Fixed
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  dropping the warnings raised by the filter it dispatched to. A
  fallback to another estimator, such as a failed UCM fit or STL on a
  non-seasonal series, now reaches the caller, along with the group it
  came from. A warning raised for several groups is reported once.

- Fixed
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md),
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md),
  and
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
  dropping rows whose grouping column is `NA`. Those rows are now
  treated as one more series and returned with the rest.

### Irregular and daily series

- Fixed
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md),
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md),
  and
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
  returning an all-`NA` column for daily and weekly series. Results were
  converted back to a data frame through the `ts` time index, which
  advances by `1/252` per observation while a daily calendar skips
  weekends and holidays. The regenerated dates therefore drifted from
  the real ones, and the join back onto the input matched nothing.
  Results now carry the dates the series was built from, so they rejoin
  the rows they were computed from.

- Fixed
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  and
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  duplicating rows for semi-annual data. The merge key floored dates to
  the calendar unit, mapping any frequency other than 12 or 4 to the
  year, which put both halves of a year on one key. The key now follows
  the frequency.

- Fixed `window = "ytd"` resetting off-calendar for daily and weekly
  series. The year came from the `ts` time index, which advances a year
  every `frequency` observations, so the reset drifted further from
  January each year. Year-to-date accumulations now reset on the
  calendar year. A `ts` passed directly to
  [`roll_series()`](https://viniciusoike.github.io/trendseries/reference/roll_series.md)
  carries no dates, so `"ytd"` is rejected there for those frequencies.

- [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  and
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  now reject a repeated date in a daily or weekly series. Two rows
  cannot occupy one position, and results are matched back by date.

- Rebuilt the `trend_ma` column of `coffee_arabica` and
  `coffee_robusta`, which was `NA` for every row because the datasets
  were generated while the join above was broken. The column now holds
  the 22-observation right-aligned moving average its documentation
  describes.

### Indexing

- New
  [`index_series()`](https://viniciusoike.github.io/trendseries/reference/index_series.md)
  rescales one or more data-frame series to a configurable base value,
  using either the earliest observation or the mean over a year or date
  range, with support for grouped data and multiple value columns.

### Centered moving averages

- Fixed the placement of the 2xN moving average used by
  [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
  and
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  when `methods = "ma"` is called with an even `window` and
  `align = "center"`. The filter weights were correct but sat one period
  early, so the trend led the series by one month for a monthly 2x12 and
  by one quarter for a quarterly 2x4. Composing two centered `RcppRoll`
  passes caused this: for an even window, `align = "center"` places
  `n / 2` observations after the anchor and only `n / 2 - 1` before it,
  and the offset survives the second pass. The 2xN filter is now applied
  directly as the symmetric weights `c(1/2, 1, ..., 1, 1/2) / N`, which
  also pads `N / 2` positions at each end instead of `N / 2 - 1` at the
  start and `N / 2 + 1` at the end. Odd windows and non-centered
  alignments are unaffected, as are the other moving average methods.

- [`roll_series()`](https://viniciusoike.github.io/trendseries/reference/roll_series.md)
  and
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  apply the same 2xN filter for `stats = "mean"` with an even window and
  `align = "center"`, so the rolling mean and the `ma` trend method
  agree given the same window and alignment. The other statistics have
  no such correction and use a window with one extra period after the
  anchor.

### Rolling aggregations

- Fixed rolling and year-to-date aggregations returning a backend
  identity value for a window with no observed values under
  `na_rm = TRUE`. A `"sum"` returned `0`, a `"chain"` returned `0`, a
  `"min"` returned `Inf`, a `"max"` returned `-Inf`, a `"mean"` returned
  `NaN`, and an `"sd"` returned `0`. All six now return `NA`, as does
  `"sd"` for a window holding a single value.

- [`roll_series()`](https://viniciusoike.github.io/trendseries/reference/roll_series.md)
  and
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  now warn when a year-to-date accumulation starts mid-year. The first
  year of a series beginning in, say, July accumulates from July rather
  than from January, so it is not comparable with the years that follow.
  The values are unchanged.

- Fixed the chain scale warning never reaching a grouped
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  call. The warning was gated on `.quiet`, which the grouped path sets
  for every group. `.quiet` now suppresses progress messages only. The
  scale and calendar checks run once for the whole call rather than once
  per group.

- `window = "ytd"` now requires a seasonal frequency. Annual data
  previously returned the series unchanged, since each year holds one
  observation.

- [`roll_series()`](https://viniciusoike.github.io/trendseries/reference/roll_series.md)
  and
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  now warn about arguments the requested combination ignores: `align`
  under `window = "ytd"`, and `percent = TRUE` without
  `stats = "chain"`.

- A grouped
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  call with a window longer than some group now names every group that
  is too short, with its number of rows. The error previously came from
  whichever group failed first and named none of them.

### Documentation

- Documented that
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  preserves the caller’s input row order.

- Reorganized the pkgdown articles and package vignettes. The *Trend
  Extraction Methods* catalogue now lives on the documentation site as a
  pkgdown article.

- Updated vignette and article plots to use the `ekioplot` visual
  identity. The package is listed in `Suggests` and is used only when
  building the vignettes.

## trendseries 1.5.0

### Rolling and year-to-date aggregations

- New
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  and
  [`roll_series()`](https://viniciusoike.github.io/trendseries/reference/roll_series.md)
  add rolling and year-to-date aggregations, mirroring the
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  /
  [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
  pair.
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  takes a data frame and adds `roll_{stat}_{window}` columns
  (e.g. `roll_sum_12`);
  [`roll_series()`](https://viniciusoike.github.io/trendseries/reference/roll_series.md)
  takes a `ts`, `xts`, or `zoo` object and returns `ts` results. Six
  statistics are available: `"sum"`, `"chain"`, `"mean"`, `"sd"`,
  `"min"`, and `"max"`.

- `stats = "chain"` calculates `prod(1 + r) - 1` which assumes the
  series is a rate, e.g., monthly inflation rate. Use `percent = TRUE`
  when rates are in percentage points; a warning is issued when the
  values look mis-scaled for the declared setting.

- `window = "ytd"` computes an expanding year-to-date accumulation that
  resets each January, for any of the six statistics.

- `align` defaults to `"right"`, the convention for accumulated economic
  indicators, rather than the centered default used for trends.

- Grouped series are supported via `group_cols`, and multiple
  `value_col` entries are suffixed with the column name.

- Rolling statistics are kept in a registry separate from the trend
  methods. A rolling sum is not in the units of the series, so it is not
  a trend and cannot be passed to
  [`detrend_series()`](https://viniciusoike.github.io/trendseries/reference/detrend_series.md).

### Missing values and period grids

Series with gaps were previously handled differently by each entry
point, and the disagreements were silent. Missing value handling is now
one policy, applied everywhere: a gap inside the observed span is
rejected, and missing values at the edges are excluded from estimation
rather than rejected.

- [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md),
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md),
  [`deseason_series()`](https://viniciusoike.github.io/trendseries/reference/deseason_series.md),
  and
  [`detrend_series()`](https://viniciusoike.github.io/trendseries/reference/detrend_series.md)
  no longer return silently misdated results for series with interior
  gaps. These functions assumed the series had no gaps. Because
  observations are positioned by period, a missing period shifted every
  later value one slot earlier, so results merged back onto the wrong
  dates and the series lost its final period. Interior gaps and
  duplicated periods are now rejected, whether the gap comes from a row
  with a missing value or from a period absent from the data.

- [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
  now rejects missing values inside the observed span of a `ts`, `xts`,
  or `zoo` input. Previously they were passed straight to the filters,
  where the outcome depended on the method and was usually silent:
  `stl`, `spline`, and `hamilton` raised an error, `hp`, `bk`, and `cf`
  returned an all-`NA` series, and the recursive methods (`ewma`, `bn`)
  propagated the gap to every later observation. Impute the gaps before
  extracting a trend.

- Leading and trailing missing values continue to work.
  [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
  excludes them from estimation and returns the result on the time base
  of the input, with `NA` for the periods that were never observed.

- [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  and
  [`roll_series()`](https://viniciusoike.github.io/trendseries/reference/roll_series.md)
  are the exception, by design. A rolling window has well-defined local
  semantics for a gap, so rows with missing values keep their calendar
  position and `na_rm` controls whether an affected window yields `NA`
  or is computed from the observations present.

- [`df_to_ts()`](https://viniciusoike.github.io/trendseries/reference/df_to_ts.md)
  was the last entry point still building a misdated series from a
  gapped input, and it now applies the same check. A missing or
  duplicated period is rejected instead of shifting every later
  observation one slot earlier. Rows are also sorted before conversion,
  and a row with no date is dropped with a warning rather than left in
  place to occupy a period it cannot be positioned in. Missing *values*
  are kept, which is what leaves the series correctly dated.

- [`df_to_ts()`](https://viniciusoike.github.io/trendseries/reference/df_to_ts.md)
  now counts the starting period in units of the frequency. It
  previously used the calendar month whatever the frequency, so a
  quarterly series beginning in April started at Q4, an annual series
  dated March started two years late, and a semiannual series beginning
  in July started at H1. Monthly series were unaffected.

- Series whose frequency has no exact calendar period (weekly, daily)
  are not grid-checked. Their starting period is now placed
  proportionally within the year rather than defaulting to the first
  period.

### Empty input

- Fixed the error raised when a data frame has no rows.
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md),
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md),
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md),
  [`deseason_series()`](https://viniciusoike.github.io/trendseries/reference/deseason_series.md),
  and
  [`detrend_series()`](https://viniciusoike.github.io/trendseries/reference/detrend_series.md)
  now say the input has no rows. The message previously came from
  frequency detection or from a complete-cases check further downstream,
  and named neither the argument nor the problem.

- Fixed
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  and
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
  returning `NULL` for a grouped call on a data frame with no rows.

- Fixed
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  and
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
  failing on a grouped call when the grouping column is a factor with
  unused levels. [`split()`](https://rdrr.io/r/base/split.html) turns an
  unused level into an empty group, which was sent through the
  conversion path and rejected for having no complete cases. Empty
  groups are now dropped, as
  [`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
  already did.

### Documentation

- Function documentation has been tightened, and the missing value
  policy is now stated on the arguments it applies to.

- Reworded the README, the vignettes, and the help pages: cut the fixed
  method counts that go stale on each release, replaced promises that
  the components sum back exactly with what the functions do, and
  removed the duplicated sections in *Getting Started*.

- Added `"henderson"` to the documented `methods` options of
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  and
  [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md).
  The method has always been accepted, but the help pages listed the
  other nineteen.

## trendseries 1.4.0

CRAN release: 2026-07-14

This release combines the 1.3.0 development series, which was never
published on CRAN, with the 1.4.0 changes.

### New Features

- [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
  is now exported and available for use. This pipe-friendly function
  decomposes a time series into trend, seasonal, and remainder
  components, adding `trend_*`, `seasonal_*`, and `remainder_*` columns
  to the input data frame. Five methods are available: `"stl"`
  (default), `"regression"`, `"classic"` (classical decomposition via
  centred moving averages,
  [`stats::decompose()`](https://rdrr.io/r/stats/decompose.html)),
  `"bsm"` (Basic Structural state-space Model estimated by the Kalman
  smoother,
  [`stats::StructTS()`](https://rdrr.io/r/stats/StructTS.html)), and
  `"seats"` (X-13ARIMA-SEATS via the optional **`seasonal`** package, a
  Suggested dependency only required for this method). It supports
  grouped decomposition via `group_cols`, and the components add back up
  to the original series. See the new *Decomposing Series* vignette.
  Additional conveniences:

  - `methods` accepts a vector (e.g. `c("stl", "classic")`), adding each
    method’s components as separate columns for side-by-side comparison.
  - `transform = "log"` provides a uniform multiplicative decomposition
    across every method (decompose on the log scale, exponentiate back).
  - `seasadj = TRUE` adds a `seasadj_{method}` column with the
    seasonally adjusted series.

- [`deseason_series()`](https://viniciusoike.github.io/trendseries/reference/deseason_series.md)
  is a new convenience wrapper around
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
  focused on seasonal adjustment. It adds a `seasadj_{method}` column
  with the deseasoned series (methods `"stl"` or `"seats"`), and
  optionally the full trend/seasonal/remainder decomposition via
  `components = TRUE`.

- [`detrend_series()`](https://viniciusoike.github.io/trendseries/reference/detrend_series.md)
  is a new convenience wrapper around
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  that returns the detrended series, i.e. the deviation from the trend
  (the *cycle* in economics). `transform = "log"` returns the log
  deviation from trend (approximately the percentage deviation, the
  output-gap convention), and `components = TRUE` also keeps the fitted
  `trend_{method}` columns.

### Bug Fixes

- The Hamilton filter now uses frequency-aware default parameters, as
  documented. The defaults were hardcoded to the quarterly values
  (`h = 8`, `p = 4`) regardless of frequency, so monthly series were
  filtered with a two-quarter horizon instead of the recommended
  two-year one. Monthly data now defaults to `h = 24`, `p = 12`
  (Hamilton 2018); quarterly behaviour is unchanged. Because the monthly
  defaults are larger, monthly series now require at least 37
  observations (`h + p + 1`) and the first 35 trend values are `NA`
  (previously 13 and 11). Pass
  `params = list(hamilton_h = , hamilton_p = )` to reproduce old
  results.

### Internal Improvements

- Removed the **`glue`** dependency. The two remaining
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) calls
  were replaced by the interpolation `cli` already provides.

- Removed dead internal code left over from earlier refactors: the
  unused `.ensure_odd_window()` and `.check_deprecated_params()`
  helpers, leftover `zlema` references, and stale
  `HoltWinters`/`roll_median` namespace imports.

- The list of valid methods is now defined in a single internal
  registry, which
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  and
  [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
  both read from. The valid decomposition methods for
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
  are defined there as well.

- The unified parameter validation (`window`, `smoothing`, `band`,
  `align`, `params`) shared by
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  and
  [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
  now lives in a single internal helper, shared by both functions.

### Documentation

- Added a *Trend Extraction Methods* vignette cataloguing all 20 trend
  methods by family.

- Added a *Detrending Series* vignette covering
  [`detrend_series()`](https://viniciusoike.github.io/trendseries/reference/detrend_series.md):
  the deseason-then-detrend workflow for seasonal data, percentage
  deviations from trend via `transform = "log"`, method comparison (HP
  vs Hamilton), and grouped detrending.

- Removed outdated references to the **`TTR`** package from the
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  and
  [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
  documentation. The EWMA `window` parameter is now documented by what
  it does: it sets `alpha = 2 / (window + 1)`.

------------------------------------------------------------------------

## trendseries 1.2.0

CRAN release: 2026-05-02

### Breaking Changes

- The `group_vars` argument in
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  is deprecated in favour of `group_cols`, and calls with `group_vars`
  now issue a deprecation warning. Replace `group_vars = ...` with
  `group_cols = ...`; `group_vars` will be removed in a future release.

### New Features

- [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  accepts multiple value columns through a character vector in
  `value_col`. Trends are extracted for each column and named
  `trend_{method}_{col}` (e.g. `trend_stl_consumption`).

- The UCM trend estimator now uses fixed variance components with
  signal-to-noise ratios derived from Hodrick-Prescott filter lambdas,
  producing smoother trends by default. The `smoothing` parameter
  overrides the default.

- Added two Transport for London datasets: `transit_london_monthly`,
  monthly totals of reported bus and Tube journeys, and
  `transit_london_avgs`, monthly averages of the reported daily journey
  counts.

### Bug Fixes and Improvements

- Updated the vignettes to use `group_cols` in place of the deprecated
  `group_vars`.
- Fixed typos, grammar, and mislabeled plot axes across the vignettes.
- Removed a stale ZLEMA reference from the moving average documentation.

------------------------------------------------------------------------

## trendseries 1.1.0

CRAN release: 2025-11-12

**Release Date**: November 2025

### Breaking Changes

- **Removed Butterworth and Savitzky-Golay filters**: The Butterworth
  low-pass filter and the Savitzky-Golay polynomial smoothing have been
  removed to focus the package on core econometric methods. The `signal`
  package dependency has been removed.

- **Removed exponential smoothing methods**: Simple and double
  exponential smoothing (`exp_simple`, `exp_double`) have been removed.
  Users can continue using EWMA for exponential smoothing. The
  `forecast` package dependency has been removed.

### Note

- **EWMA retained**: The Exponentially Weighted Moving Average (EWMA)
  method has been kept as it is categorized as a moving average method
  and is widely used in economic analysis.

## trendseries 1.0.1

**Release Date**: January 2025

### Breaking Changes

- **Removed ZLEMA filter**: The Zero-Lag Exponential Moving Average
  (ZLEMA) has been removed from the package. This method was not
  commonly used in economic analysis and added unnecessary complexity.
  Users needing zero-lag smoothing can use EWMA with appropriate alpha
  values.

### New Features

- **Added Spencer filter**: Classic 15-term Spencer moving average
  filter for trend extraction. The Spencer filter uses symmetric weights
  designed to preserve cubic polynomial trends while providing smooth
  results. Implementation uses linear extrapolation at endpoints for
  simplicity and computational efficiency.

### Bug Fixes and Improvements

#### Moving Average Enhancements

- **Implemented econometrically correct 2xN MA for centered even-window
  moving averages**:
  - When using centered alignment with even windows (e.g., 12 for
    monthly data, 4 for quarterly), the simple moving average now
    automatically applies the proper 2xN double-smoothing technique
  - This matches the X-13ARIMA-SEATS standard for seasonal adjustment
  - Example: `window=12, align="center"` now correctly applies a 2x12 MA
    instead of naive centering
  - Non-centered alignments (right/left) and odd windows continue to use
    regular single MA
- **Fixed misleading “2x” notation in messages**:
  - Previous versions displayed “2x12” in messages but didn’t actually
    implement double smoothing
  - Now the “2x” notation only appears when the 2xN algorithm is
    actually used
  - Messages clearly indicate: “Computing 2x12-period MA (auto-adjusted
    for even-window centering)” vs “Computing 12-period MA with right
    alignment”
- **Added comprehensive tests for 2xN MA**:
  - 5 new test cases validating correct behavior for monthly and
    quarterly data
  - Tests confirm 2xN MA differs from simple MA for even-window centered
    cases
  - All 106 MA filter tests passing

#### Technical Changes

- Added `glue` package to Imports for message formatting
- Added `.ma_2x()` internal function implementing proper
  double-smoothing
- Added `.ensure_odd_window()` utility function for future use
- Updated test expectations to account for new 2xN behavior

#### Impact

This is an important correctness fix for users doing seasonal adjustment
or business cycle analysis with monthly/quarterly data. The new
implementation ensures that centered moving averages with even windows
produce econometrically sound results.

------------------------------------------------------------------------

## trendseries 1.0.0

The first production release of trendseries, an R package for extracting
trends from economic time series.

### Trend extraction

Two functions cover the main workflows.
[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
takes a data frame and adds one `trend_{method}` column per requested
method, with grouped operations through dplyr.
[`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
takes `ts`, `xts`, or `zoo` objects and returns `ts` results.

Four families cover the methods in this release.

- Econometric filters: HP (one-sided and two-sided), Baxter-King,
  Christiano-Fitzgerald, Hamilton, Beveridge-Nelson, and a state-space
  UCM.
- Moving averages: simple, weighted, exponential, zero-lag, triangular,
  median, and Gaussian.
- Smoothing: STL, loess, splines, polynomial trends, and simple and
  double exponential smoothing.
- Signal processing: Kalman smoother, Savitzky-Golay, Butterworth, and
  kernel smoothing.

Both functions share one parameter system across every method, with
`window`, `smoothing`, `band`, `align`, and `params`. Defaults track the
series frequency, so the HP filter sets lambda to 1600 for quarterly and
14400 for monthly data, moving averages default to four-quarter or
twelve-month windows, and bandpass filters use the 6-to-32-quarter
business cycle range. Monthly and quarterly series are the main target;
STL and the moving average methods also run on daily and other
frequencies.

### Datasets

Ten economic datasets ship with the package.

- Brazilian data (BCB): `gdp_construction`, `ibcbr`, `vehicles`,
  `oil_derivatives`, and `electric`.
- UK data (ONS): `retail_households` and `retail_autofuel`.
- Coffee prices (CEPEA): `coffee_arabica` and `coffee_robusta`, both
  daily.
- Metadata: `series_metadata`.

### Installation

``` r

# install.packages("devtools")
devtools::install_github("viniciusoike/trendseries")
```

### Quick example

``` r

library(trendseries)

gdp_construction |>
  augment_trends(value_col = "index", methods = c("hp", "bk", "ma"))
```

### Links

- Website: <https://viniciusoike.github.io/trendseries/>
- Repository: <https://github.com/viniciusoike/trendseries>

Built on mFilter, hpfilter, RcppRoll, forecast, dlm, signal, and tsbox.
MIT license; requires R 4.1.0 or later.
