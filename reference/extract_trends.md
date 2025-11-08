# Extract trends from time series objects

Extract trend components from time series objects using various
econometric methods. Designed for monthly and quarterly economic data
analysis. Returns trend components as time series objects or a list of
time series.

## Usage

``` r
extract_trends(
  ts_data,
  methods = "stl",
  window = NULL,
  smoothing = NULL,
  band = NULL,
  align = NULL,
  params = list(),
  .quiet = FALSE
)
```

## Arguments

- ts_data:

  A time series object (`ts`, `xts`, or `zoo`) or any object convertible
  via tsbox.

- methods:

  Character vector of trend methods. Options: `"hp"`, `"bk"`, `"cf"`,
  `"ma"`, `"stl"`, `"loess"`, `"spline"`, `"poly"`, `"bn"`, `"ucm"`,
  `"hamilton"`, `"spencer"`, `"ewma"`, `"wma"`, `"triangular"`,
  `"kernel"`, `"kalman"`, `"median"`, `"gaussian"`. Default is `"stl"`.

- window:

  Unified window/period parameter for moving average methods (ma, wma,
  triangular, stl, ewma, median, gaussian). Must be positive. If NULL,
  uses frequency-appropriate defaults. For EWMA, specifies the window
  size when using TTR's optimized implementation. Cannot be used
  simultaneously with `smoothing` for EWMA method.

- smoothing:

  Unified smoothing parameter for smoothing methods (hp, loess, spline,
  ewma, kernel, kalman). For hp: use large values (1600+) or small
  values (0-1) that get converted. For EWMA: specifies the alpha
  parameter (0-1) for traditional exponential smoothing. Cannot be used
  simultaneously with `window` for EWMA method. For kernel: multiplier
  of optimal bandwidth (1.0 = optimal, \<1 = less smooth, \>1 = more
  smooth). For kalman: controls the ratio of measurement to process
  noise (higher = more smoothing). For others: typically 0-1 range.

- band:

  Unified band parameter for bandpass filters (bk, cf). Both values must
  be positive. For bk/cf: Provide as `c(low, high)` where low/high are
  periods in quarters, e.g., `c(6, 32)`.

- align:

  Unified alignment parameter for moving average methods (ma, wma,
  triangular, gaussian). Valid values: `"center"` (default, uses
  surrounding values), `"right"` (causal, uses past values only),
  `"left"` (anti-causal, uses future values only). Note: triangular only
  supports `"center"` and `"right"`. If NULL, uses `"center"` as
  default.

- params:

  Optional list of method-specific parameters for fine control:

  - **HP Filter**: `hp_onesided` (logical, default FALSE) - Use
    one-sided (real-time) filter instead of two-sided

  - **Spline**: `spline_cv` (logical/NULL) - Cross-validation method:
    NULL (none), TRUE (leave-one-out), FALSE (GCV)

  - **Polynomial**: `poly_degree` (integer, default 1), `poly_raw`
    (logical, default FALSE for orthogonal polynomials)

  - **UCM**: `ucm_type` (character, default "level") - Model type:
    "level", "trend", or "BSM"

  - **Others**: `bn_ar_order`, `hamilton_h`, `hamilton_p`,
    `kernel_type`, `kalman_measurement_noise`, `kalman_process_noise`,
    `median_endrule`, `gaussian_sigma`, `wma_weights`.

  - **Note**: Alignment parameters (`ma_align`, `wma_align`,
    `triangular_align`, `gaussian_align`) can still be passed via
    `params` but it's recommended to use the unified `align` parameter
    instead.

- .quiet:

  If `TRUE`, suppress informational messages.

## Value

If single method, returns a `ts` object. If multiple methods, returns a
named list of `ts` objects.

## Details

This function focuses on monthly (frequency = 12) and quarterly
(frequency = 4) economic data. It uses established econometric methods
with appropriate defaults:

- **HP Filter**: lambda=1600 (quarterly), lambda=14400 (monthly).
  Supports both two-sided and one-sided (real-time) variants

- **Baxter-King**: Bandpass filter for business cycles (6-32 quarters
  default)

- **Christiano-Fitzgerald**: Asymmetric bandpass filter

- **Moving Average**: Centered, frequency-appropriate windows

- **STL**: Seasonal-trend decomposition

- **Loess**: Local polynomial regression

- **Spline**: Smoothing splines

- **Polynomial**: Linear/polynomial trends

- **Beveridge-Nelson**: Permanent/transitory decomposition

- **UCM**: Unobserved Components Model (local level)

- **Hamilton**: Regression-based alternative to HP filter

- **Advanced MA**: EWMA with various implementations

- **Kernel Smoother**: Non-parametric regression with various kernel
  functions

- **Kalman Smoother**: Adaptive filtering for noisy time series

- **Median Filter**: Robust filtering using running medians to remove
  outliers

- **Gaussian Filter**: Weighted average with Gaussian (normal) density
  weights

**Parameter Usage Notes**:

- **HP Filter**: Use `hp_onesided=TRUE` for real-time analysis or when
  future data should not influence current estimates. One-sided filter
  is appropriate for nowcasting, policy analysis, and avoiding
  look-ahead bias. Default two-sided filter is optimal for historical
  analysis.

- **EWMA**: Use either `window` (TTR optimization) OR `smoothing` (alpha
  parameter), not both

- **Kalman**: Use `smoothing` parameter or `params` list for fine
  control of noise parameters

- **Spline**: Use `spline_cv` to control cross-validation (NULL=none,
  TRUE=LOO-CV, FALSE=GCV)

- **Polynomial**: Use `poly_raw=FALSE` for orthogonal polynomials (more
  stable for degree \> 2) or `poly_raw=TRUE` for raw polynomials.
  Warning issued for degree \> 3 (overfitting risk).

- **UCM**: Choose model type - "level" (simplest), "trend" (time-varying
  slope), or "BSM" (with seasonal component, requires seasonal data)

## Examples

``` r
# Single method
hp_trend <- extract_trends(AirPassengers, methods = "hp")
#> Computing HP filter (two-sided) with lambda = 14400

# Multiple methods with unified smoothing
smooth_trends <- extract_trends(
  AirPassengers,
  methods = c("hp", "loess", "ewma"),
  smoothing = 0.3
)
#> Computing HP filter (two-sided) with lambda = 4320
#> Computing loess trend with span = 0.3
#> Computing EWMA with alpha = 0.3

# EWMA with window (uses TTR optimization)
ewma_window <- extract_trends(AirPassengers, methods = "ewma", window = 12)
#> Computing EWMA with window = 12

# EWMA with alpha (traditional formula)
ewma_alpha <- extract_trends(AirPassengers, methods = "ewma", smoothing = 0.2)
#> Computing EWMA with alpha = 0.2

# Moving averages with unified window
ma_trends <- extract_trends(
  AirPassengers,
  methods = c("ma", "wma", "triangular"),
  window = 8
)
#> Computing 2x8-period MA (auto-adjusted for even-window centering)
#> Computing 8-period weighted MA with linear weights, center alignment
#> Computing 8-period triangular MA with center alignment

# Bandpass filters with unified band
bp_trends <- extract_trends(
  AirPassengers,
  methods = c("bk", "cf"),
  band = c(6, 32)
)
#> Computing Baxter-King filter with bands [6, 32]
#> Computing Christiano-Fitzgerald filter with bands [6, 32]

# Moving average with right alignment (causal filter)
ma_causal <- extract_trends(
  AirPassengers,
  methods = "ma",
  window = 12,
  align = "right"
)
#> Computing 12-period MA with right alignment

# Signal processing methods with specific parameters
finance_trends <- extract_trends(
  AirPassengers,
  methods = c("kalman", "gaussian"),
  window = 9,  # For Gaussian filter
  params = list(kalman_measurement_noise = 0.1)  # Kalman-specific parameter
)
#> Computing Kalman smoother with measurement noise = {measurement_noise}
#> Computing 9-period Gaussian filter with sigma = 2.25, center alignment

# Spline with cross-validation options
spline_trends <- extract_trends(
  AirPassengers,
  methods = "spline",
  params = list(spline_cv = FALSE)  # Use GCV instead of default
)
#> Computing spline trend with automatic smoothing, GCV

# Polynomial with orthogonal vs raw polynomials
poly_trends <- extract_trends(
  AirPassengers,
  methods = "poly",
  params = list(poly_degree = 2, poly_raw = FALSE)  # Orthogonal (default)
)
#> Computing orthogonal polynomial trend with degree = 2

# UCM with different model types
ucm_trends <- extract_trends(
  AirPassengers,
  methods = "ucm",
  params = list(ucm_type = "BSM")  # Basic Structural Model with seasonality
)
#> Computing UCM trend: Basic Structural Model with seasonal component

# HP Filter: One-sided (real-time) vs Two-sided (historical)
hp_realtime <- extract_trends(
  AirPassengers,
  methods = "hp",
  params = list(hp_onesided = TRUE)  # For nowcasting and real-time analysis
)
#> Computing HP filter (one-sided) with lambda = 14400

# Advanced: fine-tune specific methods
custom_trends <- extract_trends(
  AirPassengers,
  methods = c("median", "kalman"),
  window = 7,
  params = list(median_endrule = "constant")
)
#> Computing 7-period median filter with endrule = constant
#> Computing Kalman smoother with measurement noise = auto
```
