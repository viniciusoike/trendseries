# Detrend a time series

Pipe-friendly convenience wrapper around
[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
focused on a single task: removing the trend from a time series. It adds
a `detrend_{method}` column holding the detrended series (the deviation
from trend, often called the *cycle* in economics) and, optionally, the
underlying trend itself.

For econometric filters such as `"hp"` (the default), `"bk"`, `"cf"`,
and `"hamilton"`, the detrended series is the business-cycle component
those filters were designed to isolate (e.g. the output gap).

## Usage

``` r
detrend_series(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  methods = "hp",
  transform = "none",
  frequency = NULL,
  components = FALSE,
  window = NULL,
  smoothing = NULL,
  band = NULL,
  align = NULL,
  params = list(),
  .quiet = FALSE
)
```

## Arguments

- data:

  A `data.frame`, `tibble`, or `data.table` containing the time series
  data.

- date_col:

  Name of the date column. Defaults to `"date"`. Must be of class
  `Date`.

- value_col:

  Name of the value column(s). Defaults to `"value"`. Must be numeric. A
  character vector of length \> 1 is accepted; each column is detrended
  separately and the results are named `detrend_{method}_{col}` (e.g.
  `detrend_hp_consumption`).

- group_cols:

  Optional grouping variables for multiple time series. Can be a
  character vector of column names.

- methods:

  Character vector of trend methods used for detrending. Any method
  supported by
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  is accepted. Default is `"hp"` (Hodrick-Prescott filter with
  frequency-appropriate smoothing). When several methods are supplied,
  each one contributes its own `detrend_{method}` column so the
  detrended series can be compared side by side.

- transform:

  Transformation applied before detrending. One of:

  - `"none"` (default): the trend is fitted to the raw series and
    `detrend = value - trend`, in the units of the series.

  - `"log"`: the trend is fitted to the log series and
    `detrend = log(value) - log(trend)`, the *log deviation from trend*.
    Multiplied by 100, this is approximately the percentage deviation
    from trend (the convention for output gaps). Requires strictly
    positive values. The `trend_{method}` columns (when
    `components = TRUE`) are reported back in the units of the series.

- frequency:

  The frequency of the series. Supports 4 (quarterly) or 12 (monthly).
  Will be auto-detected if not specified.

- components:

  If `FALSE` (default), only the detrended `detrend_{method}` column is
  added. If `TRUE`, the fitted `trend_{method}` column is also kept.

- window:

  Unified window/period parameter for moving average methods; see
  [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md).
  For `"ma"`, `"median"`, and `"henderson"`, a numeric vector is
  accepted (e.g. `c(6, 12)`), which adds one detrended column per window
  value (`detrend_ma_6`, `detrend_ma_12`, ...).

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
  be positive. Provide as `c(low, high)` where low/high are periods in
  quarters, e.g., `c(6, 32)`.

- align:

  Unified alignment parameter for moving average methods (ma, wma,
  triangular, gaussian). Valid values: `"center"` (default, uses
  surrounding values), `"right"` (causal, uses past values only),
  `"left"` (anti-causal, uses future values only). Note: triangular only
  supports `"center"` and `"right"`. If NULL, uses `"center"` as
  default.

- params:

  Optional list of method-specific parameters for fine control.

- .quiet:

  If `TRUE`, suppress informational messages.

## Value

A tibble with the original columns plus, for each requested method, a
`detrend_{method}` column holding the detrended series. When
`components = TRUE`, the `trend_{method}` column is kept as well.

Each detrended column mirrors the name of the trend column it derives
from: window vectors yield `detrend_ma_6`, `detrend_ma_12`, and a trend
column renamed to avoid a naming conflict yields a matching detrended
name.

With `transform = "none"` the trend and the detrended series should add
back up to the original (`value = trend + detrend`); with
`transform = "log"` the relation is `value = trend * exp(detrend)`.
Methods with boundary effects (e.g. `"bk"`, `"hamilton"`) produce `NA`
trend values at the affected observations, and the detrended series is
`NA` there too.

Output rows are ordered by date within each group; the original row
order is not preserved.

## Details

`detrend_series()` is a thin wrapper: it calls
[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
with the requested methods and subtracts each fitted trend from the
series (on the log scale when `transform = "log"`). All trend-fitting
behaviour, validation, grouping, and the unified parameters (`window`,
`smoothing`, `band`, `align`, `params`) are inherited unchanged from
[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md).
See its documentation for method internals and parameter details.

Detrending does **not** remove seasonality: the detrended series of a
raw seasonal series still contains the seasonal swings, and seasonality
can leak into the cycle estimated by filters such as HP. For seasonal
data, seasonally adjust first and detrend the adjusted series (see
Examples), or use
[`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
for a full trend/seasonal/remainder split.

## See also

[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
for the underlying trend extraction and the full set of methods;
[`deseason_series()`](https://viniciusoike.github.io/trendseries/reference/deseason_series.md)
to remove seasonality;
[`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
for a full decomposition.

## Examples

``` r
# HP-filter detrending (the default): adds a detrend_hp column
gdp_construction |>
  detrend_series(value_col = "index")
#> Auto-detected quarterly (4 obs/year)
#> Computing HP filter (two-sided) with lambda = 1600
#> # A tibble: 124 × 3
#>    date       index detrend_hp
#>    <date>     <dbl>      <dbl>
#>  1 1995-01-01 100       -0.706
#>  2 1995-04-01 100       -1.34 
#>  3 1995-07-01 100       -1.98 
#>  4 1995-10-01 100       -2.61 
#>  5 1996-01-01  97.8     -5.45 
#>  6 1996-04-01 101.      -2.85 
#>  7 1996-07-01 107.       2.89 
#>  8 1996-10-01 103.      -2.35 
#>  9 1997-01-01 101.      -4.70 
#> 10 1997-04-01 108.       1.80 
#> # ℹ 114 more rows

# Log deviation from trend (x 100 ~ percentage gap, the output-gap convention)
gdp_construction |>
  detrend_series(value_col = "index", transform = "log")
#> Auto-detected quarterly (4 obs/year)
#> Computing HP filter (two-sided) with lambda = 1600
#> # A tibble: 124 × 3
#>    date       index detrend_hp
#>    <date>     <dbl>      <dbl>
#>  1 1995-01-01 100     -0.00644
#>  2 1995-04-01 100     -0.0125 
#>  3 1995-07-01 100     -0.0185 
#>  4 1995-10-01 100     -0.0245 
#>  5 1996-01-01  97.8   -0.0528 
#>  6 1996-04-01 101.    -0.0263 
#>  7 1996-07-01 107.     0.0290 
#>  8 1996-10-01 103.    -0.0209 
#>  9 1997-01-01 101.    -0.0439 
#> 10 1997-04-01 108.     0.0185 
#> # ℹ 114 more rows

# Keep the fitted trend alongside the detrended series
gdp_construction |>
  detrend_series(value_col = "index", components = TRUE)
#> Auto-detected quarterly (4 obs/year)
#> Computing HP filter (two-sided) with lambda = 1600
#> # A tibble: 124 × 4
#>    date       index trend_hp detrend_hp
#>    <date>     <dbl>    <dbl>      <dbl>
#>  1 1995-01-01 100       101.     -0.706
#>  2 1995-04-01 100       101.     -1.34 
#>  3 1995-07-01 100       102.     -1.98 
#>  4 1995-10-01 100       103.     -2.61 
#>  5 1996-01-01  97.8     103.     -5.45 
#>  6 1996-04-01 101.      104.     -2.85 
#>  7 1996-07-01 107.      104.      2.89 
#>  8 1996-10-01 103.      105.     -2.35 
#>  9 1997-01-01 101.      106.     -4.70 
#> 10 1997-04-01 108.      106.      1.80 
#> # ℹ 114 more rows

# Compare detrending methods side by side
gdp_construction |>
  detrend_series(value_col = "index", methods = c("hp", "stl", "loess"))
#> Auto-detected quarterly (4 obs/year)
#> Computing HP filter (two-sided) with lambda = 1600
#> Computing STL trend with s.window = periodic
#> Computing loess trend with span = 0.75
#> # A tibble: 124 × 5
#>    date       index detrend_hp detrend_stl detrend_loess
#>    <date>     <dbl>      <dbl>       <dbl>         <dbl>
#>  1 1995-01-01 100       -0.706      -2.44          -9.17
#>  2 1995-04-01 100       -1.34       -1.14          -8.48
#>  3 1995-07-01 100       -1.98       -0.181         -7.84
#>  4 1995-10-01 100       -2.61        0.604         -7.24
#>  5 1996-01-01  97.8     -5.45       -2.96          -8.88
#>  6 1996-04-01 101.      -2.85       -1.01          -5.14
#>  7 1996-07-01 107.       2.89        4.81           1.69
#>  8 1996-10-01 103.      -2.35       -0.943         -2.53
#>  9 1997-01-01 101.      -4.70       -4.90          -3.94
#> 10 1997-04-01 108.       1.80       -0.772          3.44
#> # ℹ 114 more rows

# Seasonal data: deseason first, then detrend the adjusted series
gdp_construction |>
  deseason_series(value_col = "index") |>
  detrend_series(value_col = "seasadj_stl")
#> Auto-detected quarterly (4 obs/year)
#> Computing STL decomposition with s.window = "periodic"
#> Auto-detected quarterly (4 obs/year)
#> Computing HP filter (two-sided) with lambda = 1600
#> # A tibble: 124 × 4
#>    date       index seasadj_stl detrend_hp
#>    <date>     <dbl>       <dbl>      <dbl>
#>  1 1995-01-01 100         104.       3.02 
#>  2 1995-04-01 100         102.      -0.290
#>  3 1995-07-01 100          95.6     -6.91 
#>  4 1995-10-01 100          98.4     -4.59 
#>  5 1996-01-01  97.8       102.      -1.44 
#>  6 1996-04-01 101.        103.      -1.53 
#>  7 1996-07-01 107.        103.      -1.79 
#>  8 1996-10-01 103.        101.      -4.09 
#>  9 1997-01-01 101.        105.      -0.483
#> 10 1997-04-01 108.        110.       3.31 
#> # ℹ 114 more rows

# Grouped detrending: one trend per electricity sector
electricity |>
  detrend_series(group_cols = "name_series")
#> Auto-detected monthly (12 obs/year)
#> Computing 1 method(s) for 3 group(s):
#> ℹ Methods: "hp"
#> ℹ Groups: "electric_commercial", "electric_industrial", and
#>   "electric_residential"
#> # A tibble: 1,689 × 4
#>    date       name_series         value detrend_hp
#>    <date>     <chr>               <dbl>      <dbl>
#>  1 1979-02-01 electric_commercial  1030       9.23
#>  2 1979-03-01 electric_commercial  1057      29.3 
#>  3 1979-04-01 electric_commercial  1044       9.33
#>  4 1979-05-01 electric_commercial  1038      -3.62
#>  5 1979-06-01 electric_commercial  1002     -46.6 
#>  6 1979-07-01 electric_commercial   979     -76.6 
#>  7 1979-08-01 electric_commercial   985     -77.5 
#>  8 1979-09-01 electric_commercial  1047     -22.5 
#>  9 1979-10-01 electric_commercial  1067      -9.49
#> 10 1979-11-01 electric_commercial  1113      29.6 
#> # ℹ 1,679 more rows
```
