# Add trend columns to data frame

Pipe-friendly function that adds trend columns to a tibble or
data.frame. Designed for exploratory analysis of monthly and quarterly
economic time series. Supports multiple trend extraction methods and
handles grouped data.

## Usage

``` r
augment_trends(
  data,
  date_col = "date",
  value_col = "value",
  group_vars = NULL,
  methods = "stl",
  frequency = NULL,
  suffix = NULL,
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

  Name of the value column. Defaults to `"value"`. Must be `numeric`.

- group_vars:

  Optional grouping variables for multiple time series. Can be a
  character vector of column names.

- methods:

  Character vector of trend methods. Options: `"hp"`, `"bk"`, `"cf"`,
  `"ma"`, `"stl"`, `"loess"`, `"spline"`, `"poly"`, `"bn"`, `"ucm"`,
  `"hamilton"`, `"spencer"`, `"ewma"`, `"wma"`, `"triangular"`,
  `"kernel"`, `"kalman"`, `"median"`, `"gaussian"`. Default is `"stl"`.

- frequency:

  The frequency of the series. Supports 4 (quarterly) or 12 (monthly).
  Will be auto-detected if not specified.

- suffix:

  Optional suffix for trend column names. If NULL, uses method names.

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

A tibble with original data plus trend columns named `trend_{method}` or
`trend_{method}_{suffix}` if suffix is provided.

## Details

This function is designed for monthly (frequency = 12) and quarterly
(frequency = 4) economic data. It uses economic-appropriate defaults for
all trend extraction methods.

For grouped data, the function applies trend extraction to each group
separately, maintaining the original data structure while adding trend
columns.

## Examples

``` r
# Simple STL decomposition on quarterly GDP construction data
gdp_construction |> augment_trends(value_col = "index")
#> Auto-detected quarterly (4 obs/year)
#> Computing STL trend with s.window = periodic
#> # A tibble: 122 × 3
#>    date       index trend_stl
#>    <date>     <dbl>     <dbl>
#>  1 1995-01-01 100       102. 
#>  2 1995-04-01 100       101. 
#>  3 1995-07-01 100       100. 
#>  4 1995-10-01 100        99.4
#>  5 1996-01-01  97.8     101. 
#>  6 1996-04-01 101.      102. 
#>  7 1996-07-01 107.      103. 
#>  8 1996-10-01 103.      104. 
#>  9 1997-01-01 101.      106. 
#> 10 1997-04-01 108.      109. 
#> # ℹ 112 more rows

# Multiple smoothing methods with unified parameter
gdp_construction |>
  augment_trends(
    value_col = "index",
    methods = c("hp", "loess", "ewma"),
    smoothing = 0.3
  )
#> Auto-detected quarterly (4 obs/year)
#> Computing HP filter (two-sided) with lambda = 480
#> Computing loess trend with span = 0.3
#> Computing EWMA with alpha = 0.3
#> # A tibble: 122 × 5
#>    date       index trend_hp trend_loess trend_ewma
#>    <date>     <dbl>    <dbl>       <dbl>      <dbl>
#>  1 1995-01-01 100       99.1        97.5      100  
#>  2 1995-04-01 100      100.         98.9      100  
#>  3 1995-07-01 100      101.        100.       100  
#>  4 1995-10-01 100      102.        102.       100  
#>  5 1996-01-01  97.8    103.        103.        99.3
#>  6 1996-04-01 101.     104.        104.        99.8
#>  7 1996-07-01 107.     104.        105.       102. 
#>  8 1996-10-01 103.     105.        106.       102. 
#>  9 1997-01-01 101.     106.        106.       102. 
#> 10 1997-04-01 108.     107.        107.       104. 
#> # ℹ 112 more rows

# Moving averages with unified window on monthly data
vehicles |>
  tail(60) |>
  augment_trends(
    value_col = "production",
    methods = c("ma", "wma", "triangular"),
    window = 8
  )
#> Auto-detected monthly (12 obs/year)
#> Computing 2x8-period MA (auto-adjusted for even-window centering)
#> Computing 8-period weighted MA with linear weights, center alignment
#> Computing 8-period triangular MA with center alignment
#> # A tibble: 60 × 5
#>    date       production trend_ma trend_wma trend_triangular
#>    <date>          <dbl>    <dbl>     <dbl>            <dbl>
#>  1 2020-08-01     193421      NA        NA               NA 
#>  2 2020-09-01     219033      NA        NA               NA 
#>  3 2020-10-01     230927      NA        NA               NA 
#>  4 2020-11-01     249104  216181.   213615.          224546.
#>  5 2020-12-01     261321  215282.   208187.          218335.
#>  6 2021-01-01     180904  212021.   205996.          209996.
#>  7 2021-02-01     186718  204914.   200905.          203805.
#>  8 2021-03-01     208801  195115.   193166.          198122.
#>  9 2021-04-01     191853  188457.   188418.          193806.
#> 10 2021-05-01     206221  185917    181049.          190493.
#> # ℹ 50 more rows

# Economic indicators with different methods
ibcbr |>
  tail(48) |>
  augment_trends(
    value_col = "index",
    methods = c("median", "kalman", "kernel"),
    window = 9,
    smoothing = 0.15
  )
#> Auto-detected monthly (12 obs/year)
#> Computing 9-period median filter with endrule = median
#> Computing Kalman smoother with measurement noise = auto
#> Computing kernel smoother with bandwidth = {bandwidth}, kernel = normal
#> # A tibble: 48 × 5
#>    date       index trend_median trend_kalman trend_kernel
#>    <date>     <dbl>        <dbl>        <dbl>        <dbl>
#>  1 2021-08-01  99.3         97.1         98.0         99.3
#>  2 2021-09-01  97.1         97.1         97.9         97.1
#>  3 2021-10-01  96.5         97.8         97.8         96.5
#>  4 2021-11-01  97.8         97.8         97.9         97.8
#>  5 2021-12-01  99.2         97.8         98.0         99.1
#>  6 2022-01-01  91.9         97.8         98.0         92.0
#>  7 2022-02-01  95.7         99.2         98.6         95.7
#>  8 2022-03-01 105.          99.3         99.5        105. 
#>  9 2022-04-01 100.          99.8         99.8        100. 
#> 10 2022-05-01  99.8        100.         100.          99.8
#> # ℹ 38 more rows

# Moving average with right alignment (causal filter)
vehicles |>
  tail(60) |>
  augment_trends(
    value_col = "production",
    methods = "ma",
    window = 12,
    align = "right"
  )
#> Auto-detected monthly (12 obs/year)
#> Computing 12-period MA with right alignment
#> # A tibble: 60 × 3
#>    date       production trend_ma
#>    <date>          <dbl>    <dbl>
#>  1 2020-08-01     193421       NA
#>  2 2020-09-01     219033       NA
#>  3 2020-10-01     230927       NA
#>  4 2020-11-01     249104       NA
#>  5 2020-12-01     261321       NA
#>  6 2021-01-01     180904       NA
#>  7 2021-02-01     186718       NA
#>  8 2021-03-01     208801       NA
#>  9 2021-04-01     191853       NA
#> 10 2021-05-01     206221       NA
#> # ℹ 50 more rows

# Advanced: fine-tune specific methods
electric |>
  tail(72) |>
  augment_trends(
    value_col = "consumption",
    methods = "median",
    window = 7
  )
#> Auto-detected monthly (12 obs/year)
#> Computing 7-period median filter with endrule = median
#> # A tibble: 72 × 3
#>    date       consumption trend_median
#>    <date>           <dbl>        <dbl>
#>  1 2019-08-01       10987        10987
#>  2 2019-09-01       11379        11379
#>  3 2019-10-01       11973        11973
#>  4 2019-11-01       12424        12201
#>  5 2019-12-01       12201        12383
#>  6 2020-01-01       12909        12383
#>  7 2020-02-01       12383        12383
#>  8 2020-03-01       12432        12318
#>  9 2020-04-01       12318        12318
#> 10 2020-05-01       11756        11852
#> # ℹ 62 more rows
```
