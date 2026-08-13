# Decompose time series into trend, seasonal, and remainder components

Pipe-friendly function that decomposes a time series into its trend,
seasonal, and remainder components, adding them as columns to the input
data frame.

## Usage

``` r
decompose_series(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  methods = "stl",
  trend = "linear",
  transform = "none",
  frequency = NULL,
  seasadj = FALSE,
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

  Name of the value column. Defaults to `"value"`. Must be numeric.

- group_cols:

  Optional grouping variables for multiple time series. A character
  vector of column names. When provided, decomposition is applied
  independently to each group.

- methods:

  Decomposition method(s). One or more of `"stl"`, `"regression"`,
  `"classic"`, `"bsm"`, or `"seats"`. Default is `"stl"`. When several
  methods are supplied (e.g. `c("stl", "classic")`), each one
  contributes its own `trend_*`, `seasonal_*`, and `remainder_*` columns
  so decompositions can be compared side by side.

  - `"stl"`: Seasonal-Trend decomposition via Loess
    ([`stats::stl()`](https://rdrr.io/r/stats/stl.html)).

  - `"regression"`: joint OLS trend + seasonal-dummy model.

  - `"classic"`: classical decomposition via moving averages
    ([`stats::decompose()`](https://rdrr.io/r/stats/decompose.html)).

  - `"bsm"`: Basic Structural (state-space) Model via the Kalman
    smoother
    ([`stats::StructTS()`](https://rdrr.io/r/stats/StructTS.html)).

  - `"seats"`: X-13ARIMA-SEATS decomposition (requires the
    **`seasonal`** package; see Details).

- trend:

  For `methods = "regression"` only: the polynomial form of the trend
  component. One of `"linear"`, `"quadratic"`, or `"cubic"`. Ignored by
  the other methods. Default is `"linear"`.

- transform:

  Transformation applied to the series before decomposition. One of
  `"none"` (default, additive decomposition) or `"log"`. With `"log"`,
  the series is log-transformed, decomposed additively, and the
  components are exponentiated back, yielding a *multiplicative*
  decomposition.

- frequency:

  The frequency of the series. Supports 4 (quarterly) or 12 (monthly).
  Will be auto-detected if not specified. All methods require
  `frequency > 1`.

- seasadj:

  If `TRUE`, also add a `seasadj_{method}` column holding the seasonally
  adjusted series (the series with the seasonal component removed:
  `trend + remainder` for additive decompositions, `trend * remainder`
  for multiplicative ones). Default `FALSE`.

- params:

  Optional list of method-specific parameters for fine control. Every
  parameter has a default, so this argument is only needed for
  non-standard use cases.

  For **STL** (`methods = "stl"`):

  - `s.window` or `stl_s_window`: seasonal smoothing window. Either
    `"periodic"` (default, assumes constant seasonal pattern) or a
    positive odd integer (larger values allow more slowly evolving
    seasonality).

  - `t.window` or `stl_t_window`: trend smoothing window (odd integer,
    or `NULL` to let [`stats::stl()`](https://rdrr.io/r/stats/stl.html)
    choose automatically — recommended default).

  - `robust` or `stl_robust`: logical. If `TRUE`, uses robust fitting to
    reduce the influence of outliers. Default `FALSE`.

  For **regression** (`methods = "regression"`):

  - `poly_raw`: logical. If `FALSE` (default), uses orthogonal
    polynomials (numerically stable, recommended). If `TRUE`, uses raw
    polynomials (more interpretable coefficients, less stable for degree
    \>= 2).

  **classic**, **bsm**, and **seats** take no `params`. For
  multiplicative seasonality with any method, use `transform = "log"`.

- .quiet:

  If `TRUE`, suppress informational messages.

## Value

A tibble with the original columns plus, for each requested method,
three new columns (and a fourth when `seasadj = TRUE`):

- `trend_{method}`: the estimated trend component.

- `seasonal_{method}`: the estimated seasonal component.

- `remainder_{method}`: what remains after removing trend and seasonal.

- `seasadj_{method}`: the seasonally adjusted series (only if
  `seasadj = TRUE`).

With `transform = "none"` the components should add back up to the
series (`value = trend + seasonal + remainder`); with
`transform = "log"` they should multiply back to it
(`value = trend * seasonal * remainder`). For `"classic"` the trend (and
hence remainder) is `NA` for the first and last `frequency / 2`
observations (the centred moving average has no boundary support).

Output rows are ordered by date within each group; the original row
order is not preserved.

## Details

All methods require seasonal data (`frequency > 1`). For non-seasonal
(annual) series, use
[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
to extract a trend component only.

### STL Decomposition

Uses [`stats::stl()`](https://rdrr.io/r/stats/stl.html) (Seasonal-Trend
decomposition via Loess). The seasonal component is estimated with a
loess smoother, the trend with an adaptive moving average, and the
remainder is the residual. The defaults (`s.window = "periodic"`,
`robust = FALSE`) assume a stable seasonal pattern.

### Regression Decomposition

Fits a joint OLS model: \$\$y_t = f(t) + s(t) + \epsilon_t\$\$ where
\\f(t)\\ is a polynomial in time and \\s(t)\\ is captured by period
dummy variables (month or quarter indicators). The components are
isolated via `stats::predict(type = "terms")`:

- **Trend**: constant + polynomial terms (captures the long-run level
  and direction).

- **Seasonal**: period dummy terms, centred to mean zero over the
  sample.

- **Remainder**: residuals from the full model.

By default, orthogonal polynomials (`poly_raw = FALSE`) are used for
numerical stability, which matters most for `trend = "cubic"`.

### Classical Decomposition

Uses [`stats::decompose()`](https://rdrr.io/r/stats/decompose.html). The
trend is a centred moving average of order equal to the frequency; the
seasonal component is the average detrended value for each period; the
remainder is the residual. Simple and fast, but the other methods handle
evolving seasonality and the endpoints better.

### Basic Structural Model (BSM)

Uses `stats::StructTS(type = "BSM")`, a state-space model with
stochastic level, slope, and seasonal components estimated by maximum
likelihood and extracted with the Kalman smoother
([`stats::tsSmooth()`](https://rdrr.io/r/stats/tsSmooth.html)). Unlike
the moving-average methods it produces trend and seasonal estimates for
every observation, including the endpoints, and lets both components
evolve over time. Fitting relies on numerical optimisation and can
occasionally fail to converge on short or irregular series.

### X-13ARIMA-SEATS (SEATS)

Uses the **`seasonal`** package, which wraps the U.S. Census Bureau's
X-13ARIMA-SEATS program. `seas()` is run with its automatic defaults
(model selection, log/level transformation, outlier detection, and
calendar adjustment), and the SEATS trend-cycle (`s12`) and seasonally
adjusted series (`s11`) are mapped to an additive
trend/seasonal/remainder, whichever transformation X-13 picked
internally. Because X-13 picks that transformation itself, `seats` is
best used with the default `transform = "none"`; an outer log transform
is redundant.

### Multiplicative Seasonality

When the seasonal amplitude grows with the level of the series (a
multiplicative pattern, common in economic data), set
`transform = "log"`. The series is log-transformed, decomposed
additively, and the components are exponentiated back. Every method
takes this same path, which requires strictly positive values.

## Examples

``` r
# STL decomposition (default settings work well for most economic series)
gdp_construction |>
  decompose_series(value_col = "index")
#> Auto-detected quarterly (4 obs/year)
#> Computing STL decomposition with s.window = "periodic"
#> # A tibble: 124 × 5
#>    date       index trend_stl seasonal_stl remainder_stl
#>    <date>     <dbl>     <dbl>        <dbl>         <dbl>
#>  1 1995-01-01 100       102.         -4.37         1.93 
#>  2 1995-04-01 100       101.         -1.62         0.476
#>  3 1995-07-01 100       100.          4.44        -4.62 
#>  4 1995-10-01 100        99.4         1.55        -0.946
#>  5 1996-01-01  97.8     101.         -4.37         1.42 
#>  6 1996-04-01 101.      102.         -1.62         0.613
#>  7 1996-07-01 107.      103.          4.44         0.370
#>  8 1996-10-01 103.      104.          1.55        -2.49 
#>  9 1997-01-01 101.      106.         -4.37        -0.530
#> 10 1997-04-01 108.      109.         -1.62         0.849
#> # ℹ 114 more rows

# STL with robust fitting (useful when the series has outliers)
gdp_construction |>
  decompose_series(
    value_col = "index",
    params = list(robust = TRUE)
  )
#> Auto-detected quarterly (4 obs/year)
#> Computing STL decomposition with s.window = "periodic", robust = TRUE
#> # A tibble: 124 × 5
#>    date       index trend_stl seasonal_stl remainder_stl
#>    <date>     <dbl>     <dbl>        <dbl>         <dbl>
#>  1 1995-01-01 100        103.        -4.48         1.57 
#>  2 1995-04-01 100        102.        -1.23        -0.685
#>  3 1995-07-01 100        101.         4.57        -5.66 
#>  4 1995-10-01 100        100.         1.14        -1.30 
#>  5 1996-01-01  97.8      101.        -4.48         1.34 
#>  6 1996-04-01 101.       102.        -1.23         0.238
#>  7 1996-07-01 107.       103.         4.57         0.205
#>  8 1996-10-01 103.       104.         1.14        -2.21 
#>  9 1997-01-01 101.       106.        -4.48        -0.478
#> 10 1997-04-01 108.       109.        -1.23         0.489
#> # ℹ 114 more rows

# STL with evolving seasonality (s.window controls how fast it can change)
gdp_construction |>
  decompose_series(
    value_col = "index",
    params = list(s.window = 13)
  )
#> Auto-detected quarterly (4 obs/year)
#> Computing STL decomposition with s.window = 13
#> # A tibble: 124 × 5
#>    date       index trend_stl seasonal_stl remainder_stl
#>    <date>     <dbl>     <dbl>        <dbl>         <dbl>
#>  1 1995-01-01 100       102.        -4.17          2.06 
#>  2 1995-04-01 100       101.        -0.804        -0.171
#>  3 1995-07-01 100       100.         3.89         -4.01 
#>  4 1995-10-01 100        99.5        1.10         -0.556
#>  5 1996-01-01  97.8     101.        -4.17          1.25 
#>  6 1996-04-01 101.      102.        -0.837        -0.122
#>  7 1996-07-01 107.      103.         3.88          0.904
#>  8 1996-10-01 103.      104.         1.15         -2.15 
#>  9 1997-01-01 101.      106.        -4.18         -0.687
#> 10 1997-04-01 108.      109.        -0.869         0.144
#> # ℹ 114 more rows

# Regression with cubic trend
gdp_construction |>
  decompose_series(
    value_col = "index",
    methods = "regression",
    trend = "cubic"
  )
#> Auto-detected quarterly (4 obs/year)
#> Computing regression decomposition: cubic trend (orthogonal polynomial, degree
#> = 3) + 4-period dummies
#> # A tibble: 124 × 5
#>    date       index trend_regression seasonal_regression remainder_regression
#>    <date>     <dbl>            <dbl>               <dbl>                <dbl>
#>  1 1995-01-01 100               98.6               -4.46                5.87 
#>  2 1995-04-01 100               98.9               -1.62                2.76 
#>  3 1995-07-01 100               99.1                4.45               -3.60 
#>  4 1995-10-01 100               99.5                1.64               -1.09 
#>  5 1996-01-01  97.8             99.8               -4.46                2.46 
#>  6 1996-04-01 101.             100.                -1.62                2.49 
#>  7 1996-07-01 107.             101.                 4.45                2.39 
#>  8 1996-10-01 103.             101.                 1.64                0.151
#>  9 1997-01-01 101.             101.                -4.46                4.03 
#> 10 1997-04-01 108.             102.                -1.62                7.76 
#> # ℹ 114 more rows

# Classical decomposition via moving averages (boundary trend is NA)
gdp_construction |>
  decompose_series(
    value_col = "index",
    methods = "classic"
  )
#> Auto-detected quarterly (4 obs/year)
#> Computing classical decomposition (additive)
#> # A tibble: 124 × 5
#>    date       index trend_classic seasonal_classic remainder_classic
#>    <date>     <dbl>         <dbl>            <dbl>             <dbl>
#>  1 1995-01-01 100            NA              -4.37            NA    
#>  2 1995-04-01 100            NA              -1.58            NA    
#>  3 1995-07-01 100            99.7             4.37            -4.09 
#>  4 1995-10-01 100            99.6             1.59            -1.16 
#>  5 1996-01-01  97.8         101.             -4.37             1.54 
#>  6 1996-04-01 101.          102.             -1.58             0.714
#>  7 1996-07-01 107.          103.              4.37             0.389
#>  8 1996-10-01 103.          104.              1.59            -2.74 
#>  9 1997-01-01 101.          106.             -4.37            -0.649
#> 10 1997-04-01 108.          109.             -1.58             0.932
#> # ℹ 114 more rows

# Basic Structural Model (state-space, components for every observation)
gdp_construction |>
  decompose_series(
    value_col = "index",
    methods = "bsm"
  )
#> Auto-detected quarterly (4 obs/year)
#> Computing Basic Structural Model decomposition (Kalman smoother)
#> # A tibble: 124 × 5
#>    date       index trend_bsm seasonal_bsm remainder_bsm
#>    <date>     <dbl>     <dbl>        <dbl>         <dbl>
#>  1 1995-01-01 100       104.       -3.51       -1.12e- 8
#>  2 1995-04-01 100        99.4       0.581      -6.04e-14
#>  3 1995-07-01 100        98.0       2.00        2.22e-15
#>  4 1995-10-01 100        99.0       1.02        5.77e-15
#>  5 1996-01-01  97.8     101.       -3.56        2.66e-15
#>  6 1996-04-01 101.      101.        0.0682      1.75e-15
#>  7 1996-07-01 107.      105.        2.77        1.33e-15
#>  8 1996-10-01 103.      102.        0.865      -3.33e-15
#>  9 1997-01-01 101.      105.       -3.76       -7.55e-15
#> 10 1997-04-01 108.      108.       -0.191      -5.55e-17
#> # ℹ 114 more rows

# X-13ARIMA-SEATS (requires the 'seasonal' package)
if (requireNamespace("seasonal", quietly = TRUE)) {
  gdp_construction |>
    decompose_series(
      value_col = "index",
      methods = "seats"
    )
}
#> Auto-detected quarterly (4 obs/year)
#> Computing X-13ARIMA-SEATS decomposition (SEATS)
#> # A tibble: 124 × 5
#>    date       index trend_seats seasonal_seats remainder_seats
#>    <date>     <dbl>       <dbl>          <dbl>           <dbl>
#>  1 1995-01-01 100         103.           -3.80           0.771
#>  2 1995-04-01 100         101.           -1.06           0.460
#>  3 1995-07-01 100          98.3           3.41          -1.66 
#>  4 1995-10-01 100          98.9           1.19          -0.114
#>  5 1996-01-01  97.8       101.           -3.73           0.540
#>  6 1996-04-01 101.        102.           -1.07          -0.263
#>  7 1996-07-01 107.        103.            3.68           0.953
#>  8 1996-10-01 103.        103.            1.23          -1.37 
#>  9 1997-01-01 101.        105.           -3.86          -0.226
#> 10 1997-04-01 108.        109.           -1.15           0.134
#> # ℹ 114 more rows

# Multiplicative decomposition via log transform (works for any method)
oil_derivatives |>
  decompose_series(
    value_col = "production",
    transform = "log"
  )
#> Auto-detected monthly (12 obs/year)
#> Computing STL decomposition with s.window = "periodic"
#> # A tibble: 563 × 5
#>    date       production trend_stl seasonal_stl remainder_stl
#>    <date>          <dbl>     <dbl>        <dbl>         <dbl>
#>  1 1979-02-01        167      162.        1.01          1.02 
#>  2 1979-03-01        164      164.        0.997         1.00 
#>  3 1979-04-01        165      166.        0.997         0.999
#>  4 1979-05-01        166      168.        0.980         1.01 
#>  5 1979-06-01        165      170.        0.998         0.974
#>  6 1979-07-01        170      172.        1.00          0.986
#>  7 1979-08-01        168      174.        1.00          0.962
#>  8 1979-09-01        175      176.        0.989         1.00 
#>  9 1979-10-01        179      178.        0.994         1.01 
#> 10 1979-11-01        185      181.        0.997         1.03 
#> # ℹ 553 more rows

# Several methods at once for side-by-side comparison
gdp_construction |>
  decompose_series(
    value_col = "index",
    methods   = c("stl", "classic")
  )
#> Auto-detected quarterly (4 obs/year)
#> Computing STL decomposition with s.window = "periodic"
#> Computing classical decomposition (additive)
#> # A tibble: 124 × 8
#>    date       index trend_stl seasonal_stl remainder_stl trend_classic
#>    <date>     <dbl>     <dbl>        <dbl>         <dbl>         <dbl>
#>  1 1995-01-01 100       102.         -4.37         1.93           NA  
#>  2 1995-04-01 100       101.         -1.62         0.476          NA  
#>  3 1995-07-01 100       100.          4.44        -4.62           99.7
#>  4 1995-10-01 100        99.4         1.55        -0.946          99.6
#>  5 1996-01-01  97.8     101.         -4.37         1.42          101. 
#>  6 1996-04-01 101.      102.         -1.62         0.613         102. 
#>  7 1996-07-01 107.      103.          4.44         0.370         103. 
#>  8 1996-10-01 103.      104.          1.55        -2.49          104. 
#>  9 1997-01-01 101.      106.         -4.37        -0.530         106. 
#> 10 1997-04-01 108.      109.         -1.62         0.849         109. 
#> # ℹ 114 more rows
#> # ℹ 2 more variables: seasonal_classic <dbl>, remainder_classic <dbl>

# Also return the seasonally adjusted series
gdp_construction |>
  decompose_series(
    value_col = "index",
    seasadj   = TRUE
  )
#> Auto-detected quarterly (4 obs/year)
#> Computing STL decomposition with s.window = "periodic"
#> # A tibble: 124 × 6
#>    date       index trend_stl seasonal_stl remainder_stl seasadj_stl
#>    <date>     <dbl>     <dbl>        <dbl>         <dbl>       <dbl>
#>  1 1995-01-01 100       102.         -4.37         1.93        104. 
#>  2 1995-04-01 100       101.         -1.62         0.476       102. 
#>  3 1995-07-01 100       100.          4.44        -4.62         95.6
#>  4 1995-10-01 100        99.4         1.55        -0.946        98.4
#>  5 1996-01-01  97.8     101.         -4.37         1.42        102. 
#>  6 1996-04-01 101.      102.         -1.62         0.613       103. 
#>  7 1996-07-01 107.      103.          4.44         0.370       103. 
#>  8 1996-10-01 103.      104.          1.55        -2.49        101. 
#>  9 1997-01-01 101.      106.         -4.37        -0.530       105. 
#> 10 1997-04-01 108.      109.         -1.62         0.849       110. 
#> # ℹ 114 more rows

# Grouped decomposition: one decomposition per electricity sector
electricity |>
  decompose_series(
    group_cols = "name_series"
  )
#> Auto-detected monthly (12 obs/year)
#> Decomposing 3 group(s) using "stl" method:
#> ℹ Groups: "electric_commercial", "electric_industrial", and
#>   "electric_residential"
#> # A tibble: 1,689 × 6
#>    date       name_series         value trend_stl seasonal_stl remainder_stl
#>    <date>     <chr>               <dbl>     <dbl>        <dbl>         <dbl>
#>  1 1979-02-01 electric_commercial  1030      990.        211.         -171. 
#>  2 1979-03-01 electric_commercial  1057     1005.        252.         -199. 
#>  3 1979-04-01 electric_commercial  1044     1020.        195.         -171. 
#>  4 1979-05-01 electric_commercial  1038     1030.        -59.4          67.2
#>  5 1979-06-01 electric_commercial  1002     1041.       -262.          224. 
#>  6 1979-07-01 electric_commercial   979     1050.       -382.          311. 
#>  7 1979-08-01 electric_commercial   985     1059.       -289.          214. 
#>  8 1979-09-01 electric_commercial  1047     1070.       -151.          128. 
#>  9 1979-10-01 electric_commercial  1067     1081.        -30.6          16.8
#> 10 1979-11-01 electric_commercial  1113     1082.         93.3         -61.9
#> # ℹ 1,679 more rows
```
