# Seasonally adjust (deseason) a time series

Pipe-friendly convenience wrapper around
[`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
focused on a single task: removing the seasonal component from a time
series. It adds a `seasadj_{method}` column holding the seasonally
adjusted (deseasoned) series and, optionally, the underlying trend,
seasonal, and remainder components.

## Usage

``` r
deseason_series(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  methods = "stl",
  transform = "none",
  frequency = NULL,
  components = FALSE,
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

  Seasonal-adjustment method(s). One or more of `"stl"` (default) or
  `"seats"`. When both are supplied, each contributes its own
  `seasadj_{method}` column (and component columns when
  `components = TRUE`) so the adjustments can be compared side by side.

  - `"stl"`: Seasonal-Trend decomposition via Loess
    ([`stats::stl()`](https://rdrr.io/r/stats/stl.html)).

  - `"seats"`: X-13ARIMA-SEATS decomposition (requires the
    **`seasonal`** package; see
    [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
    for details).

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

- components:

  If `FALSE` (default), only the seasonally adjusted `seasadj_{method}`
  column is added. If `TRUE`, the `trend_{method}`, `seasonal_{method}`,
  and `remainder_{method}` columns are also added (the full
  [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
  output).

- params:

  Optional list of method-specific parameters for fine control. Sensible
  defaults are provided for all parameters; this argument is only needed
  for non-standard use cases.

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

A tibble with the original columns plus, for each requested method, a
`seasadj_{method}` column holding the seasonally adjusted series. When
`components = TRUE`, the `trend_{method}`, `seasonal_{method}`, and
`remainder_{method}` columns are added as well.

The seasonally adjusted series is the series with the seasonal component
removed: `trend + remainder` for additive decompositions,
`trend * remainder` when `transform = "log"`. Output rows are ordered by
date within each group; the original row order is not preserved.

## Details

`deseason_series()` is a thin wrapper: it calls
[`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
with `seasadj = TRUE` and then keeps only the seasonally adjusted column
unless `components = TRUE`. All seasonal-adjustment behaviour,
validation, grouping, and the `transform = "log"` (multiplicative) path
are inherited unchanged from
[`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md).
See its documentation for method internals and the meaning of the
`params` argument.

For a full trend/seasonal/remainder decomposition, or for the
`regression`, `classic`, and `bsm` methods, use
[`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
directly.

## See also

[`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)
for the underlying decomposition and the full set of methods;
[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
to extract a trend component only.

## Examples

``` r
# Seasonally adjust a quarterly series (STL, the default)
gdp_construction |>
  deseason_series(value_col = "index")
#> Auto-detected quarterly (4 obs/year)
#> Computing STL decomposition with s.window = "periodic"
#> # A tibble: 124 × 3
#>    date       index seasadj_stl
#>    <date>     <dbl>       <dbl>
#>  1 1995-01-01 100         104. 
#>  2 1995-04-01 100         102. 
#>  3 1995-07-01 100          95.6
#>  4 1995-10-01 100          98.4
#>  5 1996-01-01  97.8       102. 
#>  6 1996-04-01 101.        103. 
#>  7 1996-07-01 107.        103. 
#>  8 1996-10-01 103.        101. 
#>  9 1997-01-01 101.        105. 
#> 10 1997-04-01 108.        110. 
#> # ℹ 114 more rows

# Also keep the trend, seasonal, and remainder components
gdp_construction |>
  deseason_series(value_col = "index", components = TRUE)
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

# Multiplicative adjustment via log transform (seasonal swings grow with level)
gdp_construction |>
  deseason_series(value_col = "index", transform = "log")
#> Auto-detected quarterly (4 obs/year)
#> Computing STL decomposition with s.window = "periodic"
#> # A tibble: 124 × 3
#>    date       index seasadj_stl
#>    <date>     <dbl>       <dbl>
#>  1 1995-01-01 100         104. 
#>  2 1995-04-01 100         101. 
#>  3 1995-07-01 100          96.6
#>  4 1995-10-01 100          98.7
#>  5 1996-01-01  97.8       101. 
#>  6 1996-04-01 101.        102. 
#>  7 1996-07-01 107.        104. 
#>  8 1996-10-01 103.        101. 
#>  9 1997-01-01 101.        104. 
#> 10 1997-04-01 108.        109. 
#> # ℹ 114 more rows

# X-13ARIMA-SEATS adjustment (requires the 'seasonal' package)
if (requireNamespace("seasonal", quietly = TRUE)) {
  gdp_construction |>
    deseason_series(value_col = "index", methods = "seats")
}
#> Auto-detected quarterly (4 obs/year)
#> Computing X-13ARIMA-SEATS decomposition (SEATS)
#> # A tibble: 124 × 3
#>    date       index seasadj_seats
#>    <date>     <dbl>         <dbl>
#>  1 1995-01-01 100           104. 
#>  2 1995-04-01 100           101. 
#>  3 1995-07-01 100            96.6
#>  4 1995-10-01 100            98.8
#>  5 1996-01-01  97.8         102. 
#>  6 1996-04-01 101.          102. 
#>  7 1996-07-01 107.          104. 
#>  8 1996-10-01 103.          101. 
#>  9 1997-01-01 101.          105. 
#> 10 1997-04-01 108.          109. 
#> # ℹ 114 more rows

# Compare STL and SEATS adjustments side by side
if (requireNamespace("seasonal", quietly = TRUE)) {
  gdp_construction |>
    deseason_series(value_col = "index", methods = c("stl", "seats"))
}
#> Auto-detected quarterly (4 obs/year)
#> Computing STL decomposition with s.window = "periodic"
#> Computing X-13ARIMA-SEATS decomposition (SEATS)
#> # A tibble: 124 × 4
#>    date       index seasadj_stl seasadj_seats
#>    <date>     <dbl>       <dbl>         <dbl>
#>  1 1995-01-01 100         104.          104. 
#>  2 1995-04-01 100         102.          101. 
#>  3 1995-07-01 100          95.6          96.6
#>  4 1995-10-01 100          98.4          98.8
#>  5 1996-01-01  97.8       102.          102. 
#>  6 1996-04-01 101.        103.          102. 
#>  7 1996-07-01 107.        103.          104. 
#>  8 1996-10-01 103.        101.          101. 
#>  9 1997-01-01 101.        105.          105. 
#> 10 1997-04-01 108.        110.          109. 
#> # ℹ 114 more rows

# Grouped seasonal adjustment: one adjustment per electricity sector
electricity |>
  deseason_series(group_cols = "name_series")
#> Auto-detected monthly (12 obs/year)
#> Decomposing 3 group(s) using "stl" method:
#> ℹ Groups: "electric_commercial", "electric_industrial", and
#>   "electric_residential"
#> # A tibble: 1,689 × 4
#>    date       name_series         value seasadj_stl
#>    <date>     <chr>               <dbl>       <dbl>
#>  1 1979-02-01 electric_commercial  1030        819.
#>  2 1979-03-01 electric_commercial  1057        805.
#>  3 1979-04-01 electric_commercial  1044        849.
#>  4 1979-05-01 electric_commercial  1038       1097.
#>  5 1979-06-01 electric_commercial  1002       1264.
#>  6 1979-07-01 electric_commercial   979       1361.
#>  7 1979-08-01 electric_commercial   985       1274.
#>  8 1979-09-01 electric_commercial  1047       1198.
#>  9 1979-10-01 electric_commercial  1067       1098.
#> 10 1979-11-01 electric_commercial  1113       1020.
#> # ℹ 1,679 more rows
```
