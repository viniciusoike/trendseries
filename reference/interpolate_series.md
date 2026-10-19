# Complete and interpolate a time series

Completes the calendar grid of one or more series and interpolates
missing values inside the observed span. Missing periods that are absent
from the input are made explicit before interpolation, so later
observations remain attached to the correct dates.

Leading and trailing missing values are not extrapolated. By default, a
Kalman smoother is attempted first and linear interpolation is used if
the state-space model cannot be estimated.

## Usage

``` r
interpolate_series(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  frequency = NULL,
  method = "auto",
  max_gap = NULL,
  imputed_col = FALSE,
  .quiet = FALSE
)
```

## Arguments

- data:

  A `data.frame`, `tibble`, or `data.table`.

- date_col:

  Name of the date column. It must have class `Date`.

- value_col:

  Name of one or more numeric value columns.

- group_cols:

  Optional character vector identifying separate series.

- frequency:

  Observations per year. Supported values are 1, 2, 4, 12, 52, and 365.
  When `NULL`, the frequency is inferred from the dates.

- method:

  Interpolation method. One of `"auto"`, `"kalman"`, `"linear"`, or
  `"spline"`. `"auto"` tries a structural time-series Kalman smoother
  and falls back to linear interpolation if the model cannot be
  estimated; `"kalman"` errors instead. Both keep the Kalman estimates,
  with a warning, when the model fit does not converge.

- max_gap:

  Maximum consecutive missing periods to interpolate. `NULL` uses
  `floor(0.75 * n)`, where `n` is the span from the first through the
  last observed value. Use `Inf` to remove the limit.

- imputed_col:

  If `TRUE`, add one logical column per value column named
  `imputed_{value_col}`. A flag is true only where a value was filled.

- .quiet:

  If `TRUE`, suppress fallback, non-convergence, date-normalization, and
  high missingness warnings.

## Value

A tibble on a complete date grid. Value columns are interpolated in
place; leading and trailing missing values remain `NA`. Rows are ordered
by group and date.

## Details

The Kalman method estimates a basic structural model with
[`stats::StructTS()`](https://rdrr.io/r/stats/StructTS.html) when there
are at least two observed seasonal cycles. Annual, weekly, and daily
series, and seasonal series with fewer observations, use a local linear
trend model. Only missing observations are replaced; observed values are
never smoothed.

Linear and cubic spline interpolation use
[`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) and
[`stats::spline()`](https://rdrr.io/r/stats/splinefun.html),
respectively. Cubic splines can overshoot around sharp changes, so
linear interpolation is the fallback used by `method = "auto"`.

Calendar conventions are detected separately within each group.
Consistent period starts, period ends, weekdays, and within-period
calendar positions are preserved. Inconsistent conventions are
normalized to period starts.

## Examples

``` r
monthly <- data.frame(
  date = as.Date(c("2020-01-01", "2020-02-01", "2020-04-01")),
  value = c(1, 2, 4)
)

interpolate_series(monthly, frequency = 12, method = "linear")
#> # A tibble: 4 × 2
#>   date       value
#>   <date>     <dbl>
#> 1 2020-01-01     1
#> 2 2020-02-01     2
#> 3 2020-03-01     3
#> 4 2020-04-01     4
interpolate_series(
  monthly,
  frequency = 12,
  method = "linear",
  imputed_col = TRUE
)
#> # A tibble: 4 × 3
#>   date       value imputed_value
#>   <date>     <dbl> <lgl>        
#> 1 2020-01-01     1 FALSE        
#> 2 2020-02-01     2 FALSE        
#> 3 2020-03-01     3 TRUE         
#> 4 2020-04-01     4 FALSE        
```
