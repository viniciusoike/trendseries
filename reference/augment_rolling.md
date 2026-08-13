# Add rolling aggregation columns to a data frame

Pipe-friendly companion to
[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
for rolling and year-to-date aggregations: 12-month accumulated totals,
compounded rates of change, rolling volatility, and so on. Columns are
prefixed `roll_` rather than `trend_`, because these are aggregations of
the series and not estimates of its trend.

## Usage

``` r
augment_rolling(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  stats = "sum",
  window = NULL,
  frequency = NULL,
  align = "right",
  percent = FALSE,
  na_rm = FALSE,
  suffix = NULL,
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

  Name of the value column(s). Defaults to `"value"`. Must be `numeric`.
  A character vector of length \> 1 is accepted; aggregations are
  computed for each column and named `roll_{stat}_{window}_{col}`.

- group_cols:

  Optional grouping variables for multiple time series. Can be a
  character vector of column names.

- stats:

  Character vector of rolling statistics. Options: `"sum"` (rolling
  total of flows), `"chain"` (compound accumulation of rates,
  `prod(1 + r) - 1`), `"mean"`, `"sd"`, `"min"`, `"max"`. Default is
  `"sum"`.

- window:

  Window length in periods. If `NULL`, defaults to the detected
  frequency (12 for monthly, 4 for quarterly). A numeric vector adds one
  column per window value. Alternatively, the string `"ytd"` computes an
  expanding year-to-date accumulation that resets each January (or Q1).
  Numeric windows and `"ytd"` cannot be mixed in one call.

- frequency:

  The frequency of the series. Supports 4 (quarterly) or 12 (monthly).
  Auto-detected if not specified.

- align:

  Alignment of the window relative to the output position: `"right"`
  (default, causal — uses the current and preceding observations),
  `"center"`, or `"left"`. Right alignment is the convention for
  accumulated economic indicators. Ignored when `window = "ytd"`.

- percent:

  Only used by `stats = "chain"`. If `FALSE` (default), rates are
  assumed to be decimals (0.005 for 0.5%). If `TRUE`, rates are assumed
  to be percentages (0.5 for 0.5%) and the result is returned in
  percent.

- na_rm:

  If `TRUE`, missing values are ignored within each window. The default
  `FALSE` propagates `NA`, so an incomplete window yields `NA`.

- suffix:

  Optional suffix appended to the generated column names.

- .quiet:

  If `TRUE`, suppress informational messages.

## Value

A tibble with the original data plus rolling columns named
`roll_{stat}_{window}` (e.g. `roll_sum_12`, `roll_chain_ytd`), with
`_{suffix}` appended when `suffix` is supplied.

## Details

Use `"sum"` for flows measured in levels and `"chain"` for series that
are already rates of change. Summing monthly inflation rates
approximates the 12-month accumulation but is not equal to it; `"chain"`
compounds them correctly. See
[`roll_series()`](https://viniciusoike.github.io/trendseries/reference/roll_series.md)
for the underlying computation.

`"mean"` overlaps with the simple moving average available through
`augment_trends(methods = "ma")`. The two differ in defaults rather than
in substance: rolling aggregations default to right alignment, while the
moving average trend defaults to centred alignment (and applies the 2xN
correction for even centred windows).

Rows whose value is `NA` are kept in place, so window positions stay
aligned with the calendar; `na_rm` then decides whether such a window
yields `NA` or is computed from the observations that are present. A
period that is absent from the data altogether cannot be positioned, so
it raises an error rather than shifting later observations — add the
missing rows with an `NA` value first.

## See also

[`roll_series()`](https://viniciusoike.github.io/trendseries/reference/roll_series.md)
for the time series interface,
[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
for trend estimation.

## Examples

``` r
# 12-month accumulated vehicle production
vehicles |> augment_rolling(value_col = "production", window = 12)
#> Auto-detected monthly (12 obs/year)
#> Computing 12-period rolling sum with right alignment
#> # A tibble: 539 × 3
#>    date       production roll_sum_12
#>    <date>          <dbl>       <dbl>
#>  1 1981-02-01      65251          NA
#>  2 1981-03-01      64065          NA
#>  3 1981-04-01      69042          NA
#>  4 1981-05-01      62966          NA
#>  5 1981-06-01      61271          NA
#>  6 1981-07-01      60824          NA
#>  7 1981-08-01      63871          NA
#>  8 1981-09-01      64828          NA
#>  9 1981-10-01      63211          NA
#> 10 1981-11-01      61129          NA
#> # ℹ 529 more rows

# Several windows at once
vehicles |>
  tail(60) |>
  augment_rolling(value_col = "production", window = c(3, 6, 12))
#> Auto-detected monthly (12 obs/year)
#> Computing 3-period rolling sum with right alignment
#> Computing 6-period rolling sum with right alignment
#> Computing 12-period rolling sum with right alignment
#> # A tibble: 60 × 5
#>    date       production roll_sum_3 roll_sum_6 roll_sum_12
#>    <date>          <dbl>      <dbl>      <dbl>       <dbl>
#>  1 2021-01-01     180904         NA         NA          NA
#>  2 2021-02-01     186718         NA         NA          NA
#>  3 2021-03-01     208801     576423         NA          NA
#>  4 2021-04-01     191853     587372         NA          NA
#>  5 2021-05-01     206221     606875         NA          NA
#>  6 2021-06-01     191571     589645    1166068          NA
#>  7 2021-07-01     174739     572531    1159903          NA
#>  8 2021-08-01     178900     545210    1152085          NA
#>  9 2021-09-01     156803     510442    1100087          NA
#> 10 2021-10-01     170178     505881    1078412          NA
#> # ℹ 50 more rows

# Rolling mean and volatility side by side
ibcbr |>
  augment_rolling(value_col = "index", stats = c("mean", "sd"), window = 12)
#> Auto-detected monthly (12 obs/year)
#> Computing 12-period rolling mean with right alignment
#> Computing 12-period rolling sd with right alignment
#> # A tibble: 276 × 4
#>    date       index roll_mean_12 roll_sd_12
#>    <date>     <dbl>        <dbl>      <dbl>
#>  1 2003-01-01  67.1           NA         NA
#>  2 2003-02-01  68.8           NA         NA
#>  3 2003-03-01  72.2           NA         NA
#>  4 2003-04-01  71.3           NA         NA
#>  5 2003-05-01  70.0           NA         NA
#>  6 2003-06-01  68.8           NA         NA
#>  7 2003-07-01  71.9           NA         NA
#>  8 2003-08-01  70.8           NA         NA
#>  9 2003-09-01  71.8           NA         NA
#> 10 2003-10-01  73.3           NA         NA
#> # ℹ 266 more rows

# Year-to-date accumulation, resetting each January
vehicles |> augment_rolling(value_col = "production", window = "ytd")
#> Auto-detected monthly (12 obs/year)
#> Computing year-to-date sum
#> # A tibble: 539 × 3
#>    date       production roll_sum_ytd
#>    <date>          <dbl>        <dbl>
#>  1 1981-02-01      65251        65251
#>  2 1981-03-01      64065       129316
#>  3 1981-04-01      69042       198358
#>  4 1981-05-01      62966       261324
#>  5 1981-06-01      61271       322595
#>  6 1981-07-01      60824       383419
#>  7 1981-08-01      63871       447290
#>  8 1981-09-01      64828       512118
#>  9 1981-10-01      63211       575329
#> 10 1981-11-01      61129       636458
#> # ℹ 529 more rows

# Grouped series
retail_volume |>
  augment_rolling(group_cols = "name_series", window = 12)
#> Auto-detected monthly (12 obs/year)
#> Computing 1 statistic for 9 groups:
#> ℹ Statistics: "sum"
#> ℹ Groups: "alcoholic-drinks-other-beverages-and-tobacco",
#>   "all-retailing-excluding-automotive-fuel",
#>   "all-retailing-including-automotive-fuel",
#>   "books-newspapers-and-periodicals", "clothing",
#>   "computers-and-telecomms-equipment", "electrical-household-appliances",
#>   "household-goods-stores", and
#>   "pharmaceutical-medical-cosmetic-and-toilet-goods"
#> # A tibble: 4,113 × 4
#>    date       name_series                                  value roll_sum_12
#>    <date>     <chr>                                        <dbl>       <dbl>
#>  1 1988-01-01 alcoholic-drinks-other-beverages-and-tobacco  400.          NA
#>  2 1988-02-01 alcoholic-drinks-other-beverages-and-tobacco  416.          NA
#>  3 1988-03-01 alcoholic-drinks-other-beverages-and-tobacco  434.          NA
#>  4 1988-04-01 alcoholic-drinks-other-beverages-and-tobacco  442.          NA
#>  5 1988-05-01 alcoholic-drinks-other-beverages-and-tobacco  446.          NA
#>  6 1988-06-01 alcoholic-drinks-other-beverages-and-tobacco  446.          NA
#>  7 1988-07-01 alcoholic-drinks-other-beverages-and-tobacco  454.          NA
#>  8 1988-08-01 alcoholic-drinks-other-beverages-and-tobacco  468.          NA
#>  9 1988-09-01 alcoholic-drinks-other-beverages-and-tobacco  436.          NA
#> 10 1988-10-01 alcoholic-drinks-other-beverages-and-tobacco  445.          NA
#> # ℹ 4,103 more rows
```
