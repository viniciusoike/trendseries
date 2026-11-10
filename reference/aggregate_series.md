# Aggregate series to a lower frequency

Summarise daily, weekly, or monthly series into calendar months,
quarters, or years, such as the last daily close of each month or the
quarterly sum of a monthly flow.

## Usage

``` r
aggregate_series(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  period = c("month", "quarter", "year"),
  summary = c("last", "first", "mean", "sum", "max", "min"),
  na_rm = FALSE
)
```

## Arguments

- data:

  A non-empty `data.frame`, `tibble`, or `data.table`.

- date_col:

  Name of the `Date` column. Defaults to `"date"`.

- value_col:

  Non-empty character vector naming numeric value columns. Each column
  is aggregated with the same `summary`.

- group_cols:

  Optional character vector naming grouping columns. Each group is
  aggregated separately. Grouping columns must differ from `date_col`
  and `value_col`.

- period:

  Target calendar period: `"month"`, `"quarter"`, or `"year"`.

- summary:

  Statistic computed within each period: `"last"`, `"first"`, `"mean"`,
  `"sum"`, `"max"`, or `"min"`. Use `"last"` or `"mean"` for stocks and
  prices, and `"sum"` for flows.

- na_rm:

  Whether to drop missing values before summarising. With the default
  `FALSE`, `"mean"`, `"sum"`, `"max"`, and `"min"` return `NA` for a
  period containing a missing value, while `"first"` and `"last"` return
  the value on the first or last dated row, which may be missing. With
  `TRUE`, they use the first or last observed value. A period with no
  observed values returns `NA` in either case.

## Value

A tibble with one row per group and observed period, holding the
grouping columns, the date column, and the aggregated value columns as
doubles. Other columns are dropped. Rows are sorted by group and date.

## Details

Each period is dated by its first day, so January 2024 is dated
`2024-01-01` whatever the dates of the underlying observations. Only
periods with at least one observation are returned.

Periods at the start and end of a series may be partly observed, and
their sums and means then cover fewer observations than complete
periods. With `summary = "last"`, the value is the last observation in
the period, which for trading data may fall before the last calendar
day. A weekly observation is assigned whole to the period containing its
date, so a week that straddles a month end is not split.

Grouping comes only from `group_cols`. A grouped tibble or a keyed
tsibble needs its grouping variables passed there. A warning flags calls
in which no period holds more than one observation, which happens when
`period` is not coarser than the data.

## See also

[`interpolate_series()`](https://viniciusoike.github.io/trendseries/reference/interpolate_series.md)
to fill missing values before aggregating, and
[`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
for rolling and year-to-date aggregations at the original frequency.

## Examples

``` r
# Quarterly sum of monthly vehicle production
vehicles |>
  aggregate_series(
    value_col = "production",
    period = "quarter",
    summary = "sum"
  )
#> # A tibble: 180 × 2
#>    date       production
#>    <date>          <dbl>
#>  1 1981-01-01     129316
#>  2 1981-04-01     193279
#>  3 1981-07-01     189523
#>  4 1981-10-01     193279
#>  5 1982-01-01     202631
#>  6 1982-04-01     201488
#>  7 1982-07-01     218435
#>  8 1982-10-01     243882
#>  9 1983-01-01     219661
#> 10 1983-04-01     231464
#> # ℹ 170 more rows

# Annual average of each retail series
retail_volume |>
  aggregate_series(
    group_cols = "name_series",
    period = "year",
    summary = "mean"
  )
#> # A tibble: 351 × 3
#>    name_series                                  date       value
#>    <chr>                                        <date>     <dbl>
#>  1 alcoholic-drinks-other-beverages-and-tobacco 1988-01-01  455.
#>  2 alcoholic-drinks-other-beverages-and-tobacco 1989-01-01  468.
#>  3 alcoholic-drinks-other-beverages-and-tobacco 1990-01-01  475.
#>  4 alcoholic-drinks-other-beverages-and-tobacco 1991-01-01  435.
#>  5 alcoholic-drinks-other-beverages-and-tobacco 1992-01-01  416.
#>  6 alcoholic-drinks-other-beverages-and-tobacco 1993-01-01  400.
#>  7 alcoholic-drinks-other-beverages-and-tobacco 1994-01-01  389.
#>  8 alcoholic-drinks-other-beverages-and-tobacco 1995-01-01  380.
#>  9 alcoholic-drinks-other-beverages-and-tobacco 1996-01-01  367.
#> 10 alcoholic-drinks-other-beverages-and-tobacco 1997-01-01  365 
#> # ℹ 341 more rows
```
