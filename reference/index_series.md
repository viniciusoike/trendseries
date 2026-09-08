# Index one or more series

Rescale numeric series relative to their earliest observed value or to
the arithmetic mean over a selected base period.

## Usage

``` r
index_series(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  base_period = NULL,
  base_value = 100,
  na_rm = FALSE,
  suffix = NULL,
  .quiet = FALSE
)
```

## Arguments

- data:

  A non-empty `data.frame`, `tibble`, or `data.table`.

- date_col:

  Name of the `Date` column. Defaults to `"date"`.

- value_col:

  Non-empty character vector naming numeric value columns.

- group_cols:

  Optional character vector naming grouping columns. Each group receives
  its own reference value.

- base_period:

  `NULL` to use the earliest non-missing observation; one or two
  four-digit integer years; or one or two `Date` values. Two values
  define an inclusive range and may be supplied in either order. A
  single date selects the calendar period containing it at the detected
  frequency.

- base_value:

  Finite positive number assigned to the reference. Defaults to 100.

- na_rm:

  Whether to remove missing values when averaging an explicit base
  period. With the default `FALSE`, a missing base observation raises an
  error instead of blanking the entire indexed series. This argument has
  no effect when `base_period = NULL`.

- suffix:

  Optional non-missing character suffix for generated names.

- .quiet:

  If `TRUE`, suppress frequency-detection messages. Warnings about
  incomplete base periods are never suppressed.

## Value

A tibble containing the original columns, in their original order,
followed by `index_{value_col}` columns (and `_{suffix}` when supplied).

## Details

When `base_period` is supplied, dates are matched at the detected
calendar frequency of each group. Thus, for monthly data,
`as.Date("2019-01-01")` also matches an observation dated at month end.
Weekly and daily series use exact interval containment. A partly
observed base interval produces a warning.

## See also

[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
for trend estimation and
[`augment_rolling()`](https://viniciusoike.github.io/trendseries/reference/augment_rolling.md)
for rolling and year-to-date aggregations.

## Examples

``` r
vehicles |>
  index_series(value_col = "production")
#> # A tibble: 539 × 3
#>    date       production index_production
#>    <date>          <dbl>            <dbl>
#>  1 1981-02-01      65251            100  
#>  2 1981-03-01      64065             98.2
#>  3 1981-04-01      69042            106. 
#>  4 1981-05-01      62966             96.5
#>  5 1981-06-01      61271             93.9
#>  6 1981-07-01      60824             93.2
#>  7 1981-08-01      63871             97.9
#>  8 1981-09-01      64828             99.4
#>  9 1981-10-01      63211             96.9
#> 10 1981-11-01      61129             93.7
#> # ℹ 529 more rows

retail_volume |>
  index_series(group_cols = "name_series", base_period = 2019)
#> Auto-detected monthly (12 obs/year)
#> Auto-detected monthly (12 obs/year)
#> Auto-detected monthly (12 obs/year)
#> Auto-detected monthly (12 obs/year)
#> Auto-detected monthly (12 obs/year)
#> Auto-detected monthly (12 obs/year)
#> Auto-detected monthly (12 obs/year)
#> Auto-detected monthly (12 obs/year)
#> Auto-detected monthly (12 obs/year)
#> # A tibble: 4,113 × 4
#>    date       name_series                                      value index_value
#>    <date>     <chr>                                            <dbl>       <dbl>
#>  1 1988-01-01 household-goods-stores                            69          62.1
#>  2 1988-01-01 computers-and-telecomms-equipment                 25.9        24.9
#>  3 1988-01-01 electrical-household-appliances                   43.6        37.8
#>  4 1988-01-01 pharmaceutical-medical-cosmetic-and-toilet-goods  43.4        46.7
#>  5 1988-01-01 books-newspapers-and-periodicals                 270.        216. 
#>  6 1988-01-01 alcoholic-drinks-other-beverages-and-tobacco     400.        346. 
#>  7 1988-01-01 all-retailing-including-automotive-fuel           NA          NA  
#>  8 1988-01-01 all-retailing-excluding-automotive-fuel           49          47.8
#>  9 1988-01-01 clothing                                          31.7        30.3
#> 10 1988-02-01 all-retailing-including-automotive-fuel           NA          NA  
#> # ℹ 4,103 more rows
```
