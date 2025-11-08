# Convert time series to tibble

Convert time series to tibble

## Usage

``` r
ts_to_df(x, date_colname = NULL, value_colname = NULL)
```

## Arguments

- x:

  A time series as a `ts` object

- date_colname:

  Optional name for the date column

- value_colname:

  Optional name for the value column

## Value

a `tibble`

## Examples

``` r
# example code
ts_to_df(AirPassengers)
#> # A tibble: 144 × 2
#>    date       value
#>    <date>     <dbl>
#>  1 1949-01-01   112
#>  2 1949-02-01   118
#>  3 1949-03-01   132
#>  4 1949-04-01   129
#>  5 1949-05-01   121
#>  6 1949-06-01   135
#>  7 1949-07-01   148
#>  8 1949-08-01   148
#>  9 1949-09-01   136
#> 10 1949-10-01   119
#> # ℹ 134 more rows

# Using a custom name for the value column
ts_to_df(AirPassengers, value_colname = "passengers")
#> # A tibble: 144 × 2
#>    date       passengers
#>    <date>          <dbl>
#>  1 1949-01-01        112
#>  2 1949-02-01        118
#>  3 1949-03-01        132
#>  4 1949-04-01        129
#>  5 1949-05-01        121
#>  6 1949-06-01        135
#>  7 1949-07-01        148
#>  8 1949-08-01        148
#>  9 1949-09-01        136
#> 10 1949-10-01        119
#> # ℹ 134 more rows
```
