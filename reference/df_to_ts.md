# Convert a data.frame into a time series (ts)

Converts a series, stored in a data.frame or tibble, into a ts object.

## Usage

``` r
df_to_ts(x, date_colname = "date", value_colname = "value", frequency = 12)
```

## Arguments

- x:

  A `data.frame`, `tibble` or `data.table`.

- date_colname:

  Name of the date column. Defaults to `'date'`. Must be of class
  `Date`.

- value_colname:

  Name of the value column. Defaults to `'value'`. Must be `numeric`.

- frequency:

  The frequency of the series. Can be a shortened string (e.g. "M" for
  monthly) or a number (e.g. 12).

## Value

A `ts` object

## Examples

``` r
ibc <- df_to_ts(ibcbr, value_colname = "index", frequency = "M")
class(ibc)
#> [1] "ts"
plot(ibc)
```
