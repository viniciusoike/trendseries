# UK Retail Index

Chained volume of retail sales, non-seasonally adjusted, selected
sub-series. Index numbers of sales per week (100 = 2023). The monthly
period consists of 4 weeks except for March, June, September and
December which are 5 weeks. January 2025 is also a 5 week period. The
included series consider the "All Businesses" specification and cover
Great Britain from 1998 to 2025.

## Usage

``` r
retail_volume
```

## Format

A tibble with monthly observations:

- date:

  Date column

- name_series:

  Series name derived from the unofficial SIC

- value:

  Retail sales index (chained volume)

## Source

UK Office for National Statistics (ONS). (Table 3M).

## See also

[retail_autofuel](https://viniciusoike.github.io/trendseries/reference/retail_autofuel.md)
