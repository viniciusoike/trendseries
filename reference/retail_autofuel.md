# UK Retail Sales - Automotive Fuel

Chained volume of retail sales for automotive fuel in the UK,
non-seasonally adjusted. Index numbers of sales per week (100 = 2023).
The monthly period consists of 4 weeks except for March, June, September
and December which are 5 weeks. January 2025 is also a 5 week period.
The series considers the "All Businesses" specification and covers Great
Britain from 1998 to 2025.

## Usage

``` r
retail_autofuel
```

## Format

A tibble with monthly observations:

- date:

  Date column

- value:

  Retail sales index (chained volume)

- name:

  Series name

- frequency:

  Frequency ("M")

- source:

  Data source ("ONS")

## Source

UK Office for National Statistics (ONS). (Table 3M).

## See also

[retail_volume](https://viniciusoike.github.io/trendseries/reference/retail_volume.md)
