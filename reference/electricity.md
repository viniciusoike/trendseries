# Electricity Consumption by Sector

Monthly electricity consumption in Brazil by sector (GWh), in long
format. Combines residential, commercial, and industrial sub-series from
the Brazilian National Electric System Operator (ONS) as reported by the
Brazilian Central Bank SGS.

## Usage

``` r
electricity
```

## Format

A tibble with monthly observations:

- date:

  Date column

- name_series:

  Sector identifier: `"electric_residential"`, `"electric_commercial"`,
  or `"electric_industrial"`

- value:

  Electricity consumption in GWh

## Source

ONS via Brazilian Central Bank SGS (codes 1403 — residential, 1402 —
commercial, 1404 — industrial).

## See also

[electric](https://viniciusoike.github.io/trendseries/reference/electric.md)
for the residential-only wide-format series.
