# CEPEA Arabica Coffee Prices

Daily Arabica coffee price data from CEPEA/ESALQ with inflation
adjustment. Type 6 coffee prices delivered in São Paulo (capital).

## Usage

``` r
coffee_arabica
```

## Format

A tibble with daily observations:

- date:

  Date column

- spot_rs:

  Spot price in Brazilian Reais per 60-kg bag

- spot_us:

  Spot price in US Dollars per 60-kg bag

- usd_2022:

  US Dollar price adjusted for inflation (base year 2022)

- trend:

  22-day rolling mean of inflation-adjusted prices

## Source

CEPEA - Center for Advanced Studies on Applied Economics
