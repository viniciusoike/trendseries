# Daily Robusta Coffee Price

Daily Robusta coffee price data from CEPEA/ESALQ (USP) with inflation
adjustment.

Prices are provided in Brazilian reais, with USD values calculated using
the daily USD/BRL exchange rate at 16:30. All reported prices include
taxes and freight costs.

The data tracks prices for standard 60kg sacks of type 6 Arabica coffee
negotiated in São Paulo (SP), representing production from two major
regions: Colatina (ES) and São Gabriel da Palha (ES). The final value is
an arithmetic average of the regional prices.

## Usage

``` r
coffee_robusta
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

Center for Advanced Studies in Applied Economics (CEPEA - ESALQ/USP).

## See also

[coffee_arabica](https://viniciusoike.github.io/trendseries/reference/coffee_arabica.md)
