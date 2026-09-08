# Daily Robusta Coffee Price Indicator

Daily observations of the CEPEA/ESALQ Robusta coffee price indicator,
accompanied by an inflation-adjusted dollar series and a derived
moving-average column.

`spot_rs` and `spot_us` are nominal at-sight prices in Brazilian reais
and U.S. dollars per 60-kg bag. CEPEA converts the dollar value using
the commercial selling exchange rate quoted at 16:30. `usd_2022` is
calculated in this package from `spot_us` using U.S. inflation data and
`priceR`.

CEPEA defines the indicator as type 6 Robusta coffee, screen 13 or
above, with 86 defects. It represents wholesale lot transactions to be
collected in the producing region, so the indicator does not include
freight. The reference regions are Colatina and São Gabriel da Palha in
Espírito Santo, and the final indicator is their arithmetic average.
CEPEA includes taxes in the reported values and excludes observations
outside two standard deviations from the sample mean. For term
transactions, CEPEA discounts prices at CDI from 2021-03-01; before that
it used NPR. The survey consults cooperatives, brokers, roasters, and
exporters.

The indicator is daily and its historical series begins in November
2001. The bundled observations run from 2001-11-08 through 2025-04-17.

## Usage

``` r
coffee_robusta
```

## Format

A tibble with daily observations:

- date:

  Observation date (`Date`).

- spot_rs:

  CEPEA at-sight price in Brazilian reais per 60-kg bag.

- spot_us:

  CEPEA at-sight price in U.S. dollars per 60-kg bag.

- usd_2022:

  `spot_us` adjusted to 2022 U.S. dollars using `priceR`.

- trend_ma:

  22-observation, right-aligned moving average of `usd_2022`. The first
  21 observations have no full window and are `NA`.

## Source

Original data: Centro de Estudos Avançados em Economia Aplicada (CEPEA),
Escola Superior de Agricultura Luiz de Queiroz (ESALQ), Universidade de
São Paulo (USP), [CEPEA/ESALQ Robusta price
series](https://www.cepea.org.br/br/indicador/series/cafe.aspx?id=24).
See the [CEPEA Robusta
methodology](https://www.cepea.org.br/br/metodologia/metodologia-cafe-robusta-cepea-esalq.aspx).
CEPEA identifies its coffee data as available under the [CC BY-NC 4.0
license](https://creativecommons.org/licenses/by-nc/4.0/). This package
distributes an adapted version by adding `usd_2022` and `trend_ma`.

## See also

[coffee_arabica](https://viniciusoike.github.io/trendseries/reference/coffee_arabica.md)
