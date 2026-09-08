# Daily Arabica Coffee Price Indicator

Daily observations of the CEPEA/ESALQ Arabica coffee price indicator,
accompanied by an inflation-adjusted dollar series and a derived
moving-average column.

`spot_rs` and `spot_us` are nominal at-sight prices in Brazilian reais
and U.S. dollars per 60-kg bag. CEPEA converts the dollar value using
the commercial selling exchange rate quoted at 16:30. `usd_2022` is
calculated in this package from `spot_us` using U.S. inflation data and
`priceR`.

CEPEA defines the indicator as type 6 Arabica coffee, hard cup or
better, well prepared, with up to 86 defects in a 300-g sample. It
represents wholesale lot transactions delivered to the city of São
Paulo, with freight from the producing region included. The reference
regions are Cerrado and southern Minas Gerais, Mogiana and Garça in São
Paulo, and northwestern Paraná. Regional weights are based on production
volumes reported by IBGE and are updated on the first business day of a
new crop. CEPEA excludes observations outside two standard deviations
from the sample mean and includes taxes in the reported values. For term
transactions, CEPEA discounts prices at CDI from 2021-03-01; before that
it used NPR. The survey consults cooperatives, brokers, roasters, and
exporters.

The indicator is daily and its historical series begins in September
1996. The bundled observations run from 1996-09-02 through 2025-04-17.

## Usage

``` r
coffee_arabica
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
São Paulo (USP), [CEPEA/ESALQ Arabica price
series](https://www.cepea.org.br/br/indicador/series/cafe.aspx?id=23).
See the [CEPEA Arabica
methodology](https://www.cepea.org.br/br/metodologia/metodologia-cafe-arabica-cepea-esalq.aspx).
CEPEA identifies its coffee data as available under the [CC BY-NC 4.0
license](https://creativecommons.org/licenses/by-nc/4.0/). This package
distributes an adapted version by adding `usd_2022` and `trend_ma`.

## See also

[coffee_robusta](https://viniciusoike.github.io/trendseries/reference/coffee_robusta.md)
