# Electricity Consumption by Sector

Monthly electricity consumption in Brazil by sector (GWh), in long
format. This dataset combines the residential, commercial, and
industrial SGS series by row-binding them; it does not calculate a total
or otherwise transform their values.

## Usage

``` r
electricity
```

## Format

A tibble with monthly observations:

- date:

  Date of the first day of the month

- name_series:

  Sector identifier: `"electric_residential"`, `"electric_commercial"`,
  or `"electric_industrial"`

- value:

  Electricity consumption in GWh

## Source

Centrais Elétricas Brasileiras S.A. (Eletrobras), disseminated by the
Banco Central do Brasil through the Sistema Gerenciador de Séries
Temporais (SGS). The component series are 1402 (commercial), 1403
(residential), and 1404 (industrial). See the official [SGS metadata for
series
1402](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=1402),
[1403](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=1403),
and
[1404](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=1404).

## Details

The package snapshot was downloaded through 2025-12-31. BCB may revise
historical observations and extend the series after that date. The
residential series is also available in wide format as
[electric](https://viniciusoike.github.io/trendseries/reference/electric.md).

## See also

[electric](https://viniciusoike.github.io/trendseries/reference/electric.md)
for the residential-only wide-format series.
