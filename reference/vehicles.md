# Total Vehicle Sales

Monthly total vehicle sales in Brazil, measured in units. The BCB SGS
title for series 1378 is "Vehicle sales (total)".

## Usage

``` r
vehicles
```

## Format

A tibble with monthly observations:

- date:

  Date of the first day of the month

- production:

  Total vehicle sales in units. The column name is retained for
  compatibility with existing package examples.

## Source

Associação Nacional dos Fabricantes de Veículos Automotores (Anfavea),
disseminated by the Banco Central do Brasil through the Sistema
Gerenciador de Séries Temporais (SGS), series 1378. See the [official
SGS
metadata](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=1378)
and [series
page](https://www3.bcb.gov.br/sgspub/consultarvalores/consultarValoresSeries.do?method=consultarGraficoPorId&hdOidSeriesSelecionadas=1378).

## Details

The package snapshot was downloaded through 2025-12-31. BCB may revise
historical observations and extend the series after that date.
