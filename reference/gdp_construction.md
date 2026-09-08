# Quarterly GDP — Building Industry

Quarterly index of observed gross domestic product (GDP) for Brazil's
building industry. The BCB SGS identifies the series as "Building
industry SCN-2010" and reports its unit as an index.

## Usage

``` r
gdp_construction
```

## Format

A tibble with quarterly observations:

- date:

  Date of the first day of the quarter

- index:

  Observed building-industry GDP index

## Source

Instituto Brasileiro de Geografia e Estatística (IBGE), disseminated by
the Banco Central do Brasil through the Sistema Gerenciador de Séries
Temporais (SGS), series 22087. See the [official SGS
metadata](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=22087)
and [series
page](https://www3.bcb.gov.br/sgspub/consultarvalores/consultarValoresSeries.do?method=consultarGraficoPorId&hdOidSeriesSelecionadas=22087).

## Details

The package snapshot was downloaded through 2025-12-31. BCB may revise
historical observations and extend the series after that date.
