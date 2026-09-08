# Central Bank Economic Activity Index (IBC-Br)

Monthly Central Bank Economic Activity Index (IBC-Br), a timely
indicator of national economic activity. It is built from proxies for
agriculture, industry, and services, aggregated using weights derived
from the supply and use tables of the Brazilian National Accounts.

## Usage

``` r
ibcbr
```

## Format

A tibble with monthly observations:

- date:

  Date of the first day of the month

- index:

  IBC-Br index value

## Source

Banco Central do Brasil — Departamento Econômico, disseminated through
the Sistema Gerenciador de Séries Temporais (SGS), series 24363. See the
[official SGS
metadata](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=24363)
and [series
page](https://www3.bcb.gov.br/sgspub/consultarvalores/consultarValoresSeries.do?method=consultarGraficoPorId&hdOidSeriesSelecionadas=24363).

## Details

The package snapshot was downloaded through 2025-12-31. BCB may revise
historical observations and extend the series after that date.
