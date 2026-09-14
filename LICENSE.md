# MIT License

Copyright (c) 2024 trendseries authors

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
“Software”), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

------------------------------------------------------------------------

# Data licenses and attributions

The MIT license above applies to the package **code**. The package also
distributes economic datasets obtained from third parties. Those data
remain the property of their respective sources and are redistributed
under the terms described below. Where a dataset was processed,
aggregated, or otherwise adapted, this package distributes a **derived**
version of the source data.

## Brazilian macroeconomic series

**Datasets:** `ibcbr`, `gdp_construction`, `vehicles`,
`oil_derivatives`, `electric`, `electricity`

Sources: Banco Central do Brasil (IBC-Br, series 24363); Instituto
Brasileiro de Geografia e Estatística (building-industry GDP, series
22087); Associação Nacional dos Fabricantes de Veículos Automotores
(vehicle sales, series 1378); Agência Nacional do Petróleo, Gás Natural
e Biocombustíveis (petroleum derivatives production, series 1391); and
Centrais Elétricas Brasileiras S.A. (electricity consumption, series
1402, 1403, and 1404).

All series are disseminated by the Banco Central do Brasil through the
[Sistema Gerenciador de Séries Temporais
(SGS)](https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries).
These series are public statistical data disseminated freely by the
Banco Central do Brasil; the package’s snapshots were downloaded through
2025-12-31 and the source institutions may revise historical
observations.

## Coffee price indicators

**Datasets:** `coffee_arabica`, `coffee_robusta`

Original data: Centro de Estudos Avançados em Economia Aplicada (CEPEA),
Escola Superior de Agricultura Luiz de Queiroz (ESALQ), Universidade de
São Paulo (USP) — [Arabica
indicator](https://www.cepea.org.br/br/indicador/series/cafe.aspx?id=23)
and [Robusta
indicator](https://www.cepea.org.br/br/indicador/series/cafe.aspx?id=24).

CEPEA identifies its coffee data as available under the [Creative
Commons Attribution-NonCommercial 4.0 International
license](https://creativecommons.org/licenses/by-nc/4.0/). This license
applies to the CEPEA-derived data distributed in this package; unlike
the package code, these datasets **may not be used for commercial
purposes**. The bundled data are an adapted version: the `usd_2022`
column is calculated from the source dollar price using U.S. inflation
data, and the `trend_ma` column is a 22-observation moving average.

Suggested attribution:

> Centro de Estudos Avançados em Economia Aplicada (CEPEA), Escola
> Superior de Agricultura Luiz de Queiroz (ESALQ), Universidade de São
> Paulo (USP), [CEPEA/ESALQ coffee price
> indicators](https://www.cepea.org.br/br/indicador/cafe.aspx), [CC
> BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/); adapted
> in `trendseries`. This attribution does not imply CEPEA endorses the
> package.

## UK Retail Sales Index

**Datasets:** `retail_volume`, `retail_autofuel`

Source: Office for National Statistics (ONS), [Retail Sales Index
reference
tables](https://www.ons.gov.uk/businessindustryandtrade/retailindustry/datasets/retailsalesindexreferencetables),
Table 3M, “Chained volume of retail sales, non-seasonally adjusted”. See
the [Retail Sales Index (RSI) Quality and Methodology Information
report](https://www.ons.gov.uk/businessindustryandtrade/retailindustry/methodologies/retailsalesindexrsiqmi).

Contains public sector information licensed under the [Open Government
Licence
v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/),
except where otherwise stated. This package distributes a processed
subset of the ONS reference tables.

## Transport for London Network Demand

**Datasets:** `transit_london_monthly`, `transit_london_avgs`

Source: Transport for London (TfL), [Network demand
data](https://tfl.gov.uk/corporate/publications-and-reports/network-demand-data)
and the [Network Demand
Dashboard](https://app.powerbi.com/view?r=eyJrIjoiZDgwZWY4NWMtZTFkMi00YzM2LThiMWQtNzg2ZTc2YjliNzM2IiwidCI6IjFmYmQ2NWJmLTVkZWYtNGVlYS1hNjkyLWEwODljMjU1MzQ2YiIsImMiOjh9).
The package distributes an aggregated and reshaped version of TfL’s
daily Journeys files, per TfL’s [Transport Data Service
terms](https://tfl.gov.uk/corporate/terms-and-conditions/transport-data-service).

Required attribution:

> Powered by TfL Open Data

This package is not affiliated with or endorsed by TfL.
