#' Quarterly GDP — Building Industry
#'
#' Quarterly index of observed gross domestic product (GDP) for Brazil's
#' building industry. The BCB SGS identifies the series as "Building industry
#' SCN-2010" and reports its unit as an index.
#'
#' @encoding UTF-8
#' @format A tibble with quarterly observations:
#' \describe{
#'   \item{date}{Date of the first day of the quarter}
#'   \item{index}{Observed building-industry GDP index}
#' }
#'
#' @details
#' The package snapshot was downloaded through 2025-12-31. BCB may revise
#' historical observations and extend the series after that date.
#'
#' @source Instituto Brasileiro de Geografia e Estatística (IBGE),
#' disseminated by the Banco Central do Brasil through the Sistema Gerenciador
#' de Séries Temporais (SGS), series 22087. See the [official SGS metadata](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=22087) and [series page](https://www3.bcb.gov.br/sgspub/consultarvalores/consultarValoresSeries.do?method=consultarGraficoPorId&hdOidSeriesSelecionadas=22087).
"gdp_construction"

#' Central Bank Economic Activity Index (IBC-Br)
#'
#' Monthly Central Bank Economic Activity Index (IBC-Br), a timely indicator
#' of national economic activity. It is built from proxies for agriculture,
#' industry, and services, aggregated using weights derived from the supply and
#' use tables of the Brazilian National Accounts.
#'
#' @encoding UTF-8
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date of the first day of the month}
#'   \item{index}{IBC-Br index value}
#' }
#'
#' @details
#' The package snapshot was downloaded through 2025-12-31. BCB may revise
#' historical observations and extend the series after that date.
#'
#' @source Banco Central do Brasil — Departamento Econômico, disseminated
#' through the Sistema Gerenciador de Séries Temporais (SGS), series 24363.
#' See the [official SGS metadata](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=24363) and [series page](https://www3.bcb.gov.br/sgspub/consultarvalores/consultarValoresSeries.do?method=consultarGraficoPorId&hdOidSeriesSelecionadas=24363).
"ibcbr"

#' Total Vehicle Sales
#'
#' Monthly total vehicle sales in Brazil, measured in units. The BCB SGS
#' title for series 1378 is "Vehicle sales (total)".
#'
#' @encoding UTF-8
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date of the first day of the month}
#'   \item{production}{Total vehicle sales in units. The column name is
#'     retained for compatibility with existing package examples.}
#' }
#'
#' @details
#' The package snapshot was downloaded through 2025-12-31. BCB may revise
#' historical observations and extend the series after that date.
#'
#' @source Associação Nacional dos Fabricantes de Veículos Automotores
#' (Anfavea), disseminated by the Banco Central do Brasil through the Sistema
#' Gerenciador de Séries Temporais (SGS), series 1378. See the [official SGS metadata](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=1378) and [series page](https://www3.bcb.gov.br/sgspub/consultarvalores/consultarValoresSeries.do?method=consultarGraficoPorId&hdOidSeriesSelecionadas=1378).
"vehicles"

#' Petroleum Derivatives Production
#'
#' Monthly total production of petroleum derivatives in Brazil, measured in
#' thousand barrels per day.
#'
#' @encoding UTF-8
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date of the first day of the month}
#'   \item{production}{Petroleum derivatives production in thousand barrels
#'     per day}
#' }
#'
#' @details
#' The package snapshot was downloaded through 2025-12-31. BCB may revise
#' historical observations and extend the series after that date.
#'
#' @source Agência Nacional do Petróleo, Gás Natural e Biocombustíveis (ANP),
#' disseminated by the Banco Central do Brasil through the Sistema Gerenciador
#' de Séries Temporais (SGS), series 1391. See the [official SGS metadata](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=1391) and [series page](https://www3.bcb.gov.br/sgspub/consultarvalores/consultarValoresSeries.do?method=consultarGraficoPorId&hdOidSeriesSelecionadas=1391).
"oil_derivatives"

#' Residential Electricity Consumption
#'
#' Monthly residential electricity consumption in Brazil, measured in GWh.
#'
#' @encoding UTF-8
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date of the first day of the month}
#'   \item{consumption}{Electricity consumption in GWh}
#' }
#'
#' @details
#' The package snapshot was downloaded through 2025-12-31. BCB may revise
#' historical observations and extend the series after that date.
#'
#' @source Centrais Elétricas Brasileiras S.A. (Eletrobras), disseminated by
#' the Banco Central do Brasil through the Sistema Gerenciador de Séries
#' Temporais (SGS), series 1403. See the [official SGS metadata](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=1403) and [series page](https://www3.bcb.gov.br/sgspub/consultarvalores/consultarValoresSeries.do?method=consultarGraficoPorId&hdOidSeriesSelecionadas=1403).
"electric"

#' Electricity Consumption by Sector
#'
#' Monthly electricity consumption in Brazil by sector (GWh), in long format.
#' This dataset combines the residential, commercial, and industrial SGS
#' series by row-binding them; it does not calculate a total or otherwise
#' transform their values.
#'
#' @encoding UTF-8
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date of the first day of the month}
#'   \item{name_series}{Sector identifier: `"electric_residential"`,
#'     `"electric_commercial"`, or `"electric_industrial"`}
#'   \item{value}{Electricity consumption in GWh}
#' }
#'
#' @details
#' The package snapshot was downloaded through 2025-12-31. BCB may revise
#' historical observations and extend the series after that date. The
#' residential series is also available in wide format as [electric].
#'
#' @seealso [electric] for the residential-only wide-format series.
#'
#' @source Centrais Elétricas Brasileiras S.A. (Eletrobras), disseminated by
#' the Banco Central do Brasil through the Sistema Gerenciador de Séries
#' Temporais (SGS). The component series are 1402 (commercial), 1403
#' (residential), and 1404 (industrial). See the official [SGS metadata for
#' series 1402](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=1402), [1403](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=1403), and [1404](https://www3.bcb.gov.br/sgspub/consultarmetadados/consultarMetadadosSeries.do?method=consultarMetadadosSeriesInternet&hdOidSerieSelecionada=1404).
"electricity"

#' Series Metadata
#'
#' Metadata for all economic series included in the package.
#'
#' @encoding UTF-8
#' @format A tibble with metadata:
#' \describe{
#'   \item{series_name}{Short series identifier}
#'   \item{description}{Full series description}
#'   \item{frequency}{Data frequency (D = daily, M = monthly, Q = quarterly)}
#'   \item{source}{Data source}
#'   \item{date_col}{Name of the date column in the dataset}
#'   \item{value_col}{Name of the main value column(s) in the dataset}
#'   \item{group_cols}{Grouping column(s) for long-format datasets, or `NA`}
#'   \item{date_min}{First observation date}
#'   \item{date_max}{Last observation date}
#' }
#' @source Various. BCB-SGS metadata and source institutions are documented
#' on the individual dataset pages; see the [BCB SGS series search](https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries).
"metadata_series"

#' Great Britain Retail Sales Volume Indices
#'
#' Selected non-seasonally adjusted chained volume measures of retail sales in
#' Great Britain. The data are sourced from Table 3M of the Office for National
#' Statistics (ONS) Retail Sales Index reference tables and retain the "All
#' Businesses" series.
#'
#' Values are index numbers of sales per week, with 2023 = 100. ONS uses a
#' four-week, four-week, five-week reporting cycle; `date` records the first
#' day of the corresponding reference month and should not be interpreted as
#' a calendar-month total. The Retail Sales Index covers Great Britain, not
#' the whole UK: Northern Ireland, the Isle of Man, and the Channel Islands
#' are excluded.
#'
#' The bundled snapshot covers January 1988 through January 2026. The
#' `name_series` column contains the following selected series:
#' `household-goods-stores`, `computers-and-telecomms-equipment`,
#' `electrical-household-appliances`,
#' `pharmaceutical-medical-cosmetic-and-toilet-goods`,
#' `books-newspapers-and-periodicals`,
#' `alcoholic-drinks-other-beverages-and-tobacco`, `clothing`,
#' `all-retailing-including-automotive-fuel`, and
#' `all-retailing-excluding-automotive-fuel`.
#' Missing values in `value` represent empty cells in the ONS source table.
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{First day of the reference month (`Date`).}
#'   \item{name_series}{ONS series name in lower-case, hyphenated form.}
#'   \item{value}{Non-seasonally adjusted chained volume index of sales per
#'     week (2023 = 100).}
#' }
#'
#' @seealso [retail_autofuel]
#'
#' @source
#' Office for National Statistics (ONS), [Retail Sales Index](https://www.ons.gov.uk/businessindustryandtrade/retailindustry/datasets/retailsalesindexreferencetables) reference tables, Table 3M,
#' "Chained volume of retail sales, non-seasonally adjusted".
#' See the [Retail Sales Index (RSI) Quality and Methodology Information
#' report](https://www.ons.gov.uk/businessindustryandtrade/retailindustry/methodologies/retailsalesindexrsiqmi).
#' Contains public sector information licensed under the [Open Government
#' Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/),
#' except where otherwise stated. This package distributes a processed subset
#' of the ONS table; see `data-raw/uk_data.R` for the transformation code.
"retail_volume"

#' Great Britain Retail Sales Volume Index: Automotive Fuel
#'
#' Non-seasonally adjusted chained volume measure of retail sales for
#' automotive fuel in Great Britain. This is the ONS "Automotive Fuel, All
#' Businesses" series from Table 3M of the Retail Sales Index reference
#' tables.
#'
#' Values are index numbers of sales per week, with 2023 = 100. ONS uses a
#' four-week, four-week, five-week reporting cycle; `date` records the first
#' day of the corresponding reference month and should not be interpreted as
#' a calendar-month total. The Retail Sales Index covers Great Britain, not
#' the whole UK: Northern Ireland, the Isle of Man, and the Channel Islands
#' are excluded.
#'
#' The bundled snapshot covers January 1996 through January 2026.
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{First day of the reference month (`Date`).}
#'   \item{value}{Non-seasonally adjusted chained volume index of sales per
#'     week (2023 = 100).}
#'   \item{name}{Series name, always `Retail Sales - Automotive Fuel`.}
#'   \item{frequency}{Frequency code, always `"M"` (monthly).}
#'   \item{source}{Short source label, always `"ONS"`.}
#' }
#'
#' @seealso [retail_volume]
#' @source
#' Office for National Statistics (ONS), [Retail Sales Index](https://www.ons.gov.uk/businessindustryandtrade/retailindustry/datasets/retailsalesindexreferencetables) reference tables, Table 3M,
#' "Chained volume of retail sales, non-seasonally adjusted".
#' See the [Retail Sales Index (RSI) Quality and Methodology Information
#' report](https://www.ons.gov.uk/businessindustryandtrade/retailindustry/methodologies/retailsalesindexrsiqmi).
#' Contains public sector information licensed under the [Open Government
#' Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/),
#' except where otherwise stated. This package distributes a processed subset
#' of the ONS table; see `data-raw/uk_data.R` for the transformation code.
"retail_autofuel"

#' TfL Network Demand: Monthly Bus and Tube Journeys
#'
#' Monthly totals of TfL's reported journey counts for London's bus and Tube
#' networks. The source files contain daily counts for the two modes; this
#' dataset sums those daily values by calendar month.
#'
#' These are recorded journey counts derived from TfL's ticketing system, not
#' an absolute measure of passenger numbers or journeys made. TfL notes that
#' the figures cover activity recorded through Oyster cards, contactless
#' payment cards, and paper tickets, and do not include passengers who did not
#' tap in or out. The source figures are approximate and rounded to the nearest
#' thousand. This dataset is not the separate station footfall dataset.
#'
#' TfL publishes separate files for 2019--2022 and a consolidated file for
#' subsequent observations. The version bundled with `trendseries` covers
#' daily source records from 2019-01-01 through 2025-12-27. Because the final
#' source file ends on 27 December 2025, the December 2025 monthly totals are
#' partial. TfL may revise historical rows when its source files are refreshed.
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date_month}{First day of the calendar month (`Date`).}
#'   \item{transit_mode}{Transit mode: `"bus"` or `"tube"`.}
#'   \item{journey_monthly}{Sum of the reported daily journey counts for the
#'     month.}
#' }
#'
#' @seealso [transit_london_avgs]
#' @source
#' Transport for London (TfL), [Network demand data](https://tfl.gov.uk/corporate/publications-and-reports/network-demand-data), and the [Network Demand Dashboard](https://app.powerbi.com/view?r=eyJrIjoiZDgwZWY4NWMtZTFkMi00YzM2LThiMWQtNzg2ZTc2YjliNzM2IiwidCI6IjFmYmQ2NWJmLTVkZWYtNGVlYS1hNjkyLWEwODljMjU1MzQ2YiIsImMiOjh9).
#' The package distributes an aggregated and reshaped version of TfL's daily
#' Journeys files. TfL's [Transport Data Service terms](https://tfl.gov.uk/corporate/terms-and-conditions/transport-data-service)
#' require the attribution: **Powered by TfL Open Data**. This package is not
#' affiliated with or endorsed by TfL.
"transit_london_monthly"

#' TfL Network Demand: Average Daily Journeys
#'
#' Monthly averages of TfL's reported daily journey counts for London's bus
#' and Tube networks, split by mode and UK business-day status. The average is
#' calculated over the daily observations in each month and group; it is not a
#' monthly total.
#'
#' The business-day flag uses the `QuantLib/UnitedKingdom` calendar from
#' `RQuantLib`: `1` identifies a business day and `0` identifies a Saturday,
#' Sunday, or UK holiday. The underlying counts are derived from TfL's
#' ticketing system and should not be interpreted as an absolute measure of
#' passenger numbers or journeys made. They are approximate, rounded to the
#' nearest thousand, and exclude passengers who did not tap in or out.
#'
#' The version bundled with `trendseries` covers daily source records from
#' 2019-01-01 through 2025-12-27. December 2025 is therefore a partial month.
#' TfL may revise historical rows when its source files are refreshed.
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date_month}{First day of the calendar month (`Date`).}
#'   \item{transit_mode}{Transit mode: `"bus"` or `"tube"`.}
#'   \item{is_business_day}{`1` for a UK business day and `0` for a
#'     weekend or UK holiday.}
#'   \item{avg_daily_journeys}{Arithmetic mean of the reported daily journey
#'     counts for the month, mode, and business-day group.}
#' }
#'
#' @seealso [transit_london_monthly]
#' @source
#' Transport for London (TfL), [Network demand data](https://tfl.gov.uk/corporate/publications-and-reports/network-demand-data), and the [Network Demand Dashboard](https://app.powerbi.com/view?r=eyJrIjoiZDgwZWY4NWMtZTFkMi00YzM2LThiMWQtNzg2ZTc2YjliNzM2IiwidCI6IjFmYmQ2NWJmLTVkZWYtNGVlYS1hNjkyLWEwODljMjU1MzQ2YiIsImMiOjh9).
#' The package distributes an aggregated and reshaped version of TfL's daily
#' Journeys files and classifies dates with the UK business calendar. TfL's
#' [Transport Data Service terms](https://tfl.gov.uk/corporate/terms-and-conditions/transport-data-service)
#' require the attribution: **Powered by TfL Open Data**. This package is not
#' affiliated with or endorsed by TfL.
"transit_london_avgs"

#' Daily Arabica Coffee Price Indicator
#'
#' @description
#' Daily observations of the CEPEA/ESALQ Arabica coffee price indicator,
#' accompanied by an inflation-adjusted dollar series and a derived
#' moving-average column.
#'
#' `spot_rs` and `spot_us` are nominal at-sight prices in Brazilian reais and
#' U.S. dollars per 60-kg bag. CEPEA converts the dollar value using the
#' commercial selling exchange rate quoted at 16:30. `usd_2022` is calculated
#' in this package from `spot_us` using U.S. inflation data and `priceR`.
#'
#' CEPEA defines the indicator as type 6 Arabica coffee, hard cup or better,
#' well prepared, with up to 86 defects in a 300-g sample. It represents
#' wholesale lot transactions delivered to the city of São Paulo, with freight
#' from the producing region included. The reference regions are Cerrado and
#' southern Minas Gerais, Mogiana and Garça in São Paulo, and northwestern
#' Paraná. Regional weights are based on production volumes reported by IBGE
#' and are updated on the first business day of a new crop. CEPEA excludes
#' observations outside two standard deviations from the sample mean and
#' includes taxes in the reported values. For term transactions, CEPEA
#' discounts prices at CDI from 2021-03-01; before that it used NPR. The
#' survey consults cooperatives, brokers, roasters, and exporters.
#'
#' The indicator is daily and its historical series begins in September 1996.
#' The bundled observations run from 1996-09-02 through 2025-04-17.
#'
#' @format A tibble with daily observations:
#' \describe{
#'   \item{date}{Observation date (`Date`).}
#'   \item{spot_rs}{CEPEA at-sight price in Brazilian reais per 60-kg bag.}
#'   \item{spot_us}{CEPEA at-sight price in U.S. dollars per 60-kg bag.}
#'   \item{usd_2022}{`spot_us` adjusted to 2022 U.S. dollars using `priceR`.}
#'   \item{trend_ma}{22-observation, right-aligned moving average of
#'     `usd_2022`. The first 21 observations have no full window and are `NA`.}
#' }
#'
#' @seealso [coffee_robusta]
#'
#' @source
#' Original data: Centro de Estudos Avançados em Economia Aplicada (CEPEA),
#' Escola Superior de Agricultura Luiz de Queiroz (ESALQ), Universidade de
#' São Paulo (USP), [CEPEA/ESALQ Arabica price series](https://www.cepea.org.br/br/indicador/series/cafe.aspx?id=23).
#' See the [CEPEA Arabica methodology](https://www.cepea.org.br/br/metodologia/metodologia-cafe-arabica-cepea-esalq.aspx).
#' CEPEA identifies its coffee data as available under the
#' [CC BY-NC 4.0 license](https://creativecommons.org/licenses/by-nc/4.0/).
#' This package distributes an adapted version by adding `usd_2022` and
#' `trend_ma`.
"coffee_arabica"

#' Daily Robusta Coffee Price Indicator
#'
#' @description
#' Daily observations of the CEPEA/ESALQ Robusta coffee price indicator,
#' accompanied by an inflation-adjusted dollar series and a derived
#' moving-average column.
#'
#' `spot_rs` and `spot_us` are nominal at-sight prices in Brazilian reais and
#' U.S. dollars per 60-kg bag. CEPEA converts the dollar value using the
#' commercial selling exchange rate quoted at 16:30. `usd_2022` is calculated
#' in this package from `spot_us` using U.S. inflation data and `priceR`.
#'
#' CEPEA defines the indicator as type 6 Robusta coffee, screen 13 or above,
#' with 86 defects. It represents wholesale lot transactions to be collected
#' in the producing region, so the indicator does not include freight. The
#' reference regions are Colatina and São Gabriel da Palha in Espírito Santo,
#' and the final indicator is their arithmetic average. CEPEA includes taxes
#' in the reported values and excludes observations outside two standard
#' deviations from the sample mean. For term transactions, CEPEA discounts
#' prices at CDI from 2021-03-01; before that it used NPR. The survey consults
#' cooperatives, brokers, roasters, and exporters.
#'
#' The indicator is daily and its historical series begins in November 2001.
#' The bundled observations run from 2001-11-08 through 2025-04-17.
#'
#' @format A tibble with daily observations:
#' \describe{
#'   \item{date}{Observation date (`Date`).}
#'   \item{spot_rs}{CEPEA at-sight price in Brazilian reais per 60-kg bag.}
#'   \item{spot_us}{CEPEA at-sight price in U.S. dollars per 60-kg bag.}
#'   \item{usd_2022}{`spot_us` adjusted to 2022 U.S. dollars using `priceR`.}
#'   \item{trend_ma}{22-observation, right-aligned moving average of
#'     `usd_2022`. The first 21 observations have no full window and are `NA`.}
#' }
#'
#' @seealso [coffee_arabica]
#'
#' @source
#' Original data: Centro de Estudos Avançados em Economia Aplicada (CEPEA),
#' Escola Superior de Agricultura Luiz de Queiroz (ESALQ), Universidade de
#' São Paulo (USP), [CEPEA/ESALQ Robusta price series](https://www.cepea.org.br/br/indicador/series/cafe.aspx?id=24).
#' See the [CEPEA Robusta methodology](https://www.cepea.org.br/br/metodologia/metodologia-cafe-robusta-cepea-esalq.aspx).
#' CEPEA identifies its coffee data as available under the
#' [CC BY-NC 4.0 license](https://creativecommons.org/licenses/by-nc/4.0/).
#' This package distributes an adapted version by adding `usd_2022` and
#' `trend_ma`.
"coffee_robusta"
