#' Annual Brazilian GDP 1962-2022
#'
#' A `tibble` containing the annual series of Brazilian GDP at current prices
#' in Reais (R\$)
#'
#' @format ## `gdp_brazil`
#' A `tibble` with 61 rows and 5 columns:
#' \describe{
#'   \item{date}{Date column in YMD format}
#'   \item{value}{GDP at current prices.}
#'   \item{code_bcb}{Code of the series in BCB Time Series Management System}
#'   \item{name}{Name of the series}
#'   \item{frequency}{Character indicating the frequency of the series}
#' }
#' @source Brazilian Central Bank (Time Series Management System) <https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries>
"gdp_brazil"

#' Quarterly Brazilian GDP 1995-2023
#'
#' A `tibble` containing the seasonally adjusted quarterly Brazilian GDP at
#' market prices (R\$).
#'
#' @format ## `gdp_brazil_qtr`
#' A `tibble` with 111 rows and 5 columns:
#' \describe{
#'   \item{date}{Date column in YMD format}
#'   \item{value}{GDP at market prices.}
#'   \item{code_bcb}{Code of the series in BCB Time Series Management System}
#'   \item{name}{Name of the series}
#'   \item{frequency}{Character indicating the frequency of the series}
#' }
#' @source Brazilian Central Bank (Time Series Management System) <https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries>
"gdp_brazil_qtr"

#' Central Bank Economic Activity Index
#'
#' A `tibble` containing the seasonally adjusted Central Bank Economic Activity
#' Index (IBC-Br).
#'
#' @details
#' The IBC-Br was built based on proxies for the evolution of
#' agriculture, industry and service-sector products. The proxies are aggregated
#' with weights derived from the tables of supply and use of the Brazilian
#' National Accounts.
#'
#' The seasonal adjustment is performed with the X13-ARIMA-SEATS software
#' and includes level change and temporary level change outliers in the last
#' quarter of 2008, a standard regressor for leap year and calendar regressors
#' for the monthly composition of week days controlled for the effect of
#' national holidays.
#'
#' @format ## `ibcbr`
#' A `tibble` with 251 rows and 5 columns:
#' \describe{
#'   \item{date}{Date column in YMD format}
#'   \item{value}{Index (2003 = 100)}
#'   \item{code_bcb}{Code of the series in BCB Time Series Management System}
#'   \item{name}{Name of the series}
#'   \item{frequency}{Character indicating the frequency of the series}
#' }
#' @source Brazilian Central Bank (Time Series Management System) <https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries>
"ibcbr"

#' Exchange rate USD/BRL 1984-2023
#'
#' Daily exchange rate between the US dollar (USD) and the Brazilian Real (BRL)
#'
#' @format ## `brlusd`
#' A `tibble` with 9,810 rows and 5 columns:
#' \describe{
#'   \item{date}{Date column in YMD format}
#'   \item{value}{c.m.u./US$}
#'   \item{code_bcb}{Code of the series in BCB Time Series Management System}
#'   \item{name}{Name of the series}
#'   \item{frequency}{Character indicating the frequency of the series}
#' }
#' @source Brazilian Central Bank (Time Series Management System) <https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries>
"brlusd"

#' Electric consumption residential
#'
#' A `tibble` containing the monthly series of residential electric consumption
#' in Brazil (GWh).
#'
#' @format ## `electric_consumption`
#' A `tibble` with monthly observations and 5 columns:
#' \describe{
#'   \item{date}{Date column in YMD format}
#'   \item{value}{Electric consumption in GWh}
#'   \item{code_bcb}{Code of the series in BCB Time Series Management System}
#'   \item{name}{Name of the series}
#'   \item{frequency}{Character indicating the frequency of the series}
#' }
#' @source Brazilian Central Bank (Time Series Management System) <https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries>
"electric_consumption"

#' Real estate credit
#'
#' A `tibble` containing the monthly series of real estate credit outstanding
#' in Brazil (R$ millions).
#'
#' @format ## `real_estate_credit`
#' A `tibble` with monthly observations and 5 columns:
#' \describe{
#'   \item{date}{Date column in YMD format}
#'   \item{value}{Real estate credit outstanding in R$ millions}
#'   \item{code_bcb}{Code of the series in BCB Time Series Management System}
#'   \item{name}{Name of the series}
#'   \item{frequency}{Character indicating the frequency of the series}
#' }
#' @source Brazilian Central Bank (Time Series Management System) <https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries>
"real_estate_credit"

#' Coffee Arabica Price Index (CEPEA ESALQ/USP)
#'
#' A `tibble` containing monthly coffee arabica price index data from
#' CEPEA ESALQ/USP (Center for Advanced Studies on Applied Economics,
#' "Luiz de Queiroz" College of Agriculture, University of São Paulo).
#'
#' @details
#' This dataset represents arabica coffee prices in Brazil. CEPEA/ESALQ provides
#' indicators for Brazilian agricultural commodities and is a reference for
#' coffee price analysis in Brazil.
#'
#' Note: This is simulated data for demonstration purposes. In a real implementation,
#' data would be obtained directly from CEPEA ESALQ/USP.
#'
#' @format ## `coffee_prices_cepea`
#' A `tibble` with monthly observations from 2010-2023 and 5 columns:
#' \describe{
#'   \item{date}{Date column in YMD format}
#'   \item{value}{Coffee price index (simulated)}
#'   \item{code_bcb}{NA (not from BCB)}
#'   \item{name}{Name of the series}
#'   \item{frequency}{Character indicating the frequency of the series}
#' }
#' @source CEPEA ESALQ/USP (simulated data) <https://www.cepea.esalq.usp.br/>
"coffee_prices_cepea"
