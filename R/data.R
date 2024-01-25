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
