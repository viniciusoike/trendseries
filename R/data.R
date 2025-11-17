#' GDP Construction Index
#'
#' Quarterly Brazilian GDP construction sector index (Base: average 1995 = 100).
#'
#' @format A tibble with quarterly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{index}{Construction index value}
#' }
#' @source DEPEC/GEATI/COACE. Fetched from Brazilian Central Bank SGS (code 22087)
"gdp_construction"

#' Central Bank Economic Activity Index
#'
#' Monthly Central Bank Economic Activity Index (IBC-Br). The IBC-Br was built
#' based on proxies for the evolution of agriculture, industry and service-sector
#' products. The proxies are aggregated with weights derived from the tables of
#' supply and use of the Brazilian National Accounts.
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{index}{Index (2003 = 100)}
#' }
#' @source BACEN. Fetched from Brazilian Central Bank SGS (code 24363).
"ibcbr"

#' Vehicle Production
#'
#' Monthly vehicle production in Brazil (units).
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{production}{Vehicle production in absolute units}
#' }
#' @source Anfavea. Fetched from Brazilian Central Bank SGS (code 1378).
"vehicles"

#' Oil Derivatives Production
#'
#' Monthly production of petroleum derivatives in Brazil (thousand barrels/day).
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{production}{Oil derivatives production}
#' }
#' @source ANP. Fetched from Brazilian Central Bank SGS (code 1391).
"oil_derivatives"

#' Electric Consumption Residential
#'
#' Monthly residential electric consumption in Brazil (GWh).
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{consumption}{Electric consumption in GWh}
#' }
#' @source Brazilian Central Bank SGS (code 1403)
"electric"

#' Series Metadata
#'
#' Metadata for all economic series included in the package.
#'
#' @format A tibble with metadata:
#' \describe{
#'   \item{series_name}{Short series identifier}
#'   \item{description}{Full series description}
#'   \item{value_column}{Main value column(s) in the dataset}
#'   \item{frequency}{Data frequency (D = daily, M = monthly, Q = quarterly)}
#'   \item{first_obs}{First observation date}
#'   \item{last_obs}{Last observation date}
#'   \item{n_obs}{Number of observations}
#'   \item{source}{Data source}
#' }
#' @source Various (BCB-SGS, ONS, CEPEA/ESALQ)
"series_metadata"

#' UK Retail Index
#'
#' Chained volume of retail sales, non-seasonally adjusted, selected sub-series.
#' Index numbers of sales per week (100 = 2023). The monthly period consists
#' of 4 weeks except for March, June, September and December which are 5 weeks.
#' January 2025 is also a 5 week period.
#' The included series consider the "All Businesses" specification and cover
#' Great Britain from 1998 to 2025.
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{name_series}{Series name derived from the unofficial SIC}
#'   \item{value}{Retail sales index (chained volume)}
#' }
#'
#' @seealso [retail_autofuel]
#'
#' @source UK Office for National Statistics (ONS). (Table 3M).
"retail_volume"

#' UK Retail Sales - Automotive Fuel
#'
#' Chained volume of retail sales for automotive fuel in the UK, non-seasonally
#' adjusted.
#' Index numbers of sales per week (100 = 2023). The monthly period consists
#' of 4 weeks except for March, June, September and December which are 5 weeks.
#' January 2025 is also a 5 week period.
#' The series considers the "All Businesses" specification and covers
#' Great Britain from 1998 to 2025.
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{value}{Retail sales index (chained volume)}
#'   \item{name}{Series name}
#'   \item{frequency}{Frequency ("M")}
#'   \item{source}{Data source ("ONS")}
#' }
#'
#' @seealso [retail_volume]
#' @source UK Office for National Statistics (ONS). (Table 3M).
"retail_autofuel"

#' Daily Arabica Coffee Price
#'
#' @description
#' Daily Arabica coffee price data from CEPEA/ESALQ (USP) with inflation adjustment.
#'
#' Prices are provided in Brazilian reais, with USD values calculated using the daily
#' USD/BRL exchange rate at 16:30. All reported prices include taxes and freight costs.
#'
#' The data tracks prices for standard 60kg sacks of type 6 Arabica coffee negotiated
#' in São Paulo (SP), representing production from five major regions: Cerrado,
#' Sul de Minas Gerais, Mogiana (SP), Garça (SP), and Northeast Paraná. Regional
#' prices are weighted by production volume to create the final value.
#'
#' @format A tibble with daily observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{spot_rs}{Spot price in Brazilian Reais per 60-kg bag}
#'   \item{spot_us}{Spot price in US Dollars per 60-kg bag}
#'   \item{usd_2022}{US Dollar price adjusted for inflation (base year 2022)}
#'   \item{trend}{22-day rolling mean of inflation-adjusted prices}
#' }
#'
#' @seealso [coffee_robusta]
#'
#' @source Center for Advanced Studies in Applied Economics (CEPEA - ESALQ/USP).
"coffee_arabica"

#' Daily Robusta Coffee Price
#'
#' @description
#' Daily Robusta coffee price data from CEPEA/ESALQ (USP) with inflation adjustment.
#'
#' Prices are provided in Brazilian reais, with USD values calculated using the daily
#' USD/BRL exchange rate at 16:30. All reported prices include taxes and freight costs.
#'
#' The data tracks prices for standard 60kg sacks of type 6 Arabica coffee negotiated
#' in São Paulo (SP), representing production from two major regions: Colatina (ES)
#' and São Gabriel da Palha (ES). The final value is an arithmetic average of the regional prices.
#' @format A tibble with daily observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{spot_rs}{Spot price in Brazilian Reais per 60-kg bag}
#'   \item{spot_us}{Spot price in US Dollars per 60-kg bag}
#'   \item{usd_2022}{US Dollar price adjusted for inflation (base year 2022)}
#'   \item{trend}{22-day rolling mean of inflation-adjusted prices}
#' }
#'
#' @seealso [coffee_arabica]
#'
#' @source Center for Advanced Studies in Applied Economics (CEPEA - ESALQ/USP).
"coffee_robusta"
