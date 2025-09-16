#' GDP Construction Index
#'
#' Quarterly GDP construction sector index (Base: average 1995 = 100).
#'
#' @format A tibble with quarterly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{gdp_construction}{Construction index value}
#' }
#' @source Brazilian Central Bank SGS (code 22087)
"gdp_construction"

#' Central Bank Economic Activity Index
#'
#' Monthly Central Bank Economic Activity Index (IBC-Br).
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{ibcbr}{Index (2003 = 100)}
#' }
#' @source Brazilian Central Bank SGS (code 24363)
"ibcbr"

#' Vehicle Production
#'
#' Monthly vehicle production in Brazil (thousands of units).
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{vehicles}{Vehicle production in thousands of units}
#' }
#' @source Brazilian Central Bank SGS (code 1378)
"vehicles"

#' Oil Derivatives Production
#'
#' Monthly oil derivatives production in Brazil.
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{oil_derivatives}{Oil derivatives production}
#' }
#' @source Brazilian Central Bank SGS (code 1391)
"oil_derivatives"

#' Electric Consumption Residential
#'
#' Monthly residential electric consumption in Brazil (GWh).
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{electric}{Electric consumption in GWh}
#' }
#' @source Brazilian Central Bank SGS (code 1403)
"electric"

#' Series Metadata
#'
#' Metadata for all BCB economic series in the package.
#'
#' @format A tibble with metadata:
#' \describe{
#'   \item{series_name}{Short series identifier}
#'   \item{description}{Full series description}
#'   \item{frequency}{Data frequency (M = monthly, Q = quarterly)}
#'   \item{first_obs}{First observation date}
#'   \item{last_obs}{Last observation date}
#'   \item{source}{Data source with BCB code}
#' }
#' @source Brazilian Central Bank SGS
"series_metadata"

#' UK Retail Sales - Household Goods Stores
#'
#' Monthly retail sales index for household goods stores in the UK.
#' Chained volume measure of retail sales.
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{household_goods_stores}{Retail sales index (chained volume)}
#'   \item{name}{Series name}
#'   \item{frequency}{Frequency ("M")}
#'   \item{source}{Data source ("ONS")}
#' }
#' @source UK Office for National Statistics (ONS)
"retail_households"

#' UK Retail Sales - Automotive Fuel
#'
#' Monthly retail sales index for automotive fuel in the UK.
#' Chained volume measure of retail sales.
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{automotive_fuel}{Retail sales index (chained volume)}
#'   \item{name}{Series name}
#'   \item{frequency}{Frequency ("M")}
#'   \item{source}{Data source ("ONS")}
#' }
#' @source UK Office for National Statistics (ONS)
"retail_autofuel"

#' CEPEA Arabica Coffee Prices
#'
#' Daily Arabica coffee price data from CEPEA/ESALQ with inflation adjustment.
#' Type 6 coffee prices delivered in São Paulo (capital).
#'
#' @format A tibble with daily observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{spot_rs}{Spot price in Brazilian Reais per 60-kg bag}
#'   \item{spot_us}{Spot price in US Dollars per 60-kg bag}
#'   \item{usd_2022}{US Dollar price adjusted for inflation (base year 2022)}
#'   \item{trend}{22-day rolling mean of inflation-adjusted prices}
#' }
#' @source CEPEA - Center for Advanced Studies on Applied Economics
"coffee_arabica"

#' CEPEA Robusta Coffee Prices
#'
#' Daily Robusta coffee price data from CEPEA/ESALQ with inflation adjustment.
#' Type 6 coffee prices in Espírito Santo state.
#'
#' @format A tibble with daily observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{spot_rs}{Spot price in Brazilian Reais per 60-kg bag}
#'   \item{spot_us}{Spot price in US Dollars per 60-kg bag}
#'   \item{usd_2022}{US Dollar price adjusted for inflation (base year 2022)}
#'   \item{trend}{22-day rolling mean of inflation-adjusted prices}
#' }
#' @source CEPEA - Center for Advanced Studies on Applied Economics
"coffee_robusta"
