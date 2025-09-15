#' Quarterly Brazilian GDP
#'
#' Seasonally adjusted quarterly Brazilian GDP at market prices.
#'
#' @format A tibble with quarterly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{value}{GDP at market prices}
#'   \item{code_bcb}{BCB series code (22109)}
#'   \item{name}{Series name}
#'   \item{frequency}{Frequency ("Q")}
#' }
#' @source Brazilian Central Bank SGS
"gdp_brazil_qtr"

#' Central Bank Economic Activity Index
#'
#' Monthly Central Bank Economic Activity Index (IBC-Br).
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{value}{Index (2003 = 100)}
#'   \item{code_bcb}{BCB series code (24363)}
#'   \item{name}{Series name}
#'   \item{frequency}{Frequency ("M")}
#' }
#' @source Brazilian Central Bank SGS
"ibcbr"

#' Vehicle production
#'
#' Monthly vehicle production in Brazil (thousands of units).
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{value}{Vehicle production in thousands of units}
#'   \item{code_bcb}{BCB series code (1378)}
#'   \item{name}{Series name}
#'   \item{frequency}{Frequency ("M")}
#' }
#' @source Brazilian Central Bank SGS
"vehicle_production"

#' Cement consumption
#'
#' Monthly cement consumption in Brazil (thousands of tons).
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{value}{Cement consumption in thousands of tons}
#'   \item{code_bcb}{BCB series code (1389)}
#'   \item{name}{Series name}
#'   \item{frequency}{Frequency ("M")}
#' }
#' @source Brazilian Central Bank SGS
"cement_consumption"

#' Electric consumption residential
#'
#' Monthly residential electric consumption in Brazil (GWh).
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{value}{Electric consumption in GWh}
#'   \item{code_bcb}{BCB series code (1403)}
#'   \item{name}{Series name}
#'   \item{frequency}{Frequency ("M")}
#' }
#' @source Brazilian Central Bank SGS
"electric_consumption"

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
