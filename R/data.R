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

#' Coffee prices
#'
#' Monthly coffee price proxy data from Brazilian agricultural commodity series.
#'
#' @format A tibble with monthly observations:
#' \describe{
#'   \item{date}{Date column}
#'   \item{value}{Coffee price proxy index}
#'   \item{code_bcb}{BCB series code (1378)}
#'   \item{name}{Series name}
#'   \item{frequency}{Frequency ("M")}
#' }
#' @source Brazilian Central Bank SGS
"coffee_prices"
