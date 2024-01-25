#> Download example series for package

library(rbcb)
library(dplyr)

params <- data.frame(
  code_bcb = c(1207, 22109, 24363, 1),
  name = c("GDP at current prices in R$",
           "Quarterly GDP - seasonally adjusted data - GDP at market prices",
           "Central Bank Economic Activity Index",
           "Exchange rate - Free - United States dollar (sale) - 1"
           ),
  frequency = c("A", "Q", "M", "D")
)

get_bcb_series <- function(code) {

  series <- rbcb::get_series(code, as = "tibble")

  names(series)[2] <- "value"
  series$code_bcb <- code

  series <- dplyr::left_join(series, params, by = "code_bcb")

}

gdp_brazil <- get_bcb_series(1207)
gdp_brazil_qtr <- get_bcb_series(22109)
ibcbr <- get_bcb_series(24363)
brlusd <- get_bcb_series(1)
