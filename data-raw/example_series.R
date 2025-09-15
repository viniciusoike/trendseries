#> Download example series for package

library(rbcb)
library(dplyr)

params <- data.frame(
  code_bcb = c(1207, 22109, 24363, 1, 1403, 20704),
  name = c("GDP at current prices in R$",
           "Quarterly GDP - seasonally adjusted data - GDP at market prices",
           "Central Bank Economic Activity Index",
           "Exchange rate - Free - United States dollar (sale) - 1",
           "Electric consumption residential",
           "Real estate credit"
           ),
  frequency = c("A", "Q", "M", "D", "M", "M")
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
electric_consumption <- get_bcb_series(1403)
real_estate_credit <- get_bcb_series(20704)

# Create simulated coffee price data (CEPEA ESALQ USP style)
# In a real implementation, this would be obtained from CEPEA API or data files
set.seed(123)
dates_coffee <- seq(as.Date("2010-01-01"), as.Date("2023-12-31"), by = "month")
n_obs <- length(dates_coffee)

# Simulate realistic coffee price movement with trend and seasonality
trend <- 350 + 0.5 * seq_along(dates_coffee) + 50 * sin(2 * pi * seq_along(dates_coffee) / 48)
seasonal <- 20 * sin(2 * pi * seq_along(dates_coffee) / 12)
noise <- rnorm(n_obs, 0, 15)
price_values <- trend + seasonal + noise

coffee_prices_cepea <- tibble::tibble(
  date = dates_coffee,
  value = price_values,
  code_bcb = NA,
  name = "Coffee Arabica Price Index (CEPEA ESALQ/USP)",
  frequency = "M"
)
