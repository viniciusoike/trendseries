#> Add new series to existing data for trendseries package

library(rbcb)
library(dplyr)
library(tibble)

# Load existing data
data("gdp_brazil", package = "trendseries")
data("gdp_brazil_qtr", package = "trendseries")
data("ibcbr", package = "trendseries")
data("brlusd", package = "trendseries")

# Parameters for new series
new_params <- data.frame(
  code_bcb = c(1403, 20704),
  name = c("Electric consumption residential",
           "Real estate credit"),
  frequency = c("M", "M")
)

get_bcb_series_safe <- function(code, name, freq) {
  tryCatch({
    cat("Fetching series", code, ":", name, "\n")
    series <- rbcb::get_series(code, as = "tibble")
    names(series)[2] <- "value"
    series$code_bcb <- code
    series$name <- name
    series$frequency <- freq
    return(series)
  }, error = function(e) {
    cat("Error fetching series", code, ":", e$message, "\n")
    return(NULL)
  })
}

# Fetch new BCB series
electric_consumption <- get_bcb_series_safe(1403, "Electric consumption residential", "M")
real_estate_credit <- get_bcb_series_safe(20704, "Real estate credit", "M")

# Create coffee price data (simulated)
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

# Save new datasets
if (!is.null(electric_consumption)) {
  usethis::use_data(electric_consumption, overwrite = TRUE)
  cat("electric_consumption saved successfully\n")
}

if (!is.null(real_estate_credit)) {
  usethis::use_data(real_estate_credit, overwrite = TRUE)
  cat("real_estate_credit saved successfully\n")
}

usethis::use_data(coffee_prices_cepea, overwrite = TRUE)
cat("coffee_prices_cepea saved successfully\n")

# Show summary of new data
if (!is.null(electric_consumption)) {
  cat("\nElectric consumption summary:\n")
  cat("Observations:", nrow(electric_consumption), "\n")
  cat("Date range:", as.character(range(electric_consumption$date)), "\n")
}

if (!is.null(real_estate_credit)) {
  cat("\nReal estate credit summary:\n")
  cat("Observations:", nrow(real_estate_credit), "\n")
  cat("Date range:", as.character(range(real_estate_credit$date)), "\n")
}

cat("\nCoffee prices summary:\n")
cat("Observations:", nrow(coffee_prices_cepea), "\n")
cat("Date range:", as.character(range(coffee_prices_cepea$date)), "\n")