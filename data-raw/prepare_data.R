#> Prepare all example datasets for trendseries package

library(dplyr)

# Helper function to safely fetch BCB series
get_bcb_series <- function(code, name, frequency, start_date = "01/01/2000") {
  cat("Fetching BCB series", code, ":", name, "\n")

  tryCatch({
    # Check if rbcb is available
    if (!requireNamespace("rbcb", quietly = TRUE)) {
      stop("Package 'rbcb' is required but not available")
    }

    series <- rbcb::get_series(code, start_date = start_date, as = "tibble")
    names(series)[2] <- "value"
    series$code_bcb <- code
    series$name <- name
    series$frequency <- frequency

    cat("Successfully fetched", nrow(series), "observations for", name, "\n")
    return(series)
  }, error = function(e) {
    cat("Error fetching BCB series", code, ":", e$message, "\n")
    return(NULL)
  })
}



# =============================================================================
# BRAZIL DATA - BCB SGS Series
# =============================================================================

cat("=== Fetching Brazil Data ===\n")

# Keep existing Brazil series
gdp_brazil_qtr <- get_bcb_series(
  code = 22109,
  name = "Quarterly GDP - seasonally adjusted data - GDP at market prices",
  frequency = "Q"
)

ibcbr <- get_bcb_series(
  code = 24363,
  name = "Central Bank Economic Activity Index",
  frequency = "M"
)

electric_consumption <- get_bcb_series(
  code = 1403,
  name = "Electric consumption residential",
  frequency = "M"
)

# New Brazil series
vehicle_production <- get_bcb_series(
  code = 1378,
  name = "Vehicle production",
  frequency = "M"
)

cement_consumption <- get_bcb_series(
  code = 1389,
  name = "Cement consumption",
  frequency = "M"
)



# =============================================================================
# SAVE DATASETS
# =============================================================================

cat("\n=== Saving Datasets ===\n")

# Save successful Brazil series
if (!is.null(gdp_brazil_qtr)) {
  usethis::use_data(gdp_brazil_qtr, overwrite = TRUE)
  cat("gdp_brazil_qtr saved successfully\n")
}

if (!is.null(ibcbr)) {
  usethis::use_data(ibcbr, overwrite = TRUE)
  cat("ibcbr saved successfully\n")
}

if (!is.null(electric_consumption)) {
  usethis::use_data(electric_consumption, overwrite = TRUE)
  cat("electric_consumption saved successfully\n")
}

if (!is.null(vehicle_production)) {
  usethis::use_data(vehicle_production, overwrite = TRUE)
  cat("vehicle_production saved successfully\n")
}

if (!is.null(cement_consumption)) {
  usethis::use_data(cement_consumption, overwrite = TRUE)
  cat("cement_consumption saved successfully\n")
}



# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== Summary ===\n")
cat("Brazil series:\n")
if (!is.null(gdp_brazil_qtr)) cat("- GDP quarterly:", nrow(gdp_brazil_qtr), "observations\n")
if (!is.null(ibcbr)) cat("- IBC-Br:", nrow(ibcbr), "observations\n")
if (!is.null(electric_consumption)) cat("- Electricity:", nrow(electric_consumption), "observations\n")
if (!is.null(vehicle_production)) cat("- Vehicles:", nrow(vehicle_production), "observations\n")
if (!is.null(cement_consumption)) cat("- Cement:", nrow(cement_consumption), "observations\n")

cat("\nDataset preparation complete!\n")