## UK Retail Sales Data from ONS
## ============================================================================
## This script downloads and processes UK retail sales data from the Office
## for National Statistics (ONS) using the onsr package. It creates datasets
## for household goods stores and automotive fuel retail sales indices.

# Load required libraries
library(onsr)
library(dplyr)
library(janitor)
library(stringr)
library(readr)
library(tidyr)

# Future series to add (complex - will ignore for now):
# "traffic-camera-activity"
# "uk-spending-on-cards"
# "weekly-deaths-age-sex"

## Download retail sales data ------------------------------------------------

# Fetch retail sales index data for large and small businesses
retail <- onsr::ons_get("retail-sales-index-large-and-small-businesses")

## Data processing ------------------------------------------------------------

# Clean column names and filter for relevant series
subretail <- retail |>
  janitor::clean_names() |>
  filter(
    # Focus on chained volume measures
    type_of_prices == "chained-volume-of-retail-sales",
    # Select household goods and automotive fuel categories
    sic_unofficial %in%
      c(
        "household-goods-stores-all-businesses",
        "automotive-fuel-all-businesses"
      )
  )

# Process dates and arrange chronologically
subretail <- subretail |>
  # Filter for monthly data format (YYYY - MMM)
  filter(stringr::str_detect(time, "[0-9]{4} - [A-z]{3}")) |>
  # Parse dates from "YYYY - MMM" format
  mutate(date = readr::parse_date(time, "%Y - %b"), .before = 1) |>
  arrange(date)

# Reshape data to have series as columns
subretail_series <- subretail |>
  mutate(
    # Clean series names for column names
    name_series = stringr::str_replace_all(sic_unofficial, "-", "_"),
    name_series = stringr::str_remove(name_series, "_all_businesses")
  ) |>
  tidyr::pivot_wider(
    id_cols = "date",
    names_from = "name_series",
    values_from = "v4_1"
  )

## Create final datasets ------------------------------------------------------

# Household goods stores retail sales index
retail_households <- subretail_series |>
  dplyr::select(date, household_goods_stores) |>
  dplyr::filter(!is.na(household_goods_stores)) |>
  # Add metadata columns for consistency with other package datasets
  mutate(
    name = "Retail Sales - Household Goods Stores",
    frequency = "M",
    source = "ONS"
  )

# Automotive fuel retail sales index
retail_autofuel <- subretail_series |>
  dplyr::select(date, automotive_fuel) |>
  dplyr::filter(!is.na(automotive_fuel)) |>
  # Add metadata columns for consistency with other package datasets
  mutate(
    name = "Retail Sales - Automotive Fuel",
    frequency = "M",
    source = "ONS"
  )

## Save datasets --------------------------------------------------------------

# Save to package data
usethis::use_data(retail_households, overwrite = TRUE)
usethis::use_data(retail_autofuel, overwrite = TRUE)
