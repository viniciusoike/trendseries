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
# https://www.ons.gov.uk/businessindustryandtrade/retailindustry/datasets/retailsalesindexreferencetables
retail <- onsr::ons_get("retail-sales-index-large-and-small-businesses")

## Data processing ------------------------------------------------------------

retail_volume <- retail |>
  janitor::clean_names() |>
  filter(
    type_of_prices == "chained-volume-of-retail-sales",
    str_detect(sic_unofficial, "-all-businesses$"),
    # Filter for monthly data format (YYYY - MMM)
    str_detect(time, "[0-9]{4} - [A-z]{3}")
  ) |>
  mutate(date = readr::parse_date(time, "%Y - %b"), .before = 1) |>
  arrange(date)

sectors <- c(
  "alcoholic-drinks-other-beverages-and-tobacco",
  "all-retailing-excluding-automotive-fuel",
  "all-retailing-including-automotive-fuel",
  "books-newspapers-and-periodicals",
  "computers-and-telecomms-equipment",
  "clothing",
  "electrical-household-appliances",
  "pharmaceutical-medical-cosmetic-and-toilet-goods",
  "household-goods-stores"
)

retail_long <- retail_volume |>
  mutate(name_series = str_remove(sic_unofficial, "-all-businesses$")) |>
  filter(name_series %in% sectors) |>
  select(date, name_series, value = v4_1)

# Clean column names and filter for relevant series
subretail <- retail_volume |>
  janitor::clean_names() |>
  filter(
    # Select automotive fuel categories
    sic_unofficial %in%
      c("automotive-fuel-all-businesses")
  )

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
# retail_households <- subretail_series |>
#   dplyr::select(date, value = household_goods_stores) |>
#   dplyr::filter(!is.na(value)) |>
#   # Add metadata columns for consistency with other package datasets
#   mutate(
#     name = "Retail Sales - Household Goods Stores",
#     frequency = "M",
#     source = "ONS"
#   )

# Automotive fuel retail sales index
retail_autofuel <- subretail_series |>
  dplyr::select(date, value = automotive_fuel) |>
  dplyr::filter(!is.na(value)) |>
  # Add metadata columns for consistency with other package datasets
  mutate(
    name = "Retail Sales - Automotive Fuel",
    frequency = "M",
    source = "ONS"
  )

retail_volume <- retail_long

## Save datasets --------------------------------------------------------------

# Save to package data
usethis::use_data(retail_volume, overwrite = TRUE)
# usethis::use_data(retail_households, overwrite = TRUE)
usethis::use_data(retail_autofuel, overwrite = TRUE)
