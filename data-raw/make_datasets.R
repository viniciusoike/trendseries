# Prepare all package datasets
# Following modern R development guidelines from claude/coding_guidelines.md

update <- TRUE
metadata <- TRUE

options(readr.show_col_types = FALSE)

if (update) {
  # Source all data preparation scripts
  source("data-raw/bcb_series.R")
  source("data-raw/coffee.R")
  source("data-raw/uk_data.R")
  source("data-raw/london_underground.R")
} else {
  bcb_series <- readr::read_rds("data-raw/bcb_series.rds")
  coffee_arabica <- readr::read_csv("data-raw/coffee_arabica.csv")
  coffee_robusta <- readr::read_csv("data-raw/coffee_robusta.csv")
  retail_volume <- readr::read_csv("data-raw/retail_volume.csv")
  retail_autofuel <- readr::read_csv("data-raw/retail_autofuel.csv")
  transit_london_avgs <- readr::read_csv("data-raw/transit_london_avgs.csv")
  transit_london_monthly <- readr::read_csv(
    "data-raw/transit_london_monthly.csv"
  )
  list2env(bcb_series, envir = .GlobalEnv)
}

if (metadata) {
  source("data-raw/build_metadata.R")
} else {
  metadata_series <- readr::read_rds("data-raw/metadata_series.rds")
}

# Save all datasets using usethis
usethis::use_data(
  # BCB economic series (new simplified format)
  ibcbr,
  electric,
  vehicles,
  gdp_construction,
  oil_derivatives,
  electricity,

  # Coffee price data
  coffee_arabica,
  coffee_robusta,

  # UK retail data
  retail_volume,
  retail_autofuel,

  # UK transit data
  transit_london_avgs,
  transit_london_monthly,

  # metadata
  metadata_series,

  overwrite = TRUE
)
