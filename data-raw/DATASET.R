# Prepare all package datasets
# Following modern R development guidelines from claude/coding_guidelines.md

# Source all data preparation scripts
source("data-raw/bcb_series.R")
source("data-raw/coffee.R")
source("data-raw/uk_data.R")

# Save all datasets using usethis
usethis::use_data(
  # BCB economic series (new simplified format)
  ibcbr,
  electric,
  vehicles,
  gdp_construction,
  oil_derivatives,
  series_metadata,

  # Coffee price data
  coffee_arabica,
  coffee_robusta,

  # UK retail data
  retail_households,
  retail_autofuel,

  overwrite = TRUE
)
