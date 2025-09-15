## code to prepare `DATASET` dataset goes here
source("data-raw/example_series.R")
usethis::use_data(gdp_brazil, gdp_brazil_qtr, ibcbr, brlusd,
                  electric_consumption, real_estate_credit, coffee_prices_cepea,
                  overwrite = TRUE)
