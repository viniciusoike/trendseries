# Update these datasets
metadata_description <- tibble::tribble(
  ~series_name             , ~description                                            , ~frequency , ~source           ,
  "ibcbr"                  , "Brazilian Central Bank Economic Activity Index"        , "M"        , "BCB-SGS"         ,
  "electric"               , "Electric consumption (residential)"                    , "M"        , "BCB-SGS"         ,
  "vehicles"               , "Vehicle production"                                    , "M"        , "Anfavea"         ,
  "gdp_construction"       , "GDP - Construction - Index (Base: average 1995 = 100)" , "Q"        , "BCB-SGS"         ,
  "oil_derivatives"        , "Oil derivatives production"                            , "M"        , "ANP"             ,
  "electricity"            , "Electricity consumption (res., comm., indus.)"         , "M"        , "BCB-SGS"         ,
  "coffee_arabica"         , "Daily Arabica coffee price with inflation adjustment." , "D"        , "CEPEA/ESALQ/USP" ,
  "coffee_robusta"         , "Daily Robusta coffee price with inflation adjustment." , "D"        , "CEPEA/ESALQ/USP" ,
  "retail_autofuel"        , "Retail Sales - Automotive Fuel"                        , "M"        , "ONS"             ,
  "retail_volume"          , "Retail Sales - Several Subindices"                     , "M"        , "ONS"             ,
  "transit_london_avgs"    , "Riding with transit"                                   , "M"        , "TFL"             ,
  "transit_london_monthly" , "Riding with transit"                                   , "M"        , "TFL"
)

metadata_series <- tibble::tribble(
  ~series_name             , ~date_col    , ~value_col                                , ~group_cols                                ,
  "ibcbr"                  , "date"       , "index"                                   , NA_character_                              ,
  "electric"               , "date"       , "consumption"                             , NA_character_                              ,
  "vehicles"               , "date"       , "production"                              , NA_character_                              ,
  "gdp_construction"       , "date"       , "index"                                   , NA_character_                              ,
  "oil_derivatives"        , "date"       , "production"                              , NA_character_                              ,
  "electricity"            , "date"       , "value"                                   , "name_series"                              ,
  "coffee_arabica"         , "date"       , list(c("spot_rs", "spot_us", "usd_2022")) , NA_character_                              ,
  "coffee_robusta"         , "date"       , list(c("spot_rs", "spot_us", "usd_2022")) , NA_character_                              ,
  "retail_autofuel"        , "date"       , "value"                                   , NA_character_                              ,
  "retail_volume"          , "date"       , "value"                                   , "name_series"                              ,
  "transit_london_avgs"    , "date_month" , "avg_daily_journeys"                      , list(c("transit_mode", "is_business_day")) ,
  "transit_london_monthly" , "date_month" , "journey_monthly"                         , "transit_mode"                             ,
)

.get_date_range <- function(dat, date_col) {
  get(dat, envir = .GlobalEnv) |>
    dplyr::summarise(
      date_min = min(.data[[date_col]], na.rm = TRUE),
      date_max = max(.data[[date_col]], na.rm = TRUE)
    ) |>
    dplyr::mutate(series_name = !!dat)
}

metadata_dates <- purrr::map2(
  metadata_series$series_name,
  metadata_series$date_col,
  .get_date_range
)

metadata_dates <- dplyr::bind_rows(metadata_dates)

metadata_series <- metadata_series |>
  dplyr::left_join(metadata_dates, by = "series_name") |>
  dplyr::left_join(metadata_description, by = "series_name") |>
  dplyr::arrange(series_name)


readr::write_rds(metadata_series, "data-raw/metadata_series.rds")
