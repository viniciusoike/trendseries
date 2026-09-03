# Update these datasets
metadata_description <- tibble::tribble(
  ~series_name             , ~description                                                             , ~frequency , ~source                  ,
  "ibcbr"                  , "Central Bank Economic Activity Index (IBC-Br)"                          , "M"        , "BCB/Depec via BCB-SGS"  ,
  "electric"               , "Electric energy consumption - Brazil - residential"                     , "M"        , "Eletrobras via BCB-SGS" ,
  "vehicles"               , "Vehicle sales (total)"                                                  , "M"        , "Anfavea via BCB-SGS"    ,
  "gdp_construction"       , "Quarterly GDP - observed data - Building industry"                      , "Q"        , "IBGE via BCB-SGS"       ,
  "oil_derivatives"        , "Petroleum derivatives production - Total"                               , "M"        , "ANP via BCB-SGS"        ,
  "electricity"            , "Electric energy consumption - Brazil - by sector"                       , "M"        , "Eletrobras via BCB-SGS" ,
  "coffee_arabica"         , "Daily CEPEA/ESALQ Arabica price indicator with inflation-adjusted USD." , "D"        , "CEPEA/ESALQ/USP"        ,
  "coffee_robusta"         , "Daily CEPEA/ESALQ Robusta price indicator with inflation-adjusted USD." , "D"        , "CEPEA/ESALQ/USP"        ,
  "retail_autofuel"        , "Retail Sales - Automotive Fuel"                                         , "M"        , "ONS"                    ,
  "retail_volume"          , "Retail Sales - Several Subindices"                                      , "M"        , "ONS"                    ,
  "transit_london_avgs"    , "Average daily TfL Bus and Tube journeys by UK business day"             , "M"        , "TfL Open Data"          ,
  "transit_london_monthly" , "Monthly TfL Bus and Tube journey totals"                                , "M"        , "TfL Open Data"
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
