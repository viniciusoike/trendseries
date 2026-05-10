# BCB Series Data Preparation

library(dplyr)
import::from(rbcb, get_series)

.rename_to_value <- function(df) dplyr::rename(df, value = consumption)

.get_bcb_series <- function(code, series_name, value_col) {
  req <- code
  names(req) <- series_name
  series <- rbcb::get_series(req, end_date = "2025-12-31", as = "tibble")
  dplyr::rename(series, !!value_col := dplyr::all_of(series_name))
}

series_params <- tibble::tribble(
  ~code , ~series_name          , ~value_col    ,
  24363 , "ibcbr"               , "index"       ,
   1403 , "electric"            , "consumption" ,
   1402 , "electric_commercial" , "consumption" ,
   1404 , "electric_industrial" , "consumption" ,
   1378 , "vehicles"            , "production"  ,
  22087 , "gdp_construction"    , "index"       ,
   1391 , "oil_derivatives"     , "production"
)

build_bcb_series <- function(params) {
  series <- purrr::pmap(
    params[, c("code", "series_name", "value_col")],
    .get_bcb_series
  )
  series <- rlang::set_names(series, params$series_name)

  individual_names <- c(
    "ibcbr",
    "electric",
    "vehicles",
    "gdp_construction",
    "oil_derivatives"
  )

  electric_names <- c("electric", "electric_commercial", "electric_industrial")
  electricity <- dplyr::bind_rows(
    purrr::map(series[electric_names], .rename_to_value),
    .id = "name_series"
  ) |>
    dplyr::mutate(
      name_series = gsub("^electric$", "electric_residential", name_series)
    ) |>
    dplyr::select(date, name_series, value)

  return(append(series[individual_names], list(electricity = electricity)))
}

series_data <- build_bcb_series(series_params)
readr::write_rds(series_data, "data-raw/bcb_series.rds")
list2env(series_data, envir = .GlobalEnv)
