# BCB Series Data Preparation
# Modern R workflow following claude/coding_guidelines.md and CLAUDE.md

library(dplyr)
library(cli)

# Modern helper function with improved error handling
get_bcb_series <- function(code, series_name, description, frequency) {
  cli_inform("Fetching BCB series {code}: {series_name}")

  tryCatch(
    {
      # Check if rbcb is available
      if (!requireNamespace("rbcb", quietly = TRUE)) {
        cli_abort("Package {.pkg rbcb} is required but not available")
      }

      # Fetch all available observations
      raw_data <- rbcb::get_series(code, as = "tibble")

      if (nrow(raw_data) == 0) {
        cli_warn("No data returned for series {code}")
        return(NULL)
      }

      # Create simple tibble with standardized column names
      clean_data <- raw_data |>
        select(date, value = 2) |>
        filter(!is.na(value))

      # Set series name as column name
      names(clean_data)[2] <- series_name

      cli_inform("Successfully fetched {nrow(clean_data)} observations for {series_name}")
      return(clean_data)
    },
    error = function(e) {
      cli_abort("Error fetching BCB series {code}: {e$message}")
    }
  )
}

# =============================================================================
# FETCH BCB SERIES
# =============================================================================

cli_h1("Fetching BCB Economic Series")

# Define series to fetch
series_definitions <- list(
  list(
    code = 24363,
    name = "ibcbr",
    description = "Central Bank Economic Activity Index",
    frequency = "M"
  ),
  list(
    code = 1403,
    name = "electric",
    description = "Electric consumption residential",
    frequency = "M"
  ),
  list(
    code = 1378,
    name = "vehicles",
    description = "Vehicle production",
    frequency = "M"
  ),
  list(
    code = 22087,
    name = "gdp_construction",
    description = "GDP - Construction - Index (Base: average 1995 = 100)",
    frequency = "Q"
  ),
  list(
    code = 1391,
    name = "oil_derivatives",
    description = "Oil derivatives production",
    frequency = "M"
  )
)

# Fetch each series using modern purrr approach
series_data <- purrr::map(series_definitions, ~ {
  get_bcb_series(
    code = .x$code,
    series_name = .x$name,
    description = .x$description,
    frequency = .x$frequency
  )
})

# Extract individual series (remove NULL entries)
series_data <- purrr::compact(series_data)
names(series_data) <- purrr::map_chr(series_definitions[seq_along(series_data)], "name")

# Assign to individual objects
list2env(series_data, envir = .GlobalEnv)

# =============================================================================
# CREATE METADATA TIBBLE
# =============================================================================

cli_h2("Creating Series Metadata")

# Create metadata for successfully fetched series
series_metadata <- purrr::map_dfr(series_definitions, ~ {
  series_name <- .x$name

  # Only include metadata for successfully fetched series
  if (exists(series_name) && !is.null(get(series_name))) {
    data <- get(series_name)

    tibble(
      series_name = series_name,
      description = .x$description,
      frequency = .x$frequency,
      first_obs = min(data$date, na.rm = TRUE),
      last_obs = max(data$date, na.rm = TRUE),
      source = glue::glue("BCB-SGS {.x$code}")
    )
  }
})

# =============================================================================
# SUMMARY
# =============================================================================

cli_h2("Summary")

if (nrow(series_metadata) > 0) {
  cli_inform("Successfully prepared {nrow(series_metadata)} BCB series:")

  purrr::walk(seq_len(nrow(series_metadata)), ~ {
    row <- series_metadata[.x, ]
    obs_count <- nrow(get(row$series_name))
    cli_inform("- {row$series_name}: {obs_count} observations ({row$first_obs} to {row$last_obs})")
  })
} else {
  cli_warn("No series were successfully fetched")
}

cli_inform("BCB series preparation complete!")