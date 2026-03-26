library(readr)
library(dplyr)
library(lubridate)
library(bizdays)
library(RQuantLib)

load_quantlib_calendars(from = "2019-01-01", to = "2025-12-31")
# url: https://tfl.gov.uk/corporate/publications-and-reports/network-demand-data

build_url_year <- function(year = 2019) {
  valid_years <- 2019:2025
  stopifnot(year %in% valid_years)

  if (year >= 2024) {
    year <- "2024_2025%20"
  } else {
    year <- as.character(year)
  }

  url <- stringr::str_glue(
    "https://crowding.data.tfl.gov.uk/Network%20Demand/Journeys_{year}.csv"
  )

  return(url)
}

import_csv <- function(url) {
  readr::read_csv(url, show_col_types = FALSE)
}

clean_csv <- function(dat) {
  clean_dat <- dat |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = readr::parse_date(as.character(travel_date), format = "%Y%m%d"),
      day_of_week = lubridate::wday(date, label = TRUE),
      is_weekend = dplyr::if_else(day_of_week %in% c("Sat", "Sun"), 1L, 0L)
    )

  clean_dat <- clean_dat |>
    dplyr::select(-travel_date) |>
    tidyr::pivot_longer(
      cols = c(tube_journey_count, bus_journey_count),
      names_to = "transit_mode",
      values_to = "journey_count"
    )

  sel_cols <- c(
    "date",
    "day_of_week",
    "is_weekend",
    "transit_mode",
    "journey_count"
  )

  clean_dat <- clean_dat |>
    dplyr::mutate(
      transit_mode = stringr::str_remove(transit_mode, "_journey.+")
    ) |>
    dplyr::select(dplyr::all_of(sel_cols))
}

get_demand_data <- function(year = 2019) {
  url <- build_url_year(year)
  dat <- import_csv(url)
  clean_dat <- clean_csv(dat)
  return(clean_dat)
}

safe_demand_data <- purrr::safely(get_demand_data)

years <- 2019:2025

transit <- lapply(years, safe_demand_data)

any(!sapply(transit, \(x) is.null(x$error)))

transit <- dplyr::bind_rows(lapply(transit, \(x) x$result))
transit <- distinct(transit, .keep_all = TRUE)

transit_month <- transit |>
  dplyr::mutate(
    date_month = lubridate::floor_date(date, unit = "month")
  ) |>
  dplyr::summarise(
    journey_monthly = sum(journey_count, na.rm = TRUE),
    .by = c("date_month", "transit_mode")
  ) |>
  arrange(transit_mode, date_month)

london_calendar <- seq(
  from = lubridate::make_date(lubridate::year(min(transit$date)), 1, 1),
  to = lubridate::make_date(lubridate::year(max(transit$date)), 12, 31),
  by = "1 day"
)

dim_calendar <- tibble::tibble(
  date = london_calendar,
  is_business_day = as.integer(is.bizday(date, "QuantLib/UnitedKingdom"))
)

transit_month_avg <- transit |>
  dplyr::left_join(dim_calendar, by = dplyr::join_by(date)) |>
  dplyr::mutate(date_month = lubridate::floor_date(date, unit = "month")) |>
  dplyr::summarise(
    avg_daily_journeys = mean(journey_count, na.rm = TRUE),
    .by = c("date_month", "transit_mode", "is_business_day")
  )

transit_london_avgs <- transit_month_avg
transit_london_monthly <- transit_month

usethis::use_data(transit_london_avgs, overwrite = TRUE)
usethis::use_data(transit_london_monthly, overwrite = TRUE)

library(ggplot2)
library(dplyr)

load_all()

transit_trends <- augment_trends(
  transit_month_avg,
  value_col = "avg_daily_journeys",
  date_col = "date_month",
  group_col = c("is_business_day", "transit_mode"),
  params = list(s.window = 13, robust = TRUE)
) |>
  mutate(
    transit_mode = stringr::str_to_title(transit_mode)
  )

transit_labels <- transit_trends |>
  dplyr::filter(date_month == max(date_month)) |>
  mutate(
    label_num = scales::number(
      avg_daily_journeys,
      scale = 1e-6,
      suffix = "M",
      accuracy = 0.1,
      decimal.mark = ","
    )
  )


ggplot(
  transit_trends,
  aes(date_month, color = as.factor(is_business_day))
) +
  geom_line(aes(y = avg_daily_journeys), lwd = 0.5, alpha = 0.4) +
  geom_point(aes(y = avg_daily_journeys), alpha = 0.4, show.legend = FALSE) +
  geom_line(aes(y = trend_stl), lwd = 0.9) +
  geom_rect(
    data = tibble(
      xmin = as.Date("2020-03-19"),
      xmax = as.Date("2023-03-01"),
      ymin = 0,
      ymax = Inf
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_label(
    data = transit_labels,
    aes(date_month %m+% months(4), avg_daily_journeys, label = label_num),
    family = "Georgia",
    size = 3,
    hjust = 1,
    show.legend = FALSE
  ) +
  facet_wrap(vars(transit_mode), ncol = 1) +
  scale_x_date(
    breaks = seq(as.Date("2019-01-01"), as.Date("2026-01-01"), by = "year"),
    labels = c("2020", "20", "21", "22", "23", "24", "25", "26"),
    expand = expansion(c(0.01, 0.05))
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-6, suffix = "M"),
    expand = expansion(c(0.005, 0.05))
  ) +
  scale_color_manual(
    values = MetBrewer::met.brewer("VanGogh2", 2),
    labels = c("Weekends/Holidays", "Business Days")
  ) +
  labs(
    title = "Riding with transit",
    subtitle = "Daily monthly ridership averages across London's transit systems",
    x = NULL,
    y = "Journeys (million)",
    caption = "Source: TFL (2019-2025)"
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    legend.title = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = "gray20", linewidth = 0.5),
    axis.ticks.x = element_line(color = "gray20", linewidth = 0.5),
    strip.text = element_text(hjust = 0, size = 12, face = "bold"),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(
      size = 12,
      color = "gray40",
      margin = margin(b = 8)
    ),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
    axis.text = element_text(size = 10)
  )
