# Build the trendseries document carousel ----------------------------------
# Run from the repository root: Rscript promo/linkedin/build.R

here::i_am("promo/linkedin/build.R")
pkgload::load_all(here::here())
pkgload::load_all(file.path(dirname(here::here()), "ekioplot"))
library(ggplot2)

# Shared presentation ------------------------------------------------------

output_dir <- here::here("promo", "linkedin")
plot_dir <- file.path(output_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
version <- unname(read.dcf(here::here("DESCRIPTION"), fields = "Version")[1, 1])
navy <- "#1E3A5F"
teal <- "#00605E"
sector_labels <- c(
  electric_residential = "Residential",
  electric_commercial = "Commercial",
  electric_industrial = "Industrial"
)
chart_theme <- theme_ekio(base_size = 19, background = "white") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17),
    axis.text = element_text(size = 17),
    strip.background = element_rect(fill = "#E9EEEC", colour = NA),
    strip.text = element_text(colour = navy, size = 18, face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 18),
    plot.margin = margin(8, 14, 8, 8)
  )

five_years <- as.Date(paste0(seq(2005, 2025, by = 5), "-01-01"))

# Every displayed example is evaluated verbatim. The resulting object is the
# chart's complete data source; only plotting and labels live outside the code.
example_data <- function(code) {
  return(eval(parse(text = code), envir = new.env(parent = globalenv())))
}

save_chart <- function(plot, name, height = 6.2) {
  path <- file.path(plot_dir, paste0(name, ".png"))
  ggsave(
    path,
    plot + chart_theme,
    width = 9.5,
    height = height,
    dpi = 120,
    bg = "white"
  )
  return(path)
}

# Rebase grouped series ----------------------------------------------------

index_code <- '# rebase each series to its 2019 average
electricity |>
  dplyr::filter(date >= as.Date("2003-01-01")) |>
  index_series(
    group_cols = "name_series",
    base_period = 2019
  )'
indexed <- example_data(index_code)
index_plot <- ggplot(indexed, aes(date, index_value)) +
  geom_hline(yintercept = 100, colour = "#8796A5", linewidth = 0.5) +
  geom_line(colour = navy, linewidth = 0.8) +
  facet_wrap(
    vars(name_series),
    ncol = 1,
    labeller = labeller(name_series = sector_labels)
  ) +
  scale_x_date(breaks = five_years, date_labels = "%Y") +
  scale_y_continuous(breaks = c(60, 100, 140)) +
  labs(y = "2019 average = 100")

# Estimate trends for many series at once -------------------------------------------------

trend_code <- '# estimate an STL trend for each series
electricity |>
  dplyr::filter(date >= as.Date("2003-01-01")) |>
  augment_trends(
    group_cols = "name_series", methods = "stl"
  )'
trended <- example_data(trend_code)
trend_plot <- ggplot(trended, aes(date)) +
  geom_line(aes(y = value, colour = "Observed"), linewidth = 0.5) +
  geom_line(aes(y = trend_stl, colour = "STL trend"), linewidth = 1) +
  facet_wrap(
    vars(name_series),
    ncol = 1,
    scales = "free_y",
    labeller = labeller(name_series = sector_labels)
  ) +
  scale_colour_manual(values = c(Observed = "#829D9C", "STL trend" = navy)) +
  scale_x_date(breaks = five_years, date_labels = "%Y") +
  scale_y_continuous(
    breaks = scales::breaks_extended(n = 3),
    labels = scales::label_number(scale = 0.001, suffix = "k")
  ) +
  labs(y = "Electricity (GWh)", colour = NULL)

# Remove seasonality and trend --------------------------------------------

cycle_code <- '# remove seasonality, then the trend (in logs)
gdp_construction |>
  dplyr::filter(date >= as.Date("2003-01-01")) |>
  deseason_series(value_col = "index") |>
  detrend_series(
    value_col = "seasadj_stl", transform = "log"
  )'
cycled <- example_data(cycle_code)
cycle_plot <- ggplot(cycled, aes(date, detrend_hp)) +
  geom_hline(yintercept = 0, colour = "#8796A5", linewidth = 0.5) +
  geom_line(colour = navy, linewidth = 0.8) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_date(breaks = five_years, date_labels = "%Y") +
  labs(y = "Construction GDP: log deviation")

# Preserve the quarterly tsibble ------------------------------------------

quarter_code <- '# estimate trends in a quarterly tsibble
gdp_construction |>
  dplyr::mutate(date = tsibble::yearquarter(date)) |>
  tsibble::as_tsibble(index = date) |>
  augment_trends(value_col = "index", methods = "hp") |>
  dplyr::slice_tail(n = 3)'
quarterly <- example_data(quarter_code)
quarter_output <- paste(
  capture.output(print(quarterly, width = 58)),
  collapse = "\n"
)
stopifnot(
  inherits(quarterly, "tbl_ts"),
  inherits(quarterly$date, "yearquarter")
)

# Calculate rolling change -------------------------------------------------

change_code <- '# compare each month with a year earlier
ibcbr |>
  augment_rolling(
    value_col = "index", stats = "change",
    window = 12, percent = TRUE
  )'
changed <- example_data(change_code)
change_plot <- ggplot(changed, aes(date, roll_change_12)) +
  geom_hline(yintercept = 0, colour = "#8796A5", linewidth = 0.5) +
  geom_line(colour = teal, linewidth = 0.9, na.rm = TRUE) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  scale_x_date(breaks = five_years, date_labels = "%Y") +
  labs(y = "IBC-Br: change over 12 months")

# Accumulate vehicle sales -------------------------------------------------

# Select complete calendar years so every January remains visible.
# The compatibility column is explicitly renamed on the card.
ytd_code <- '# sum sales from January to each month
vehicles |>
  dplyr::rename(sales = production) |>
  dplyr::filter(date >= as.Date("2023-01-01")) |>
  augment_rolling(
    value_col = "sales",
    stats = "sum", window = "ytd"
  )'
ytd <- example_data(ytd_code)
ytd_plot <- ggplot(ytd, aes(date, roll_sum_ytd, group = format(date, "%Y"))) +
  geom_line(colour = navy, linewidth = 1) +
  geom_point(
    data = ytd[format(ytd$date, "%m") == "01", ],
    colour = teal,
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-6)
  ) +
  scale_x_date(
    breaks = ytd$date[format(ytd$date, "%m") == "01"],
    date_labels = "Jan %Y"
  ) +
  labs(y = "Sales (million units)")
ytd_note <- sprintf(
  "%s to %s. Dots mark the January resets.",
  format(min(ytd$date), "%Y"),
  format(max(ytd$date), "%Y")
)

# Assemble and export ------------------------------------------------------

cards <- list(
  promo_cover(
    "Explore trends and cycles in R",
    "Trends, cycles and rolling summaries in tidy tables."
  ),
  promo_feature(
    "Rebase many series to a common index",
    chart = save_chart(index_plot, "index", 6.8),
    alt = "Three sector indices with a reference line at 100.",
    code = index_code
  ),
  promo_feature(
    "Estimate trends for many series at once",
    chart = save_chart(trend_plot, "trends", 6.8),
    alt = "Observed electricity in GWh and STL trends for three sectors.",
    code = trend_code
  ),
  promo_feature(
    "Separate trend and cycle in one pipe",
    chart = save_chart(cycle_plot, "cycle"),
    alt = "Construction GDP log deviation from its HP trend after STL seasonal adjustment.",
    code = cycle_code,
    note = "Log deviations approximate percentage deviations from trend."
  ),
  promo_feature(
    "Use your tsibbles as they are",
    badge = "New in 1.7.0",
    code = quarter_code,
    output = quarter_output,
    note = "Construction GDP. The returned object keeps its tsibble class."
  ),
  promo_feature(
    "Compute year-over-year change",
    chart = save_chart(change_plot, "change"),
    alt = "IBC-Br percentage change over 12 months.",
    badge = "New in 1.7.0",
    code = change_code
  ),
  promo_feature(
    "Accumulate values year to date",
    chart = save_chart(ytd_plot, "ytd"),
    alt = paste("Year-to-date vehicle sales.", ytd_note),
    code = ytd_code,
    note = "Dots mark the January resets."
  ),
  promo_closing(
    "Try it in R",
    "Start with the bundled datasets.\nFind methods and examples in the docs."
  )
)
brand <- promo_brand(
  "trendseries",
  version,
  logo = file.path(output_dir, "assets", "trendseries-hexlogo.png"),
  docs_url = "https://viniciusoike.github.io/trendseries/"
)
artifacts <- render_promo(
  cards,
  brand,
  html = file.path(output_dir, "index.html"),
  pdf = file.path(output_dir, "trendseries-carousel.pdf"),
  png_dir = file.path(output_dir, "pages")
)
