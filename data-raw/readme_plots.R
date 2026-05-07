library(trendseries)
library(ggplot2)

main_color <- ekioplot::ekio_blue[["700"]]

data(gdp_construction)
# Computes multiple trends at once
series <- gdp_construction |>
  # Automatically detects frequency
  # Trends are added as new columns to the original dataset
  augment_trends(
    value_col = "index",
    methods = c("hp", "stl", "ma")
  )

theme_series <- theme_minimal(base_family = "Avenir", base_size = 10) +
  theme_sub_plot(
    margin = margin(10, 10, 10, 10),
    title = element_text(family = "Lora", size = 14),
    subtitle = element_text(color = "gray60")
  ) +
  theme_sub_panel(
    grid.minor = element_blank()
  ) +
  theme_sub_axis_x(
    line = element_line(color = "gray20", linewidth = 0.5),
    ticks = element_line(linewidth = 0.35)
  )

p_series <- ggplot(series, aes(date)) +
  geom_line(aes(y = index), color = main_color, lwd = 0.6, alpha = 0.6) +
  geom_line(aes(y = trend_hp), color = main_color, lwd = 1) +
  labs(
    title = "Construction Activity Index",
    subtitle = "Trend extraction using HP Filter",
    x = NULL,
    y = "Index (1995 = 100)"
  ) +
  theme_series

ggsave(
  "man/figures/example_trendseries.png",
  p_series,
  width = 6,
  height = 6 / 1.618
)


png("man/figures/example_plot.png")
stl_trend <- extract_trends(AirPassengers, methods = "stl")
plot.ts(AirPassengers)
lines(stl_trend, col = "#C53030")
dev.off()
