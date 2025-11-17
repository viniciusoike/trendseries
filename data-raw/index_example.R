library(dplyr)
library(ggplot2)
library(trendseries)

retail_trends <- retail_volume |>
  dplyr::filter(date >= as.Date("2018-01-01")) |>
  augment_trends(
    methods = "stl",
    group_vars = "name_series"
  )

p <- ggplot(retail_trends, aes(x = date)) +
  geom_line(aes(y = value), alpha = 0.5) +
  geom_line(aes(y = trend_stl), lwd = 0.8) +
  facet_wrap(vars(name_series), scales = "free_y") +
  labs(
    x = NULL,
    y = "Index (2023 = 100)",
    title = "Retail Volume Index",
    subtitle = "Selected retail volume indices with STL trend."
    ) +
  theme_bw()

ggsave("man/figures/index_example.png", p, width = 10, height = 6)
