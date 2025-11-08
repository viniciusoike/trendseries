library(hexSticker)
library(ggplot2)
library(trendseries)
library(dplyr)

elec_trend <- electric |>
  filter(date >= as.Date("2014-01-01"), date <= as.Date("2020-01-01")) |>
  mutate(consumption = log(consumption)) |>
  augment_trends(value_col = "consumption", methods = "stl")

ekioplot::ekio_colors()

p <- ggplot(elec_trend, aes(x = date)) +
  geom_line(aes(y = consumption), color = "#ffffff", alpha = 0.5, lwd = 0.6) +
  # geom_point(
  #   aes(y = electric),
  #   fill = "#2c3e50",
  #   color = "white",
  #   alpha = 0.4,
  #   size = 0.2,
  #   shape = 21
  # ) +
  geom_line(aes(y = trend_stl), color = "gray90", lwd = 0.9) +
  theme_minimal() +
  theme_transparent() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
sysfonts::font_add("Avenitr", "Avenir.ttc")
font_add_google("Roboto Slab", "Roboto Slab")
font_add_google("EB Garamond", "EB Garamond")
## Automatically use showtext to render text for future devices
showtext_auto()

sticker(
  p,
  package = "trendseries",
  p_size = 22,
  s_x = 1,
  s_y = 1.15,
  p_x = 1,
  p_y = 0.65,
  s_width = 2,
  s_height = 1,
  p_family = "Roboto Slab",
  h_fill = "#2c3e50",
  h_color = "#ffffff",
  p_color = "#ffffff",
  white_around_sticker = FALSE,
  filename = "inst/figures/sticker.png"
)
