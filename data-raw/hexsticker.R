library(hexSticker)
library(ggplot2)
library(trendseries)
library(dplyr)

elec_trend <- electric |>
  filter(date >= as.Date("2015-01-01")) |>
  augment_trends(value_col = "electric", methods = "stl")

ekioplot::ekio_colors()

p <- ggplot(elec_trend, aes(x = date)) +
  geom_line(aes(y = electric), color = "#2c3e50", alpha = 0.5, lwd = 0.4) +
  geom_point(
    aes(y = electric),
    fill = "#2c3e50",
    color = "white",
    alpha = 0.4,
    size = 0.2,
    shape = 21
  ) +
  geom_line(aes(y = trend_stl), color = "#2c3e50", lwd = 0.8) +
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
font_add_google("Lato", "Lato")
## Automatically use showtext to render text for future devices
showtext_auto()

sticker(
  p,
  package = "trendseries",
  p_size = 22,
  s_x = 1,
  s_y = 1.15,
  s_width = 2,
  s_height = 2,
  p_family = "Lato",
  filename = "inst/figures/sticker.png"
)
