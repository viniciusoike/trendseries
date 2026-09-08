library(dplyr)
library(ggplot2)
library(hexSticker)
library(showtext)
library(trendseries)

elec_trend <- electric |>
  filter(date >= as.Date("2014-01-01"), date <= as.Date("2020-01-01")) |>
  mutate(consumption = log(consumption)) |>
  augment_trends(value_col = "consumption", methods = "stl")

cream <- "#EFE8DC"
navy <- "#152A44"

p <- ggplot(elec_trend, aes(x = date)) +
  geom_line(aes(y = consumption), color = cream, alpha = 0.5, lwd = 0.6) +
  geom_line(aes(y = trend_stl), color = "gray90", lwd = 0.9) +
  theme_minimal() +
  theme_transparent() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

font_add_google("Lato", "Lato")
showtext_auto()

sticker(
  p,
  package = "trendseries",
  p_size = 22,
  s_x = 1,
  s_y = 1.125,
  p_x = 1,
  p_y = 0.65,
  s_width = 2,
  s_height = 1,
  p_family = "Lato",
  p_fontface = "bold",
  h_fill = navy,
  h_color = cream,
  p_color = cream,
  white_around_sticker = FALSE,
  filename = "man/figures/logo.png"
)
