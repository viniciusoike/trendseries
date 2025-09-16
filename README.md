# trendseries

[![R-CMD-check](https://github.com/viniciusoike/trendseries/workflows/R-CMD-check/badge.svg)](https://github.com/viniciusoike/trendseries/actions)

Extract trends from monthly and quarterly economic time series using established econometric methods.

## Overview

`trendseries` provides a modern, pipe-friendly interface for exploratory time series analysis focused on **monthly and quarterly economic data**. The package offers two main functions designed for different workflows:

- **`augment_trends()`**: Pipe-friendly function that adds trend columns to tibbles/data.frames
- **`extract_trends()`**: Direct time series analysis for ts/xts/zoo objects

## Key Features

- **Economic Focus**: Optimized for monthly (freq=12) and quarterly (freq=4) data
- **Multiple Methods**: HP filter, Baxter-King, Christiano-Fitzgerald, moving averages, STL, loess, splines, polynomial trends
- **Smart Defaults**: Economic-appropriate parameters (λ=1600 for quarterly, λ=14400 for monthly HP filter)
- **Robust Integration**: Works with tsbox for seamless format conversion
- **Modern R**: Native pipe `|>`, cli messaging, tidyverse style

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("viniciusoike/trendseries")
```

## Quick Start

### Pipe-friendly workflow with `augment_trends()`

```r
library(trendseries)

# Single trend method
gdp_construction |>
  augment_trends(methods = "hp")

# Multiple methods with custom parameters
gdp_construction |>
  augment_trends(
    methods = c("hp", "bk", "ma"),
    hp_lambda = 1600,
    ma_window = 8
  )

# Works with grouped data
combined_data |>
  augment_trends(
    group_vars = "series_id",
    methods = c("hp", "stl")
  )
```

### Direct time series analysis with `extract_trends()`

```r
# Convert to time series
ts_data <- df_to_ts(gdp_construction, frequency = 4)

# Extract single trend
hp_trend <- extract_trends(ts_data, methods = "hp")

# Extract multiple trends
all_trends <- extract_trends(
  ts_data,
  methods = c("hp", "bk", "cf", "ma")
)
```

## Available Methods

| Method | Description | Best For |
|--------|-------------|----------|
| `hp` | Hodrick-Prescott filter | Business cycle analysis |
| `bk` | Baxter-King bandpass filter | Business cycle extraction |
| `cf` | Christiano-Fitzgerald filter | Asymmetric business cycles |
| `ma` | Moving averages | Simple trend smoothing |
| `stl` | Seasonal-trend decomposition | Seasonal data |
| `loess` | Local polynomial regression | Flexible smoothing |
| `spline` | Smoothing splines | Smooth trend curves |
| `poly` | Polynomial trends | Linear/quadratic trends |

## Included Data

The package includes Brazilian economic indicators:

- `gdp_brazil`: Annual GDP 1962-2022
- `gdp_construction`: Quarterly GDP 1995-2023 (seasonally adjusted)
- `ibcbr`: Central Bank Economic Activity Index (monthly)
- `brlusd`: Daily USD/BRL exchange rates 1984-2023
- `electric_consumption`: Monthly residential electric consumption 1979-2025
- `real_estate_credit`: Monthly real estate credit outstanding 2011-2025
- `coffee_prices_cepea`: Monthly coffee arabica price index 2010-2023 (CEPEA ESALQ/USP)

## Example: Comparative Trend Analysis

```r
library(trendseries)

# Extract multiple trends for comparison
gdp_trends <- gdp_construction |>
  augment_trends(
    methods = c("hp", "bk", "ma", "stl"),
    .quiet = TRUE
  )

# Analyze electric consumption trends
electric_trends <- electric_consumption |>
  augment_trends(
    methods = c("hp", "ma"),
    hp_lambda = 14400  # monthly data
  )

# Plot results
library(ggplot2)
library(dplyr)

gdp_trends |>
  select(date, value, starts_with("trend_")) |>
  pivot_longer(cols = starts_with("trend_"),
               names_to = "method",
               values_to = "trend") |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = value), alpha = 0.3) +
  geom_line(aes(y = trend, color = method), size = 1) +
  facet_wrap(~method) +
  theme_minimal() +
  labs(title = "GDP Trends by Method")
```

## Dependencies

Core dependencies: `cli`, `hpfilter`, `lubridate`, `mFilter`, `stats`, `tibble`, `tsbox`, `zoo`

Optional: `dplyr` (for enhanced data manipulation)

## Development

This package follows modern R development practices:
- Native pipe `|>`
- `cli` for messaging
- Comprehensive testing with `testthat`
- `roxygen2` documentation

Built as part of Curso de Verão IME (2024).
