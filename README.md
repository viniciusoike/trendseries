# trendseries

[![R-CMD-check](https://github.com/viniciusoike/trendseries/workflows/R-CMD-check/badge.svg)](https://github.com/viniciusoike/trendseries/actions)

**Extract trends from monthly and quarterly economic time series using established econometric methods.**

`trendseries` provides a modern, pipe-friendly interface for exploratory analysis of economic data. The package offers smart defaults optimized for business cycle analysis and comparative trend extraction.

## Two Main Functions

- **`augment_trends()`**: Add trend columns to tibbles/data.frames (pipe-friendly)
- **`extract_trends()`**: Extract trends from ts/xts/zoo objects (time series focused)

## Key Features

- Optimized for monthly and quarterly economic data
- Multiple methods: HP filter, Baxter-King, Christiano-Fitzgerald, moving averages (SMA, WMA, EWMA, ZLEMA), STL, exponential smoothing, Kalman filter, and more
- Smart defaults for business cycle analysis (λ=1600 quarterly, λ=14400 monthly)
- Native pipe `|>`, modern R practices, comprehensive error messages

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("viniciusoike/trendseries")
```

## Quick Start

```r
library(trendseries)

# Pipe-friendly workflow with tibbles
gdp_construction |>
  augment_trends(methods = c("hp", "bk", "ma"))

# Direct time series analysis
ts_data <- df_to_ts(gdp_construction, frequency = 4)
extract_trends(ts_data, methods = "hp")
```

## Available Methods

| Method | Description |
|--------|-------------|
| `hp` | Hodrick-Prescott filter |
| `hp_1s` | One-sided HP filter (real-time analysis) |
| `bk` | Baxter-King bandpass filter |
| `cf` | Christiano-Fitzgerald filter |
| `ma` | Simple moving average |
| `wma` | Weighted moving average |
| `ewma` | Exponential weighted moving average |
| `zlema` | Zero-lag exponential moving average |
| `triangular` | Triangular moving average |
| `median` | Median filter |
| `gaussian` | Gaussian-weighted moving average |
| `ema` | Exponential smoothing (Holt, Holt-Winters) |
| `stl` | Seasonal-trend decomposition |
| `kalman` | Kalman filter/smoother |
| `savgol` | Savitzky-Golay filter |
| `butterworth` | Butterworth filter |
| `loess` | Local polynomial regression |
| `spline` | Smoothing splines |
| `poly` | Polynomial trends |

## Included Data

Brazilian economic indicators for testing and examples:

- `gdp_brazil`: Annual GDP 1962-2022
- `gdp_construction`: Quarterly GDP 1995-2023 (seasonally adjusted)
- `ibcbr`: Central Bank Economic Activity Index (monthly)
- `electric_consumption`: Monthly residential electric consumption 1979-2025
- `real_estate_credit`: Monthly real estate credit outstanding 2011-2025
- `coffee_prices_cepea`: Monthly coffee arabica price index 2010-2023

## Learn More

See the [Getting Started vignette](https://viniciusoike.github.io/trendseries/articles/getting-started.html) for detailed examples and usage patterns.

## License

MIT + file LICENSE
