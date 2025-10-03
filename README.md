# trendseries <img src="man/figures/logo.png" align="right" height="139" alt="" />

[![R-CMD-check](https://github.com/viniciusoike/trendseries/workflows/R-CMD-check/badge.svg)](https://github.com/viniciusoike/trendseries/actions)

**Extract trends from monthly and quarterly economic time series using established econometric methods.**

`trendseries` provides a modern, pipe-friendly interface for exploratory analysis of economic data. The package offers smart defaults optimized for business cycle analysis and comparative trend extraction.

## Two Main Functions

- **`augment_trends()`**: Add trend columns to tibbles/data.frames (pipe-friendly)
- **`extract_trends()`**: Extract trends from ts/xts/zoo objects (time series focused)

## Key Features

- **Optimized for monthly and quarterly economic data**, with smart defaults for business cycle analysis
- Flexible methods like STL and moving averages also support daily and other frequencies
- Multiple methods: HP filter, Baxter-King, Christiano-Fitzgerald, moving averages, STL, exponential smoothing, Kalman filter, and 20+ more
- Smart defaults: λ=1600 (quarterly), λ=14400 (monthly) for HP filter
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
| `hp` | Hodrick-Prescott filter (use `params = list(hp_onesided = TRUE)` for real-time) |
| `bk` | Baxter-King bandpass filter |
| `cf` | Christiano-Fitzgerald filter |
| `ma` | Simple moving average |
| `wma` | Weighted moving average |
| `ewma` | Exponential weighted moving average |
| `zlema` | Zero-lag exponential moving average |
| `triangular` | Triangular moving average |
| `median` | Median filter |
| `gaussian` | Gaussian-weighted moving average |
| `exp_simple` | Simple exponential smoothing |
| `exp_double` | Double exponential smoothing (Holt) |
| `stl` | Seasonal-trend decomposition |
| `kalman` | Kalman filter/smoother |
| `sg` | Savitzky-Golay filter |
| `butter` | Butterworth filter |
| `loess` | Local polynomial regression |
| `spline` | Smoothing splines |
| `poly` | Polynomial trends |
| `hamilton` | Hamilton regression filter |
| `bn` | Beveridge-Nelson decomposition |
| `ucm` | Unobserved components model |
| `kernel` | Kernel smoother |

## Included Data

Economic indicators for testing and examples:

### Brazilian Data (BCB)
- `gdp_construction`: Quarterly construction GDP index (1995-2023, seasonally adjusted)
- `ibcbr`: Central Bank Economic Activity Index (monthly)
- `vehicles`: Monthly vehicle production (thousands of units)
- `oil_derivatives`: Monthly oil derivatives production
- `electric`: Monthly residential electric consumption (GWh)

### UK Retail Sales (ONS)
- `retail_households`: Monthly household goods stores retail sales index
- `retail_autofuel`: Monthly automotive fuel retail sales index

### Coffee Prices (CEPEA)
- `coffee_arabica`: Daily arabica coffee prices with inflation adjustment
- `coffee_robusta`: Daily robusta coffee prices with inflation adjustment

### Metadata
- `series_metadata`: Metadata for all BCB series

## Learn More

See the [Getting Started vignette](https://viniciusoike.github.io/trendseries/articles/getting-started.html) for detailed examples and usage patterns.

## License

MIT + file LICENSE
