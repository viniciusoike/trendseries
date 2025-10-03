---
title: "trendseries: Extract Trends from Economic Time Series"
output: html_document
---

# Extract trends time series

[![R-CMD-check](https://github.com/viniciusoike/trendseries/workflows/R-CMD-check/badge.svg)](https://github.com/viniciusoike/trendseries/actions)

**`trendseries` provides a modern, pipe-friendly interface for exploratory analysis of economic data.** The package simplifies the workflow of trend extraction, especially when working with data frames and grouped time series. It offers smart defaults optimized for business cycle analysis and includes established econometric methods alongside general-purpose smoothing techniques.

## Why trendseries?

Working with economic time series in R often involves cumbersome conversions between data frames and `ts` objects. Most filtering methods are designed for `ts` objects, but modern data analysis workflows use `tibble` and `data.frame` with date columns.

`trendseries` bridges this gap by:

- **Working directly with data frames** - No manual conversions to `ts` objects
- **Pipe-friendly design** - Integrates seamlessly with tidyverse workflows using `|>`
- **Smart defaults** - Economic-appropriate parameters (λ=1600 quarterly, λ=14400 monthly)
- **Grouped operations** - Apply trends to multiple series at once
- **Comprehensive methods** - From HP filters to Kalman smoothing

# Getting Started

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("viniciusoike/trendseries")
```

## Two Main Functions

The package architecture is built around two complementary functions:

### `augment_trends()` - For data frames and tibbles

Add trend columns to your existing data in a pipe-friendly workflow. Perfect for exploratory analysis and when working with multiple series.

```r
library(trendseries)

# Simple workflow
gdp_construction |>
  augment_trends(value_col = "gdp_construction", methods = "hp")

# Grouped analysis
multi_series |>
  augment_trends(
    value_col = "value",
    methods = c("hp", "ma"),
    group_vars = "country"
  )
```

### `extract_trends()` - For time series objects

Extract trends directly from `ts`, `xts`, or `zoo` objects. Focused on pure econometric analysis.

```r
# Convert to ts and extract trend
ts_data <- df_to_ts(gdp_construction, frequency = 4)
extract_trends(ts_data, methods = "hp")
```
### Unified Parameter System

All methods use a consistent parameter interface when possible.

- **`window`**: Controls the period for moving averages and related methods.
- **`smoothing`**: Controls smoothness for HP, LOESS, splines.
- **`band`**: Specifies frequency bands for bandpass filters (e.g., `c(6, 32)` for business cycles).
- **`params`**: Named list for method-specific advanced parameters.

## Examples

### Basic Trend Extraction

```r
library(trendseries)
library(dplyr)

# Load Brazilian GDP construction data (quarterly)
data("gdp_construction")

# Extract HP filter trend
gdp_with_trend <- gdp_construction |>
  augment_trends(
    value_col = "gdp_construction",
    methods = "hp"
  )
```

### Comparing Multiple Methods

```r
# Compare HP filter, LOESS, and moving average
gdp_construction |>
  augment_trends(
    value_col = "gdp_construction",
    methods = c("hp", "loess", "ma")
  ) |>
  select(date, gdp_construction, starts_with("trend_"))
```

### Working with Monthly Data

```r
# Load monthly vehicle production data
data("vehicles")

# Extract trend with 12-month moving average
vehicles |>
  augment_trends(
    value_col = "vehicles",
    methods = "ma",
    window = 12
  )
```

### Grouped Time Series

```r
# Combine datasets
combined_data <- full_join(ibcbr, vehicles, by = "date") |>
  tidyr::pivot_longer(
    cols = c(ibcbr, vehicles),
    names_to = "indicator",
    values_to = "value"
  )

# Extract trends for each indicator
combined_data |>
  augment_trends(
    value_col = "value",
    methods = "hp",
    group_vars = "indicator"
  )
```

## Available Methods

`trendseries` includes a comprehensive set of trend extraction methods optimized for economic time series analysis.

### Economic & Econometric Filters

| Method | Description | Key Parameters |
|--------|-------------|----------------|
| `hp` | Hodrick-Prescott filter (two-sided or one-sided) | `smoothing` (λ), `params = list(hp_onesided = TRUE)` for real-time |
| `bk` | Baxter-King bandpass filter | `band` (cycle range) |
| `cf` | Christiano-Fitzgerald asymmetric filter | `band` |
| `hamilton` | Hamilton regression filter | `params = list(hamilton_h, hamilton_p)` |
| `bn` | Beveridge-Nelson decomposition | - |
| `ucm` | Unobserved components model | `params = list(ucm_type)` |

### Moving Averages

| Method | Description | Key Parameters |
|--------|-------------|----------------|
| `ma` | Simple moving average (SMA) | `window`, `align` |
| `wma` | Weighted moving average | `window`, `align` |
| `ewma` | Exponential weighted moving average | `smoothing` or `window` |
| `zlema` | Zero-lag exponential moving average | `window` |
| `triangular` | Triangular moving average | `window`, `align` |
| `median` | Median filter (robust) | `window` |
| `gaussian` | Gaussian-weighted moving average | `window`, `align` |

### Smoothing Methods

| Method | Description | Key Parameters |
|--------|-------------|----------------|
| `loess` | Local polynomial regression | `smoothing` (span) |
| `spline` | Smoothing splines | `smoothing` |
| `savgol` | Savitzky-Golay filter | `window`, `poly_degree` |
| `butterworth` | Butterworth filter | `order`, `cutoff` |
| `lowess` | Locally weighted regression | `smoothing` |

### Advanced Methods

| Method | Description | Key Parameters |
|--------|-------------|----------------|
| `stl` | Seasonal-trend decomposition | `window` (s.window parameter) |
| `kalman` | Kalman filter/smoother | `smoothing` or `params` |
| `exp_simple` | Simple exponential smoothing | `smoothing` (alpha) |
| `exp_double` | Double exponential smoothing (Holt) | `params = list(exp_alpha, exp_beta)` |
| `poly` | Polynomial trends | `params = list(poly_degree, poly_raw)` |

## Key Features

### Smart Economic Defaults

The package automatically applies economic-appropriate defaults based on data frequency:

- **HP filter**: λ=1600 (quarterly), λ=14400 (monthly)
- **Moving averages**: 4-quarter window (quarterly), 12-month window (monthly)
- **Bandpass filters**: 6-32 quarter business cycle range
- **STL decomposition**: Seasonality-appropriate parameters

### Grouped Operations

Work with multiple time series effortlessly:

```r
# Apply same trend method to multiple countries
multi_country_gdp |>
  augment_trends(
    value_col = "gdp",
    methods = "hp",
    group_vars = c("country", "sector")
  )
```

### Intelligent Column Naming

Avoid naming conflicts with automatic column renaming:

```r
# Creates: trend_hp, trend_ma, trend_loess
data |>
  augment_trends(methods = c("hp", "ma", "loess"))

# With custom suffixes for different parameters
data |>
  augment_trends(methods = "ma", window = 4) |>
  augment_trends(methods = "ma", window = 12)
# Creates: trend_ma (window=4), trend_ma (window=12, auto-renamed)
```

### Modern R Practices

- Native pipe `|>` compatible
- Works seamlessly with dplyr, tidyr, ggplot2
- Comprehensive error messages via `cli` package
- Type-safe parameter validation

## Included Datasets

The package includes economic indicators for testing and examples:

### Brazilian Data (BCB - Central Bank)
- **`gdp_construction`**: Quarterly construction sector GDP index (1995-2023, seasonally adjusted)
- **`ibcbr`**: Monthly Central Bank Economic Activity Index (smooth, good for learning)
- **`vehicles`**: Monthly vehicle production in thousands of units (cyclical patterns)
- **`oil_derivatives`**: Monthly oil derivatives production
- **`electric`**: Monthly residential electricity consumption in GWh

### UK Retail Sales (ONS)
- **`retail_households`**: Monthly household goods stores retail sales index
- **`retail_autofuel`**: Monthly automotive fuel retail sales index

### Coffee Prices (CEPEA - Daily data)
- **`coffee_arabica`**: Daily arabica coffee prices with inflation adjustment
- **`coffee_robusta`**: Daily robusta coffee prices with inflation adjustment

### Metadata
- **`series_metadata`**: Metadata for all BCB economic series in the package

All datasets are documented and ready to use:

```r
data("gdp_construction")
?gdp_construction
```

## Learn More

Explore detailed documentation and examples in our vignettes:

- **[Getting Started](articles/trendseries.html)** - Complete introduction with step-by-step examples
- **[Economic Filters](articles/economic-filters.html)** - Deep dive into HP, Baxter-King, and Christiano-Fitzgerald filters
- **[Moving Averages](articles/moving-averages.html)** - Comprehensive guide to MA methods
- **[Advanced Methods](articles/advanced-methods.html)** - STL, Kalman filtering, and more

### Function Documentation

- `?augment_trends` - Add trends to data frames
- `?extract_trends` - Extract trends from time series objects
- `?df_to_ts` - Convert data frames to ts objects

## How It Compares

### vs. Traditional Workflow

**Without trendseries:**
```r
# Manual ts conversion
gdp_ts <- ts(data$gdp, frequency = 4, start = c(1996, 1))

# Apply filter
trend <- mFilter::hpfilter(gdp_ts, freq = 1600)

# Convert back and merge
trend_df <- data.frame(
  date = as.Date(time(gdp_ts)),
  trend = as.numeric(trend$trend)
)
result <- left_join(data, trend_df, by = "date")
```

**With trendseries:**
```r
# One step
data |>
  augment_trends(value_col = "gdp", methods = "hp")
```

### vs. tsibble/fable

While `tsibble`/`fable` provides excellent forecasting tools, `trendseries` focuses specifically on trend extraction with:

- **More econometric methods**: HP filter, Hamilton filter, bandpass filters
- **Simpler workflow**: Works with standard data frames, no need for `tsibble` objects
- **Specialized defaults**: Optimized for economic time series analysis
- **Lighter dependencies**: Built on established packages like `mFilter` and `TTR`

Use `fable` for forecasting, `trendseries` for trend extraction and exploratory analysis.

## Getting Help

- **Documentation**: `?augment_trends`, `?extract_trends`
- **Examples**: `example(augment_trends)`
- **Vignettes**: `vignette(package = "trendseries")`
- **Issues**: [GitHub Issues](https://github.com/viniciusoike/trendseries/issues)
- **Website**: [https://viniciusoike.github.io/trendseries/](https://viniciusoike.github.io/trendseries/)

## Design Philosophy

`trendseries` is designed for **exploratory time series analysis**, with a primary focus on monthly and quarterly economic data. It prioritizes:

1. **Simplicity** - Minimal boilerplate, maximum insight
2. **Consistency** - Unified interface across all methods
3. **Flexibility** - Easy parameter experimentation
4. **Integration** - Works seamlessly with tidyverse tools

The package is **optimized for** economic time series (monthly/quarterly frequencies), with smart defaults tailored for business cycle analysis. While certain methods (STL, moving averages, smoothing methods) can handle daily and other frequencies, the defaults and parameter suggestions are calibrated for standard economic data. This focused scope enables better defaults and a cleaner API.

## Acknowledgements

This package builds upon excellent work from the R community:

- `mFilter` - Economic filters (HP, BK, CF)
- `hpfilter` - One-sided HP filter implementation
- `RcppRoll` - Fast C++ implementations of rolling statistics
- `forecast` - Exponential smoothing methods
- `dlm` - Dynamic linear models and Kalman filtering
- `signal` - Signal processing filters
- `tsbox` - Time series conversions

## License

MIT + file LICENSE
