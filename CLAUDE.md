# Trendseries Package Development Guide

## Package Vision
Trendseries is designed to facilitate exploratory time series analysis for **monthly and quarterly economic data**. The package provides a simple, modern interface for extracting trends from economic time series using established econometric methods.

## Target Use Cases
- Exploratory analysis of economic indicators (GDP, inflation, employment, etc.)
- Business cycle analysis and trend extraction
- Comparative analysis of multiple economic time series
- Parameter experimentation for trend extraction methods

## Core Architecture: Two Main Functions

### 1. `augment_trends()`
**Purpose**: Pipe-friendly function for tibble/data.frame workflows
- Adds trend columns to existing data
- Supports grouped operations for multiple time series
- Integrates seamlessly with dplyr workflows
- Handles naming conflicts intelligently
- Returns original data with additional trend columns

### 2. `extract_trends()`
**Purpose**: Direct time series object analysis
- Works with ts, xts, zoo objects
- Returns trend components as time series objects
- Focused on pure econometric analysis
- Optimized for ts-based workflows

## Data Focus
- **Monthly data** (frequency = 12)
- **Quarterly data** (frequency = 4)
- **No support for daily/high-frequency data** (by design)
- Optimized defaults for economic time series

## Coding Standards

### R Style Guidelines
- Follow **tidyverse style guide** strictly
- Use **native pipe** `|>` (not magrittr `%>%`)
- Keep pipe chains to **maximum 4-5 operations**
- Use **`<-` for assignment** (never `=`)
- Always include **explicit `return()`** statements in functions

### Error Handling and Messages
- Use **`cli` package** for all user messages, warnings, and errors
- Provide informative error messages with suggested solutions
- Include parameter validation with clear feedback

### Function Design
- Clear, descriptive parameter names
- Comprehensive input validation
- Consistent naming conventions across functions
- Type checking for critical parameters

## Dependencies Strategy

### Core Dependencies
- `tsbox`: Robust time series format conversion
- `mFilter`: Econometric filtering methods (Baxter-King, Christiano-Fitzgerald)
- `cli`: Modern messaging and error handling
- `slider`: Rolling window operations

### Existing Dependencies to Keep
- `hpfilter`: HP filter implementation
- `tibble`: Modern data frame functionality
- `zoo`: Time series data handling
- `lubridate`: Date manipulation
- `glue`: String interpolation

### Optional Integration
- `dplyr`: For enhanced data manipulation (suggested, not required)

## Trend Methods Priority

### Essential Economic Filters
1. **Hodrick-Prescott (HP)**: Standard business cycle filter
2. **Baxter-King**: Bandpass filter for business cycles
3. **Christiano-Fitzgerald**: Asymmetric bandpass filter
4. **Moving Averages**: Economic-appropriate windows
5. **STL Decomposition**: Seasonal-trend decomposition
6. **Base R Smoothers**: loess, smooth.spline, lowess

### Smart Parameter Defaults
- HP filter: λ=1600 (quarterly), λ=14400 (monthly)
- Moving averages: 4-quarter, 12-month standard windows
- STL: Economic seasonality-appropriate parameters

## Testing Strategy
- Comprehensive unit tests for both main functions
- Integration tests with real economic data
- Parameter validation testing
- Multi-series operation testing
- Performance benchmarking with realistic data sizes

## Development Workflow
1. Always run `devtools::check()` before committing
2. Test functions with provided Brazilian economic datasets
3. Ensure backward compatibility where possible
4. Document all parameter choices and defaults
5. Include real economic examples in documentation

## Current Package Status
- **Completed**: Basic time series conversion, single-series trend extraction
- **Issues**: Naming collisions, limited multi-series support, missing key econometric filters
- **Redesign Focus**: Two-function architecture, modern R practices, economic data optimization