# Trendseries Package Development Guide

## Important: Always Read Coding Guidelines First
**Before writing any code, always read `claude/coding_guidelines.md`** for modern R development patterns, tidyverse best practices, and rlang usage guidelines. This comprehensive guide covers:
- Modern tidyverse patterns (dplyr 1.1+, native pipe `|>`)
- Advanced rlang metaprogramming with `{{}}`, `!!`, and data masking
- Function design, error handling, and performance patterns
- Migration from legacy R patterns to modern approaches

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
- **Primary focus: Monthly data** (frequency = 12) **and Quarterly data** (frequency = 4)
- **Optimized defaults for economic time series** with business cycle analysis in mind
- Methods like STL and moving averages can handle daily and other frequencies
- Smart defaults are calibrated for standard economic frequencies

## Coding Standards

### R Style Guidelines
- Follow **tidyverse style guide** strictly
- Use **native pipe** `|>` (not magrittr `%>%`)
- Keep pipe chains to **maximum 4-5 operations**. Don't go over 10-15 lines of code without breaking into smaller functions.
- Use **`<-` for assignment** (never `=`)
- Always include **explicit `return()`** statements in functions

### For vignettes and examples
- Always split data processing and plotting into separate code chunks.


### Error Handling and Messages
- Use **`cli` package** for all user messages, warnings, and errors. Avoid using `cat`, `print`, or `message` directly.
- Provide informative error messages with suggested solutions.
- Include parameter validation with clear feedback

### Function Design
- Clear, descriptive parameter names
- Comprehensive input validation
- Consistent naming conventions across functions
- Type checking for critical parameters

## Dependencies Strategy

### Core Dependencies
- `tsbox`: Robust time series format conversion
- `mFilter`: Econometric filtering methods (Baxter-King, Christiano-Fitzgerald, HP)
- `cli`: Modern messaging and error handling
- `RcppRoll`: Fast C++ implementations of rolling statistics (mean, median, etc.)
- `forecast`: Optimized exponential smoothing (ses, holt) with automatic parameter selection
- `dlm`: Dynamic linear models and Kalman filtering
- `signal`: Signal processing including Savitzky-Golay and Butterworth filters
- `hpfilter`: One-sided and two-sided HP filter implementations

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
4. Document all parameter choices and defaults
5. Include real economic examples in documentation

## Current Package Status
- **Completed**: Major package restructure with optimized methods and simplified API
- **Performance Optimizations**:
  - RcppRoll package for fast C++ rolling statistics (replaces TTR for better performance)
  - forecast package for optimized exponential smoothing with automatic parameter selection
  - signal package for Savitzky-Golay and Butterworth filtering
  - dlm package for robust Kalman smoothing
  - hpfilter package for both one-sided and two-sided HP filtering
- **API**: Unified parameter system (window, smoothing, band, align, params) with reduced function signatures
- **Focus**: Two-function architecture with modern R practices and economic data optimization
- **Version**: Ready for 1.0.0 release
