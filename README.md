
<!-- README.md is generated from README.Rmd. Please edit that file -->

# trendseries: extract trends from time series

<!-- badges: start -->

<img src="man/figures/logo.png" align="right" height="200" alt="trendseries hexsticker"/>
[![CRAN
status](https://www.r-pkg.org/badges/version/trendseries)](https://CRAN.R-project.org/package=trendseries)
[![R-universe](https://viniciusoike.r-universe.dev/badges/trendseries)](https://viniciusoike.r-universe.dev/trendseries)
<!-- badges: end -->

`trendseries` provides a unified interface to extract trends, cycles,
and seasonal components from time series. Most filtering methods in R
are designed for `ts` objects, but datasets typically come in a
`data.frame` format with a date column, which makes applying filters
cumbersome. `trendseries` bridges this gap: `augment_trends()`,
`decompose_series()`, `deseason_series()`, and `detrend_series()` all
work directly on `data.frame`/`tibble` objects, while `extract_trends()`
provides the same methods for `ts`/`xts`/`zoo` objects when you need to
stay in native time-series format.

## Installation

`trendseries` is available on CRAN

``` r
install.packages("trendseries")
```

You can install the newest version of trendseries from
[R-Universe](https://viniciusoike.r-universe.dev/trendseries).

``` r
install.packages(
  'trendseries',
  repos = c(
    'https://viniciusoike.r-universe.dev',
    'https://cloud.r-project.org'
  )
)
```

## Core Functions

Six core functions cover `data.frame`/`tibble`/`data.table` workflows.

- **`augment_trends()`**: adds trend columns to the original dataset.
- **`augment_rolling()`**: add rolling window trend columns to the
  original dataset.
- **`decompose_series()`**: splits a series into trend, seasonal, and
  remainder components.
- **`deseason_series()`**: wraps `decompose_series()` to return a
  seasonally adjusted series.
- **`detrend_series()`**: wraps `augment_trends()` to return the
  deviation from trend (the cycle).
- **`index_series()`**: rescales one or more series to a common base
  period and value.

Some functions like `augment_trends()` also have a
`ts`/`xts`/`zoo`-native counterpart via **`extract_trends()`**, for
workflows that stay in native time-series format.

## Usage

The example below computes three filters (HP, STL, and moving average)
on a quarterly index of construction activity. `augment_trends()`
detects the frequency of the data and picks conventional defaults for
the HP filter.

``` r
library(trendseries)
library(ggplot2)
data(gdp_construction)
# Computes multiple trends at once
series <- gdp_construction |>
  # Automatically detects frequency
  # Trends are added as new columns to the original dataset
  augment_trends(
    value_col = "index",
    methods = c("hp", "stl", "ma")
  )
#> Auto-detected quarterly (4 obs/year)

series
#> # A tibble: 124 × 5
#>    date       index trend_hp trend_stl trend_ma
#>    <date>     <dbl>    <dbl>     <dbl>    <dbl>
#>  1 1995-01-01 100       101.     102.      NA
#>  2 1995-04-01 100       101.     101.      NA
#>  3 1995-07-01 100       102.     100.      99.7
#>  4 1995-10-01 100       103.      99.4     99.6
#>  5 1996-01-01  97.8     103.     101.     101.
#>  6 1996-04-01 101.      104.     102.     102.
#>  7 1996-07-01 107.      104.     103.     103.
#>  8 1996-10-01 103.      105.     104.     104.
#>  9 1997-01-01 101.      106.     106.     106.
#> 10 1997-04-01 108.      106.     109.     109.
#> # ℹ 114 more rows
```

<figure>
<img src="man/figures/example_trendseries.png"
alt="Construction Activity Index with the observed series and trend extracted using the Hodrick–Prescott filter." />
<figcaption aria-hidden="true">Construction Activity Index with the
observed series and trend extracted using the Hodrick–Prescott
filter.</figcaption>
</figure>

An equivalent `extract_trends()` function is also available for `ts`
objects.

``` r
stl_trend <- extract_trends(AirPassengers, methods = "stl")
#> Computing STL trend with s.window = periodic
plot.ts(AirPassengers)
lines(stl_trend, col = "#C53030")
```

<img src="man/figures/README-unnamed-chunk-4-1.svg" alt="" width="100%" style="display: block; margin: auto;" />

`index_series()` rescales a series to a common base, either using its
earliest observation or the mean over a selected year or date range. It
also supports grouped data and multiple value columns.

``` r
data(ibcbr)
indexed <- ibcbr |>
  index_series(value_col = "index", base_period = 2019)
#> Auto-detected monthly (12 obs/year)

head(indexed)
#> # A tibble: 6 × 3
#>   date       index index_index
#>   <date>     <dbl>       <dbl>
#> 1 2003-01-01  67.1        69.3
#> 2 2003-02-01  68.8        71.1
#> 3 2003-03-01  72.2        74.5
#> 4 2003-04-01  71.3        73.6
#> 5 2003-05-01  70.0        72.2
#> 6 2003-06-01  68.8        71.0
```

## Available Methods

The methods below come from four families: econometric filters, bandpass
filters, moving averages, and smoothing. The [Trend Extraction
Methods](https://viniciusoike.github.io/trendseries/articles/methods.html)
vignette describes each one — when to use it and which parameters it
takes.

| Method       | Category       | Description                            |
|--------------|----------------|----------------------------------------|
| `hp`         | econometric    | Hodrick-Prescott filter                |
| `hamilton`   | econometric    | Hamilton regression filter             |
| `bn`         | econometric    | Beveridge-Nelson decomposition         |
| `ucm`        | econometric    | Unobserved components model            |
| `bk`         | bandpass       | Baxter-King bandpass filter            |
| `cf`         | bandpass       | Christiano-Fitzgerald bandpass filter  |
| `ma`         | moving average | Simple moving average                  |
| `wma`        | moving average | Weighted moving average                |
| `ewma`       | moving average | Exponentially weighted moving average  |
| `triangular` | moving average | Triangular moving average              |
| `median`     | moving average | Median filter                          |
| `gaussian`   | moving average | Gaussian-weighted moving average       |
| `spencer`    | moving average | Spencer’s 15-term moving average       |
| `henderson`  | moving average | Henderson moving average               |
| `stl`        | smoothing      | Seasonal-trend decomposition via Loess |
| `loess`      | smoothing      | Local polynomial regression            |
| `spline`     | smoothing      | Smoothing splines                      |
| `poly`       | smoothing      | Polynomial trends                      |
| `kernel`     | smoothing      | Kernel smoother                        |
| `kalman`     | smoothing      | Kalman filter/smoother                 |

## Learn More

To learn more about the package be sure to visit the
[webiste](https://viniciusoike.github.io/trendseries/)

The vignettes below cover each function in detail.

- [Introduction to
  trendseries](https://viniciusoike.github.io/trendseries/articles/trendseries.html)

- [Augmenting
  Trends](https://viniciusoike.github.io/trendseries/articles/augment-trends.html)

- [Decomposing
  Series](https://viniciusoike.github.io/trendseries/articles/decompose-series.html)

- [Detrending
  Series](https://viniciusoike.github.io/trendseries/articles/detrend-series.html)

- [Trend Extraction
  Methods](https://viniciusoike.github.io/trendseries/articles/methods.html)

- [Moving
  Averages](https://viniciusoike.github.io/trendseries/articles/moving-averages.html)

- [Econometric
  Filters](https://viniciusoike.github.io/trendseries/articles/econometric-filters.html)

## Included data and attribution

The package includes daily Arabica and Robusta coffee price indicators
from the Centro de Estudos Avançados em Economia Aplicada (CEPEA),
Escola Superior de Agricultura Luiz de Queiroz (ESALQ), Universidade de
São Paulo (USP). See the
[Arabica](https://www.cepea.org.br/br/indicador/series/cafe.aspx?id=23)
and
[Robusta](https://www.cepea.org.br/br/indicador/series/cafe.aspx?id=24)
source series, and the [Arabica
methodology](https://www.cepea.org.br/br/metodologia/metodologia-cafe-arabica-cepea-esalq.aspx)
and [Robusta
methodology](https://www.cepea.org.br/br/metodologia/metodologia-cafe-robusta-cepea-esalq.aspx).

CEPEA identifies its coffee data as available under the [Creative
Commons Attribution-NonCommercial 4.0 International
license](https://creativecommons.org/licenses/by-nc/4.0/). That license
applies to the CEPEA-derived data; the package code is licensed under
MIT. The bundled data are an adapted version: `usd_2022` is calculated
from the source dollar price using U.S. inflation data, and `trend_ma`
is a 22-observation moving-average column. The first observations are
missing while the moving-average window fills.

Suggested attribution:

> Centro de Estudos Avançados em Economia Aplicada (CEPEA), Escola
> Superior de Agricultura Luiz de Queiroz (ESALQ), Universidade de São
> Paulo (USP), [CEPEA/ESALQ coffee price
> indicators](https://www.cepea.org.br/br/indicador/cafe.aspx), [CC
> BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/); adapted
> in `trendseries`. This attribution does not imply CEPEA endorses the
> package.

## TfL Network Demand data

`trendseries` includes `transit_london_monthly` and
`transit_london_avgs`, which are derived from Transport for London’s
(TfL) daily **Journeys** files. The source covers Bus and Tube journeys
only; it is distinct from TfL’s station-footfall files. The bundled
snapshot contains daily records from 2019-01-01 through 2025-12-27. TfL
can revise historical rows when the source files are refreshed, so these
datasets should be treated as a versioned snapshot rather than a live
feed.

`transit_london_monthly` sums the reported daily journey counts by
calendar month. `transit_london_avgs` calculates the mean daily count by
month, mode, and UK business-day status. The counts are recorded
ticketing activity, not an absolute measure of passenger numbers or
journeys made; they exclude people who did not tap in or out and are
approximate, rounded to the nearest thousand.

Source and methodology: [TfL Network demand
data](https://tfl.gov.uk/corporate/publications-and-reports/network-demand-data),
the [Network Demand
Dashboard](https://app.powerbi.com/view?r=eyJrIjoiZDgwZWY4NWMtZTFkMi00YzM2LThiMWQtNzg2ZTc2YjliNzM2IiwidCI6IjFmYmQ2NWJmLTVkZWYtNGVlYS1hNjkyLWEwODljMjU1MzQ2YiIsImMiOjh9),
and TfL’s [Transport Data Service
terms](https://tfl.gov.uk/corporate/terms-and-conditions/transport-data-service).

Required attribution:

> Powered by TfL Open Data

The package is not affiliated with or endorsed by TfL.

## ONS Retail Sales Index data

The package also includes a processed subset of the [ONS Retail Sales
Index](https://www.ons.gov.uk/businessindustryandtrade/retailindustry/datasets/retailsalesindexreferencetables),
specifically Table 3M’s non-seasonally adjusted chained volume indices
for selected retail sectors in Great Britain. See the [ONS Retail Sales
Index
methodology](https://www.ons.gov.uk/businessindustryandtrade/retailindustry/methodologies/retailsalesindexrsiqmi)
for details on coverage and methods. Contains public sector information
licensed under the [Open Government Licence
v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/),
except where otherwise stated.
