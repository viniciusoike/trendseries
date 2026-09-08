# Getting Started with trendseries

## What is trendseries?

`trendseries` is a pipe-friendly interface to the trend, seasonal, and
cyclical structure of economic time series.

- **[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)**
  fits a smooth **trend** to a series.
- **[`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)**
  splits a series into **trend**, **seasonal**, and **remainder**
  components.
- **[`deseason_series()`](https://viniciusoike.github.io/trendseries/reference/deseason_series.md)**
  removes the **seasonal** component, returning a seasonally adjusted
  series.
- **[`detrend_series()`](https://viniciusoike.github.io/trendseries/reference/detrend_series.md)**
  removes the trend, returning the **deviation from trend** (a.k.a. the
  cycle, or output gap).
- **[`index_series()`](https://viniciusoike.github.io/trendseries/reference/index_series.md)**
  rescales one or more series to a common base period and value.

All five share the same pipe-friendly `data.frame` interface, the same
underlying trend methods, and the same unified parameter system.
Throughout this vignette (and the package documentation generally) the
terms `data.frame` and “data frame” refer to any dataset in a
rectangular format, i.e., `data.frame`/`tibble`/`data.table`.

### Why trendseries?

Most filtering methods in R are designed for `ts` objects, but analysis
workflows use data frames with a date column. Converting back and forth
is tedious and error-prone. `trendseries` works on data frames
throughout, and keeps the `ts`-native interface available for when you
need it.

The package also sources smoothing functions across different packages
and provides a unified interface when possible. The methods are the ones
applied to economic series: econometric filters such as
Hodrick-Prescott, Hamilton, and Spencer, alongside general-purpose
smoothers such as STL and moving averages.

## A simple example

Each function works by adding columns to the data frame, named after the
component and the method used (`trend_stl`, `seasadj_stl`, `detrend_hp`,
etc.). The examples below use the IBC-Br series (`ibcbr`), the Brazilian
Central Bank’s monthly index of economic activity.

[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
fits a smooth trend to a series and returns it as a new column.

``` r

ibcbr_trend <- augment_trends(ibcbr, value_col = "index", methods = "stl")

head(ibcbr_trend)
#> # A tibble: 6 × 3
#>   date       index trend_stl
#>   <date>     <dbl>     <dbl>
#> 1 2003-01-01  67.1      70.2
#> 2 2003-02-01  68.8      70.2
#> 3 2003-03-01  72.2      70.3
#> 4 2003-04-01  71.3      70.4
#> 5 2003-05-01  70.0      70.5
#> 6 2003-06-01  68.8      70.7
```

``` r

ggplot(ibcbr_trend, aes(date)) +
  geom_line(aes(y = index, color = "Original"), linewidth = 0.5, alpha = 0.5) +
  geom_line(aes(y = trend_stl, color = "Trend (STL)"), linewidth = 0.7) +
  labs(
    title = "Brazilian economic activity (IBC-Br)",
    x = NULL,
    y = "Index",
    color = NULL
  ) +
  theme_bw()
```

![](trendseries_files/figure-html/ibcbr-plot-1.png)

Every trend method reachable through
[`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
is also reachable through
[`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md),
which takes `ts`/`xts`/`zoo` objects instead of data frames and returns
them, for users who prefer to stay in base R’s time series ecosystem.

``` r

stl_trend <- extract_trends(AirPassengers, methods = "stl")
plot.ts(AirPassengers)
lines(stl_trend, col = "#D3742A")
```

![](trendseries_files/figure-html/extract-1.png)

### Indexing series

Use
[`index_series()`](https://viniciusoike.github.io/trendseries/reference/index_series.md)
to compare series on a common base. By default it uses the earliest
non-missing observation; `base_period` can instead select a year or a
date range and use its mean as the reference value.

``` r

ibcbr_indexed <- ibcbr |>
  index_series(value_col = "index", base_period = 2019)

head(ibcbr_indexed)
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

The function also calculates separate references for grouped data and
supports multiple value columns.

## Where to go next

Each function has its own vignette with worked examples, parameter
details, and guidance on choosing between methods.

| Vignette | Covers |
|----|----|
| [Augmenting Trends](https://viniciusoike.github.io/trendseries/articles/augment-trends.html) | [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)/[`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md): grouping, multiple methods, finer control |
| [Decomposing Series](https://viniciusoike.github.io/trendseries/articles/decompose-series.html) | [`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md)/[`deseason_series()`](https://viniciusoike.github.io/trendseries/reference/deseason_series.md): trend/seasonal/remainder splits |
| [Detrending Series](https://viniciusoike.github.io/trendseries/articles/detrend-series.html) | [`detrend_series()`](https://viniciusoike.github.io/trendseries/reference/detrend_series.md): cycles, output gaps, the deseason-then-detrend workflow |
| [Trend Extraction Methods](https://viniciusoike.github.io/trendseries/articles/methods.html) | Catalogue of the trend methods, by family |
| [Moving Averages](https://viniciusoike.github.io/trendseries/articles/moving-averages.html) | SMA, WMA, EWMA, Triangular, Median, Gaussian, Spencer, Henderson |
| [Econometric Filters](https://viniciusoike.github.io/trendseries/articles/econometric-filters.html) | HP, BK, CF, Hamilton, Beveridge-Nelson, UCM |

## Acknowledgements

`trendseries` builds on existing packages.

- `mFilter` for economic filters.
- `hpfilter` for Hodrick-Prescott filtering.
- `tsbox` for time series conversions.

## Getting Help

- Check the documentation:
  [`?augment_trends`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md),
  [`?decompose_series`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md),
  [`?deseason_series`](https://viniciusoike.github.io/trendseries/reference/deseason_series.md),
  [`?detrend_series`](https://viniciusoike.github.io/trendseries/reference/detrend_series.md),
  [`?index_series`](https://viniciusoike.github.io/trendseries/reference/index_series.md)
- View examples: `example(augment_trends)`
- Read other vignettes: `vignette(package = "trendseries")`
- Report bugs: GitHub issues
