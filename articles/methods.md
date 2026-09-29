# Trend Extraction Methods

``` r

library(trendseries)
library(ekioplot)

series_palette <- unname(c(
  ekio_pal("blue")["700"],
  ekio_pal("blue")["400"],
  ekio_pal("teal")["600"]
))
highlight_gold <- unname(ekio_pal("gold")["light"])
highlight_orange <- unname(ekio_pal("orange")["400"])
```

This article catalogues the trend-extraction methods in `trendseries`:
what each one does, when to reach for it, and which parameters it
accepts. For worked examples of specific filters, see the companion
[Moving
Averages](https://viniciusoike.github.io/trendseries/articles/moving-averages.md)
and [Econometric
Filters](https://viniciusoike.github.io/trendseries/articles/econometric-filters.md)
articles. To split a series into trend, seasonal, and remainder
components instead of extracting a single smooth trend, see [Decomposing
Series](https://viniciusoike.github.io/trendseries/articles/decompose-series.md).

## The two interfaces

Two functions share the same engine and the same parameters, and either
one reaches every method.

- [`augment_trends()`](https://viniciusoike.github.io/trendseries/reference/augment_trends.md)
  — pipe-friendly. Takes a `data.frame`/`tibble`, adds `trend_{method}`
  columns, and supports grouped series via `group_cols`.
- [`extract_trends()`](https://viniciusoike.github.io/trendseries/reference/extract_trends.md)
  — takes a `ts`/`xts`/`zoo` object and returns `ts` objects (a single
  `ts` for one method, a named list for several).

``` r

# Data-frame interface: adds a trend_stl column
head(augment_trends(ibcbr, value_col = "index", methods = "stl"))
```

``` r

# Time-series interface: returns a ts object
hp_trend <- extract_trends(AirPassengers, methods = "hp")
class(hp_trend)
```

Pass several methods at once to compare them.

``` r

trends <- augment_trends(
  ibcbr,
  value_col = "index",
  methods = c("hp", "stl", "henderson")
)
head(trends)
```

## The unified parameter system

Rather than exposing every method’s idiosyncratic arguments,
`trendseries` routes a small set of *generic* parameters to whichever
method-specific option they correspond to. All of them have defaults, so
you can leave them unset.

- `window` — number of observations in the smoothing window.
- `smoothing` — amount of smoothing. The scale depends on the method;
  see [Reading `smoothing`](#reading-smoothing).
- `band` — `c(low, high)`, the cycle periods removed by a bandpass
  filter.
- `align` — `"center"` (default) or `"right"`, which uses only past and
  current observations. `ma` and `wma` also accept `"left"`.
- `params` — a named list for any remaining method-specific option.

``` r

# A wider HP smoothing and a 5-month moving average, in one call
augment_trends(
  ibcbr,
  value_col = "index",
  methods = c("hp", "ma"),
  smoothing = 1600,
  window = 5
)
```

Some defaults follow the frequency of the series. For monthly data the
HP `lambda` defaults to 129600 and the `ma`, `wma`, and `triangular`
windows to 12; for quarterly data, to 1600 and 4. The HP default scales
1600 by the fourth power of the frequency ratio (Ravn and Uhlig 2002),
which gives 6.25 for annual data. This keeps the trend-cycle cutoff near
ten years at every frequency. The `band` default covers cycles of 1.5 to
8 years: `c(6, 32)` for quarterly data and `c(18, 96)` for monthly.

### Reading `smoothing`

The same number means different things to different methods.

- `hp`: a value above 1 is the `lambda` itself. A value of 1 or less is
  a fraction of the frequency’s standard `lambda`, so `smoothing = 0.5`
  on monthly data gives `lambda = 64800`.
- `loess`: the span, the share of observations in each local fit.
- `spline`: the `spar` penalty. Left unset, `spline` picks it by
  generalised cross-validation.
- `ewma`: the weight `alpha` on the newest observation. Pass either
  `window` or `smoothing` to `ewma`, not both.
- `kernel`: a multiplier on the automatically chosen bandwidth.
- `kalman`: the ratio of measurement noise to process noise in a
  local-level model. Larger values give a smoother trend.

## Method catalogue

| Method | Description | Typical use | `window` | `smoothing` | `band` | `align` | One-sided |
|:---|:---|:---|:--:|:--:|:--:|:--:|:--:|
| `hp` | Hodrick-Prescott filter | General-purpose business-cycle trend |  | ✓ |  |  | optional |
| `bn` | Beveridge-Nelson decomposition | Permanent component of an I(1) series |  |  |  |  |  |
| `ucm` | Unobserved components model | Model-based, stochastic trend |  |  |  |  |  |
| `hamilton` | Hamilton regression filter | Regression-based alternative to HP |  |  |  |  |  |
| `bk` | Baxter-King bandpass filter | Remove a band of cycle frequencies |  |  | ✓ |  |  |
| `cf` | Christiano-Fitzgerald bandpass filter | Band removal that keeps the endpoints |  |  | ✓ |  |  |
| `ma` | Simple moving average | Quick, intuitive smoothing | ✓ vector |  |  | ✓ | optional |
| `spencer` | Spencer’s 15-term moving average | Classic actuarial graduation |  |  |  |  |  |
| `ewma` | Exponentially weighted moving average | Real-time, recent points weigh more | ✓ | ✓ |  |  | ✓ |
| `wma` | Weighted moving average | Smoothing with custom weights | ✓ |  |  | ✓ | optional |
| `triangular` | Triangular moving average | Smoother than a simple moving average | ✓ |  |  | ✓ | optional |
| `median` | Median filter | Smoothing robust to outliers and spikes | ✓ vector |  |  |  |  |
| `gaussian` | Gaussian-weighted moving average | Smooth, bell-weighted average | ✓ |  |  | ✓ | optional |
| `henderson` | Henderson moving average | Trend filter used inside X-11 | ✓ vector |  |  |  |  |
| `stl` | Seasonal-trend decomposition via Loess | Trend of a strongly seasonal series | ✓ |  |  |  |  |
| `loess` | Local polynomial regression (loess) | Flexible non-parametric trend |  | ✓ |  |  |  |
| `spline` | Smoothing splines | Smooth curve, GCV penalty by default |  | ✓ |  |  |  |
| `poly` | Polynomial trend | Simple global trend shape |  |  |  |  |  |
| `kernel` | Kernel smoother | Non-parametric, bandwidth-controlled |  | ✓ |  |  |  |
| `kalman` | Kalman filter/smoother | Local-level trend of a noisy series |  | ✓ |  |  |  |

Every method also accepts method-specific options through `params`. A
few entries in the table need a note.

- `stl`: `window` sets the **seasonal** window (`s.window`), not the
  trend window. Set the trend window with
  `params = list(t.window = ...)`.
- `vector`: pass several windows, such as `window = c(13, 23)`, to get
  one trend per window.
- `triangular` and `gaussian` accept `align = "center"` or `"right"`
  only.
- One-sided methods use only past and current observations, which
  matters for real-time work. `ewma` is always one-sided. The methods
  marked *optional* are one-sided with `align = "right"`, or, for `hp`,
  with `params = list(hp_onesided = TRUE)`.

### Moving averages

Moving-average methods replace each point with a (possibly weighted)
average of its neighbours, which makes them a good starting point for
exploratory work. Control the smoothing through `window`, and the
alignment through `align`. `ma`, `median`, and `henderson` also accept a
*vector* of windows, returning one trend per window.

``` r

augment_trends(
  ibcbr,
  value_col = "index",
  methods = "henderson",
  window = c(13, 23)
)
```

See [Moving
Averages](https://viniciusoike.github.io/trendseries/articles/moving-averages.md)
for the full treatment.

### Smoothing methods

Smoothing methods fit a flexible curve to the data. `stl` and `loess`
are locally adaptive; `spline` and `kernel` trade off fit against
smoothness through a penalty or a bandwidth; `poly` imposes a single
global shape. The `smoothing` parameter tunes how aggressively they
smooth.

``` r

loess_trend <- extract_trends(AirPassengers, methods = "loess", smoothing = 0.3)
plot(AirPassengers, col = "grey60", ylab = "Air passengers")
lines(loess_trend, col = highlight_orange, lwd = 2)
```

### Econometric filters

These are the typical filters of applied macroeconomics. The
Hodrick-Prescott filter (`hp`) is perhaps the most widely used;
`hamilton` is a regression-based alternative designed to avoid the
spurious cycles HP can introduce; `bn` and `ucm` are model-based
decompositions into permanent and transitory parts. For `ucm`, the
default is a basic structural model (`"BSM"`) for frequencies 2 to 12
and a local-level model otherwise. Its variances are estimated by
maximum likelihood, so `smoothing` does not control this method;
explicit BSM requests support frequencies up to 12.

``` r

augment_trends(
  gdp_construction,
  value_col = "index",
  methods = c("hp", "hamilton")
) |>
  head()
```

The HP filter has a one-sided (real-time) variant for nowcasting, where
future observations must not influence the current estimate.

``` r

extract_trends(
  AirPassengers,
  methods = "hp",
  params = list(hp_onesided = TRUE)
) |>
  head()
```

See [Econometric
Filters](https://viniciusoike.github.io/trendseries/articles/econometric-filters.md)
for details.

### Bandpass filters

Bandpass filters (`bk`, `cf`) isolate the fluctuations whose period
falls inside a chosen band. The trend they return is the series *minus*
that band: the long-run movement plus any high-frequency noise. To get
the band itself, use
[`detrend_series()`](https://viniciusoike.github.io/trendseries/reference/detrend_series.md).
Specify the band with `band = c(low, high)`, in periods of the series
(months for monthly data). `bk` leaves missing values at both ends of
the series; `cf` does not.

``` r

extract_trends(
  AirPassengers,
  methods = "cf",
  band = c(18, 96)
) |>
  head()
```

## Decomposition vs. trend extraction

The methods above estimate a single smooth *trend*. When you instead
want to split a series into **trend + seasonal + remainder**, use
[`decompose_series()`](https://viniciusoike.github.io/trendseries/reference/decompose_series.md),
which has methods of its own.

| Method | Engine | Notes |
|----|----|----|
| `stl` | [`stats::stl()`](https://rdrr.io/r/stats/stl.html) | Loess-based, robust option available. |
| `regression` | OLS | Polynomial trend + seasonal dummies. |
| `classic` | [`stats::decompose()`](https://rdrr.io/r/stats/decompose.html) | Classical moving-average; additive or multiplicative. |
| `bsm` | [`stats::StructTS()`](https://rdrr.io/r/stats/StructTS.html) | State-space model; components for every point. |
| `seats` | X-13ARIMA-SEATS | Requires the optional **`seasonal`** package. |

``` r

decompose_series(gdp_construction, value_col = "index", methods = "stl") |>
  head()
```

The dedicated [Decomposing
Series](https://viniciusoike.github.io/trendseries/articles/decompose-series.md)
article covers these in depth.

## References

Ravn, M. O., & Uhlig, H. (2002). On adjusting the Hodrick-Prescott
filter for the frequency of observations. *The Review of Economics and
Statistics*, 84(2), 371–376.
