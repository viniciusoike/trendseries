# London Transit - Average Daily Journeys

Average daily journeys on London's bus and tube networks, split by
business day and non-business day. Aggregated from daily Transport for
London (TfL) network demand data using the UK calendar.

## Usage

``` r
transit_london_avgs
```

## Format

A tibble with monthly observations:

- date_month:

  First day of each month (Date)

- transit_mode:

  Transit mode: `"bus"` or `"tube"`

- is_business_day:

  1 for business days, 0 for weekends/holidays

- avg_daily_journeys:

  Average daily journeys for the given day type

## Source

Transport for London (TfL) network demand data.

## See also

[transit_london_monthly](https://viniciusoike.github.io/trendseries/reference/transit_london_monthly.md)
