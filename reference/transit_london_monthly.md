# London Transit - Monthly Journey Totals

Monthly total journeys on London's bus and tube (underground) networks.
Aggregated from daily Transport for London (TfL) network demand data.

## Usage

``` r
transit_london_monthly
```

## Format

A tibble with monthly observations:

- date_month:

  First day of each month (Date)

- transit_mode:

  Transit mode: `"bus"` or `"tube"`

- journey_monthly:

  Total journeys in the month

## Source

Transport for London (TfL) network demand data.

## See also

[transit_london_avgs](https://viniciusoike.github.io/trendseries/reference/transit_london_avgs.md)
