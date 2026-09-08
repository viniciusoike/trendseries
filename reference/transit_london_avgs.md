# TfL Network Demand: Average Daily Journeys

Monthly averages of TfL's reported daily journey counts for London's bus
and Tube networks, split by mode and UK business-day status. The average
is calculated over the daily observations in each month and group; it is
not a monthly total.

## Usage

``` r
transit_london_avgs
```

## Format

A tibble with monthly observations:

- date_month:

  First day of the calendar month (`Date`).

- transit_mode:

  Transit mode: `"bus"` or `"tube"`.

- is_business_day:

  `1` for a UK business day and `0` for a weekend or UK holiday.

- avg_daily_journeys:

  Arithmetic mean of the reported daily journey counts for the month,
  mode, and business-day group.

## Source

Transport for London (TfL), [Network demand
data](https://tfl.gov.uk/corporate/publications-and-reports/network-demand-data),
and the [Network Demand
Dashboard](https://app.powerbi.com/view?r=eyJrIjoiZDgwZWY4NWMtZTFkMi00YzM2LThiMWQtNzg2ZTc2YjliNzM2IiwidCI6IjFmYmQ2NWJmLTVkZWYtNGVlYS1hNjkyLWEwODljMjU1MzQ2YiIsImMiOjh9).
The package distributes an aggregated and reshaped version of TfL's
daily Journeys files and classifies dates with the UK business calendar.
TfL's [Transport Data Service
terms](https://tfl.gov.uk/corporate/terms-and-conditions/transport-data-service)
require the attribution: **Powered by TfL Open Data**. This package is
not affiliated with or endorsed by TfL.

## Details

The business-day flag uses the `QuantLib/UnitedKingdom` calendar from
`RQuantLib`: `1` identifies a business day and `0` identifies a
Saturday, Sunday, or UK holiday. The underlying counts are derived from
TfL's ticketing system and should not be interpreted as an absolute
measure of passenger numbers or journeys made. They are approximate,
rounded to the nearest thousand, and exclude passengers who did not tap
in or out.

The version bundled with `trendseries` covers daily source records from
2019-01-01 through 2025-12-27. December 2025 is therefore a partial
month. TfL may revise historical rows when its source files are
refreshed.

## See also

[transit_london_monthly](https://viniciusoike.github.io/trendseries/reference/transit_london_monthly.md)
