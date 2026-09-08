# TfL Network Demand: Monthly Bus and Tube Journeys

Monthly totals of TfL's reported journey counts for London's bus and
Tube networks. The source files contain daily counts for the two modes;
this dataset sums those daily values by calendar month.

## Usage

``` r
transit_london_monthly
```

## Format

A tibble with monthly observations:

- date_month:

  First day of the calendar month (`Date`).

- transit_mode:

  Transit mode: `"bus"` or `"tube"`.

- journey_monthly:

  Sum of the reported daily journey counts for the month.

## Source

Transport for London (TfL), [Network demand
data](https://tfl.gov.uk/corporate/publications-and-reports/network-demand-data),
and the [Network Demand
Dashboard](https://app.powerbi.com/view?r=eyJrIjoiZDgwZWY4NWMtZTFkMi00YzM2LThiMWQtNzg2ZTc2YjliNzM2IiwidCI6IjFmYmQ2NWJmLTVkZWYtNGVlYS1hNjkyLWEwODljMjU1MzQ2YiIsImMiOjh9).
The package distributes an aggregated and reshaped version of TfL's
daily Journeys files. TfL's [Transport Data Service
terms](https://tfl.gov.uk/corporate/terms-and-conditions/transport-data-service)
require the attribution: **Powered by TfL Open Data**. This package is
not affiliated with or endorsed by TfL.

## Details

These are recorded journey counts derived from TfL's ticketing system,
not an absolute measure of passenger numbers or journeys made. TfL notes
that the figures cover activity recorded through Oyster cards,
contactless payment cards, and paper tickets, and do not include
passengers who did not tap in or out. The source figures are approximate
and rounded to the nearest thousand. This dataset is not the separate
station footfall dataset.

TfL publishes separate files for 2019–2022 and a consolidated file for
subsequent observations. The version bundled with `trendseries` covers
daily source records from 2019-01-01 through 2025-12-27. Because the
final source file ends on 27 December 2025, the December 2025 monthly
totals are partial. TfL may revise historical rows when its source files
are refreshed.

## See also

[transit_london_avgs](https://viniciusoike.github.io/trendseries/reference/transit_london_avgs.md)
