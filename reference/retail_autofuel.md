# Great Britain Retail Sales Volume Index: Automotive Fuel

Non-seasonally adjusted chained volume measure of retail sales for
automotive fuel in Great Britain. This is the ONS "Automotive Fuel, All
Businesses" series from Table 3M of the Retail Sales Index reference
tables.

## Usage

``` r
retail_autofuel
```

## Format

A tibble with monthly observations:

- date:

  First day of the reference month (`Date`).

- value:

  Non-seasonally adjusted chained volume index of sales per week (2023 =
  100).

- name:

  Series name, always `Retail Sales - Automotive Fuel`.

- frequency:

  Frequency code, always `"M"` (monthly).

- source:

  Short source label, always `"ONS"`.

## Source

Office for National Statistics (ONS), [Retail Sales
Index](https://www.ons.gov.uk/businessindustryandtrade/retailindustry/datasets/retailsalesindexreferencetables)
reference tables, Table 3M, "Chained volume of retail sales,
non-seasonally adjusted". See the [Retail Sales Index (RSI) Quality and
Methodology Information
report](https://www.ons.gov.uk/businessindustryandtrade/retailindustry/methodologies/retailsalesindexrsiqmi).
Contains public sector information licensed under the [Open Government
Licence
v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/),
except where otherwise stated. This package distributes a processed
subset of the ONS table; see `data-raw/uk_data.R` for the transformation
code.

## Details

Values are index numbers of sales per week, with 2023 = 100. ONS uses a
four-week, four-week, five-week reporting cycle; `date` records the
first day of the corresponding reference month and should not be
interpreted as a calendar-month total. The Retail Sales Index covers
Great Britain, not the whole UK: Northern Ireland, the Isle of Man, and
the Channel Islands are excluded.

The bundled snapshot covers January 1996 through January 2026.

## See also

[retail_volume](https://viniciusoike.github.io/trendseries/reference/retail_volume.md)
