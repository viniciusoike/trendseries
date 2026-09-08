# Great Britain Retail Sales Volume Indices

Selected non-seasonally adjusted chained volume measures of retail sales
in Great Britain. The data are sourced from Table 3M of the Office for
National Statistics (ONS) Retail Sales Index reference tables and retain
the "All Businesses" series.

## Usage

``` r
retail_volume
```

## Format

A tibble with monthly observations:

- date:

  First day of the reference month (`Date`).

- name_series:

  ONS series name in lower-case, hyphenated form.

- value:

  Non-seasonally adjusted chained volume index of sales per week (2023 =
  100).

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

The bundled snapshot covers January 1988 through January 2026. The
`name_series` column contains the following selected series:
`household-goods-stores`, `computers-and-telecomms-equipment`,
`electrical-household-appliances`,
`pharmaceutical-medical-cosmetic-and-toilet-goods`,
`books-newspapers-and-periodicals`,
`alcoholic-drinks-other-beverages-and-tobacco`, `clothing`,
`all-retailing-including-automotive-fuel`, and
`all-retailing-excluding-automotive-fuel`. Missing values in `value`
represent empty cells in the ONS source table.

## See also

[retail_autofuel](https://viniciusoike.github.io/trendseries/reference/retail_autofuel.md)
