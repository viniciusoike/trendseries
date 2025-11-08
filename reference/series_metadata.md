# Series Metadata

Metadata for all economic series included in the package.

## Usage

``` r
series_metadata
```

## Format

A tibble with metadata:

- series_name:

  Short series identifier

- description:

  Full series description

- value_column:

  Main value column(s) in the dataset

- frequency:

  Data frequency (D = daily, M = monthly, Q = quarterly)

- first_obs:

  First observation date

- last_obs:

  Last observation date

- n_obs:

  Number of observations

- source:

  Data source

## Source

Various (BCB-SGS, ONS, CEPEA/ESALQ)
