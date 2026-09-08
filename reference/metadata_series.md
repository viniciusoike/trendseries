# Series Metadata

Metadata for all economic series included in the package.

## Usage

``` r
metadata_series
```

## Format

A tibble with metadata:

- series_name:

  Short series identifier

- description:

  Full series description

- frequency:

  Data frequency (D = daily, M = monthly, Q = quarterly)

- source:

  Data source

- date_col:

  Name of the date column in the dataset

- value_col:

  Name of the main value column(s) in the dataset

- group_cols:

  Grouping column(s) for long-format datasets, or `NA`

- date_min:

  First observation date

- date_max:

  Last observation date

## Source

Various. BCB-SGS metadata and source institutions are documented on the
individual dataset pages; see the [BCB SGS series
search](https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries).
