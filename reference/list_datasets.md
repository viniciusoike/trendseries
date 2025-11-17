# List Available Datasets

Returns a tibble with metadata for all datasets included in the
trendseries package.

## Usage

``` r
list_datasets()
```

## Value

A tibble with the following columns:

- name:

  Dataset name

- description:

  Brief description of the dataset

- frequency:

  Data frequency (D = daily, M = monthly, Q = quarterly)

- n_obs:

  Number of observations

- first_date:

  First observation date

- last_date:

  Last observation date

- value_cols:

  Main value column(s) in the dataset

- source:

  Data source

## Examples

``` r
# List all available datasets
list_datasets()
#> # A tibble: 9 × 8
#>   name       description frequency n_obs first_date last_date  value_cols source
#>   <chr>      <chr>       <chr>     <int> <date>     <date>     <chr>      <chr> 
#> 1 gdp_const… GDP Constr… Q           122 1995-01-01 2025-04-01 index      BCB-S…
#> 2 ibcbr      Central Ba… M           271 2003-01-01 2025-07-01 index      BCB-S…
#> 3 vehicles   Vehicle Pr… M           534 1981-02-01 2025-07-01 production BCB-S…
#> 4 electric   Electric C… M           558 1979-02-01 2025-07-01 consumpti… BCB-S…
#> 5 oil_deriv… Oil Deriva… M           557 1979-02-01 2025-06-01 production BCB-S…
#> 6 retail_vo… UK Retail … D          4077 1988-01-01 2025-09-01 name_seri… ONS   
#> 7 retail_au… UK Retail … M           357 1996-01-01 2025-09-01 value      ONS   
#> 8 coffee_ar… CEPEA Arab… D          7128 1996-09-02 2025-04-17 spot_rs, … CEPEA…
#> 9 coffee_ro… CEPEA Robu… D          5794 2001-11-08 2025-04-17 spot_rs, … CEPEA…

# Filter for monthly data
list_datasets() |>
  dplyr::filter(frequency == "M")
#> # A tibble: 5 × 8
#>   name       description frequency n_obs first_date last_date  value_cols source
#>   <chr>      <chr>       <chr>     <int> <date>     <date>     <chr>      <chr> 
#> 1 ibcbr      Central Ba… M           271 2003-01-01 2025-07-01 index      BCB-S…
#> 2 vehicles   Vehicle Pr… M           534 1981-02-01 2025-07-01 production BCB-S…
#> 3 electric   Electric C… M           558 1979-02-01 2025-07-01 consumpti… BCB-S…
#> 4 oil_deriv… Oil Deriva… M           557 1979-02-01 2025-06-01 production BCB-S…
#> 5 retail_au… UK Retail … M           357 1996-01-01 2025-09-01 value      ONS   
```
