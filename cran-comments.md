# trendseries 1.6.0

This submission contains bug fixes for data-frame alignment and filtering,
new `index_series()` functionality, and documentation changes.

## Changes

- Reorganized the pkgdown articles and package vignettes.
- Updated vignette plots to use `ekioplot`, which is listed in `Suggests` and
  is used only when building the vignettes.

## R CMD check results

0 errors | 0 warnings | 1 note

The note flags URLs hosted on `tfl.gov.uk` and `cepea.org.br` as
"(possibly) invalid" because both sites return HTTP 403 to automated
clients. The URLs are valid and resolve in a browser; they cite the
original sources of the bundled `transit_london_*` and `coffee_*`
datasets, and no alternative canonical URL exists for them.

## Reverse dependencies

There are currently no reverse dependencies for this package.
