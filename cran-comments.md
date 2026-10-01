# trendseries 1.7.0

This submission updates the CRAN version, 1.4.0, and includes the changes
from the unreleased 1.5.0, 1.6.0, and 1.6.1 versions.

## Changes

- Added `augment_rolling()` and `roll_series()` for rolling and year-to-date
  aggregations, and `index_series()` for rebasing series to a base period.
- `augment_trends()` and `detrend_series()` now accept tsibbles. `tsibble` is
  listed in `Suggests` and used only when the input is a tsibble.
- Changed the default HP `lambda` and the default `bk`/`cf` band to scale
  with the series frequency. Monthly results differ from 1.4.0; NEWS.md
  explains how to reproduce the earlier values.
- Fixed several estimation bugs, including the `ucm` method and the
  `bn_ar_order` argument.

## Test environments

- Local macOS, R 4.5.1
- GitHub Actions: Windows (R release), Ubuntu (R release and devel)

## R CMD check results

0 errors | 0 warnings | 1 note

The note flags URLs hosted on `tfl.gov.uk` and `cepea.org.br` as
"(possibly) invalid" because both sites return HTTP 403 to automated
clients. The URLs are valid and resolve in a browser; they cite the
original sources of the bundled `transit_london_*` and `coffee_*`
datasets, and no alternative canonical URL exists for them.

## Reverse dependencies

We checked the one reverse dependency, `realestatebr`, against this version.
It showed no new problems.
