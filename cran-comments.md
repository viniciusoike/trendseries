## Resubmission

This is a minor version update (1.1.0 → 1.2.0).

### Changes since last CRAN release:

* `augment_trends()` now supports multiple value columns
* The `group_vars` argument in `augment_trends()` is deprecated in favour of `group_cols`
* Improved UCM trend smoothness with HP-equivalent default parameters
* Added two new datasets (`transit_london_monthly`, `transit_london_avgs`)
* Fixed typos, grammar, and deprecated API usage in vignettes

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.1
* win-builder (devel and release)
* R-hub (ubuntu-latest, windows-latest, macos-latest)

## R CMD check results

0 errors | 0 warnings | 1 note

The following note appeared on win-builder (devel and release):

    Possibly misspelled words in DESCRIPTION:
      Christiano (9:18), Hodrick (8:45), Ravn (11:36), Uhlig (11:45)

These are proper names of economists whose methods are implemented in the
package: the Hodrick-Prescott filter, the Christiano-Fitzgerald filter, and
the Ravn-Uhlig (2002) frequency convention. They are spelled correctly.

## Downstream dependencies

There are currently no downstream dependencies for this package.
