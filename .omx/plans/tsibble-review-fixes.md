# Fix tsibble review findings

Status: completed on `feature/tsibble-1.7` at `a7b20ca`. This plan follows
the review comment on PR #36.

## Requirements summary

Make accepted empty key arguments work in both tsibble entry points. Keep the
input tsibble's interval and gap semantics when adding computed columns.
Preserve the existing supported index classes and data-frame behavior.

## Starting evidence

- `R/tsibble.R:59-80` accepts `group_cols = character()` for a keyless tsibble
  and forwards it to the data-frame path. Both public functions then fail with
  `argument 1 is not a vector`; omitting `group_cols` succeeds.
- `R/tsibble.R:106-111` reconstructs the result with `as_tsibble()`. For a
  monthly series sampled every third month with `frequency = 4`, the interval
  changes from `1M` to `3M` and `has_gaps()` changes from `TRUE` to `FALSE`.
- `tests/testthat/test-tsibble.R:269-314` covers conflicting metadata and
  unordered input, but neither case above.

## Acceptance criteria

1. On a keyless tsibble, `group_cols = character()` gives the same trend or
   detrended values as omitted `group_cols` in both public functions.
2. For supported input, the result retains the input index, key, interval,
   regularity flag, and existing columns. A sparse monthly example keeps its
   original `has_gaps()` result after computing with `frequency = 4`.
3. Existing keyed, unkeyed, data-frame, and missing-period tests pass. No new
   dependency or public API is added.

## Implementation steps

1. In an isolated checkout of `feature/tsibble-1.7`, add failing regression
   cases to `tests/testthat/test-tsibble.R` for both entry points with explicit
   empty keys and for interval/gap preservation. Run the focused tests first.
2. In `R/tsibble.R:59-80`, normalize a validated empty key selection to
   `NULL` before calling the data-frame path. Keep rejection of conflicting
   nonempty groups.
3. In `R/tsibble.R:85-113`, retain the input tsibble and append only computed
   columns after the existing row, index, and key checks. Confirm that column
   assignment retains interval metadata; use the smallest tsibble-supported
   restoration if it does not.
4. Format changed R code with AIR. Run the focused file, full package tests,
   package check, and inspect the diff. Update the review note and PR only
   after the behavior is verified.

## Risks and mitigations

- Appending columns must not overwrite existing user columns. Derive new
  names from the result and assert existing columns stay identical.
- Sparse monthly input uses an explicit quarterly frequency for the regression
  case; the test must distinguish preserved tsibble metadata from trend
  computation semantics.

## Stop condition

Stop when both regressions pass, the full suite and package check pass or have
documented unrelated failures, and the final diff contains only this fix and
its tests. The broader 1.7.0 release remains under the parent release plan.

## Completion evidence

- New regression tests failed before the fix for empty keys and changed
  interval/gap metadata, then passed after it. The focused tsibble tests and
  full package test suite passed.
- AIR format and `git diff --check` passed. The fix changed only
  `R/tsibble.R` and `tests/testthat/test-tsibble.R`.
- The built `trendseries_1.6.1.tar.gz` passed local macOS
  `R CMD check --as-cran` with 0 errors, 0 warnings, and 1 note for the
  previously observed TFL/CEPEA HTTP 403 URLs. Log:
  `/tmp/trendseries-tsibble-fix-check.Yti5b7/trendseries.Rcheck/00check.log`.
- Commit `a7b20ca` was pushed to `origin/feature/tsibble-1.7`. The broader
  1.7.0 release gates remain open.
