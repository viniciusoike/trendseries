# Finish and review tsibble integration

Status: implemented and reviewed on `feature/tsibble-1.7` at `20346e9`.
Created 2026-09-30; completed 2026-09-30. The release plan remains open.
Parent plan: `trendseries-1.7.0-release.md`.

## Requirements summary

Support tsibble input and output in `augment_trends()` and `detrend_series()`
for the 1.7.0 release. Preserve each function's existing computations and
missing-value rules. Do not extend this contract to other data-frame entry
points. Preserve the dirty WIP worktree at
`/Users/viniciusreginatto/.t3/worktrees/trendseries/t3code-594fe3dd`.

## Current evidence and review priorities

- Local `main` is four commits behind `origin/main` (`b981561`). The WIP is
  based on an older branch, so integrate into a fresh branch from the current
  remote main rather than merging the old tree wholesale.
- WIP `R/tsibble.R:48-69` converts the index to `Date`, calls the data-frame
  path, then restores original index values by position and reconstructs a
  tsibble. Review row correspondence, key/index uniqueness, index class,
  sorting, and regular versus irregular metadata. `as_tsibble()` sorts by key
  and index and validates key/index uniqueness by default:
  https://tsibble.tidyverts.org/reference/as-tsibble.html.
- WIP `R/augment_trends.R:174-207` reads index, key, and frequency defaults.
  Its `yearweek = 52` rule (`R/tsibble.R:24-36`) needs a direct calendar test;
  do not promise weekly support if 53-week years cannot follow the package's
  period rules.
- Current `R/detrend_series.R:109-175` converts input to a tibble before its
  `augment_trends()` call. The WIP changes only its documentation, so tsibble
  behavior in this wrapper is unfinished.
- WIP `tests/testthat/test-tsibble.R:1-129` has five tests for
  `augment_trends()`, some using unseeded random values. It has no direct
  `detrend_series()`, missing-period, index-alignment, or irregular-series
  test. Use deterministic fixtures and observable expected results.

## Proposed public contract

- A supported tsibble supplies `date_col` from its index, `group_cols` from
  its key, and `frequency` from a supported index class or the existing date
  detector. The result remains a valid tsibble with the same index class and
  key, plus the function's computed columns.
- Start with `Date`, `yearmonth`, and `yearquarter` indices. Confirm other
  index classes against existing converter behavior before including them;
  reject unsupported classes clearly. Document how irregular dates and
  missing periods follow the existing data-frame rules.
- Explicit `date_col`, `group_cols`, and `frequency` keep the corresponding
  data-frame argument semantics. Test their interaction with tsibble metadata;
  reject combinations that cannot yield a valid or correctly aligned tsibble.
- `tsibble` stays optional in `Suggests`. Ordinary data-frame calls continue
  to work without it. Do not add `tidyselect` to `Imports` solely for this
  adapter if the existing `rlang` dependency or tsibble API suffices.

## Acceptance criteria

1. Deterministic tests compare tsibble results with equivalent data-frame
   calls for `augment_trends()` and `detrend_series()`. Cover keyed and unkeyed
   monthly/quarterly data, multiple keys, multiple value columns, and trend
   name collisions.
2. The result retains the original index values/class and key columns, has
   one row per input row in the correct key/index correspondence, and passes
   tsibble validation. A grouped result never takes values from another key.
3. `detrend_series()` works with `transform = "none"` and `"log"`, with
   `components = FALSE` and `TRUE`; its detrended values satisfy the existing
   additive or multiplicative identity where estimates are defined.
4. Tests specify outcomes for `Date` indices, missing values and calendar
   periods, irregular input, explicit overrides, unsupported index classes,
   and invalid/conflicting metadata. Any unsupported case fails with an
   actionable error rather than returning misdated values.
5. The optional-dependency path is checked: package loading and ordinary
   data-frame use do not require `tsibble`; tsibble-specific tests skip when
   it is unavailable. Existing data-frame tests stay green.
6. Help pages, a small worked example, and the tsibble comparison in
   `vignettes/articles/augment-trends.Rmd:363-367` state exactly which two
   functions accept tsibbles and which indices are supported. Generated help
   and rendered documentation match the source.

## Work sequence

1. **Protect and transplant.** Record the WIP diff and status; create an
   isolated branch/worktree from current `origin/main`. Compare the WIP with
   current `R/augment_trends.R`, `R/detrend_series.R`, and `R/converters.R`.
   Copy only behavior still needed; keep the original dirty worktree intact.
2. **Write failing tests first.** Expand `tests/testthat/test-tsibble.R` around
   the six criteria above. Give special attention to `detrend_series()` and
   index/key alignment. Run the focused file against the incomplete port and
   record which tests fail for the intended missing behavior.
3. **Finish the adapter.** Reuse the existing data-frame computation path;
   resolve tsibble metadata before either public function discards the class.
   Restore metadata only after proving row correspondence, and validate the
   reconstructed result. Keep changes inside the two public functions and a
   small shared helper if both need it. Format final R code with AIR.
4. **Review in a separate pass.** Trace both public functions through
   conversion, grouping, filtering, and reconstruction. Inspect failure
   paths, warnings, optional dependency handling, key/index validity, and
   accidental behavior changes for data frames. For each concrete defect,
   add a regression test before fixing it. Avoid unrelated refactoring.
5. **Document and verify.** Regenerate roxygen help; add a concise example
   and update the vignette wording. Run focused tests, the full package test
   suite, `R CMD check` on a built source tarball, and the repository's
   applicable lint/static checks. Inspect the generated help and rendered
   vignette. Record exact commands/results and any platform or dependency
   checks that could not run.

## Stop condition

Tsibble integration is ready for the parent release plan only when all six
criteria pass on the current base and the separate review has no unresolved
correctness finding. If an index class or override cannot be supported
correctly within this scope, document and test its rejection rather than
silently widening the implementation.

## Completion evidence

- Implementation worktree: `/tmp/trendseries-tsibble-1.7`. The original dirty
  tsibble WIP worktree was preserved.
- Focused tsibble tests: 50 expectations passed. Full `testthat::test_local()`
  suite passed. AIR format check and staged diff check passed.
- Independent code review found two defects, both fixed with failing tests:
  reordered key columns were rejected, and unordered input was silently
  sorted on return. The follow-up review found no remaining correctness issue.
- Regenerated help and README; rendered the updated article from the local
  package and confirmed its quarterly tsibble example returns `[1Q]`.
- The final built 1.6.1 development tarball passed local macOS
  `R CMD check --as-cran` with 0 errors, 0 warnings, and 1 note for existing
  TFL/CEPEA URLs returning HTTP 403. Log:
  `/tmp/trendseries-tsibble-check-release/trendseries.Rcheck/00check.log`.
- `DESCRIPTION` remains at 1.6.1. Version bump, wider release review,
  reverse-dependency check, and CRAN submission belong to the parent plan.
