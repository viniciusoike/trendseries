# trendseries 1.7.0 release plan

Status: agreed scope; implementation deferred. Saved 2026-09-30.

## Requirements summary

Release `trendseries` 1.7.0 to CRAN with tested `tsibble` support for
`augment_trends()` and its `detrend_series()` wrapper. Review the current
release code for correctness, document the supported behavior, then prepare
and verify the exact CRAN submission tarball. This plan does not authorize
implementation or submission yet.

Keep the unfinished interpolation feature and broad refactoring outside this
release. Other data-frame entry points, including rolling, indexing,
decomposition, and deseasoning, are outside the 1.7.0 tsibble contract.

## Starting evidence

- Local `main` is clean but four commits behind `origin/main` (`b981561`). Start
  implementation from the current remote main; preserve other worktrees.
- `DESCRIPTION:4` declares 1.6.1. The current remote `NEWS.md` has a
  development section above the 1.6.1 notes for recently merged rolling
  features. Reconcile both under 1.7.0 after release contents are settled.
- The uncommitted WIP at
  `/Users/viniciusreginatto/.t3/worktrees/trendseries/t3code-594fe3dd`
  changes `R/augment_trends.R`, adds `R/tsibble.R`, and has five tests in
  `tests/testthat/test-tsibble.R`. It predates the newest main changes and has
  no direct `detrend_series()` test. Preserve it while integrating.
- The existing `detrend_series()` converts input to a tibble before calling
  `augment_trends()` (`R/detrend_series.R:109-175` on current local main), so
  supporting the wrapper needs explicit work and tests.
- `vignettes/articles/augment-trends.Rmd:363-367` describes tsibble as an
  alternative workflow and must reflect the new supported input.
- `cran-comments.md` still describes 1.6.0. The current GitHub CI matrix
  checks Windows and Linux but omits macOS.

## Acceptance criteria

1. `augment_trends()` accepts supported tsibble inputs and returns a valid
   tsibble with the same index class, index values, keys, original values,
   and correctly aligned trend columns. Index and keys supply defaults;
   documented explicit arguments behave consistently.
2. `detrend_series()` has the same supported tsibble input/output contract,
   including keyed groups, `components`, and both transformations. Its
   computed values match the equivalent data-frame path.
3. Tests cover monthly `yearmonth`, quarterly `yearquarter`, keyed groups,
   `Date` indices, explicit overrides, missing periods and values, row and
   column alignment, and failures for unsupported index types or conflicting
   metadata. Tests are deterministic and skip cleanly when optional `tsibble`
   is unavailable.
4. The release-focused review records each concrete correctness finding and
   fixes release-blocking defects with a failing regression test first. No
   unrelated refactor is required for this release.
5. Help pages, a worked example, the relevant vignette, `NEWS.md`, and
   `cran-comments.md` describe the actual supported scope. Generated help
   and rendered documentation agree with the source.
6. `DESCRIPTION` declares 1.7.0 only after contents are frozen. The built
   source tarball passes `R CMD check --as-cran` on an appropriate current R;
   targeted tests, package tests, CI, and a `realestatebr` reverse-dependency
   check pass, or any remaining note is explained in the submission comments.
   CRAN checks and the exact submitted tarball are reviewed before submission.

## Implementation steps

1. Refresh from `origin/main` in an isolated worktree or branch. Preserve the
   dirty tsibble WIP worktree. Compare its changes with current
   `R/augment_trends.R`, `R/detrend_series.R`, and the conversion helpers
   (`R/converters.R`) before porting code.
2. Write or expand focused tests in `tests/testthat/test-tsibble.R` first.
   Exercise both public functions and edge cases in criterion 3. Run them
   against the WIP to expose gaps before implementing. Keep `tsibble` optional
   in `Suggests`; avoid an extra dependency if existing tools suffice.
3. Implement the smallest adapter needed for those two functions. Preserve
   tsibble's index/key validity and the package's existing date, frequency,
   grouping, and missing-value rules. Format final R code with AIR.
4. Review the current exported-function paths for release-impacting bugs,
   especially interactions with the new adapter and recent indexing and
   rolling changes. Use reproductions and regression tests before fixes;
   defer speculative cleanup.
5. Update roxygen source and generated help, `NEWS.md`, the relevant vignette,
   and a short tsibble example. Revise the comparison at
   `vignettes/articles/augment-trends.Rmd:363-367`. Render and inspect the
   documentation artifact.
6. Freeze contents; bump to 1.7.0; update `cran-comments.md` from fresh check
   results. Build a source tarball and check that exact file with
   `R CMD check --as-cran`. Run targeted tests, the full package suite,
   lint/static checks where configured, CI on the final commit, and a
   `realestatebr` reverse-dependency check. Investigate any platform gap,
   particularly macOS. Review current CRAN check results, then submit only
   the verified tarball.

## Risks and mitigations

- The WIP is based on an older tree. Port its intent onto current main rather
  than merging its old branch wholesale.
- Tsibble coercion can sort rows and reject duplicate key/index pairs. Test
  alignment and validity explicitly; do not restore indices by position
  without proving the row correspondence.
- A weekly or other special index may not map cleanly to this package's
  calendar-frequency rules. State and test the supported index classes;
  reject unsupported cases clearly rather than silently misdate results.
- Green CI does not substitute for checking the built CRAN tarball, and CI
  currently lacks macOS. Verify the exact artifact and report any platform
  check that could not run.

## Stop condition

Stop before CRAN submission if the supported tsibble contract, release-code
correctness review, documentation, exact-tarball check, or reverse-dependency
check has an unresolved release-blocking result. Report the blocker and the
specific evidence. Submission is a separate final action after these gates.
