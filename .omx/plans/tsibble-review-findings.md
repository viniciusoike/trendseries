# Tsibble review findings and fixes

Status: proposed. Findings come from a read-only review of `feature/tsibble-1.7`
at `a7b20ca`, posted as review comments on PR #37
(https://github.com/viniciusoike/trendseries/pull/37).

Both proposed code changes were prototyped and verified against the working
tree, then reverted. The plan below records the approach and its evidence; the
branch itself is unchanged.

## Requirements summary

Resolve the four review findings on PR #37 without changing public API, adding a
dependency, or regressing existing behaviour. One finding is a real defect worth
fixing now, two are cleanups, and one is a "verified correct, do not touch" note
that should survive as a comment.

## Findings

### 1. `frequency = NULL` is silently overridden (real defect)

`R/tsibble.R:76` branches on `is.null(frequency)`, which cannot distinguish an
unsupplied argument from an explicit `frequency = NULL`. Both are forced to the
index-derived value, so explicit `NULL` stops meaning auto-detect as documented,
and the tsibble path diverges from the data-frame path:

```
df path,  freq=NULL  -> detected 4
tsibble,  freq=NULL  -> 12   (wrong)
```

Repro: a `yearmonth`-indexed tsibble holding quarterly observations. The
data-frame path detects frequency 4 correctly; through the tsibble path the
caller must guess that `frequency = 4` is now required.

### 2. `regular = tsibble::is_regular(data)` has no effect

`R/tsibble.R:23` passes `regular` into `as_tsibble()`. `as_tsibble()` never fills
gaps; the argument only sets an interval attribute that this code never reads.
Verified: `regular = TRUE` and `regular = FALSE` produce identical round trips,
and `is_regular()` returns `TRUE` even for a series sampled every third month.

### 3. The ordering guard must not be replaced with `is_ordered()`

`R/tsibble.R:19-36` round-trips through `as_tsibble()` to detect unsorted rows.
This looks redundant, because tsibble already validated the object on
construction. It is not: `[` subsetting returns a `tbl_ts` with no ordering
guarantee, and `tsibble::is_ordered()` misses it.

```
k[c(2,1,3,4), ]                    -> 2020 Feb, 2020 Jan, ...
tsibble::is_ordered(sub)           -> TRUE      # misses it
as_tsibble() round trip re-sorts   -> detected correctly
```

`augment_trends()` aborts correctly on that input today. No code change; add a
comment so the next reviewer does not simplify this into a silent hole.

### 4. `missing()` forwarding is correct

`R/augment_trends.R:174` uses `missing()` to detect whether the caller was silent
about `date_col`/`group_cols`. This is the only way to honour "defaults to the
tsibble index/key" while still rejecting an explicit mismatch. Verified working
for keyed, keyless, and explicit-empty cases across both entry points, including
`detrend_series()` forwarding `date_col` downstream. A wrapper that always
forwards `date_col` will fail on a tsibble whose index has a different name, but
that surfaces as a clear error rather than a wrong result. No change.

## Proposed solution

### Finding 1: thread a `missing(frequency)` flag

Add a fourth flag parameter to `.tsibble_arguments()`, matching the existing
`date_missing` and `group_missing` pattern:

- `R/tsibble.R`: add `frequency_missing` after `group_missing` in the signature;
  replace the `is.null(frequency)` branch with `if (frequency_missing)`.
- `R/augment_trends.R:181` and `R/detrend_series.R:136`: pass `missing(frequency)`
  as the seventh argument to `.tsibble_arguments()`.

When the caller omits `frequency`, the index still supplies 12 or 4. When the
caller passes `NULL` explicitly, the argument stays `NULL` and the existing
`.check_regular_grid()` detection runs, matching the data-frame contract.

### Finding 2: drop the dead `regular` argument

Delete `regular = tsibble::is_regular(data)` from the `as_tsibble()` call in
`R/tsibble.R`. No other change; `regular` is not read anywhere in the file.

### Finding 3: comment only

Add a short comment above the `as_tsibble()` round trip in `R/tsibble.R`
recording that `tsibble::is_ordered()` does not catch `[`-subsetted
out-of-order tsibbles, so the round trip stays.

### Finding 4: nothing

Recorded here so it is not re-investigated.

## Verification evidence

Prototyped in a scratch copy of the worktree, then reverted with
`git checkout --`. Observed with the fix applied:

```
quarterly-obs, freq=NULL explicit -> NULL   (was 12)
quarterly-obs, freq omitted       -> 12
monthly, freq omitted             -> 12
monthly, freq=4 explicit          -> 4
end-to-end freq=NULL on quarterly-obs yearmonth -> tbl_ts | n: 4
```

`devtools::test(filter = "tsibble")` passed 61/61 with both changes applied and
the `regular` argument removed, so finding 2 in particular causes no regression.

## Implementation steps

1. In the `feature/tsibble-1.7` worktree, add failing regression tests to
   `tests/testthat/test-tsibble.R` for finding 1: a `yearmonth`-indexed tsibble
   holding quarterly observations, asserting that `frequency = NULL` and an
   omitted `frequency` produce different trends (the same divergence the
   data-frame path already has). Run the focused file first to confirm failure.
2. Apply the finding 1 change in `R/tsibble.R`, `R/augment_trends.R`, and
   `R/detrend_series.R`. Confirm the new tests pass.
3. Remove the `regular` argument and add the finding 3 comment in one pass.
4. Format changed R code with AIR. Run the focused file, the full package suite,
   and `R CMD check --as-cran`.
5. Update PR #37: reply to each review comment, push the commit, and adjust the
   PR body if the verification numbers changed.

## Risks and mitigations

- Changing the `.tsibble_arguments()` signature touches three files. Keep the
  flag last in the parameter list so the existing positional call sites stay
  readable, and grep for every caller before editing.
- The finding 1 change alters behaviour only for explicit `frequency = NULL` on a
  tsibble whose index class disagrees with its observation spacing. That is the
  buggy case; a complete monthly or quarterly series is unaffected because
  detection returns the same value.
- Removing `regular` could in principle change `as_tsibble()` behaviour for a
  gapped input. The 61-test suite covers sparse monthly input and passed with
  the argument gone, which is the evidence that it does not.

## Stop condition

Stop when the new regression tests pass, the focused tsibble tests and full
package suite pass, `R CMD check --as-cran` reports no new findings, and the
final diff contains only these three changes plus their tests. The broader 1.7.0
release remains under `.omx/plans/trendseries-1.7.0-release.md`.