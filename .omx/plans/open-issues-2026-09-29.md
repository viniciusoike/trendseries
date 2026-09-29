# Open issues plan: trendseries

## Requirements summary

Review the four open issues on 2026-09-29. All describe useful data preparation gaps. Keep the public APIs small, preserve existing indexing behavior, and add no dependencies. This plan covers implementation in separate, reviewable changes; it does not authorize implementation.

## Assessment and order

| Order | Issue | Decision | Reason |
| --- | --- | --- | --- |
| 1 | [#31: group-specific index bases](https://github.com/viniciusoike/trendseries/issues/31) | Do | `index_series()` accepts one base period for all groups. Its existing group loop can resolve a base for each group (`R/index_series.R:69-91`). |
| 2 | [#30: expanding sum and chain](https://github.com/viniciusoike/trendseries/issues/30) | Do, with a precise result definition | `window = "ytd"` already uses expanding statistics within each year (`R/roll_series.R:543-553`, `R/roll_series.R:597-649`). An all-sample form fits that path. `chain` returns cumulative *change*, so the IPCA level requested in the issue is `100 * (1 + roll_chain_all)` when rates are in decimal form, or `100 * (1 + roll_chain_all / 100)` with `percent = TRUE`. |
| 3 | [#29: change in a level series](https://github.com/viniciusoike/trendseries/issues/29) | Do | `chain` compounds input rates; it cannot compute change from levels (`R/roll_series.R:12-15`). The rolling data-frame path rejects absent calendar periods (`R/converters.R:381-405`), so this needs date-key matching rather than another rolling statistic. |
| 4 | [#32: lower-frequency aggregation](https://github.com/viniciusoike/trendseries/issues/32) | Do | No exported function reduces a data-frame series to calendar months, quarters, or years (`NAMESPACE:3-12`). The package already uses calendar period keys (`R/converters.R:173-189`, `R/converters.R:277-292`). |

## Acceptance criteria and implementation steps

### 1. `index_series()` (#31)

1. Write failing tests in `tests/testthat/test-index_series.R`. Cover two groups with distinct base dates, a month-start base date against month-end observations, invalid or conflicting group base dates, and a leading missing value in one group. Existing tests establish the current earliest non-missing rule (`tests/testthat/test-index_series.R:1-14`).
2. In `R/index_series.R`, accept `base_period = "base_date"` when `base_date` is an existing `Date` column constant and nonmissing within each group. Resolve each group's value through the existing `.resolve_base_period()` and coverage checks (`R/index_series.R:69-91`, `R/index_series.R:260-276`). Keep year and Date inputs unchanged. Do not add a keyed data-frame input until a real use case requires its join rules.
3. When `base_period = NULL`, keep the earliest non-missing result but warn if the earliest dated value is missing and the reference moves later. Name the group, value column, and both dates. Preserve `.quiet` as a control for informational messages, not correctness warnings (`R/index_series.R:12-24`, `R/index_series.R:288-296`).
4. Update roxygen help, the indexing vignette, and `NEWS.md`.

**Pass condition:** Each group indexes to 100 at its requested period; results retain input rows and order; the issue's leading-`NA` example warns and retains today's values. Existing index tests pass.

### 2. `window = "all"` (#30)

1. Write failing tests in `tests/testthat/test-roll_series.R` and `tests/testthat/test-augment_rolling.R` for all-sample sum and chain, decimal and percentage rates, missing values with each `na_rm` setting, grouped data, output names, and invalid combinations.
2. Extend the existing string-window validation and dispatch in `R/roll_series.R:245-269` and `R/roll_series.R:463-473`. Reuse `.expanding_stat()` over the whole vector (`R/roll_series.R:597-649`); the grouped augmentation already calls the core separately for each group (`R/augment_rolling.R:366-381`). Update its short-group guard for `"all"` (`R/augment_rolling.R:277-295`). Name outputs `sum_all` and `chain_all` in `roll_series()`, and `roll_sum_all` and `roll_chain_all` in `augment_rolling()`. Warn when non-right `align` is supplied because an expanding window has no alignment choice (`R/roll_series.R:298-319`).
3. Document that `chain_all` is cumulative change from the start of the available sample, with the first return included. Show the explicit conversion to a base-100 price level; a new index-level statistic is outside this issue's minimal scope.

**Pass condition:** A constant monthly rate yields the expected cumulative compound change at each row and resets only for `"ytd"`; `"all"` never resets. Grouped results stay within groups, and all existing rolling tests pass.

### 3. `augment_change()` (#29)

1. Write tests first in `tests/testthat/test-augment_change.R` for monthly and quarterly year-over-year changes, a one-period lag, absent prior calendar periods, month-end and leap-day dates, unsorted/interleaved groups, duplicate periods, and zero or invalid input values.
2. Add a data-frame function in `R/augment_change.R` with `date_col`, `value_col`, `group_cols`, `frequency`, `lag`, `method`, and `suffix`. Support `method = "percent"` (`100 * (x / prior - 1)`), `"log"` (`100 * (log(x) - log(prior))`), or `"difference"` (`x - prior`), one method per call. Default `lag` to the detected observations per year within each group; permit an explicit positive integer lag. Initially support calendar frequencies 1, 2, 4, and 12, matching `.frequency_unit()` (`R/converters.R:173-189`). Allow `frequency` to override detection on sparse series (`R/converters.R:740-817`).
3. Match normalized calendar periods *within each group* instead of lagging row positions. Leave a result `NA` when the prior period is absent or either value is missing. Reject duplicate periods. Define zero denominators for percent change and nonpositive values for log change as `NA` with a warning. Preserve all original rows, dates, order, and columns; use `.index_group_indices()` and `.unique_column_name()` (`R/index_series.R:242-257`, `R/utils.R:306-324`).
4. Update generated help, `NAMESPACE`, `_pkgdown.yml`, `README.Rmd`/`README.md`, and `NEWS.md`.

**Pass condition:** Removing a month does not turn a 12-month comparison into an 11-month comparison; valid comparisons match hand-calculated percentages, log changes, and differences. Existing package tests pass.

### 4. `aggregate_series()` (#32)

1. Write failing tests for daily to monthly `last` using the final *observed* trading date, monthly to quarterly `sum`/`mean`, all six requested summaries, interleaved groups, missing values, partial boundary periods, duplicate dates, and input order independence.
2. Add a data-frame function with `date_col`, `value_col`, `group_cols`, `frequency = "month" | "quarter" | "year"`, `summary = "last" | "first" | "mean" | "sum" | "max" | "min"`, and `na_rm = FALSE`. Use calendar-period keys and sort within each group before `first`/`last`; choose the first day of each target period as the output date, consistent with the package's calendar normalization (`R/converters.R:277-292`). With `na_rm = FALSE`, a selected missing first/last value stays missing and an aggregate containing `NA` stays missing. Empty/all-missing groups must not yield a misleading zero, `Inf`, or `-Inf`.
3. Reject missing dates and repeated dates within a group; preserve group columns; return one row per observed group-period. Document that boundary periods may be partial and that `last` is the last observed close, not necessarily a close on the calendar's final day. Update generated help, `NAMESPACE`, `_pkgdown.yml`, `README.Rmd`/`README.md`, and `NEWS.md`.

**Pass condition:** The issue's daily Ibovespa example returns one monthly value equal to the last dated close in each month; monthly flows sum correctly to quarters; grouped values never mix. Existing package tests pass.

## Risks and verification

- Calendar frequency detection can misread sparse series (`R/converters.R:774-817`). Provide an explicit override for `augment_change()` and test sparse inputs; do not silently use row offsets.
- A new leading-`NA` warning may affect callers that turn warnings into errors. Keep the numeric result unchanged and document the warning.
- `"all"` must mean sample-to-date growth, not a base-100 index. Verify the documented conversion with a direct example.
- For each change, run the focused test file first, then format final R code with AIR, regenerate roxygen files/README when touched, and run the package test suite and `R CMD check`. Inspect generated help and README output. Stop when those checks pass or report the exact unresolved check.
