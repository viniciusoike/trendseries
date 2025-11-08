# CRAN Submission Helper Script
# Run this before submitting to CRAN

library(devtools)
library(rhub)

# 1. Final local check
cat("Running final local check...\n")
check(cran = TRUE)

# 2. Test on win-builder (development and release)
cat("\nSubmitting to win-builder...\n")
check_win_devel()
check_win_release()

# 3. Test on R-hub
cat("\nSubmitting to R-hub...\n")
check_rhub()

# 4. Spell check
cat("\nRunning spell check...\n")
spell_check()

# 5. Check URLs
cat("\nChecking URLs...\n")
urlchecker::url_check()

cat("\n=== Pre-submission checklist ===\n")
cat("1. All checks pass: R CMD check --as-cran\n")
cat("2. Win-builder: devel and release\n")
cat("3. R-hub: multiple platforms\n")
cat("4. URLs validated\n")
cat("5. Spelling checked\n")
cat("\nReview results, then submit with: devtools::submit_cran()\n")
