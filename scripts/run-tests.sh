#!/usr/bin/env bash
# run-tests.sh — run the ERT suite; quiet on success, failing conditions
# on failure.  Always captures full output to .test-output.txt.
#
# Usage:    scripts/run-tests.sh
# Success:  prints the ERT summary line and notes the output file
# Failure:  prints the ERT failure block (from the first `Test ' line
#           through the summary and FAILED list), notes the output file,
#           and exits non-zero.

set -u

OUTFILE=".test-output.txt"

# A dependency too old for the code under test fails the suite in a
# hundred places that each look like something else, so say so first.
# Quiet on success, like the rest of this script.
if ! deps=$(eask exec emacs --batch -l scripts/check-deps.el 2>&1); then
    printf '%s\n' "$deps"
    exit 1
fi

output=$(eask run script test 2>&1)
rc=$?

printf '%s\n' "$output" >"$OUTFILE"

if [ "$rc" -ne 0 ]; then
    # Keep only the ERT failure report (backtraces, conditions, summary,
    # FAILED list), skipping any chatter emitted before the first test
    # block.
    if printf '%s\n' "$output" | grep -qE '^Test '; then
        printf '%s\n' "$output" | awk '/^Test / {seen=1} seen {print}'
    else
        printf '%s\n' "$output"
    fi
    printf 'Full output: %s\n' "$OUTFILE"
    exit "$rc"
fi

summary=$(printf '%s\n' "$output" | grep -E '^Ran [0-9]+ tests,' | tail -n1)
if [ -z "$summary" ]; then
    printf '%s\n' "$output"
    printf 'test: FAILED (no ERT summary found)\n'
    printf 'Full output: %s\n' "$OUTFILE"
    exit 1
fi
printf '%s\n' "$summary"
printf 'Full output: %s\n' "$OUTFILE"
