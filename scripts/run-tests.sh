#!/usr/bin/env bash
# run-tests.sh — run the ERT suite; quiet on success, failing conditions
# on failure.  `ert-quiet' is enabled in org-mcp-test.el itself, so ERT
# already emits only the summary line on success and the failing tests'
# backtraces + condition blocks on failure.  This wrapper additionally
# filters intra-test chatter from dependencies (e.g. "MCP metrics: …"
# lines) that ERT doesn't control.
#
# Usage:    scripts/run-tests.sh
# Success:  prints only the ERT summary line, e.g.
#             Ran 214 tests, 214 results as expected, 0 unexpected (…)
# Failure:  prints the ERT failure block (from the first `Test ' line
#           through the summary and FAILED list) and exits non-zero.

set -u

output=$(eask run script test 2>&1)
rc=$?

if [ "$rc" -ne 0 ]; then
    # Keep only the ERT failure report (backtraces, conditions, summary,
    # FAILED list), skipping any chatter emitted before the first test
    # block.
    if printf '%s\n' "$output" | grep -qE '^Test '; then
        printf '%s\n' "$output" | awk '/^Test / {seen=1} seen {print}'
    else
        printf '%s\n' "$output"
    fi
    exit "$rc"
fi

summary=$(printf '%s\n' "$output" | grep -E '^Ran [0-9]+ tests,' | tail -n1)
if [ -z "$summary" ]; then
    printf '%s\n' "$output"
    printf 'test: FAILED (no ERT summary found)\n'
    exit 1
fi
printf '%s\n' "$summary"
