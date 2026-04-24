#!/usr/bin/env bash
# quiet.sh — run a lint stage silently; always capture its output to
# .lint-output.txt, and on failure point the user at the file.
#
# Usage:   scripts/quiet.sh CMD [ARGS...]
# Success: exits 0 with no output.
# Failure: prints "lint failed: output at .lint-output.txt" and
#          propagates the command's exit code.

set -u

if [ "$#" -lt 1 ]; then
    printf 'usage: %s CMD [ARGS...]\n' "$0" >&2
    exit 2
fi

OUTFILE=".lint-output.txt"

"$@" >"$OUTFILE" 2>&1
rc=$?
if [ "$rc" -ne 0 ]; then
    printf 'lint failed: output at %s\n' "$OUTFILE"
    exit "$rc"
fi
