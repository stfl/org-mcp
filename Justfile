default:
    @just --list

# Run the ERT test suite.
test:
    @scripts/run-tests.sh

# Lint + test — the combined pre-commit entrypoint.
check: fmt lint test

fmt: elisp-autofmt shfmt

elisp-autofmt:
    @scripts/quiet.sh eask format elisp-autofmt

shfmt:
    @shfmt -i 4 -l -w scripts/quiet.sh scripts/run-tests.sh

# Run the full lint suite (silent on success; failing stage writes
# .lint-output.txt and exits non-zero, short-circuiting the rest).
lint: byte-compile elisp-lint org-lint shellcheck zizmor
    @echo "lint: OK"

# --- Individual lint stages -------------------------------------------------
# Each recipe is a thin wrapper around scripts/quiet.sh; silent on
# success, writes failing-stage output to .lint-output.txt on failure.

byte-compile:
    @scripts/quiet.sh eask recompile

elisp-lint:
    @rm -f ./*.elc
    @scripts/quiet.sh eask lint elisp-lint; rc=$?; rm -f ./*.elc; exit $rc

org-lint:
    @scripts/quiet.sh eask run script org-lint

shellcheck:
    @scripts/quiet.sh shellcheck scripts/quiet.sh scripts/run-tests.sh

zizmor:
    @scripts/quiet.sh zizmor .github/workflows/claude-code-review.yml .github/workflows/claude.yml .github/workflows/elisp-test.yml

# --- Passthrough ------------------------------------------------------------

act *args:
    act {{args}}
