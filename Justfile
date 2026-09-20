default:
    @just --list

# Run the ERT test suite.
test:
    @scripts/run-tests.sh

# Lint + test — the combined pre-commit entrypoint.
check: fmt lint test

fmt: elisp-autofmt shfmt

# Format org-mcp.el with the devshell's Emacs and elisp-autofmt.
# The script is silent unless it fails, and its message is the one to
# read, so it runs without scripts/quiet.sh.
elisp-autofmt:
    @emacs -Q --batch -l scripts/format-elisp.el org-mcp.el

shfmt:
    @shfmt -i 4 -l -w scripts/quiet.sh scripts/run-tests.sh

# Run the full lint suite (silent on success; failing stage writes
# .lint-output.txt and exits non-zero, short-circuiting the rest).
lint: install-deps byte-compile elisp-lint script-compile org-lint shellcheck zizmor
    @echo "lint: OK"

# --- Individual lint stages -------------------------------------------------
# Each recipe is a thin wrapper around scripts/quiet.sh; silent on
# success, writes failing-stage output to .lint-output.txt on failure.

install-deps:
    @scripts/quiet.sh eask install-deps

alias compile := byte-compile
byte-compile: install-deps
    @scripts/quiet.sh eask recompile

elisp-lint:
    @rm -f ./*.elc
    @scripts/quiet.sh eask lint elisp-lint; rc=$?; rm -f ./*.elc; exit $rc

# Byte-compile the scripts, which the package linters do not see, and
# take any warning as a failure.
script-compile:
    @scripts/quiet.sh emacs -Q --batch --eval '(progn (require (quote bytecomp)) (setq byte-compile-error-on-warn t) (dolist (file (file-expand-wildcards "scripts/*.el")) (unless (byte-compile-file file) (kill-emacs 1))))'; rc=$?; rm -f scripts/*.elc; exit $rc

org-lint:
    @scripts/quiet.sh eask run script org-lint

shellcheck:
    @scripts/quiet.sh shellcheck scripts/quiet.sh scripts/run-tests.sh

zizmor:
    @scripts/quiet.sh zizmor .github/workflows/claude-code-review.yml .github/workflows/claude.yml .github/workflows/elisp-test.yml

# --- Passthrough ------------------------------------------------------------

act *args:
    act {{args}}
