;;; check-deps.el --- Refuse a dependency too old to test against -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage, from the repository root:
;;
;;   eask exec emacs --batch -l scripts/check-deps.el
;;
;; `scripts/run-tests.sh' runs it first.  It exits non-zero, naming the
;; fix, when mcp-server-lib is missing or older than the version Eask
;; requires.
;;
;; The case it catches: `eask install-deps' leaves an installed MELPA
;; snapshot in place when the version in Eask rises, because a date
;; version like 20260319.1344 compares greater than 0.4.0.  A checkout
;; whose .eask predates the requirement therefore keeps the old
;; mcp-server-lib, and the suite fails in bulk, in places that each
;; look like something else.
;;
;; The probe is `mcp-server-lib-server-registered-p', which arrived in
;; 0.4.0, the version Eask requires -- not
;; `mcp-server-lib-register-server', which is only 0.3.0 and would let
;; a 0.3.0 copy through to fail later in the ERT helpers.
;;
;; The require is soft so that a missing mcp-server-lib reaches the
;; message below.  A hard require signals `file-missing' first, and the
;; script prints a backtrace instead of the remedy -- which is the
;; failure it exists to replace.  Byte-compilation does not care either
;; way: the require sits inside the `cond', so the compiler never
;; evaluates it.

;;; Code:

(defconst check-deps--stale
  (concat
   "The installed mcp-server-lib is older than the version Eask "
   "requires.\n"
   "eask leaves a MELPA date version in place when that requirement "
   "rises, so remove it and reinstall:\n"
   "  rm -rf .eask/*/elpa/mcp-server-lib-*\n"
   "  just install-deps")
  "What to tell someone whose mcp-server-lib is too old.")

(defconst check-deps--missing
  "mcp-server-lib is not installed.  Run: just install-deps"
  "What to tell someone who has no mcp-server-lib at all.")

(cond
 ((not (require 'mcp-server-lib nil t))
  (message "%s" check-deps--missing)
  (kill-emacs 1))
 ((not (fboundp 'mcp-server-lib-server-registered-p))
  (message "%s" check-deps--stale)
  (kill-emacs 1)))

(provide 'check-deps)
;;; check-deps.el ends here
