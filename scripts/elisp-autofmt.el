;;; elisp-autofmt.el --- Format Elisp files with elisp-autofmt -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage, from the repository root inside the Nix devshell:
;;
;;   emacs -Q --batch -l scripts/elisp-autofmt.el FILE...
;;
;; `just fmt' runs it on org-mcp.el.  The devshell provides Emacs 31
;; with elisp-autofmt, and the Python its formatter runs on, so every
;; machine gets the same layout.
;;
;; The layout depends on the Emacs version and on the definitions the
;; formatter knows, so this script fixes both:
;;
;; - Any Emacs but 31 is refused, with a hint to use the devshell.
;; - cl-macs is loaded and named in `elisp-autofmt-load-packages-local',
;;   so the formatter indents cl-defun, cl-flet and cl-labels as they
;;   declare.
;; - Directory-local variables are off.  Batch Emacs refuses
;;   .dir-locals.el as a whole anyway, because it holds a variable not
;;   marked safe, so the code is laid out for Emacs's default
;;   `fill-column' of 70.
;;
;; The formatter keeps its cache in .elisp-autofmt-cache/.

;;; Code:

(defvar elisp-autofmt-cache-directory)
(defvar elisp-autofmt-load-packages-local)
(declare-function elisp-autofmt-buffer "elisp-autofmt")

(unless (and (= emacs-major-version 31)
             (require 'elisp-autofmt nil t))
  (error
   "Needs Emacs 31 with elisp-autofmt; run: nix develop --command just fmt"))
(require 'cl-macs)

(setq elisp-autofmt-cache-directory
      (expand-file-name ".elisp-autofmt-cache"))

(dolist (file command-line-args-left)
  (let ((enable-local-variables nil))
    (with-current-buffer (find-file-noselect file)
      (setq-local elisp-autofmt-load-packages-local '("cl-macs"))
      (elisp-autofmt-buffer)
      (save-buffer)
      (kill-buffer))))

;; The files are formatted; keep Emacs from visiting them as well.
(setq command-line-args-left nil)

;;; elisp-autofmt.el ends here
