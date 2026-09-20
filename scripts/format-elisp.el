;;; format-elisp.el --- Format Elisp files with elisp-autofmt -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage, from the repository root inside the Nix devshell:
;;
;;   emacs -Q --batch -l scripts/format-elisp.el FILE...
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
;; - `fill-column' is 70, the width the code is laid out for, which
;;   org-mcp.el also carries as a file-local variable for an editing
;;   session.  Batch Emacs refuses .dir-locals.el as a whole, because
;;   it holds a variable not marked safe, so local variables are off
;;   here and the width is set directly.
;;
;; A file that is not formatted, because it is missing or because
;; elisp-autofmt complains about it, exits non-zero.
;;
;; The formatter keeps its cache in .elisp-autofmt-cache/.

;;; Code:

;; What went wrong is in the error message; a backtrace of this script
;; only buries it.
(setq backtrace-on-error-noninteractive nil)

(defvar elisp-autofmt-cache-directory)
(defvar elisp-autofmt-load-packages-local)
(declare-function elisp-autofmt-buffer "elisp-autofmt")

(unless (and (= emacs-major-version 31)
             (require 'elisp-autofmt nil t))
  (error
   "Needs Emacs 31 with elisp-autofmt; run: nix develop --command just fmt"))
(require 'cl-macs)
(require 'seq)

(setq elisp-autofmt-cache-directory
      (expand-file-name ".elisp-autofmt-cache"))

(defconst org-mcp-format--passing-messages
  '("elisp-autofmt: using an out of date cache "
    "elisp-autofmt: unable to generate "
    "elisp-autofmt: unable to find library "
    "elisp-autofmt: unable to find file "
    "elisp-autofmt: no definitions loaded for "
    "elisp-autofmt: not a regular file ")
  "Beginnings of the elisp-autofmt messages that do not fail a format.
Each one is about a source of definitions elisp-autofmt reads
indentation from, such as a cache it could not write, and narrows the
definitions the formatter goes by; the buffer is still formatted.  A
cache directory that cannot be written, for instance, leaves an
earlier cache to format from.  When the definitions run out entirely,
elisp-autofmt says \"not formatted\" instead, which is not here and so
fails.")

(defun org-mcp-format--buffer ()
  "Format the current buffer, signaling when elisp-autofmt complains.
`elisp-autofmt-buffer' reports a failure, such as unbalanced
parentheses or a formatter that will not run, by `message' and leaves
the buffer as it is.  Its complaints all begin with \"elisp-autofmt:
\", so every one that `org-mcp-format--passing-messages' does not
cover is collected here and raised, which is what makes `just fmt'
fail on a file it did not format."
  (let ((complaints nil)
        (message-fn (symbol-function 'message)))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (when format-string
                   (let ((text (apply #'format-message format-string args)))
                     (when (and (string-prefix-p "elisp-autofmt: " text)
                                (not
                                 (seq-some
                                  (lambda (passing)
                                    (string-prefix-p passing text))
                                  org-mcp-format--passing-messages)))
                       (push text complaints))))
                 (apply message-fn format-string args))))
      (elisp-autofmt-buffer))
    (when complaints
      (error "%s" (mapconcat #'identity (nreverse complaints) "\n")))))

(dolist (file command-line-args-left)
  (unless (file-readable-p file)
    (error "Cannot read file: %s" file))
  (let ((enable-local-variables nil))
    (with-current-buffer (find-file-noselect file)
      (setq-local elisp-autofmt-load-packages-local '("cl-macs"))
      (setq-local fill-column 70)
      (org-mcp-format--buffer)
      (let ((save-silently t))
        (save-buffer))
      (kill-buffer))))

;; The files are formatted; keep Emacs from visiting them as well.
(setq command-line-args-left nil)

;;; format-elisp.el ends here
