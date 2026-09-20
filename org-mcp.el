;;; org-mcp.el --- MCP server for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis, Stefan Lendl

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;;         Stefan Lendl <git@stfl.dev>
;; Keywords: convenience, files, matching, outlines
;; Version: 0.9.0
;; Package-Requires: ((emacs "30.1") (mcp-server-lib "0.4.0") (org-ql "0.9"))
;; Homepage: https://github.com/laurynas-biveinis/org-mcp

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a Model Context Protocol (MCP) server for
;; Org-mode.

;;; Code:

(require 'cl-lib)
(require 'mcp-server-lib)
(require 'org)
(require 'org-id)
(require 'org-ql)
(require 'org-clock)
(require 'url-util)

(defcustom org-mcp-allowed-files nil
  "List of paths to Org files that can be accessed via MCP.
Entries may be absolute or relative paths.  Relative paths are
resolved against `org-directory', matching the behavior of
`org-agenda-files'.  Tilde expansion (`~/...') and environment
variable substitution apply.  Absolute paths pass through
unchanged, so absolute and relative entries are interchangeable.

When nil (the default), org-mcp falls back to `org-agenda-files',
so an existing Org-mode configuration works out of the box.  Set
this variable explicitly to expose a different (or narrower) set
of files to MCP.

Each entry names a file.  A directory entry is ignored: it does not
make the files under it reachable.  To let calls reach files under a
directory, see `org-mcp-file-scope-override'."
  :type '(repeat file)
  :group 'org-mcp)

(defcustom org-mcp-file-scope-override nil
  "Whether a call may reach an Org file outside the allowed files.
The allowed files are the ones `org-mcp-allowed-files' resolves to.
A call names another file by passing its path, and this setting
decides whether that is permitted:

  nil          Refuse.  Only the allowed files are reachable.  This
               is the default.

  A list of    Permit a file under one of these directories, the
  directories  override roots.  Relative roots resolve against
               `org-directory'.

  t            Permit any Org file.  Every Org file the Emacs
               process can read and write becomes readable and
               writable by MCP clients.  Prefer a list of roots.

The permission covers reading and writing alike and applies only to
the call that names the file; nothing carries over to later calls.
A file an ID resolves to, or the file of the running clock, is not
named by the call and stays within the allowed files.

A file reachable this way is an existing local file ending in
`.org' or `.org_archive'; an encrypted `.org.gpg' file is not.
A path that is remote (TRAMP) as written, once `.', `..' and `~'
are expanded, or at the end of a symlink is refused before TRAMP
can open a connection for it, and a remote root is ignored.  Symlinks,
including roots that are symlinks, are resolved before the root
check, so a symlink pointing out of a root is refused."
  :type
  '(choice
    (const :tag "Refuse" nil)
    (const :tag "Permit any Org file" t)
    (repeat :tag "Permit under these directories" directory))
  :group 'org-mcp)

(defcustom org-mcp-node-field-lists
  '((reference link) (outline title todo level link))
  "Named lists of node fields a call can ask for by name.
A call says how much of a node it wants in its `fields' parameter,
either as a list of field names or, more shortly, as the name of a
list here.  Each entry is (NAME FIELD...), NAME the name a call
sends as a string and FIELD a field of `org-mcp--node-fields'; a
call naming a list that is not here, or a list naming a field that
does not exist, is refused.

The two lists it starts with are the two shapes the surface itself
has a word for: `reference' is a node carrying its link alone, the
smallest thing a later call can be made from, and `outline' adds
what it takes to show the node in an outline.  They are a starting
point rather than the set: a workflow that asks the same question
repeatedly gives that question a name here, and the whole point of
the setting is that org-mcp does not decide which names exist."
  :type '(alist :key-type symbol :value-type (repeat symbol))
  :group 'org-mcp)

(defcustom org-mcp-clock-continuous-threshold 30
  "Max minutes since last clock-out for continuous clocking.
When `org-clock-continuously' is non-nil and a new clock-in occurs
within this many minutes of the last clock-out, the new clock starts
at the previous clock's end time."
  :type 'integer
  :group 'org-mcp)

(defcustom org-mcp-ql-extra-properties nil
  "Alist of extra properties to include in org-ql query results.
Each entry is (KEY . FUNCTION) where KEY is a symbol used as the
JSON key and FUNCTION is called with no arguments at point during
`org-mcp--ql-node-at-point'.  Non-nil return values are included
in the result alist."
  :type '(alist :key-type symbol :value-type function)
  :group 'org-mcp)

(defcustom org-mcp-query-inbox-fn nil
  "Function returning an org-ql sexp for inbox items.
Called with no arguments.  When nil, the query-inbox tool is disabled."
  :type '(choice (const :tag "Disabled" nil) function)
  :group 'org-mcp)

(defcustom org-mcp-query-backlog-fn nil
  "Function returning an org-ql sexp for backlog items.
Called with one optional TAG-FILTER argument, which is either nil
or an org-ql sexp of the form `(tags TAG)' built from the tag
string supplied by the MCP caller at call time.
When nil, the query-backlog tool is disabled."
  :type '(choice (const :tag "Disabled" nil) function)
  :group 'org-mcp)

(defcustom org-mcp-query-next-fn nil
  "Function returning an org-ql sexp for next action items.
Called with one optional TAG-FILTER argument, which is either nil
or an org-ql sexp of the form `(tags TAG)' built from the tag
string supplied by the MCP caller at call time.
When nil, the query-next tool is disabled."
  :type '(choice (const :tag "Disabled" nil) function)
  :group 'org-mcp)

(defcustom org-mcp-query-sort-fn nil
  "Sort comparator for GTD query tools.
Passed as the `:sort' argument to `org-ql-select'.
When nil, no sorting is applied."
  :type '(choice (const :tag "No sorting" nil) function)
  :group 'org-mcp)

(defconst org-mcp--server-id "org-mcp"
  "Server ID for org-mcp MCP server registration.")

(defconst org-mcp-version
  (eval-when-compile
    (require 'lisp-mnt)
    ;; `byte-compile-current-file' names the file while the compiler
    ;; runs.  Loading from source binds it only when some dependency has
    ;; already pulled in bytecomp, which is not ours to rely on, so read
    ;; it defensively.
    (lm-version
     (or (bound-and-true-p byte-compile-current-file)
         load-file-name
         buffer-file-name)))
  "Version org-mcp reports as `serverInfo.version' in the handshake.
Read from this file's `Version:' header, at compile time when the
package is byte-compiled, so it cannot drift from the package
metadata the way a second copy of the string would.")

;; Error handling helpers
;;
;; A refusal a client acts on carries its class as a prefix on the
;; message.  The transport has no error code of any kind:
;; `mcp-server-lib-tool-throw' carries a string, and the org://{link}
;; resource re-signals that same string as JSON-RPC `invalid-params', so
;; a marker inside the message is the only thing both paths keep.  The
;; prefixes live in the helpers below and nowhere else, so that no call
;; site can raise a conflict that is not marked as one.  The vocabulary
;; is published contract: docs/writing.org, "How a refusal is classed".

(defconst org-mcp--refusal-conflict "conflict: "
  "Marker on a refusal saying the client's belief is stale.
The recovery is to read the file again and re-plan; sending the same
call again refuses it again.  Only `org-mcp--tool-conflict-error'
writes it.")

(defconst org-mcp--refusal-blocked "blocked: "
  "Marker on a refusal Org itself made, such as a vetoed TODO change.
The recovery is to tell the user why Org said no.  Only
`org-mcp--tool-blocked-error' writes it.")

(defun org-mcp--id-not-found-error (id)
  "Throw error for ID not found.
The refusal is unmarked, the validation class: an ID org-mcp cannot
resolve is as likely a heading that has gone, a conflict, as one a
client invented, and nothing here tells the two apart."
  (mcp-server-lib-tool-throw (format "Cannot find ID '%s'" id)))

(defun org-mcp--tool-validation-error (message &rest args)
  "Throw validation error MESSAGE with ARGS for tool operations.
Validation is the unmarked default class: the call itself was
malformed, and the recovery is to correct it and send it again."
  (mcp-server-lib-tool-throw (apply #'format message args)))

(defun org-mcp--tool-conflict-error (message &rest args)
  "Throw conflict refusal MESSAGE with ARGS, marked `conflict:'.
A conflict says the file is not as the client believed it to be, so
the recovery is to read it again and re-plan rather than to retry."
  (mcp-server-lib-tool-throw
   (concat org-mcp--refusal-conflict (apply #'format message args))))

(defun org-mcp--tool-blocked-error (message &rest args)
  "Throw Org-veto refusal MESSAGE with ARGS, marked `blocked:'.
Org refused the change itself, so neither reading again nor
correcting the call helps; the user decides what to do next."
  (mcp-server-lib-tool-throw
   (concat org-mcp--refusal-blocked (apply #'format message args))))

(defun org-mcp--state-mismatch-error (expected found context)
  "Throw a conflict refusal for a precondition that no longer holds.
EXPECTED is the expected value, FOUND is the actual value,
CONTEXT describes what is being compared."
  (org-mcp--tool-conflict-error
   "%s mismatch: expected '%s', found '%s'"
   context expected found))

(defun org-mcp--saved-then-failed-error (what err)
  "Throw an error for a save that wrote the file and then failed.
WHAT names, as a clause, the change the file holds, so that a client
reading the message repeats neither the change nor the save.  ERR is
the error a function the save ran, such as one on `after-save-hook',
signalled."
  (org-mcp--tool-validation-error
   "%s, but a function run by the save failed: %s"
   what (error-message-string err)))

(defun org-mcp--tool-file-access-error (locator &optional hint)
  "Throw file access error for tool operations.
LOCATOR is the link or path the call sent, naming the file it may not
reach.  HINT, when non-nil, is a sentence appended to the message."
  (mcp-server-lib-tool-throw
   (concat
    (format "'%s': the referenced file not in allowed list" locator)
    (and hint (concat ".  " hint)))))

;; Helpers

(cl-defun org-mcp--file-buffer-context (file-path)
  "Return canonical buffer context for FILE-PATH.
The result is a plist with:

- `:buffer'     the canonical visited buffer
- `:existing-p' non-nil when the buffer was already visiting FILE-PATH
- `:modified-p' non-nil when that buffer was already modified before
                org-mcp touched it

If no buffer is visiting FILE-PATH yet, the buffer is opened with
`find-file-noselect'."
  (let* ((existing-buf (find-buffer-visiting file-path))
         (buf (or existing-buf (find-file-noselect file-path))))
    (list
     :buffer buf
     :existing-p (not (null existing-buf))
     :modified-p
     (with-current-buffer buf
       (buffer-modified-p)))))

(defun org-mcp--get-file-buffer (file-path)
  "Return the canonical visited buffer for FILE-PATH."
  (plist-get (org-mcp--file-buffer-context file-path) :buffer))

(defun org-mcp--read-file (file-path)
  "Read and return the current canonical contents of FILE-PATH."
  (with-current-buffer (org-mcp--get-file-buffer file-path)
    (save-restriction
      (widen)
      (buffer-string))))

(defun org-mcp--paths-equal-p (path1 path2)
  "Return t if PATH1 and PATH2 refer to the same file.
Handles symlinks and path variations by normalizing both paths."
  (string= (file-truename path1) (file-truename path2)))

(defun org-mcp-allowed-files ()
  "Return the effective allowed-files list (the function form).
Mirrors the Emacs idiom of `org-agenda-files': the same symbol
serves as a defcustom holding the user-configured list and as a
function returning the resolved list at call time.

When the variable `org-mcp-allowed-files' is non-nil, its entries
are returned verbatim (entries may be relative; expansion happens
in `org-mcp--expanded-allowed-files').  When nil, the result of
`(org-agenda-files t)' is returned, which uniformly handles list,
string-pointing-to-file, and directory forms of `org-agenda-files'
and yields fully absolute paths."
  (if org-mcp-allowed-files
      org-mcp-allowed-files
    (org-agenda-files t)))

(defvar org-mcp--file-set 'allowed
  "The files the running call works on.
The symbol `allowed' stands for the allowed files, where every call
works unless it names a set of files itself.  For a call that does,
`org-mcp--with-file-set' binds that set here as a list of files, and
`org-mcp--expanded-allowed-files' returns it in place of the allowed
files.  While `org-mcp--named-file-set', which that macro calls,
builds the set, it binds the allowed files here instead, computed
once.  Nothing else binds this variable, and both bind it with
`let', so the set ends with the call, even when the call fails, and
no later call sees it.")

(defun org-mcp--expanded-allowed-files ()
  "Return the allowed files, each made absolute.
While a call runs over a set of files it names, this returns that
set instead, from `org-mcp--file-set': the named set replaces the
allowed files for that call.

Pulls the source list from the function `org-mcp-allowed-files'
(which falls back to `org-agenda-files' when the variable
`org-mcp-allowed-files' is nil), then resolves relative entries
against `org-directory' exactly as `org-agenda-files' does.
Absolute entries pass through after tilde and environment variable
expansion.

The result holds files only.  A directory entry is dropped, so it
never makes the files under it reachable, neither through
`org-mcp--find-allowed-file' nor when the set is bound to
`org-agenda-files', where Org would expand it.  Directory entries of
`org-agenda-files' arrive here already expanded into their files by
the function `org-agenda-files'."
  (if (listp org-mcp--file-set)
      org-mcp--file-set
    (cl-remove-if
     #'file-directory-p
     (mapcar
      (lambda (f)
        (expand-file-name f org-directory))
      (org-mcp-allowed-files)))))

(defun org-mcp--local-file-name (name &optional dir)
  "Return NAME as an absolute local file name, or nil when it is remote.
NAME is expanded against DIR, or `default-directory' when DIR is
nil, with file name handlers disabled, so TRAMP takes no part.
Expansion collapses `.', `..', `~' and a relative name the way
every later file operation would, and `file-remote-p' checks the
result: `/tmp/../ssh:host:/x.org' is remote only once collapsed.

A `/:' quote on NAME is removed first.  Without handlers `/:' would
be an ordinary directory, while every later operation, handlers
enabled, removes it and reaches another file.  A name still quoted
after expansion is refused, as NAME is then no plain file name.
Callers use the returned name from here on, never NAME itself."
  (let ((expanded
         (let ((file-name-handler-alist nil))
           (expand-file-name (file-name-unquote name) dir))))
    (unless (or (file-name-quoted-p expanded t)
                (file-remote-p expanded))
      expanded)))

(defun org-mcp--local-truename (name &optional dir)
  "Return the truename of NAME, or nil when NAME or its target is remote.
NAME is made absolute by `org-mcp--local-file-name' against DIR.
Symlinks are then followed with file name handlers disabled, so a
local link whose target is a TRAMP name is never handed to TRAMP.
The resolved name is refused when it is remote, or quoted with
`/:', since a quoted link target is followed no further here but
would be once handlers are back."
  (when-let* ((local (org-mcp--local-file-name name dir))
              (truename
               (let ((file-name-handler-alist nil))
                 (file-truename local))))
    (unless (or (file-name-quoted-p truename t)
                (file-remote-p truename))
      truename)))

(defun org-mcp--override-roots ()
  "Return the roots of `org-mcp-file-scope-override', each made absolute.
Relative roots resolve against `org-directory'.  A remote root is
dropped before any file operation on it; it never permits anything.
Returns nil unless the setting is a list of directories."
  (when (consp org-mcp-file-scope-override)
    (delq
     nil
     (mapcar
      (lambda (root)
        (org-mcp--local-file-name root org-directory))
      org-mcp-file-scope-override))))

(defun org-mcp--org-file-name-p (name)
  "Return non-nil when NAME ends in `.org' or `.org_archive'.
These are the Org files a scope override reaches.  Case matters."
  (let ((case-fold-search nil))
    (string-match-p "\\.org\\(?:_archive\\)?\\'" name)))

(defun org-mcp--override-permits-p (name truename)
  "Return non-nil when `org-mcp-file-scope-override' permits TRUENAME.
TRUENAME is the local truename, from `org-mcp--local-truename', of
the file or directory a call names as NAME.  NAME must be absolute.
Under a list of roots TRUENAME must lie inside one of them, each
resolved the same way at every call; under t any TRUENAME is
permitted, and under nil none is."
  (and org-mcp-file-scope-override
       (file-name-absolute-p name)
       (or (eq org-mcp-file-scope-override t)
           (cl-some
            (lambda (root)
              (when-let* ((root-truename
                           (org-mcp--local-truename root)))
                (file-in-directory-p truename root-truename)))
            (org-mcp--override-roots)))))

(defun org-mcp--find-allowed-file (filename &optional named)
  "Return the absolute path of FILENAME when a call may reach it, else nil.
This is the one place that decides whether a path is reachable.

FILENAME is resolved by `org-mcp--local-truename' first, and refused
when it, or what it points to, is remote (TRAMP), before TRAMP can
open a connection for it.  A FILENAME in the allowed files is
reachable, and the expanded allowed-files entry is returned.

NAMED non-nil means the call itself names FILENAME, which makes it
a scope override when FILENAME lies outside the allowed files.
`org-mcp-file-scope-override' then decides: its truename must end
in `.org' or `.org_archive' and be an existing regular file, and
`org-mcp--override-permits-p' must permit it.  A permitted FILENAME
is returned as its truename.  Without NAMED, as for a file an ID
resolves to, only the allowed files are reachable."
  (when-let* ((truename (org-mcp--local-truename filename)))
    (if-let* ((found
               (cl-find
                truename
                (org-mcp--expanded-allowed-files)
                :test #'org-mcp--paths-equal-p)))
        (expand-file-name found)
      (when (and named
                 (org-mcp--org-file-name-p truename)
                 (org-mcp--override-permits-p filename truename)
                 (file-regular-p truename))
        truename))))

(defun org-mcp--named-file-set (files)
  "Return the Org files FILES names, each one reachable by the call.
FILES is the `files' parameter of a call: an array of paths, or a
single path.  Every entry must be absolute, as `file-name-absolute-p'
reads it, so `~/' counts; a relative entry is refused rather than
resolved against `default-directory'.  An entry naming a file must
pass the gate, `org-mcp--find-allowed-file', as a file the call names.

An entry naming a directory is walked for Org files when
`org-mcp--override-permits-p' permits the directory's local
truename.  Otherwise, as under nil or outside every root, the
directory is not read at all: the entry stands for the allowed files
under it, and is refused when there are none.  A remote directory is
never read either, since it has no local truename.

The walk, `directory-files-recursively', descends into every
subdirectory except hidden ones, whose names start with `.', and
never follows a symlink to a directory, so it cannot loop or leave
the directory that way.  A subdirectory it cannot read is skipped; a
named directory it cannot search is an error.  So is a directory
that fails while the walk lists it, such as one removed meanwhile;
the error names it as the call reaches it.  In each directory it
takes the files Org takes from a directory in `org-agenda-files':
names matching `org-agenda-file-regexp', by default every `.org'
file and no archive, and of those the ones not hidden that
`org-mcp--org-file-name-p' accepts.  Of those it skips what is not a
regular file, such as a dangling symlink or a FIFO.  Every file it
takes must pass the gate; one that does not, such as a symlink out
of every root, fails the call with an error naming it as the call
reaches it, the entry followed by the path below it, never the file
the symlink resolves to.

The walk runs with file name handlers disabled: every name it builds
is local, and no handler should take part in reading it.

Each file is returned once, where FILES first reaches it.  Entries
come in order.  A walked directory's files come in the order
`directory-files-recursively' returns them: depth first, each
subdirectory's files before the files beside them, each directory's
names in alphabetical order.  A directory that is not walked gives
its allowed files in the order of the allowed files."
  (let ((entries
         (cond
          ((stringp files)
           (list files))
          ((sequencep files)
           (append files nil))))
        (found nil)
        ;; The allowed files, computed once for the whole set, and
        ;; never the set of an enclosing call.  The gate reads them
        ;; from here.
        (org-mcp--file-set
         (let ((org-mcp--file-set 'allowed))
           (org-mcp--expanded-allowed-files))))
    (unless (and entries (cl-every #'stringp entries))
      (org-mcp--tool-validation-error
       "files must be a non-empty array of paths"))
    ;; A relative entry would resolve against `default-directory',
    ;; which depends on whatever buffer is current in Emacs.
    (dolist (entry entries)
      (unless (file-name-absolute-p entry)
        (org-mcp--tool-validation-error
         "files entry names no file by its full path: %s.  Send a full \
path, such as /home/user/notes.org"
         entry)))
    (cl-flet
        ((add
          (file locator)
          ;; LOCATOR is FILE as the call reaches it, for the refusal.
          (let ((allowed
                 (or (org-mcp--find-allowed-file file t)
                     (org-mcp--tool-file-access-error locator))))
            (unless (member allowed found)
              (push allowed found))))
         (below
          (entry truename path)
          ;; PATH as the call reaches it.  ENTRY names the directory
          ;; whose local truename is TRUENAME; a PATH under TRUENAME is
          ;; ENTRY followed by the path below it, any other PATH, such
          ;; as TRUENAME itself, is ENTRY.
          (if (and (stringp path)
                   (string-prefix-p
                    (file-name-as-directory truename) path))
              (concat
               (file-name-as-directory entry)
               (file-relative-name path truename))
            entry)))
      (dolist (entry entries)
        (let ((truename (org-mcp--local-truename entry)))
          (cond
           ((not (and truename (file-directory-p truename)))
            (add entry entry))
           ((org-mcp--override-permits-p entry truename)
            (let ((file-name-handler-alist nil))
              (unless (file-accessible-directory-p truename)
                (org-mcp--tool-validation-error
                 "Cannot read directory: %s"
                 entry))
              (dolist
                  (path
                   (condition-case err
                       (let ((case-fold-search nil))
                         (directory-files-recursively
                          truename
                          org-agenda-file-regexp
                          nil
                          (lambda (dir)
                            (and (not
                                  (string-prefix-p
                                   "." (file-name-nondirectory dir)))
                                 ;; With a function as PREDICATE,
                                 ;; rather than t, the walk signals
                                 ;; on a subdirectory it cannot read
                                 ;; instead of skipping it.
                                 (file-readable-p dir)))))
                     ;; A directory can still fail when it is listed:
                     ;; one removed after the predicate passed it, an
                     ;; I/O error, or a denial `file-readable-p' did
                     ;; not foresee.  The error data ends with the
                     ;; directory.  The whole call fails, so it never
                     ;; searches only part of the set.
                     (file-error
                      (org-mcp--tool-validation-error
                       "Cannot read directory: %s"
                       (below entry truename (car (last err)))))))
                (let ((name (file-name-nondirectory path)))
                  (when (and (not (string-prefix-p "." name))
                             (org-mcp--org-file-name-p name)
                             (file-regular-p path))
                    (add path (below entry truename path)))))))
           (t
            (let ((under
                   (cl-remove-if-not
                    (lambda (file)
                      (when-let* ((file-truename
                                   (org-mcp--local-truename file)))
                        (file-in-directory-p file-truename truename)))
                    org-mcp--file-set)))
              (unless under
                (org-mcp--tool-file-access-error entry))
              (dolist (file under)
                (add file entry))))))))
    (nreverse found)))

(defun org-mcp--refresh-file-buffers
    (file-path &optional except-buffer)
  "Refresh clean buffers visiting FILE-PATH except EXCEPT-BUFFER.
Preserves user edits by skipping already-modified buffers. Preserves
narrowing state across the refresh operation."
  (dolist (buf (buffer-list))
    (when (not (eq buf except-buffer))
      (with-current-buffer buf
        (when-let* ((buf-file (buffer-file-name)))
          (when (and (org-mcp--paths-equal-p buf-file file-path)
                     (not (buffer-modified-p)))
            (let ((was-narrowed (buffer-narrowed-p))
                  (narrow-start nil)
                  (narrow-end nil))
              ;; Save narrowing markers if narrowed
              (when was-narrowed
                (setq narrow-start (point-min-marker))
                (setq narrow-end (point-max-marker)))
              (condition-case err
                  (save-mark-and-excursion
                    (unwind-protect
                        (progn
                          (revert-buffer t t t)
                          ;; Check if buffer was modified by hooks
                          (when (buffer-modified-p)
                            (org-mcp--tool-validation-error
                             "Buffer for file %s was modified during \
refresh.  Check your `after-revert-hook' for functions that modify \
the buffer"
                             file-path)))
                      ;; Restore narrowing even if revert fails
                      (when was-narrowed
                        (narrow-to-region narrow-start narrow-end))))
                (error
                 (org-mcp--tool-validation-error
                  "Failed to refresh buffer for file %s: %s. \
Check your Emacs hooks (`before-revert-hook', \
`after-revert-hook', `revert-buffer-function')"
                  file-path (error-message-string err)))))))))))

(defvar org-mcp--unsaved-change-p nil
  "Non-nil when the running tool call leaves a change unsaved.
A change stays unsaved when its buffer still differs from its file
after the save step.  That happens when the buffer already had
unsaved edits, because org-mcp never saves such a buffer, unless a
hook saved it during the call.  `org-mcp--modify-and-save' binds it
after saving the buffer it edits, and `org-mcp--complete-and-save'
reports it as the `saved' response field.  A tool that also edits
another buffer binds it around `org-mcp--modify-and-save' so the
response covers both edits.")

(defun org-mcp--complete-and-save (response-alist)
  "Return the JSON response for a change to the heading at point.
RESPONSE-ALIST is an alist of response fields.  The `link' field is
the heading's link from `org-mcp--link-at-point'; no identifier is
created for it.  The `saved' field is false when
`org-mcp--unsaved-change-p' is non-nil.  When no link can be made,
whatever the error, the tool error says that the change itself was
made, so a client does not repeat it."
  (let
      ((link
        (condition-case err
            (org-mcp--link-at-point)
          (error
           (org-mcp--tool-validation-error
            "The change was made%s, but no link to it could be made: %s"
            (if org-mcp--unsaved-change-p
                " and left unsaved"
              "")
            (if (eq (car err) 'mcp-server-lib-tool-error)
                (cadr err)
              (error-message-string err)))))))
    (json-encode
     (append
      `((success . t)
        (saved
         .
         ,(if org-mcp--unsaved-change-p
              :json-false t)))
      response-alist `((link . ,link))))))

(defun org-mcp--maybe-save-buffer
    (buf file-path preexisting-modified-p)
  "Save BUF when it was clean before org-mcp wrote to it.
FILE-PATH is refreshed in other visiting buffers only after an actual
save. If PREEXISTING-MODIFIED-P is non-nil, BUF is left dirty and
unsaved so pre-existing user edits are preserved."
  (unless preexisting-modified-p
    (with-current-buffer buf
      (when (buffer-modified-p)
        (save-buffer)))
    (org-mcp--refresh-file-buffers file-path buf)))

(defmacro org-mcp--with-org-file (file-path &rest body)
  "Execute BODY in the canonical Org buffer for FILE-PATH."
  (declare (indent 1) (debug (form body)))
  `(let ((buf (org-mcp--get-file-buffer ,file-path))
         (result nil))
     (with-current-buffer buf
       (save-restriction
         (widen)
         (setq result
               (save-mark-and-excursion
                 (save-match-data
                   (goto-char (point-min))
                   ,@body)))))
     result))

(defmacro org-mcp--with-allowed-agenda-files (&rest body)
  "Execute BODY with `org-agenda-files' bound to existing allowed files.
The binding is the subset of `org-mcp-allowed-files' that exists on
disk, with each entry expanded to an absolute path (relative entries
resolved against `org-directory').  This is the single ingress point
that maps the org-mcp security boundary onto Org's multi-file
convention, so tool handlers can rely on `org-agenda-files' instead
of re-implementing the filter.

BODY passes the variable to Org explicitly and never lets Org read
it through the function `org-agenda-files': while the agenda is
restricted, as by `org-agenda-set-restriction-lock', that function
returns the file of the restriction, not the binding."
  (declare (indent 0) (debug (body)))
  `(let ((org-agenda-files
          (cl-remove-if-not
           #'file-exists-p (org-mcp--expanded-allowed-files))))
     ,@body))

(defun org-mcp--blank-param-p (value)
  "Return non-nil when VALUE, an optional parameter of a call, is blank.
Clients may fill an optional parameter they do not use with an empty
value, so JSON null, false, \"\", [] and {}, which decodes to nil,
all mean that the call does not send it."
  (member value '(nil "" [] :json-false)))

(defun org-mcp--boolean-param (value name)
  "Return VALUE, the call's boolean parameter NAME, as t or nil.
JSON true and \"true\" are true.  A blank VALUE, see
`org-mcp--blank-param-p', JSON false and null included, is false, and
so are \"false\" and :false, the keyword `json-parse-string' decodes
false to.  Any other VALUE is refused with an error naming NAME."
  (cond
   ((member value '(t "true"))
    t)
   ((or (org-mcp--blank-param-p value)
        (member value '(:false "false")))
    nil)
   (t
    (org-mcp--tool-validation-error "%s must be true or false: %S"
                                    name
                                    value))))

(defun org-mcp--files-given (files)
  "Return FILES, a call's `files' parameter, or nil when it is blank.
See `org-mcp--blank-param-p'.  Every tool taking `files' reads it
through here."
  (unless (org-mcp--blank-param-p files)
    files))

(defun org-mcp--link-given (link)
  "Return LINK, an optional link parameter of a call, or nil when it is blank.
Clients may fill an optional parameter they do not use with an empty
value, so JSON null, false and a string holding nothing but whitespace
all mean that the call names no link.  Any other value is returned
for `org-mcp--link-parse' to check."
  (unless (or (memq link '(nil :json-false))
              (and (stringp link) (string-blank-p link)))
    link))

(defmacro org-mcp--closing-opened-buffers (files &rest body)
  "Run BODY, then kill the buffers it opened to visit FILES.
FILES, a list of files, is evaluated before BODY runs.  A buffer that
already visited one of them then is left open, and so is a buffer
BODY leaves modified.  Every other buffer visiting one of them is
killed once BODY returns or exits non-locally, so scanning files a
call names leaves the user's buffer list as it was."
  (declare (indent 1) (debug (form body)))
  (let ((unvisited (make-symbol "unvisited")))
    `(let ((,unvisited (cl-remove-if #'find-buffer-visiting ,files)))
       (unwind-protect
           (progn
             ,@body)
         (dolist (file ,unvisited)
           (when-let* ((buffer (find-buffer-visiting file)))
             (unless (buffer-modified-p buffer)
               (kill-buffer buffer))))))))

(defmacro org-mcp--with-file-set (files &rest body)
  "Run BODY over the files a call names in FILES, or the allowed files.
FILES is the call's `files' parameter.  Unless it is blank, see
`org-mcp--files-given', `org-mcp--named-file-set' checks and expands
it, and the resulting set replaces the allowed files for BODY through
`org-mcp--file-set'; the buffers BODY opens to visit that set are
killed afterwards by `org-mcp--closing-opened-buffers'.  When it is
blank, BODY runs over the allowed files and leaves the buffers it
opens for them, as the agenda does.  Either way BODY runs inside
`org-mcp--with-allowed-agenda-files', so `org-agenda-files' holds the
existing files it works on."
  (declare (indent 1) (debug (form body)))
  (macroexp-let2 nil files `(org-mcp--files-given ,files)
    `(let ((org-mcp--file-set
            (if ,files
                (org-mcp--named-file-set ,files)
              'allowed)))
       (org-mcp--closing-opened-buffers (and ,files org-mcp--file-set)
         (org-mcp--with-allowed-agenda-files
           ,@body)))))

(defmacro org-mcp--modify-and-save
    (file-path operation response-alist &rest body)
  "Execute BODY to modify Org file at FILE-PATH.
BODY runs in the canonical visited buffer for FILE-PATH and leaves
point in the entry of the heading it changed; the response's `link'
links to that heading.  If the buffer was already modified before
BODY runs, org-mcp leaves it dirty and unsaved.  Otherwise it saves
the buffer and refreshes other clean visiting buffers afterward.

BODY and the save form one change.  When either signals an error,
the buffer is put back as it was and the error propagates.  If a
hook saved a buffer that was clean partway through, org-mcp saves it
again, so the file is put back too.  If the save wrote the file and
then failed, as in `after-save-hook', the change is kept and the
error says it was made, so a client does not repeat it.

The response reports `saved' as false when the buffer still differs
from its file after the save, so a hook that saved the buffer while
BODY ran yields true.  RESPONSE-ALIST is evaluated after the save,
with point where BODY left it, so a link that cannot be made never
keeps the change from being saved.  OPERATION is retained for
call-site clarity and compatibility.  BODY can access FILE-PATH,
OPERATION, and RESPONSE-ALIST as variables."
  (declare (indent 3) (debug (form form form body)))
  (let ((position (make-symbol "position"))
        (group (make-symbol "group"))
        (done (make-symbol "done")))
    `(let* ((ctx (org-mcp--file-buffer-context ,file-path))
            (buf (plist-get ctx :buffer))
            (preexisting-modified-p (plist-get ctx :modified-p))
            (,position nil))
       (ignore ,operation)
       (unwind-protect
           (progn
             (with-current-buffer buf
               ;; `atomic-change-group' written out, with its bindings,
               ;; so that a save which wrote the file and then failed
               ;; keeps the change.  Cancelling the group undoes BODY's
               ;; edits, with undo turned off in BUF too, and undoing
               ;; the first edit of a clean buffer marks it unmodified
               ;; again unless its file was written during the call.
               (let ((,group (prepare-change-group))
                     (undo-outer-limit nil)
                     (undo-limit most-positive-fixnum)
                     (undo-strong-limit most-positive-fixnum)
                     (,done nil))
                 (unwind-protect
                     (progn
                       (activate-change-group ,group)
                       (save-restriction
                         (widen)
                         (save-mark-and-excursion
                           (save-match-data
                             (goto-char (point-min))
                             ,@body
                             (setq ,position (point-marker)))))
                       (unless preexisting-modified-p
                         (when (buffer-modified-p)
                           (condition-case err
                               (save-buffer)
                             (error
                              ;; An unmodified buffer means the file
                              ;; holds the change.
                              (unless (buffer-modified-p)
                                (setq ,done t)
                                (org-mcp--saved-then-failed-error
                                 "The change was made and saved" err))
                              (signal (car err) (cdr err))))))
                       (setq ,done t))
                   (if ,done
                       (accept-change-group ,group)
                     (cancel-change-group ,group)
                     ;; Still modified: a hook wrote the file during the
                     ;; call.  BUF holds no edits of the user's, so
                     ;; saving it puts the file back.  A second failure
                     ;; must not replace the first error.
                     (when (and (not preexisting-modified-p)
                                (buffer-modified-p))
                       (ignore-errors
                         (save-buffer)))))))
             ;; Outside the change group: the file is written by now,
             ;; and undoing the edit would part the buffer from it.
             (unless preexisting-modified-p
               (org-mcp--refresh-file-buffers ,file-path buf))
             (let ((org-mcp--unsaved-change-p
                    (or org-mcp--unsaved-change-p
                        (buffer-modified-p buf))))
               (with-current-buffer buf
                 (org-with-wide-buffer
                  (goto-char ,position)
                  (org-mcp--complete-and-save ,response-alist)))))
         (when ,position
           (set-marker ,position nil))))))

(defun org-mcp--percent-decode (string)
  "Return STRING with its percent-encoding undone once.
The escapes are UTF-8 bytes, as `url-hexify-string' writes them, and
raw non-ASCII characters in STRING may sit between them.  Every escape
decodes to its byte, `%0A' and `%0D' included."
  (decode-coding-string
   (url-unhex-string (encode-coding-string string 'utf-8) t) 'utf-8))

(defun org-mcp--node-text-at-point ()
  "Extract content of current headline including the headline itself.
Point should be at the headline."
  (let ((start (line-beginning-position)))
    (org-end-of-subtree t t)
    ;; Remove trailing newline if present
    (when (and (> (point) start) (= (char-before) ?\n))
      (backward-char))
    (buffer-substring-no-properties start (point))))

(defun org-mcp--link-at-point ()
  "Return the native Org link to the heading at point.
Every response field that carries a link takes it from here.
The link is the one a non-interactive `org-store-link' makes when it
may use existing identifiers only, in this order:

  heading with an :ID:        id:ID
  heading with a :CUSTOM_ID:  file:PATH::#CUSTOM_ID
  any other heading           file:PATH::*TITLE
  before the first heading    id:ID of a file-level :ID:, else
                              file:PATH::LINE, or file:PATH

PATH is the file name as `abbreviate-file-name' writes it.  The link
is returned as its text, without brackets or description.  Point may
be anywhere in the heading's entry and is not moved; the buffer is
read widened.  No identifier is created.

An `id:' link made before the first heading addresses the whole file;
see `org-mcp--target-heading-p'.  A `file:' link made there searches
for the text of its line.  org-mcp cannot resolve that one: its
resolver accepts only a search that ends on a heading.  No tool links
such a position: every write links the heading it changed, and every
read lists the headings Org's parser finds.

Throws a tool error when `org-store-link' changes the buffer, or makes
anything but an `id:' link or a `file:' link searching for the
heading's custom ID or title, or, before the first heading, an `id:'
or `file:' link.  Neither happens in stock Org; advice on
`org-store-link' can cause both."
  (org-with-wide-buffer
   (unless (org-before-first-heading-p)
     (org-back-to-heading t))
   (let*
       ((at-heading (not (org-before-first-heading-p)))
        (tick (buffer-chars-modified-tick))
        (stored
         ;; Each binding stops a user setting from changing the form
         ;; of the link or from creating an identifier.  They are made
         ;; with this buffer current, so they also override a
         ;; buffer-local value, such as the `org-id-link-to-org-use-id'
         ;; that Doom's org-roam module sets from `find-file-hook'.
         (let ( ;; Link by an existing :ID:, never create one.
               (org-id-link-to-org-use-id 'use-existing)
               ;; A heading without its own :ID: gets its own link,
               ;; not `id:PARENT::*Title' through an ancestor's :ID:.
               (org-id-link-consider-parent-id nil)
               ;; No `::' search after an `id:' link.
               (org-id-link-use-context nil)
               ;; A `file:' link names the heading, not the bare file.
               (org-link-context-for-files t)
               ;; An active region is no search string.
               (org-ignore-region t)
               ;; No package's search string replaces the heading's.
               (org-create-file-search-functions nil)
               ;; Only the `id:' link type stores links.  Another
               ;; store function that claims Org buffers would take
               ;; the link over (Org 9.8 keeps the last one that
               ;; matches) or, matching together with `id:', prompt
               ;; for a choice (Org 9.7).
               (org-link-parameters
                (mapcar
                 (lambda (entry)
                   (if (equal (car entry) "id")
                       entry
                     (cons
                      (car entry)
                      (org-plist-delete (cdr entry) :store))))
                 org-link-parameters))
               ;; `org-store-link' sets this global with `setq'; keep
               ;; the user's value, such as a pending capture's.
               (org-store-link-plist nil)
               ;; A prompt signals instead of waiting for input.
               (inhibit-interaction t))
           ;; Before anything else, Org links a <<target>> around
           ;; point, and at the start of a line that includes one
           ;; ending the previous line.
           (save-restriction
             (narrow-to-region (line-beginning-position) (point-max))
             (org-store-link nil nil))))
        (link
         ;; A non-interactive `org-store-link' returns a bracket link,
         ;; and Org has no function returning the bare one, so take
         ;; it apart with Org's own regexp and unescaping.
         (and stored
              (string-match org-link-bracket-re stored)
              (org-link-unescape
               (match-string-no-properties 1 stored)))))
     (unless (= tick (buffer-chars-modified-tick))
       (org-mcp--tool-validation-error
        "org-store-link changed %s while linking to it; org-mcp \
creates no identifiers, so advice on org-store-link must leave \
non-interactive calls alone"
        (buffer-name)))
     (unless (and link
                  (string-match-p
                   (if at-heading
                       "\\`\\(?:id:\\|file:.*::[*#]\\)"
                     "\\`\\(?:id:\\|file:\\)")
                   link))
       (org-mcp--tool-validation-error
        "org-store-link made %s, not an id: or file: link to the \
heading, in %s; advice on org-store-link changes the link"
        (or stored "no link") (buffer-name)))
     link)))

(defun org-mcp--tag-sets-at-point ()
  "Return the tags of the heading at point as (EFFECTIVE . OWN).

EFFECTIVE is `org-get-tags' called without its LOCAL argument, so the
list follows `org-use-tag-inheritance' and
`org-tags-exclude-from-inheritance' — including their list and regexp
forms — rather than a policy fixed here.  OWN is the part of that same
list written on the heading itself: Org marks every inherited entry
with the `inherited' text property, so one call yields both sets, and
there is no second scan and no second notion of what a tag is.

Both lists are plain strings; the text property is an artifact of how
Org reports inheritance, not something a caller should have to know."
  (let ((tags (org-get-tags)))
    (cons
     (mapcar #'substring-no-properties tags)
     (mapcar
      #'substring-no-properties
      (cl-remove-if
       (lambda (tag) (get-text-property 0 'inherited tag)) tags)))))

(defun org-mcp--title-at-point ()
  "Return the title of the heading at point, as Org reads it.
`org-get-heading' drops the TODO keyword, the priority cookie, the
tags and the COMMENT keyword, and Org's own
`org-link--normalize-string' then drops statistics cookies and
collapses runs of whitespace.  That is the normalization
`org-link-search' applies to a heading before matching a `::*title'
link against it, so every read, every write precondition and Org's
own link resolution agree on what a heading is called.

`ol.el' exports no public equivalent, and this private function is
load-bearing in seven places inside `ol.el' itself.  A test pins its
behaviour, so a change in Org fails the suite loudly instead of
drifting through every read; reimplementing the rule here with a
regexp would be the second definition this one exists to remove."
  (org-link--normalize-string (org-get-heading t t t t)))

(defun org-mcp--titles-equal-p (a b)
  "Return non-nil when A and B name the same heading to Org.
The comparison is the one `org-link-search' makes when it resolves a
`::*title' link: both titles are normalized as
`org-mcp--title-at-point' normalizes a heading, split into words and
compared letter case aside.  A write precondition therefore accepts
every title that reaches the heading through a link, rather than
refusing the call a link has just resolved."
  (cl-flet ((words
             (title)
             (mapcar
              #'upcase
              (split-string (org-link--normalize-string title)))))
    (equal (words a) (words b))))

(defun org-mcp--heading-metadata-at-point ()
  "Return canonical heading metadata at point as a plist.

Reads the heading in one `org-element-at-point' call and one
`org-get-tags' call, so callers do not have to chain
`org-entry-get'/`org-get-tags'/`org-get-todo-state' themselves.

Returned plist keys:
  :title       string, with TODO/priority/tags/comment stripped
  :todo        string or nil
  :priority    one-character string or nil
  :tags        list of strings, the tags in effect on the heading
  :local-tags  list of strings, the tags written on the heading itself
  :level       integer
  :scheduled   Org timestamp string or nil
  :deadline    Org timestamp string or nil
  :closed      Org timestamp string or nil

The two tag lists come from `org-mcp--tag-sets-at-point', so every
caller reports the same tags for the same heading under the same
configuration.  Timestamps are returned as their `:raw-value' so the
result matches `org-entry-get' (canonical Org abbreviation, no
locale-dependent reformatting)."
  (let* ((el (org-element-at-point))
         (priority-char (org-element-property :priority el))
         (tag-sets (org-mcp--tag-sets-at-point))
         (sched (org-element-property :scheduled el))
         (deadl (org-element-property :deadline el))
         (clsd (org-element-property :closed el)))
    (list
     :title (org-mcp--title-at-point)
     :todo (org-element-property :todo-keyword el)
     :priority (and priority-char (char-to-string priority-char))
     :tags (car tag-sets)
     :local-tags (cdr tag-sets)
     :level (org-element-property :level el)
     :scheduled (and sched (org-element-property :raw-value sched))
     :deadline (and deadl (org-element-property :raw-value deadl))
     :closed (and clsd (org-element-property :raw-value clsd)))))

(defun org-mcp--body-bounds ()
  "Return the body of the heading at point as (BEGIN . END).
The body begins where `org-end-of-meta-data' with FULL leaves point,
past planning lines, drawers and blank lines.  It ends at the first
child, which `org-goto-first-child' finds, or, when there is none,
after the last non-blank character of the subtree, where
`org-end-of-subtree' leaves point.  An empty body ends where it
begins, at the start of the next heading's line or at the end of the
buffer.  A line starting with `*' is part of the body unless Org
reads it as a heading.

Both ends are found from the heading itself.  Past an empty body lies
the next heading, a child or a sibling, and `org-goto-first-child'
called there would find that heading's first child instead.  Point
does not move."
  (save-excursion
    (org-back-to-heading t)
    (let ((end
           (save-excursion
             (if (org-goto-first-child)
                 (point)
               (org-end-of-subtree t)))))
      (org-end-of-meta-data t)
      (cons (point) (max (point) end)))))

(defun org-mcp--insert-body-text (text)
  "Insert TEXT at point, the end of a heading's body, on lines of its own.
A line break goes before TEXT unless point starts a line, and after
it unless TEXT ends in one or a line break follows point."
  (unless (bolp)
    (insert "\n"))
  (insert text)
  (unless (or (bolp) (eq (char-after) ?\n))
    (insert "\n")))

(defconst org-mcp--special-properties
  '("TODO"
    "TAGS"
    "ALLTAGS"
    "PRIORITY"
    "SCHEDULED"
    "DEADLINE"
    "CLOSED"
    "CATEGORY"
    "ITEM"
    "FILE"
    "BLOCKED"
    "CLOCKSUM"
    "CLOCKSUM_T"
    "TIMESTAMP"
    "TIMESTAMP_IA")
  "Org special properties that cannot be set via `org-node-set-properties'.")


;; Nodes

(defconst org-mcp--node-fields
  '(title
    todo
    priority
    tags
    local_tags
    scheduled
    deadline
    closed
    file
    id
    level
    link
    content
    properties
    children)
  "Every field a node can carry.
`org-mcp--node-at-point' builds each of these and nothing else, and
a field a call asks for is checked against this list before any
file is opened.  The tool descriptions and docs/reading.org
describe the same names to a client, so a field added here is added
to the builder, to that page and to the node description in the
same change.")

(defconst org-mcp--node-child-fields '(title todo level link)
  "The fields a child node carries.
A child is a node like any other, asked for with few fields: its
title and TODO state show the outline, and its link addresses it in
the call that reads it in full.")

(defconst org-mcp--node-read-fields
  '(title
    todo
    priority
    tags
    local_tags
    scheduled
    deadline
    closed
    file
    id
    level
    link
    content
    children)
  "The fields the org-node-read tool and the org://{link} resource carry.")

(defconst org-mcp--node-query-fields
  '(title
    todo
    priority
    tags
    local_tags
    scheduled
    deadline
    closed
    file
    id
    level
    link
    properties)
  "The fields a query result carries.
The same node a read returns, without the body and the children a
match list would read every matched subtree to fill, and with the Org
property drawer a query is asked about.")

(defun org-mcp--node-field (name)
  "Return the node field NAME names, or refuse NAME as not one.
NAME is a string, the way a call's `fields' parameter sends it, or
a symbol, the way `org-mcp-node-field-lists' holds it.  The match is
by name and never by `intern', so nothing a call sends becomes a
symbol, and the refusal names every field there is to ask for."
  (let ((text (format "%s" name)))
    (or (cl-find
         text
         org-mcp--node-fields
         :key #'symbol-name
         :test #'string=)
        (org-mcp--tool-validation-error
         "Unknown node field: %s.  Valid fields: %s"
         text (mapconcat #'symbol-name org-mcp--node-fields ", ")))))

(defun org-mcp--named-node-fields (name)
  "Return the fields the list called NAME holds, or refuse NAME.
The lists are `org-mcp-node-field-lists', which the user owns, so
the refusal names the lists that are configured rather than a set
this server decided on, and says how to ask for fields without
naming a list at all."
  (or (cdr
       (cl-find
        name
        org-mcp-node-field-lists
        :key (lambda (entry) (format "%s" (car entry)))
        :test #'string=))
      (org-mcp--tool-validation-error
       "Unknown field list: %s.  Configured lists: %s.  \
Fields are also named directly, as an array such as \
[\"title\", \"link\"]"
       name
       (if org-mcp-node-field-lists
           (mapconcat (lambda (entry) (format "%s" (car entry)))
                      org-mcp-node-field-lists
                      ", ")
         "none"))))

(defun org-mcp--node-field-names (fields)
  "Return the field names a call's FIELDS parameter asks for.
FIELDS is an array of names, which is the list of names itself, or
a string naming a list in `org-mcp-node-field-lists'.  The names
themselves are not checked here; `org-mcp--node-field' checks each
one, so a name from a configured list is checked as a name a call
spelled out is."
  (cond
   ((stringp fields)
    (org-mcp--named-node-fields fields))
   ((or (vectorp fields) (consp fields))
    (append fields nil))
   (t
    (org-mcp--tool-validation-error
     "fields takes an array of field names, or the name of a \
configured list as a string, not: %S"
     fields))))

(defun org-mcp--node-fields-given (fields default)
  "Return the node fields a call asking for FIELDS wants.
FIELDS is the `fields' parameter of a call, see
`org-mcp--node-field-names'.  A blank FIELDS, see
`org-mcp--blank-param-p', means the call asks for nothing in
particular and takes DEFAULT, the fields that endpoint carries when
it is not asked.

Every name is resolved here, at the parameter, rather than in
`org-mcp--node-at-point': a node is then built from fields that are
known to exist, and a call that misspells one is refused before a
file is opened.  A field named twice is dropped to once, since it
would otherwise be a key sent twice."
  (if (org-mcp--blank-param-p fields)
      default
    (delete-dups
     (mapcar
      #'org-mcp--node-field (org-mcp--node-field-names fields)))))

(defun org-mcp--file-title ()
  "Return the title of the file the current buffer visits.
It is the `#+TITLE:' keyword, which `org-get-title' reads, and the
file's own name when the file sets none, so every node has a title."
  (or (org-get-title) (file-name-nondirectory (buffer-file-name))))

(defun org-mcp--file-link ()
  "Return the native Org link to the file the current buffer visits.
It is the `id:' link of the file's own property drawer when the file
has one -- the ID org-roam gives a file node, which `org-id-open'
resolves to the top of the file -- and `file:PATH' otherwise, with
PATH written as `abbreviate-file-name' writes it, as every heading
link in a response is written.  Either form names the whole file when
a later call sends it back, see `org-mcp--target-heading-p'.

The ID comes from Org's own parse of the file-level drawer rather
than from `org-entry-get', which reads the first heading's drawer
instead when the file opens on a heading.  No identifier is created."
  (let ((id
         (org-with-wide-buffer
          (goto-char (point-min))
          (org-element-property :ID (org-element-org-data-parser)))))
    (if (org-string-nw-p id)
        (concat "id:" id)
      (concat "file:" (abbreviate-file-name (buffer-file-name))))))

(defun org-mcp--node-child-positions (file-node)
  "Return the buffer positions of the children of the node at point.
FILE-NODE non-nil means the node is the file the buffer visits, and
its children are the level-1 headings Org's parser finds.  A sibling
walk started at the first heading would follow that heading's own
level instead, and miss the level-1 headings of a file that opens at
a deeper one.

Otherwise the children are the headings one level below the heading
at point, which `org-goto-first-child' and `org-get-next-sibling'
walk.  Point does not move."
  (save-excursion
    (if file-node
        (org-element-map
         (org-element-parse-buffer 'headline) 'headline
         (lambda (h)
           (when (= (org-element-property :level h) 1)
             (org-element-property :begin h)))
         nil nil 'headline)
      (let ((child-level (1+ (org-current-level)))
            (positions '()))
        (org-back-to-heading t)
        (when (org-goto-first-child)
          (cl-loop
           do
           (when (= (org-current-level) child-level)
             (push (point) positions))
           while (org-get-next-sibling)))
        (nreverse positions)))))

(defun org-mcp--node-content-bounds (file-node children)
  "Return the body of the node at point as (BEGIN . END).
FILE-NODE non-nil means the node is the file, whose body is its
preamble: everything before CHILDREN, the positions
`org-mcp--node-child-positions' returned, or the whole file when it
holds no heading.  Otherwise the body is the region
`org-mcp--body-bounds' delimits, the one org-node-set-content writes
within, so what a client reads and what a write replaces are the same
region."
  (if file-node
      (cons (point-min) (or (car children) (point-max)))
    (org-mcp--body-bounds)))

(defun org-mcp--node-properties ()
  "Return the Org property drawer of the heading at point, or nil.
`org-mcp--special-properties' are left out: Org computes them rather
than storing them, and each is a node field in its own right."
  (cl-remove-if
   (lambda (pair)
     (member (car pair) org-mcp--special-properties))
   (org-entry-properties nil 'standard)))

(defun org-mcp--node-at-point
    (fields &optional child-fields file-node)
  "Return the node at point as an alist carrying FIELDS.
One node shape serves a file, a heading, a child and a query result,
so a client learns one vocabulary to walk an outline.

FIELDS is a list of node field names, in the order the node lists
them; `org-mcp--node-fields' names every one there is.  A field the
node has no value for -- no TODO state, no tag of its own, an empty
body -- is left out rather than sent as null.

CHILD-FIELDS is what the `children' field builds each child with, and
defaults to `org-mcp--node-child-fields'.

FILE-NODE non-nil builds the node of the file the buffer visits: a
node at level 0, carrying the file's title, a link to the file and
its preamble as its content.  The caller says which of the two it
asked for, because point cannot: a file that opens on a heading has
no position before that heading."
  (let* ((meta
          (unless file-node
            (org-mcp--heading-metadata-at-point)))
         (children
          (when (or (memq 'children fields)
                    (and file-node (memq 'content fields)))
            (org-mcp--node-child-positions file-node)))
         (link
          (when (or (memq 'link fields) (memq 'id fields))
            (if file-node
                (org-mcp--file-link)
              (org-mcp--link-at-point))))
         (node '()))
    (dolist (field fields (nreverse node))
      (let ((value
             (pcase field
               ('title
                (if file-node
                    (org-mcp--file-title)
                  (plist-get meta :title)))
               ('todo (plist-get meta :todo))
               ('priority (plist-get meta :priority))
               ('tags
                (when-let* ((tags (plist-get meta :tags)))
                  (vconcat tags)))
               ('local_tags
                (when-let* ((tags (plist-get meta :local-tags)))
                  (vconcat tags)))
               ('scheduled (plist-get meta :scheduled))
               ('deadline (plist-get meta :deadline))
               ('closed (plist-get meta :closed))
               ('file (buffer-file-name))
               ;; The ID the link names, so `id' and `link' always
               ;; agree; a blank :ID: gives neither.
               ('id
                (and link
                     (string-prefix-p "id:" link)
                     (substring link 3)))
               ('level
                (if file-node
                    0
                  (plist-get meta :level)))
               ('link link)
               ('content
                (let* ((bounds
                        (org-mcp--node-content-bounds
                         file-node children))
                       (text
                        (buffer-substring-no-properties
                         (car bounds) (cdr bounds))))
                  (unless (string-blank-p text)
                    (string-trim text))))
               ('properties (org-mcp--node-properties))
               ('children
                (vconcat
                 (mapcar
                  (lambda (position)
                    (save-excursion
                      (goto-char position)
                      (org-mcp--node-at-point
                       (or child-fields org-mcp--node-child-fields))))
                  children)))
               ;; A call's fields are resolved against
               ;; `org-mcp--node-fields' before they reach here, so
               ;; this catches a field list written in this file
               ;; that the builder does not build.
               (_ (error "Unknown node field: %s" field)))))
        (when value
          (push (cons field value) node))))))

(defun org-mcp--generate-outline (file-path)
  "Return the outline of FILE-PATH: its headings and theirs.
The file node's `children' alone, each child carrying its own
children, so the answer is the top-level headings and their direct
ones.  The generation below those is left out, as are the fields a
whole read carries."
  (org-mcp--with-org-file file-path
    (org-mcp--node-at-point '(children)
                            (append
                             org-mcp--node-child-fields '(children))
                            t)))

;; Links

(defconst org-mcp--link-forms-hint
  "Send id:<uuid>, file:<path>::#<custom-id>, file:<path>::*<title> or \
file:<path>, with the file's full path"
  "The sentence that tells a client which link forms a call takes.")

(defun org-mcp--not-a-link-error (link)
  "Throw the error for LINK, a string that is not written as an Org link.
The message names the link forms a call takes instead, and says when
LINK starts with `org://', the scheme of the resource, not of a link."
  (org-mcp--tool-validation-error
   "Not an Org link: %s.  %s%s"
   link
   (if (and (stringp link)
            (string-prefix-p "org://" (string-trim link)))
       "Drop org://, which only a resource URI starts with.  "
     "")
   org-mcp--link-forms-hint))

(defun org-mcp--link-written-p (string)
  "Return non-nil when STRING is written as an Org link.
It is bracketed, with or without a description, starts with a link
type such as `id:' or `file:', or is a search such as `#custom-id' or
`*Title'.  A string starting with `org://', the scheme of the
resource, is no link, and neither is any other string, such as an ID,
a path or a title on its own."
  (when (stringp string)
    (let ((trimmed (string-trim string)))
      (and (not (string-prefix-p "org://" trimmed))
           (or (string-prefix-p "[[" trimmed)
               (string-match-p "\\`[#*]" trimmed)
               (string-match-p org-link-types-re trimmed))))))

(defun org-mcp--link-parse (link)
  "Parse LINK with `org-element-link-parser' and return the link object.
LINK is bracketed, with or without a description, or bare; see
`org-mcp--link-written-p'.  A search on its own, such as `#custom-id'
or `*Title', is parsed here and refused by `org-mcp--link-target' for
naming no file.  A string that is no link is refused by
`org-mcp--not-a-link-error'.  Link abbreviations are not expanded,
since an abbreviation can call a function, and
`org-link-translation-function' is not applied."
  (unless (org-mcp--link-written-p link)
    (org-mcp--not-a-link-error link))
  (let ((trimmed (string-trim link)))
    (with-temp-buffer
      (let ((org-link-abbrev-alist nil)
            (org-link-abbrev-alist-local nil)
            (org-link-translation-function nil))
        (insert
         (if (string-prefix-p "[[" trimmed)
             trimmed
           (org-link-make-string trimmed)))
        (goto-char (point-min))
        (let ((object (org-element-link-parser)))
          (unless (and object
                       (= (org-element-property :end object)
                          (point-max)))
            (org-mcp--tool-validation-error
             "Not a single Org link: %s"
             link))
          object)))))

(defun org-mcp--link-full-path-error (link)
  "Throw the error for LINK, which names no local file by its full path."
  (org-mcp--tool-validation-error
   "Link names no local file by its full path: %s.  Send a full path, \
such as file:/home/user/notes.org::*Heading, or an id: link"
   link))

(defun org-mcp--link-file (object link)
  "Return the file that the file link OBJECT names, if the call may reach it.
LINK is the link as the client sent it, for error messages.  The
path must be absolute and local.  `org-mcp--local-file-name' expands
it with file name handlers disabled, so a path that is remote as
written, or only once `.', `..' or `~' are expanded, is refused
before TRAMP can open a connection for it.  The expanded name then
goes through the scope gate, `org-mcp--find-allowed-file', as a file
the call names, so `org-mcp-file-scope-override' applies.  No buffer
is visited.

A refused path that holds `#' is most likely a path with an outline
path appended, bracketed so that Org reads it as a file link; its
refusal names the link forms to send.  Whether such a file exists
does not change the refusal, so it tells nothing about files the call
may not reach."
  (let ((application (org-element-property :application object))
        (path (org-element-property :path object)))
    (unless (member application '(nil "emacs"))
      (org-mcp--tool-validation-error
       "Link type 'file+%s' is not supported: %s"
       application link))
    (let ((local
           (and (file-name-absolute-p path)
                (org-mcp--local-file-name path))))
      (unless local
        (org-mcp--link-full-path-error link))
      (or (org-mcp--find-allowed-file local t)
          (org-mcp--tool-file-access-error
           link
           (and (string-search "#" path)
                org-mcp--link-forms-hint))))))

(defun org-mcp--link-id-file (id link)
  "Return the allowed file that holds ID, which LINK names.
Emacs's ID index names the file, through `org-id-find-id-file', which
only reads the index.  That file must pass the scope gate,
`org-mcp--find-allowed-file', as a file the call does not name, before
anything touches it: a remote file, or one outside the allowed files,
is refused without a single file operation on it.  Only then is the
file searched for ID, with `org-id-find-id-in-file', which reads the
buffer visiting it, or else its contents into a temporary buffer.

When the index lacks ID, or the file it names does not hold it, the
index is rescanned once with `org-id-update-id-locations', as
`org-id-find' does on a miss, and the file it then names goes through
the gate in turn; the caller finds ID in that file's buffer.
`org-id-find' is not called itself: it reads the file the index names
before any gate could refuse it, and so asks TRAMP about a remote one.

The index is read in a temporary buffer because `org-id-find-id-file'
falls back to the current buffer's file for an ID it lacks.  Errors
from the index, such as the refusal to rescan when
`org-id-track-globally' is off, count as an unknown ID.  Neither error
this function throws names a file."
  (cl-flet ((indexed-file
             ()
             (with-temp-buffer
               (ignore-errors
                 (org-id-find-id-file id))))
            (reachable
             (file)
             (or (org-mcp--find-allowed-file file)
                 (org-mcp--tool-file-access-error link))))
    (unless (org-string-nw-p id)
      (org-mcp--id-not-found-error id))
    (let* ((indexed (indexed-file))
           (file (and indexed (reachable indexed))))
      (if (and file (org-id-find-id-in-file id file))
          file
        (ignore-errors
          (org-id-update-id-locations nil t))
        (reachable
         (or (indexed-file) (org-mcp--id-not-found-error id)))))))

(defun org-mcp--link-id-in-files (id files)
  "Return the first of the files FILES names that holds ID.
FILES is the call's `files' parameter.  `org-mcp--named-file-set'
checks and expands it, so it reaches as far as
`org-mcp-file-scope-override' permits, and its files are searched in
the order that function returns them.  Each is searched with
`org-id-find-id-in-file', which reads the file, or the buffer visiting
it, and consults no ID index: an ID in a file Emacs never indexed is
found, Org's rescan never runs, and nothing is added to
`org-id-locations'.  An ID none of them holds is an error naming FILES
as the call sent them.  The search opens no buffer on these files:
Org reads a file that no buffer visits into a work buffer of its own."
  (let ((set (org-mcp--named-file-set files)))
    (or (and (org-string-nw-p id)
             (cl-find-if
              (lambda (file) (org-id-find-id-in-file id file)) set))
        (org-mcp--tool-validation-error
         "Cannot find ID '%s' in files: %s"
         id
         (if (stringp files)
             files
           (mapconcat #'identity files ", "))))))

(defun org-mcp--check-files (object link files)
  "Return FILES, sent with LINK, or nil when it is blank.
OBJECT is LINK as `org-mcp--link-parse' parsed it.  FILES is the
call's `files' parameter, read through `org-mcp--files-given'.  It
applies only to an `id:' link, the one link type org-mcp resolves
without a file, so with any other LINK a FILES that is not blank is
refused here, before any file is opened: with a `file:' link, which
names its file already, or a link of another type."
  (when-let* ((files (org-mcp--files-given files)))
    (unless (equal (org-element-property :type object) "id")
      (org-mcp--tool-validation-error
       "files applies only to an id: link: %s"
       link))
    files))

(defun org-mcp--link-target (link &optional files id-file)
  "Return the target of LINK, a native Org link, visiting no buffer.
The value is a plist: `:link' is LINK, `:file' the allowed file it
names, `:id' the ID of an `id:' link, and `:search' the part after
`::', if any.  Whether an `id:' link without a search part names a
heading or its whole file is decided in the file's buffer, by
`org-mcp--target-heading-p'.  Only `id:' and `file:' links are
accepted.  A string that is not a link is refused by
`org-mcp--link-parse', and every other link type here, before any
file is opened, and so is a link that names no file, such as
`[[#custom-id]]' or `[[*Title]]'.

FILES is the call's `files' parameter, checked against LINK by
`org-mcp--check-files'.  When it is not blank, the ID of an `id:' link
is looked up in those files by `org-mcp--link-id-in-files' rather
than through Org's ID index.  ID-FILE, when non-nil, is a file the
call already reaches: the ID of an `id:' link is taken to be in it,
with no lookup, and the caller finds the ID in that file's buffer."
  (let*
      ((object (org-mcp--link-parse link))
       (link (string-trim link))
       (files (org-mcp--check-files object link files))
       (type (org-element-property :type object))
       (path (org-element-property :path object))
       (target
        (pcase type
          ("id"
           ;; The parser keeps the search part of an `id:' link in
           ;; its path; split it off the way `org-id-open' does.
           (let* ((search
                   (and (string-match "::\\(.*\\)\\'" path)
                        (match-string 1 path)))
                  (id
                   (if search
                       (substring path 0 (match-beginning 0))
                     path)))
             (list
              :link link
              :file
              (cond
               (id-file
                id-file)
               (files
                (org-mcp--link-id-in-files id files))
               (t
                (org-mcp--link-id-file id link)))
              :id id
              :search search)))
          ("file"
           (list
            :link link
            :file (org-mcp--link-file object link)
            :search (org-element-property :search-option object)))
          ((or "custom-id" "fuzzy" "coderef")
           (org-mcp--link-full-path-error link))
          (_
           (org-mcp--tool-validation-error
            "Link type '%s' is not supported: send an id: or file: link"
            type)))))
    ;; `org-link-search' turns a regexp search into a sparse tree,
    ;; which would refold the user's buffer.
    (when (string-match-p
           "\\`/.*/\\'" (or (plist-get target :search) ""))
      (org-mcp--tool-validation-error
       "Regexp search is not supported in a link: %s"
       link))
    target))

(defun org-mcp--link-goto (target)
  "Move point to where TARGET points in the current buffer.
TARGET comes from `org-mcp--link-target', and the current buffer
visits its file, widened.  This is the lookup `org-id-open' and
`org-open-file' perform, without their navigation: `org-link-open' is
never called, because it switches windows, widens the user's buffer,
pushes the mark ring and unfolds headings.  A search that matches
several headings goes to the first, as it does in Org, and a search
that matches nothing is an error rather than an offer to create a
heading."
  (let ((id (plist-get target :id))
        (search (plist-get target :search)))
    (goto-char (point-min))
    (when id
      ;; The lookup yields the file only.  The ID is located in this
      ;; buffer, which may hold unsaved edits the file lacks.
      (goto-char
       (or (org-find-entry-with-id id)
           (org-mcp--id-not-found-error id))))
    (when search
      (save-restriction
        ;; An `id:' link searches the ID's subtree, as `org-id-open'
        ;; does, and a `file:' link the whole file.
        (when (and id (not (org-before-first-heading-p)))
          (org-narrow-to-subtree))
        (if (and (not id) (string-match-p "\\`[0-9]+\\'" search))
            ;; A line number, as `org-link-open-as-file' reads it.
            (org-goto-line (string-to-number search))
          (let ((org-link-search-must-match-exact-headline t)
                (org-execute-file-search-functions nil))
            (condition-case err
                (org-link-search search nil t)
              (error
               (org-mcp--tool-validation-error
                "Cannot resolve link %s: %s"
                (plist-get target :link)
                (error-message-string err))))))))))

(defun org-mcp--target-heading-p (target)
  "Return non-nil when TARGET names a heading rather than a whole file.
TARGET comes from `org-mcp--link-target', and the current buffer
visits its file, widened.  A link with a search part names a heading.
Without one, a `file:' link names its file, and so does an `id:' link
whose ID Org finds before the first heading, in the file-level
property drawer where org-roam keeps the ID of a file node:
`org-id-open' opens the file at its top for it.  Point does not move."
  (or (plist-get target :search)
      (and (plist-get target :id)
           (save-excursion
             (org-mcp--link-goto target)
             (not (org-before-first-heading-p))))))

(defun org-mcp--goto-heading (target)
  "Move point to the start of the heading TARGET names, or throw a tool error.
TARGET comes from `org-mcp--link-target', and the current buffer
visits its file, widened.  A TARGET naming a whole file, see
`org-mcp--target-heading-p', is refused."
  (org-mcp--link-goto target)
  (unless (and (org-mcp--target-heading-p target) (org-at-heading-p))
    (org-mcp--tool-validation-error
     "Link does not point to a heading: %s"
     (plist-get target :link)))
  ;; A search can land inside the heading's line, on a target or a
  ;; word of the title; every caller starts at the heading.
  (org-back-to-heading t))

(defun org-mcp--read-link
    (link read-heading read-file &optional files)
  "Read what native Org LINK points to.
READ-HEADING is called with no arguments and point at the heading
LINK names.  READ-FILE is called with the file when LINK names a
whole file, see `org-mcp--target-heading-p'.  FILES is the call's
`files' parameter; see `org-mcp--link-target'."
  (let* ((target (org-mcp--link-target link files))
         (file (plist-get target :file)))
    (org-mcp--with-org-file file
      (if (org-mcp--target-heading-p target)
          (progn
            (org-mcp--goto-heading target)
            (funcall read-heading))
        (funcall read-file file)))))

;; Clock helpers

(defun org-mcp--clock-round-time (time)
  "Round TIME per `org-clock-rounding-minutes'.
TIME is an Emacs time value.  Returns rounded time.

This helper mirrors the rounding logic that `org-clock-in' and
`org-clock-out' apply inline before writing CLOCK lines.  Org does
not expose a public rounding helper, so we reimplement the same
semantics: when `org-clock-rounding-minutes' is a positive integer
greater than 1, round the minutes to the nearest multiple and zero
the seconds.  Any change to Org's rounding behaviour should be
reflected here."
  (if (and (boundp 'org-clock-rounding-minutes)
           (numberp org-clock-rounding-minutes)
           (> org-clock-rounding-minutes 1))
      (let* ((r org-clock-rounding-minutes)
             (decoded (decode-time time))
             (minutes (nth 1 decoded))
             (rounded (* r (round minutes r))))
        (apply #'encode-time
               (append (list 0 rounded) (nthcdr 2 decoded))))
    time))

(defun org-mcp--clock-format-timestamp (time)
  "Format TIME as an inactive Org clock timestamp, e.g. `[YYYY-MM-DD Day HH:MM]'.
Delegates the format to `org-time-stamp-format' so the output tracks
Org's own `org-timestamp-formats' customization."
  (format-time-string (org-time-stamp-format t t) time))

(defun org-mcp--clock-parse-timestamp (str)
  "Parse ISO timestamp STR to Emacs time.
STR should be in ISO 8601 format like 2026-03-23T14:30:00.
The `T' separator is normalised to a space so `org-time-string-to-time'
accepts it."
  (let ((normalised (replace-regexp-in-string "T" " " (or str ""))))
    (condition-case _
        (org-time-string-to-time normalised)
      (error
       (org-mcp--tool-validation-error "Cannot parse timestamp: '%s'"
                                       str)))))

(defun org-mcp--clock-duration-string (seconds)
  "Format SECONDS as clock duration string `H:MM'.
Delegates to `org-duration-from-minutes' with the `h:mm' specifier
that Org itself uses for CLOCK lines."
  (org-duration-from-minutes (round seconds 60) 'h:mm))

(defun org-mcp--clock-element-start-str (clock)
  "Return CLOCK element's start timestamp text, without surrounding brackets.
CLOCK is an Org element of type `clock'.  For a closed-range clock
the returned text is the start half of the range."
  (let ((raw
         (org-element-property
          :raw-value (org-element-property :value clock))))
    (when (and raw (string-match "\\`\\[\\([^]]+\\)\\]" raw))
      (match-string 1 raw))))

(defun org-mcp--clock-element-start-time (clock)
  "Return CLOCK element's start as an Emacs time value."
  (org-timestamp-to-time (org-element-property :value clock)))

(defun org-mcp--clock-element-end-time (clock)
  "Return CLOCK element's end as an Emacs time value.
Returns nil for open (unclosed) clocks."
  (let ((value (org-element-property :value clock)))
    (when (eq (org-element-property :type value) 'inactive-range)
      (org-timestamp-to-time value t))))

(defmacro org-mcp--with-wide-clock-buffer (file &rest body)
  "Run BODY in the buffer `org-find-open-clocks' searches for FILE, widened.
`org-find-open-clocks' searches only the accessible part of that
buffer, so it misses a clock outside the user's narrowing, and so does
a read at a clock's marker.  BODY runs with the buffer, found the same
way, widened, and the user's narrowing is restored afterwards."
  (declare (indent 1) (debug (form body)))
  `(with-current-buffer (or (get-file-buffer ,file)
                            (find-file-noselect ,file))
     (org-with-wide-buffer ,@body)))

(defun org-mcp--clock-find-active ()
  "Return the open CLOCK entry currently in effect, or nil when there is none.
The Emacs session's own running clock is authoritative: whenever
`org-clock-is-active' reports one whose CLOCK line is still open, that
clock is described, whether or not its file is in the allowed list.
A line closed since by a hand that left the Emacs clock pointing at
it, such as the user's own edit, leaves no clock running there.  With
no running clock, allowed files are scanned in order with
`org-find-open-clocks', each in full even where the user's buffer is
narrowed, see `org-mcp--with-wide-clock-buffer', and the first
dangling CLOCK line is described, which keeps clocks left unclosed by
an earlier session or another tool discoverable.

The value is an alist with keys `file', `heading', `start', `allowed'
and `marker'.  `allowed' is t when the clock's file is in the allowed
list and nil otherwise.  Start timestamps are read through the Org
element API."
  (or (when-let* ((buf (org-clock-is-active))
                  (file (buffer-file-name buf)))
        (with-current-buffer buf
          (org-with-wide-buffer
           (goto-char org-clock-marker)
           (let ((el (org-element-at-point)))
             (when (and (eq (org-element-type el) 'clock)
                        (eq
                         (org-element-property :status el) 'running))
               (list
                (cons 'file (expand-file-name file))
                (cons
                 'heading
                 (save-excursion
                   (org-back-to-heading t)
                   (org-mcp--title-at-point)))
                (cons 'start (org-mcp--clock-element-start-str el))
                (cons
                 'allowed (and (org-mcp--find-allowed-file file) t))
                (cons 'marker org-clock-marker)))))))
      (catch 'found
        (dolist (file (org-mcp--expanded-allowed-files))
          (when (file-exists-p file)
            (org-mcp--with-wide-clock-buffer file
              (when-let* ((open (org-find-open-clocks file))
                          (marker (car (car open))))
                (with-current-buffer (marker-buffer marker)
                  (save-excursion
                    (goto-char marker)
                    (let* ((el (org-element-at-point))
                           (start-str
                            (org-mcp--clock-element-start-str el))
                           (heading
                            (save-excursion
                              (org-back-to-heading t)
                              (org-mcp--title-at-point))))
                      (throw 'found
                             (list
                              (cons 'file (expand-file-name file))
                              (cons 'heading heading)
                              (cons 'start start-str)
                              (cons 'allowed t)
                              (cons 'marker marker))))))))))
        nil)))

(defun org-mcp--clock-check-clock-out (active clock-out)
  "Refuse a clock-in unless CLOCK-OUT names the running clock ACTIVE.
ACTIVE is the running clock as `org-mcp--clock-find-active' returns
it, or nil when none runs.  CLOCK-OUT is the call's `clock_out'
parameter, a link to the heading of the running clock; a value
`org-mcp--link-given' reads as blank counts as not sent.

With no clock running, a CLOCK-OUT is refused: it names no clock.  A
clock running outside the allowed files is refused whatever CLOCK-OUT
holds, since org-mcp tells a client nothing about that clock, not even
its heading.  Any other running clock needs a CLOCK-OUT that names the
heading holding its CLOCK line.  An `id:' CLOCK-OUT is looked up in
the running clock's file only, with no ID index and no `files', and a
`file:' CLOCK-OUT must name that file.  The link `org-mcp--link-at-point'
makes for that heading names it too, even when, as a title link, it
resolves to an earlier heading of the same title.  A missing or wrong
CLOCK-OUT is refused with the running clock's heading named by its
title and link, and the clock's start, so the client can ask the user
about it.  Nothing is changed.

A CLOCK-OUT that disagrees with the running clock is a conflict: the
client believed something about the world that no longer holds, and
reading the clock again is what puts it right."
  (let ((clock-out (org-mcp--link-given clock-out)))
    (cond
     ((not active)
      (when clock-out
        (org-mcp--tool-conflict-error
         "clock_out names a clock to close, but no clock is running: %s"
         clock-out)))
     ((not (alist-get 'allowed active))
      (org-mcp--tool-validation-error
       "A clock is running in a file outside the allowed files.  Ask the \
user to clock out of it before clocking in"))
     (t
      (let ((file (alist-get 'file active))
            (marker (alist-get 'marker active)))
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker) (org-back-to-heading t)
           (let* ((heading (point))
                  (link (org-mcp--link-at-point))
                  (running
                   (format "'%s' (%s) since %s"
                           (alist-get 'heading active)
                           link
                           (alist-get 'start active))))
             (unless clock-out
               (org-mcp--tool-conflict-error
                "A clock is running on %s.  Ask the user whether to \
clock out of it, then send its link as clock_out"
                running))
             (let ((target (org-mcp--link-target clock-out nil file)))
               (unless (or
                        ;; A title link org-mcp handed out for the
                        ;; running heading finds the first heading of
                        ;; that title, which may be another one.
                        (equal
                         (org-element-property
                          :raw-link (org-mcp--link-parse clock-out))
                         link)
                        (and
                         (org-mcp--paths-equal-p
                          (plist-get target :file) file)
                         ;; A link that resolves to no heading in
                         ;; the file names no running clock either.
                         (ignore-error mcp-server-lib-tool-error
                           (org-mcp--goto-heading target)
                           (= (point) heading))))
                 (org-mcp--tool-conflict-error
                  "clock_out does not name the running clock: %s.  \
The clock runs on %s"
                  clock-out running)))))))))))

(defun org-mcp--clock-save-closed (buf file preexisting-modified-p)
  "Save BUF, which holds the clock org-mcp closed, and report a failed save.
FILE is the file BUF visits.  A non-nil PREEXISTING-MODIFIED-P leaves
BUF unsaved with the user's own edits in it, see
`org-mcp--maybe-save-buffer'.

Closing the clock also stopped Emacs's own clock, which undo cannot
start again, so a failed save is not undone the way
`org-mcp--modify-and-save' undoes one.  The call fails instead with a
message saying that the clock was closed and whether the file holds
the close, so a client neither closes the clock a second time nor
takes it for still running."
  (condition-case err
      (org-mcp--maybe-save-buffer buf file preexisting-modified-p)
    (error
     (if (buffer-modified-p buf)
         (org-mcp--tool-validation-error
          "The running clock was closed but not saved: %s"
          (error-message-string err))
       (org-mcp--saved-then-failed-error
        "The running clock was closed and saved" err)))))

(defun org-mcp--clock-find-last-closed ()
  "Return the most recent closed-clock end time across allowed files.
Walks clock elements via `org-element-map' and picks the latest
`:value' end timestamp.  Returns an Emacs time, or nil when no closed
clocks exist."
  (let ((latest nil))
    (dolist (file (org-mcp--expanded-allowed-files))
      (when (file-exists-p file)
        (org-mcp--with-org-file file
          (org-element-map
           (org-element-parse-buffer 'element) 'clock
           (lambda (clock)
             (when (eq (org-element-property :status clock) 'closed)
               (let ((end-time
                      (org-mcp--clock-element-end-time clock)))
                 (when (and end-time
                            (or (not latest)
                                (time-less-p latest end-time)))
                   (setq latest end-time)))))))))
    latest))

(defun org-mcp--insert-log-note
    (note purpose &optional state prev-state)
  "Insert NOTE at current heading via Org's log-note machinery.

NOTE is the user-supplied note text (may be multi-line).
PURPOSE is a symbol from `org-log-note-headings' (e.g. `note', `state').
STATE and PREV-STATE are the new and previous TODO state strings used
when PURPOSE is `state'.

Honors `org-log-note-headings' for the entry template and
`org-log-into-drawer' for placement.  The interactive
`org-add-log-note' post-command-hook is bypassed by populating the
*Org Note* buffer directly and calling `org-store-log-note', which
already does the formatting and insertion."
  (move-marker org-log-note-marker (point))
  (setq
   org-log-note-purpose purpose
   org-log-note-state state
   org-log-note-previous-state prev-state
   org-log-note-extra nil
   org-log-note-effective-time (org-current-effective-time))
  (move-marker org-log-note-return-to (point))
  (setq org-log-note-window-configuration
        (current-window-configuration))
  (save-current-buffer
    (set-buffer (get-buffer-create "*Org Note*"))
    (erase-buffer)
    (insert note)
    (org-store-log-note)))

(defun org-mcp--clock-insert-entry (start &optional end)
  "Insert CLOCK line at current heading.
START is the clock start time.  END is optional clock end time.
If END is provided, inserts a closed clock entry with duration.

Placement is delegated to `org-clock-find-position', which respects
`org-clock-into-drawer' and `org-clock-drawer-name' the same way
`org-clock-in' would: a drawer (LOGBOOK or custom) is created when
configured, and CLOCK lines are inserted bare under the heading
when `org-clock-into-drawer' is nil and no drawer already exists.

Why this writes the CLOCK line by hand instead of calling
`org-clock-in' / `org-clock-out': `org-mcp--modify-and-save' may run
inside an existing user buffer, and those Org APIs start mode-line/idle
timers, set `org-clock-marker' and `org-clock-hd-marker', push to
`org-clock-history', and invoke `org-resolve-clocks' interactively
on dangling clocks -- all of which either leak timers, leave stale
global clock state behind, or block a non-TTY MCP server.
Formatting is delegated to Org via `org-mcp--clock-format-timestamp'
and `org-mcp--clock-duration-string', and the `CLOCK:' prefix uses
`org-clock-string' so the wire format tracks Org's own constant."
  (org-back-to-heading t)
  (org-clock-find-position nil)
  (if end
      (let* ((duration (float-time (time-subtract end start)))
             ;; `org-clock-out' writes the duration as `%2d:%02d', so
             ;; an hour count below ten is padded to two columns.  Org
             ;; exposes no helper for that padding, so mirror it here
             ;; and keep every closed CLOCK line of a file alike,
             ;; whichever writer made it.
             (dur-str
              (format "%5s"
                      (org-mcp--clock-duration-string duration))))
        (insert
         (format "%s %s--%s => %s\n"
                 org-clock-string
                 (org-mcp--clock-format-timestamp start)
                 (org-mcp--clock-format-timestamp end)
                 dur-str)))
    (insert
     (format "%s %s\n"
             org-clock-string
             (org-mcp--clock-format-timestamp start)))))

(defun org-mcp--clock-resolve-dangling ()
  "Delete unclosed CLOCK entries under current heading.
Point must be at a heading.  Running clocks under the current
subtree are discovered via `org-element-map', then each deletion
is delegated to Org's `org-clock-clock-cancel', which removes the
CLOCK line and collapses the containing drawer when it becomes
empty via `org-remove-empty-drawer-at'.  Returns count of deleted
entries.

`org-find-open-clocks' is deliberately not used here because another
buffer may already be visiting the same file (e.g. the buffer opened
earlier by `org-mcp--clock-find-active'). That API returns markers in
whichever buffer `get-file-buffer' finds first, which is not guaranteed
to be the buffer currently being edited. Element-map on the current
buffer guarantees the markers we operate on."
  (org-back-to-heading t)
  (let* ((subtree-begin (point))
         (subtree-end
          (save-excursion
            (org-end-of-subtree t t)
            (point)))
         (clocks nil)
         (count 0))
    (save-restriction
      (narrow-to-region subtree-begin subtree-end)
      (org-element-map
       (org-element-parse-buffer 'element) 'clock
       (lambda (el)
         (when (eq (org-element-property :status el) 'running)
           ;; Build a (marker . start-time) cons matching the shape
           ;; `org-clock-clock-cancel' expects: marker positioned at
           ;; end-of-line of `CLOCK: [...]' so that
           ;; `org-clock-cancel's `looking-back' succeeds.
           (save-excursion
             (goto-char (org-element-property :begin el))
             (end-of-line)
             (push (cons
                    (copy-marker (point) t)
                    (org-mcp--clock-element-start-time el))
                   clocks))))))
    (dolist (clock clocks)
      (org-clock-clock-cancel clock)
      (cl-incf count))
    count))

(defun org-mcp--clock-remove-empty-logbook ()
  "Remove the clock drawer at current heading if it is empty.
Point must be at a heading.  The drawer name comes from
`org-clock-drawer-name', which respects `org-clock-into-drawer'
(returns nil when clocks are not placed in a drawer; in that case
there is nothing to clean up).  When a custom drawer name is
configured we also sweep the legacy `LOGBOOK' name so previously
captured clocks are tidied alongside new ones.  Delegates to
`org-remove-empty-drawer-at', which is a no-op when the drawer has
any contents (additional CLOCK entries, state notes, plain notes,
etc.)."
  (save-excursion
    (org-back-to-heading t)
    (let* ((subtree-begin (point))
           (subtree-end
            (save-excursion
              (org-end-of-subtree t t)
              (point)))
           (configured (org-clock-drawer-name))
           (names
            (delete-dups (delq nil (list configured "LOGBOOK")))))
      (save-restriction
        (narrow-to-region subtree-begin subtree-end)
        (dolist (name names)
          (let ((drawer-pos
                 (org-element-map
                  (org-element-parse-buffer 'element) 'drawer
                  (lambda (el)
                    (when (equal
                           (org-element-property
                            :drawer-name el)
                           name)
                      (org-element-property :begin el)))
                  nil t)))
            (when drawer-pos
              (org-remove-empty-drawer-at drawer-pos))))))))

(defun org-mcp--clock-delete-entry (start-time)
  "Delete CLOCK entry whose start matches START-TIME under current heading.
Point must be at a heading.  START-TIME is an Emacs time value.
Walks clock elements via `org-element-map' and deletes by :begin / :end
positions.  Removes the LOGBOOK drawer if it becomes empty.
Returns an alist with deleted entry info, or nil if not found."
  (org-back-to-heading t)
  (let* ((subtree-begin (point))
         (subtree-end
          (save-excursion
            (org-end-of-subtree t t)
            (point)))
         (target (float-time start-time))
         (found nil))
    (save-restriction
      (narrow-to-region subtree-begin subtree-end)
      (let* ((tree (org-element-parse-buffer 'element))
             (match
              (catch 'match
                (org-element-map
                 tree 'clock
                 (lambda (clock)
                   (let ((s
                          (float-time
                           (org-mcp--clock-element-start-time
                            clock))))
                     (when (= s target)
                       (throw 'match clock)))))
                nil)))
        (when match
          (let* ((begin (org-element-property :begin match))
                 (end (org-element-property :end match))
                 (end-time (org-mcp--clock-element-end-time match))
                 (duration (org-element-property :duration match))
                 (start-str
                  (org-mcp--clock-format-timestamp start-time))
                 (end-str
                  (when end-time
                    (org-mcp--clock-format-timestamp end-time))))
            (setq found
                  `((start . ,start-str)
                    ,@
                    (when end-str
                      `((end . ,end-str)))
                    ,@
                    (when duration
                      `((duration . ,duration)))))
            (delete-region begin end)))))
    (when found
      (org-mcp--clock-remove-empty-logbook))
    found))

(defun org-mcp--validate-todo-state (state)
  "Validate STATE is a valid TODO keyword.
Reads the buffer-local `org-todo-keywords-1', which Org populates
from the user customization merged with any per-file `#+TODO:'
directives.  Must be called from within an Org-mode buffer (e.g.
inside `org-mcp--modify-and-save')."
  (unless (member state org-todo-keywords-1)
    (org-mcp--tool-validation-error
     "Invalid TODO state: '%s' - valid states: %s"
     state (mapconcat #'identity org-todo-keywords-1 ", "))))

(defun org-mcp--todo-block-reason (from to)
  "Return what Org names for vetoing the change FROM to TO, or nil.
Point is on the heading.  FROM is its TODO keyword, or nil when it
has none; TO is the keyword the call asks for.  A non-nil result
means Org refuses the change: the blocker it names, as a string, or
t when it names none.

`org-todo' runs `org-blocker-hook' itself, but called from Lisp it
reports a veto only by leaving the entry alone and writing a
message, which no caller can read.  The hook is therefore asked here
first, with the change plist `org-todo' builds and under the same
`org-inhibit-blocking' and `NOBLOCKING' exemptions, so that the veto
is known before anything is written.  Org has no function that
answers for one named transition: `org-entry-blocked-p' asks only
whether the entry may be finished.  `org-todo' asks the hook again
for a change that goes through, which is why the hook is a predicate
and not a place for side effects."
  (when (and org-blocker-hook
             (not org-inhibit-blocking)
             (not (org-entry-get nil "NOBLOCKING")))
    (let ((org-blocked-by-checkboxes nil)
          (position
           (save-excursion
             (org-back-to-heading t)
             (point))))
      (unless (save-excursion
                (save-match-data
                  (org-with-wide-buffer
                   (run-hook-with-args-until-failure
                    'org-blocker-hook
                    (list
                     :type 'todo-state-change
                     :from from
                     :to to
                     :position position)))))
        (cond
         (org-blocked-by-checkboxes
          "contained checkboxes")
         ((org-string-nw-p org-block-entry-blocking)
          (format "\"%s\"" org-block-entry-blocking))
         (t
          t))))))

(defun org-mcp--set-todo-state (state)
  "Set the TODO state of the heading at point to STATE.
Returns the state Org left the heading in, which is read back rather
than assumed: `org-auto-repeat-maybe' resets a repeating entry moved
to a done keyword to its not-done keyword, and `REPEAT_TO_STATE'
picks which one.

A change Org vetoes -- a TODO dependency, an unchecked checkbox, an
ordered subtree -- is refused before anything is written, so the
heading keeps the state it had and the caller saves nothing.  The
refusal carries Org's own reason for it where Org names one."
  (let ((previous (org-get-todo-state)))
    (when-let* ((blocker (org-mcp--todo-block-reason previous state)))
      (org-mcp--tool-blocked-error
       "TODO state change from %s to %s blocked%s"
       (or previous "(no state)")
       state
       (if (stringp blocker)
           (format " (by %s)" blocker)
         "")))
    ;; Bind `post-command-hook' to nil so that any interactive log-note
    ;; hook `org-todo' may schedule (e.g. when `org-log-done' is set)
    ;; cannot fire later -- org-mcp attaches its own note explicitly
    ;; through `org-mcp--insert-log-note'.
    (let ((post-command-hook nil))
      (org-todo state))
    (or (org-get-todo-state) "")))

(defun org-mcp--mutex-tag-groups (alist)
  "Return mutex tag groups from ALIST as a list of lists of tag strings.
A mutex group is delimited by `:startgroup' / `:endgroup' tokens.

Org's `org-tag-alist-to-groups' covers only the grouptag form
\(`:startgrouptag' / `:grouptags' / `:endgrouptag'); it does not
expose mutex (`:startgroup' / `:endgroup') groups, and Org has no
other public API that does.  Revisit if Org gains one."
  (let (groups
        current
        in-group)
    (dolist (entry alist)
      (let ((token
             (if (consp entry)
                 (car entry)
               entry)))
        (cond
         ((eq token :startgroup)
          (setq
           in-group t
           current nil))
         ((eq token :endgroup)
          (when (and in-group current)
            (push (nreverse current) groups))
          (setq
           in-group nil
           current nil))
         ((and in-group (stringp token))
          (push token current)))))
    (nreverse groups)))

(defun org-mcp--validate-and-normalize-tags (tags)
  "Validate and normalize TAGS.
TAGS can be a single tag string or list of tag strings.
Returns normalized tag list.

Org permits free-form tags in headlines, so any name matching
`org-tag-re' is accepted regardless of whether it appears in
`org-tag-alist' or `org-tag-persistent-alist'.  Mutual-exclusivity
groups (`:startgroup' / `:endgroup') in those alists are still
enforced because they express a conflict, not an allow-list."
  (let ((tag-list (org-mcp--normalize-tags-to-list tags))
        (tag-name-re (concat "\\`" org-tag-re "\\'")))
    (dolist (tag tag-list)
      (unless (string-match-p tag-name-re tag)
        (org-mcp--tool-validation-error "Invalid tag name: %s" tag)))
    (when org-tag-alist
      (org-mcp--validate-mutex-tag-groups tag-list org-tag-alist))
    (when org-tag-persistent-alist
      (org-mcp--validate-mutex-tag-groups
       tag-list org-tag-persistent-alist))
    tag-list))

(defun org-mcp--validate-mutex-tag-groups (tags tag-alist)
  "Validate that TAGS don't violate mutex groups in TAG-ALIST.
TAGS is a list of tag strings.
Errors if multiple tags from same mutex group."
  (dolist (group (org-mcp--mutex-tag-groups tag-alist))
    (let ((conflict (cl-intersection tags group :test #'string=)))
      (when (> (length conflict) 1)
        (org-mcp--tool-validation-error
         "Tags %s are mutually exclusive (cannot use together)"
         (mapconcat (lambda (tag) (format "'%s'" tag)) conflict
                    ", "))))))

(defun org-mcp--validate-headline-title (title)
  "Validate that TITLE is not empty or whitespace-only.
Throws an MCP tool error if validation fails."
  (when (or (string-empty-p title)
            (string-match-p "^[[:space:]]*$" title)
            ;; Explicitly match NBSP for Emacs 27.2 compatibility
            ;; In Emacs 27.2, [[:space:]] doesn't match NBSP (U+00A0)
            (string-match-p "^[\u00A0]*$" title))
    (org-mcp--tool-validation-error
     "Headline title cannot be empty or contain only whitespace"))
  (when (string-match-p "[\n\r]" title)
    (org-mcp--tool-validation-error
     "Headline title cannot contain newlines")))

(defun org-mcp--validate-date-string (date-str)
  "Validate that DATE-STR is a recognizable date format.
Accepts ISO-like dates: YYYY-MM-DD with optional HH:MM time.
Throws an MCP tool error if the format is invalid."
  (unless
      (string-match-p
       "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( [0-9]\\{2\\}:[0-9]\\{2\\}\\)?$"
       date-str)
    (org-mcp--tool-validation-error
     "Invalid date format '%s' - expected YYYY-MM-DD or YYYY-MM-DD HH:MM"
     date-str)))

(defun org-mcp--validate-body-no-headlines (body level)
  "Validate that BODY doesn't contain headlines at LEVEL or higher.
LEVEL is the Org outline level (1 for *, 2 for **, etc).
Throws an MCP tool error if invalid headlines are found."
  ;; Build regex to match headlines at the current level or higher
  ;; For level 3, this matches ^*, ^**, or ^***
  ;; Matches asterisks + space/tab (headlines need content)
  (let ((regex (format "^\\*\\{1,%d\\}[ \t]" level)))
    (when (string-match regex body)
      (org-mcp--tool-validation-error
       "Body cannot contain headlines at level %d or higher"
       level))))

(defun org-mcp--validate-body-no-unbalanced-blocks (body)
  "Validate that BODY doesn't contain unbalanced blocks.
Each #+BEGIN_/#+END_ marker in BODY must be part of a balanced block
element as classified by Org's parser.  Org's block element types all
share the `-block' suffix (e.g. `example-block', `src-block',
`special-block'), so every #+BEGIN_X / #+END_X line whose enclosing
element does not end in `-block' is unbalanced.  Markers nested inside
another block are correctly ignored as literal text by
`org-element-at-point'.  A marker may be indented, as in a list item,
and a line Org reads as a heading inside a block leaves the block
unbalanced.
Throws an MCP tool error if unbalanced blocks are found."
  (with-temp-buffer
    (let ((org-inhibit-startup t))
      (delay-mode-hooks
        (org-mode)))
    (insert body)
    (goto-char (point-min))
    ;; Org's parser has no element for an unbalanced block: it reads the
    ;; marker line as plain text, so no `org-element-map' finds it.  The
    ;; markers are therefore found by text and classified by the parser,
    ;; as `org-lint-invalid-block' does, with the leading whitespace
    ;; Org's block syntax allows.
    (while (re-search-forward
            "^[ \t]*#\\+\\(BEGIN\\|END\\|begin\\|end\\)_\\(\\S-+\\)"
            nil t)
      (let* ((marker-type (upcase (match-string 1)))
             (block-type (upcase (match-string 2)))
             (etype (org-element-type (org-element-at-point))))
        (unless (and etype
                     (string-suffix-p "-block" (symbol-name etype)))
          (cond
           ((string= marker-type "BEGIN")
            (org-mcp--tool-validation-error
             "Body contains unclosed %s block"
             block-type))
           ((string= marker-type "END")
            (org-mcp--tool-validation-error
             "Orphaned END_%s without BEGIN_%s"
             block-type block-type))))))))

(defun org-mcp--normalize-tags-to-list (tags)
  "Normalize TAGS parameter to a list format.
TAGS can be:
- nil or empty list -> returns nil
- vector (JSON array) -> converts to list
- string -> wraps in list
- list -> returns as-is
Throws error for invalid types."
  (cond
   ((null tags)
    nil) ; No tags (nil or empty list)
   ((vectorp tags)
    (append tags nil)) ; Convert JSON array (vector) to list
   ((listp tags)
    tags) ; Already a list
   ((stringp tags)
    (list tags)) ; Single tag string
   (t
    (org-mcp--tool-validation-error "Invalid tags format: %s" tags))))

(defun org-mcp--navigate-to-parent-or-top (parent)
  "Navigate to the parent headline PARENT names, or to the file's top level.
PARENT is a target plist as from `org-mcp--link-target'; one that
names a whole file, see `org-mcp--target-heading-p', means top level.
Returns parent level (integer) if parent exists, nil for top-level.
At the top level, point goes to the start of the file's first heading,
or to the end of a file with none.  That is past the file's preamble,
everything before the first heading: a file-level property drawer,
keyword lines such as #+TITLE and any text, which a new heading must
neither split nor join.
Assumes point is in an Org buffer."
  (if (org-mcp--target-heading-p parent)
      (progn
        (org-mcp--goto-heading parent)
        ;; Save parent level before moving point
        ;; Ensure we're at the beginning of headline
        (org-back-to-heading t)
        (org-current-level))
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (outline-next-heading))
    nil))

(defun org-mcp--goto-after-child (target parent)
  "Move point past the subtree of the child of PARENT that TARGET names.
TARGET comes from `org-mcp--link-target'.  PARENT is the position of
the parent heading in the current buffer, or nil for the top level of
the file, where the child is a heading with no parent.  Throws a
validation error unless TARGET names such a heading in the current
buffer's file.  An ID this buffer does not hold, such as one in
another file, is such an error too, not an unknown ID: the sibling
is only ever looked for here."
  (unless (and (org-mcp--paths-equal-p
                (plist-get target :file) (buffer-file-name))
               (or (not (plist-get target :id))
                   (org-with-wide-buffer
                    (org-find-entry-with-id (plist-get target :id))))
               (progn
                 (org-mcp--goto-heading target)
                 (save-excursion
                   (if parent
                       (and (org-up-heading-safe) (= (point) parent))
                     (not (org-up-heading-safe))))))
    (org-mcp--tool-validation-error
     "Sibling %s not found under parent"
     (plist-get target :link)))
  (org-end-of-subtree t t))

(defun org-mcp--position-for-new-child (previous-sibling parent-level)
  "Position point where a new heading goes under its parent.
PARENT-LEVEL is the parent's level, with point at the parent heading,
or nil for the top level of the file, with point past the file's
preamble, where the heading goes when PREVIOUS-SIBLING is nil.
PREVIOUS-SIBLING is nil or the target, from `org-mcp--link-target',
of the sibling to insert after: a direct child of the parent, or a
heading with no parent at the top level.
If PREVIOUS-SIBLING is non-nil, positions after that sibling's subtree.
If nil, positions at end of parent's subtree.
Throws validation error if the sibling is not found under the parent."
  (cond
   (previous-sibling
    (org-mcp--goto-after-child
     previous-sibling
     (and parent-level
          (progn
            (org-back-to-heading t)
            (point)))))
   (parent-level
    ;; No sibling named: insert at end of parent's subtree.  With
    ;; TO-HEADING, `org-end-of-subtree' stops at the start of the next
    ;; heading or at the end of the buffer.  At a heading, go back one
    ;; char to be at the end of the parent's content.  No heading
    ;; predicate is asked: at the end of the buffer, the last line may
    ;; be the parent's own heading, with no newline after it.
    (org-end-of-subtree t t)
    (unless (eobp)
      (backward-char 1)))))

(defun org-mcp--ensure-newline ()
  "Ensure there is a newline or buffer start before point."
  (unless (or (bobp) (looking-back "\n" 1))
    (insert "\n")))

(defun org-mcp--insert-heading (title parent-level)
  "Insert a new Org heading at the appropriate level.
TITLE is the headline text to insert.
PARENT-LEVEL is the parent's heading level (integer) if inserting
as a child, or nil if inserting at top-level.
Assumes point is positioned where the heading should be inserted.
After insertion, point is left on the heading line at end-of-line."
  (if parent-level
      ;; We're inside a parent.  Pass the explicit LEVEL argument to
      ;; `org-insert-heading' so the new heading lands at parent + 1
      ;; regardless of what heading point currently sits inside (e.g.
      ;; at the end of a sibling subtree positioned via previous_sibling).
      ;; This is what avoids the "creates a sibling of the parent
      ;; instead of a child" pitfall of bare `org-insert-heading' when
      ;; the parent has no children.
      (progn
        (org-mcp--ensure-newline)
        (org-insert-heading nil nil (1+ parent-level))
        (insert title))
    ;; Top-level heading
    ;; Check if there are no headlines yet (empty buffer or only
    ;; headers before us).  `outline-next-heading' moves past a
    ;; heading at point, so the first line is asked on its own.
    (let ((has-headline
           (save-excursion
             (goto-char (point-min))
             (or (org-at-heading-p) (outline-next-heading)))))
      (if (not has-headline)
          (progn
            (org-mcp--ensure-newline)
            (insert "* "))
        ;; Has headlines - use `org-insert-heading'
        ;; Ensure proper spacing before inserting
        (org-mcp--ensure-newline)
        (org-insert-heading nil nil t))
      (insert title))))

(defun org-mcp--replace-body-content
    (old-body new-body body-content body-begin body-end)
  "Replace body content in the current buffer.
OLD-BODY is the non-empty substring to replace.
NEW-BODY is the replacement text.
BODY-CONTENT is the current body content string.
BODY-BEGIN is the buffer position where body starts.
BODY-END is the buffer position where body ends."
  (let ((new-body-content
         ;; Case matters, as when the caller counted the occurrences.
         (let ((pos
                (let ((case-fold-search nil))
                  (string-match
                   (regexp-quote old-body) body-content))))
           (if pos
               (concat
                (substring body-content 0 pos)
                new-body
                (substring body-content (+ pos (length old-body))))
             body-content))))
    (delete-region body-begin body-end)
    (goto-char body-begin)
    (insert new-body-content)))

;; Tool handlers

(defun org-mcp--tool-config-todo ()
  "Return the TODO keyword configuration.
Walks `org-todo-keywords' directly rather than the parsed
`org-todo-keywords-1' / `org-done-keywords' so the response can
preserve each keyword's raw form (the fast-access key plus
state-logging directives, e.g. \"TODO(t!)\" = fast key `t' and
log a timestamp on entry) along with the explicit `\"|\"'
separator position.  Clients of this tool depend on those
fields, and the parsed siblings discard them."
  (let ((seq-list '())
        (sem-list '()))
    (dolist (seq org-todo-keywords)
      (let* ((type (car seq))
             (keywords (cdr seq))
             (type-str (symbol-name type))
             (keyword-vec [])
             (before-bar t))
        (dolist (kw keywords)
          (if (string= kw "|")
              (setq before-bar nil)
            ;; Check if this is the last keyword and no "|" seen
            (let ((is-last-no-bar
                   (and before-bar (equal kw (car (last keywords))))))
              (when is-last-no-bar
                (setq keyword-vec (vconcat keyword-vec ["|"])))
              (push `((state
                       .
                       ,(car (org-remove-keyword-keys (list kw))))
                      (isFinal
                       . ,(or is-last-no-bar (not before-bar)))
                      (sequenceType . ,type-str))
                    sem-list)))
          (setq keyword-vec (vconcat keyword-vec (vector kw))))
        (push
         `((type . ,type-str) (keywords . ,keyword-vec)) seq-list)))
    (json-encode
     `((sequences . ,(vconcat (nreverse seq-list)))
       (semantics . ,(vconcat (nreverse sem-list)))))))

(defun org-mcp--tool-config-tags ()
  "Return the tag configuration as literal Elisp strings."
  (json-encode
   `((org-use-tag-inheritance
      .
      ,(prin1-to-string org-use-tag-inheritance))
     (org-tags-exclude-from-inheritance
      . ,(prin1-to-string org-tags-exclude-from-inheritance))
     (org-tag-alist . ,(prin1-to-string org-tag-alist))
     (org-tag-persistent-alist
      . ,(prin1-to-string org-tag-persistent-alist)))))

(defun org-mcp--tool-config-tag-candidates (&optional files)
  "Return the union of all candidate tags across a set of files.
The files are the ones FILES names, see `org-mcp--with-file-set',
or the allowed files when FILES is nil.
Mirrors the set Org's interactive tag completion offers via
`org-global-tags-completion-table': configured tags from
`org-tag-alist' / `org-tag-persistent-alist', any per-file
`#+TAGS:' / `#+FILETAGS:', plus every tag actually present on
headlines in those files.  Group keywords (`:startgroup' etc.)
are filtered out.  Tags are returned sorted and deduplicated.

MCP Parameters:
  files - Files and directories to collect tags from, replacing the
          allowed files (array of strings, optional)"
  (org-mcp--with-file-set files
    (let* ((table
            (append
             ;; The files are passed explicitly, and none means no
             ;; call: given none, Org takes them from the function
             ;; `org-agenda-files', which while the agenda is
             ;; restricted returns the file it is restricted to,
             ;; whether or not the call may reach it.
             (and org-agenda-files
                  (org-global-tags-completion-table org-agenda-files))
             org-tag-alist org-tag-persistent-alist))
           (tags
            (delete-dups
             (delq
              nil
              (mapcar
               (lambda (entry)
                 (let ((token
                        (if (consp entry)
                            (car entry)
                          entry)))
                   (and (stringp token) token)))
               table)))))
      (json-encode `((tags . ,(vconcat (sort tags #'string<))))))))

(defun org-mcp--tool-config-priority ()
  "Return the priority configuration."
  (json-encode
   `((highest . ,(char-to-string org-priority-highest))
     (lowest . ,(char-to-string org-priority-lowest))
     (default . ,(char-to-string org-priority-default)))))

(defun org-mcp--tool-config-allowed-files ()
  "Return the allowed Org files and the scope override policy.
Each file is returned as an absolute path; relative entries in
`org-mcp-allowed-files' are resolved against `org-directory'.
`override_allowed' reports whether `org-mcp-file-scope-override'
permits naming files outside them: it is t, or a list with at least
one local root.  `override_roots', present only in the second case,
lists those roots as absolute paths."
  (let ((roots (org-mcp--override-roots)))
    (json-encode
     `((files . ,(vconcat (org-mcp--expanded-allowed-files)))
       (override_allowed
        .
        ,(if (or (eq org-mcp-file-scope-override t) roots)
             t
           :json-false))
       ,@
       (when roots
         `((override_roots . ,(vconcat roots))))))))

(defun org-mcp--tool-node-set-todo
    (link after &optional before note files)
  "Update the TODO state of the headline LINK names.
Returns the link to the updated headline, and as the response's
`after' the state Org left it in, which is the state asked for
unless Org made another of it: a repeating entry moved to a done
keyword comes back in its not-done keyword.  A change Org vetoes is refused and nothing
is written; see `org-mcp--set-todo-state'.
AFTER is the new TODO state to set.
BEFORE, when provided, is checked against the actual state.
NOTE, when provided, is stored in LOGBOOK as part of the state change entry.
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.

MCP Parameters:
  link - Link to the headline
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
  after - New TODO state (must be in `org-todo-keywords')
  before - Expected current TODO state (string, optional)
           When provided, must match the actual state or the tool
           errors; omit to skip the check
  note - Optional note to attach to this state transition (string, optional)
         When provided, stored in LOGBOOK as part of the state change entry
         Empty or whitespace-only values are ignored
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (let* ((target (org-mcp--link-target link files))
         (file-path (plist-get target :file))
         (actual-prev nil)
         (actual-new nil))
    (org-mcp--modify-and-save file-path "update"
                              `((before . ,actual-prev)
                                (after . ,actual-new))
      ;; Validate inside the Org buffer so `org-todo-keywords-1'
      ;; reflects merged user-customization + per-file `#+TODO:'.
      (org-mcp--validate-todo-state after)
      (org-mcp--goto-heading target)

      ;; Capture actual previous state
      (beginning-of-line)
      (setq actual-prev (or (org-get-todo-state) ""))

      ;; Check current state matches (only when caller provided it)
      (when before
        (unless (string= actual-prev before)
          (org-mcp--state-mismatch-error
           before (or (org-get-todo-state) "(no state)") "State")))

      ;; Update the state, refusing a change Org vetoes and reading
      ;; back what Org made of the one it took.
      (setq actual-new (org-mcp--set-todo-state after))

      ;; Add note to state transition if provided
      (when (and note (not (string-empty-p (string-trim note))))
        (org-mcp--insert-log-note note 'state after actual-prev)))))

(defun org-mcp--tool-node-create
    (title
     todo
     content
     parent
     &optional
     tags
     previous_sibling
     properties
     files)
  "Add a new TODO item to an Org file.
Returns the new headline's link; no identifier is created, so the
link is `id:' only when PROPERTIES sets an ID.
TITLE is the headline text.
TODO is the TODO state from `org-todo-keywords'.  A state Org
vetoes for the new heading, such as a done keyword under an ordered
parent whose earlier siblings are unfinished, is refused and no
heading is added; see `org-mcp--set-todo-state'.
CONTENT is the optional body text.
PARENT is the link to the parent item, or to a whole file for
its top level.
TAGS is an optional single tag string or list of tag strings.
PREVIOUS_SIBLING is an optional link to the sibling to insert after: a
direct child of the parent, or a heading with no parent when
PARENT names a whole file.  An `id:' PREVIOUS_SIBLING is looked up in
the parent's file.
PROPERTIES is an optional alist of property names and values, checked
by `org-mcp--validate-properties' like those of `org-node-set-properties'.
A blank PROPERTIES, see `org-mcp--blank-param-p', sets none.
FILES, when not blank, names the files an `id:' PARENT is looked
up in; see `org-mcp--link-target'.  It applies to PARENT only.

MCP Parameters:
  title - The headline text
  todo - TODO state from `org-todo-keywords'
  content - Optional body text
  parent - Link to the parent item
           Formats:
             - id:{id}
             - file:{absolute-path}::#{custom-id}
             - file:{absolute-path}::*{title} (first match)
             - file:{absolute-path} (top level of the file)
             - id:{id} of the file-level property drawer (top level
               of the file)
             - any of these as [[link]] or [[link][description]]
  tags - Tags to add (optional, single string or array of strings)
  previous_sibling - Link to the sibling to insert after (optional),
                     a direct child of the parent, or a top-level
                     heading of the file when parent names a whole
                     file
                     Formats:
                       - id:{id}
                       - file:{absolute-path}::#{custom-id}
                       - file:{absolute-path}::*{title} (first match)
                       - any of these as [[link]] or
                         [[link][description]]
  properties - JSON object of properties for the new headline
               (optional), such as ID or CUSTOM_ID
               Values are single-line strings or numbers, written as
               given, or booleans: true or false writes the text t
               or nil; null or empty values are skipped
               Special properties (TODO, TAGS, PRIORITY, etc.) are
               forbidden
               properties itself given as null, false, \"\" or {}
               means no properties
  files - Files and directories to look up an id: link of parent
          in, in order, instead of Emacs's ID index (array of
          strings, optional); refused with any other parent"
  (org-mcp--validate-headline-title title)
  (let*
      ((tag-list (org-mcp--validate-and-normalize-tags tags))
       (property-list
        (unless (org-mcp--blank-param-p properties)
          (org-mcp--validate-properties properties)))
       ;; A link that names a whole file means top level.
       (parent-target (org-mcp--link-target parent files))
       (file-path (plist-get parent-target :file))
       ;; The sibling can only be a child of the parent, or a heading
       ;; with no parent at the top level, so its `id:' link is taken
       ;; to be in the parent's file: no ID index is consulted, and
       ;; neither are FILES.  Resolving it here refuses a bad link
       ;; before the parent's buffer is changed.
       (sibling-target
        (when-let* ((sibling (org-mcp--link-given previous_sibling)))
          (org-mcp--link-target sibling nil file-path))))

    ;; Add the TODO item
    (org-mcp--modify-and-save file-path "add TODO"
                              `((file
                                 .
                                 ,(file-name-nondirectory file-path))
                                (title . ,title))
      ;; Validate inside the Org buffer so `org-todo-keywords-1'
      ;; reflects merged user-customization + per-file `#+TODO:'.
      (org-mcp--validate-todo-state todo)
      (let ((parent-level
             (org-mcp--navigate-to-parent-or-top parent-target)))

        ;; Handle positioning after navigation to parent
        (org-mcp--position-for-new-child sibling-target parent-level)

        ;; Validate body before inserting heading
        ;; Calculate the target level for validation
        (let ((target-level
               (if parent-level
                   ;; Child heading - parent level + 1
                   (1+ parent-level)
                 ;; Top-level heading
                 1)))

          ;; Validate body content if provided
          (when content
            (org-mcp--validate-body-no-headlines content target-level)
            (org-mcp--validate-body-no-unbalanced-blocks content)))

        ;; Insert the new heading
        (org-mcp--insert-heading title parent-level)

        (org-mcp--set-todo-state todo)

        (when tag-list
          (org-set-tags tag-list))

        ;; Add body if provided
        (if content
            (progn
              (end-of-line)
              (insert "\n" content)
              (unless (string-suffix-p "\n" content)
                (insert "\n"))
              ;; Move back to the heading, where the properties go
              (org-back-to-heading t))
          ;; No body - ensure newline after heading
          (end-of-line)
          (unless (looking-at "\n")
            (insert "\n")))

        ;; Set properties once the body is in place; Org puts the
        ;; drawer between the heading and the body.  A client-set ID
        ;; is not added to Org's ID locations, as when a user types
        ;; the property by hand.
        (pcase-dolist (`(,name . ,value) property-list)
          (when value
            (org-set-property name value)))))))

;; Resource handlers

(defun org-mcp--read-structured (link &optional fields files)
  "Return structured JSON for what LINK, a native Org link, points to.
The org-node-read tool and the org://{link} resource both read through
here, so they resolve a link the same way.  A file and a heading come
back as the same node, `org-mcp--node-at-point' builds both, and the
file is the one at level 0.

FIELDS is the org-node-read tool's `fields' parameter, defaulting
to `org-mcp--node-read-fields' and resolved before the link is, so
a misspelled field is refused without opening a file.  The
resource passes none and takes that default: a resource is picked
from a client's UI, which has nowhere to say how much of the node it
wants.

FILES is the org-node-read tool's `files' parameter; see
`org-mcp--link-target'.  The resource passes none."
  (let ((fields
         (org-mcp--node-fields-given
          fields org-mcp--node-read-fields)))
    (org-mcp--read-link link
                        (lambda ()
                          (json-encode
                           (org-mcp--node-at-point fields)))
                        (lambda (_file)
                          (json-encode
                           (org-mcp--node-at-point fields nil t)))
                        files)))

(defun org-mcp--handle-org-resource (params)
  "Handler for the org://{link} template.
PARAMS holds `link', the rest of the URI after `org://' as the client
sent it: mcp-server-lib does not decode template variables.  Its
percent-encoding is undone here, exactly once, by
`org-mcp--percent-decode', so a URI that mixes raw non-ASCII
characters with encoded ones decodes to the same link.

The link is then read as the org-node-read tool reads it, and a tool error,
such as the refusal of a link, becomes a resource error with the same
message."
  (let ((link
         (org-mcp--percent-decode
          (alist-get "link" params nil nil #'string=))))
    (condition-case err
        (org-mcp--read-structured link)
      (mcp-server-lib-tool-error
       (mcp-server-lib-resource-signal-error
        mcp-server-lib-jsonrpc-error-invalid-params (cadr err))))))

(defun org-mcp--tool-node-set-title
    (link before after &optional files)
  "Rename the headline LINK names from BEFORE to AFTER.
Preserves the current TODO state and tags.
Returns the link to the renamed headline.
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.

MCP Parameters:
  link - Link to the headline
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
  before - Current title without TODO state or tags
  after - New title without TODO state or tags
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (org-mcp--validate-headline-title after)

  (let* ((target (org-mcp--link-target link files))
         (file-path (plist-get target :file)))

    ;; Rename the headline in the file
    (org-mcp--modify-and-save file-path "rename"
                              `((before . ,before) (after . ,after))
      ;; Navigate to the headline
      (org-mcp--goto-heading target)

      ;; Verify current title matches
      (beginning-of-line)
      (let ((actual-title (org-mcp--title-at-point)))
        (unless (org-mcp--titles-equal-p actual-title before)
          (org-mcp--state-mismatch-error
           before actual-title "Title")))

      (org-edit-headline after))))

(defun org-mcp--tool-node-set-content
    (link before after &optional append files)
  "Edit or append to body content of an Org node.
LINK is the link to the node to edit.
BEFORE is the substring to search for (replace mode only).
AFTER is the replacement or appended text.
APPEND if non-nil, append AFTER to end of body instead of replacing.
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.

MCP Parameters:
  link - Link to the node
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
  before - Substring to replace within the body (replace mode only).
           Must be unique.  Use \"\" to add to empty nodes.
           Ignored when append is true.
  after - Replacement or appended text
  append - Append to end of body instead of replacing (optional,
           default false); true or \"true\" append, false, \"false\"
           and null replace, and any other value is refused
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (let ((append (org-mcp--boolean-param append "append")))
    (if append
        ;; Append mode
        (progn
          (when (or (null after)
                    (string-empty-p after)
                    (string-match-p "\\`[[:space:]]*\\'" after))
            (org-mcp--tool-validation-error
             "after is the content to append and cannot be empty or \
whitespace-only"))

          (org-mcp--validate-body-no-unbalanced-blocks after)

          (let* ((target (org-mcp--link-target link files))
                 (file-path (plist-get target :file)))

            (org-mcp--modify-and-save file-path "append body" nil
              (org-mcp--goto-heading target)

              (org-mcp--validate-body-no-headlines
               after (org-current-level))

              ;; Save the heading position for the response's link
              (let ((heading-pos (point)))
                (goto-char (cdr (org-mcp--body-bounds)))
                (org-mcp--insert-body-text after)
                ;; Return to the heading for the response's link
                (goto-char heading-pos)))))

      ;; Replace mode
      (org-mcp--validate-body-no-unbalanced-blocks after)

      (let*
          ((target (org-mcp--link-target link files))
           (file-path (plist-get target :file))
           ;; The replacement leaves point at the end of the new body,
           ;; which is the first child's heading when there is one; the
           ;; response links to the heading whose body changed.
           (heading nil))

        (org-mcp--modify-and-save file-path "edit body" nil
          (org-mcp--goto-heading target)
          (setq heading (point-marker))

          (org-mcp--validate-body-no-headlines
           after (org-current-level))

          ;; Get body boundaries
          (let* ((bounds (org-mcp--body-bounds))
                 (body-begin (car bounds))
                 (body-end (cdr bounds))
                 (body-content
                  (buffer-substring-no-properties
                   body-begin body-end))
                 (occurrence-count 0))

            ;; Check if body is empty
            (when (string-match-p "\\`[[:space:]]*\\'" body-content)
              ;; Empty BEFORE + empty body -> add content
              (if (string= before "")
                  ;; Treat as single replacement
                  (setq occurrence-count 1)
                (org-mcp--tool-validation-error
                 "Node has no body content")))

            ;; Count occurrences (unless already handled above)
            (unless (= occurrence-count 1)
              ;; Empty BEFORE with non-empty body is an error
              (if (and (string= before "")
                       (not
                        (string-match-p
                         "\\`[[:space:]]*\\'" body-content)))
                  (org-mcp--tool-validation-error
                   "An empty before asserts the node has no content, \
and this node has some; send the part of the content to replace")
                ;; Normal occurrence counting
                (let ((case-fold-search nil)
                      (search-pos 0))
                  (while (string-match
                          (regexp-quote before) body-content
                          search-pos)
                    (setq occurrence-count (1+ occurrence-count))
                    (setq search-pos (match-end 0))))))

            ;; Validate occurrences
            (cond
             ((= occurrence-count 0)
              (org-mcp--tool-validation-error
               "Body text not found: %s"
               before))
             ((> occurrence-count 1)
              (org-mcp--tool-validation-error
               "Text appears %d times (must be unique)"
               occurrence-count)))

            ;; Perform replacement.  An empty BEFORE got here only with
            ;; a blank body, which AFTER replaces as a whole.
            (if (string= before "")
                (progn
                  (delete-region body-begin body-end)
                  (goto-char body-begin)
                  (org-mcp--insert-body-text after))
              (org-mcp--replace-body-content
               before after body-content body-begin body-end)))

          (goto-char heading)
          (set-marker heading nil))))))

(defun org-mcp--validate-properties (properties)
  "Validate PROPERTIES and return them as (NAME . VALUE) pairs.
PROPERTIES is the alist a JSON object decodes to.  NAME is a string.
VALUE is a string, or nil for a JSON null or an empty string; a JSON
number becomes its decimal text, and JSON true and false become \"t\"
and \"nil\".  Throws a validation error when PROPERTIES is not a
non-empty object, when a name is not a valid Org property name or is
a special property, which has its own tool, or when a value is an
array or object or spans several lines.  Org property values are
single lines, and a line break would add structure such as a heading
to the file.  Values are otherwise taken as given, the strings \"t\"
and \"nil\" included; `ID' and `CUSTOM_ID' are ordinary properties
here."
  (unless (and properties (listp properties))
    (org-mcp--tool-validation-error
     "Properties must be a non-empty JSON object"))
  (mapcar
   (lambda (pair)
     (let ((name
            (if (symbolp (car pair))
                (symbol-name (car pair))
              (car pair)))
           (value (cdr pair)))
       ;; Org has no public predicate for property names.  This is the
       ;; check `org-set-property' and `org-entry-put' make themselves,
       ;; but only once the heading is being edited; making it first
       ;; keeps a refused call from touching the buffer.  It matches
       ;; whitespace by syntax class, so it runs under Org's syntax
       ;; table rather than that of whichever buffer is current.
       (unless (with-syntax-table org-mode-syntax-table
                 (org--valid-property-p name))
         (org-mcp--tool-validation-error "Invalid property name: '%s'"
                                         name))
       (when (member (upcase name) org-mcp--special-properties)
         (org-mcp--tool-validation-error
          "Cannot set special property '%s' - use the dedicated tool"
          name))
       (cons
        name
        (cond
         ((or (null value) (equal value ""))
          nil)
         ;; mcp-server-lib decodes JSON with `json-read-from-string':
         ;; true is t, false is :json-false and null is nil, so false
         ;; writes a value where null deletes.  `org-entry-put' writes
         ;; the text "nil" as given, and `org-entry-get' reads it back
         ;; as nil.
         ((eq value t)
          "t")
         ((eq value :json-false)
          "nil")
         ((numberp value)
          (number-to-string value))
         ((not (stringp value))
          (org-mcp--tool-validation-error
           "Property '%s' must be a string, a number, a boolean or null"
           name))
         ((string-match-p "[\n\r]" value)
          (org-mcp--tool-validation-error
           "Property '%s' must be a single line"
           name))
         (t
          value)))))
   properties))

(defun org-mcp--tool-node-set-properties (link after &optional files)
  "Set or delete properties on the headline LINK names.
AFTER is an alist of property name-value pairs.
String, number and boolean values set the property; null/empty values
delete it.
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.

MCP Parameters:
  link - Link to the headline
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
  after - JSON object of property name-value pairs (required)
          String or number value: set property to that value;
          it must be a single line
          true or false: set property to the text t or nil;
          false keeps the property
          null or empty string: delete the property
          ID and CUSTOM_ID are accepted and written as given
          Special properties (TODO, TAGS, PRIORITY, etc.) are
          forbidden
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (setq after (org-mcp--validate-properties after))
  (let* ((target (org-mcp--link-target link files))
         (file-path (plist-get target :file))
         (set-props nil)
         (deleted-props nil))

    (org-mcp--modify-and-save file-path "set properties"
                              `((properties_set . ,set-props)
                                (properties_deleted . ,deleted-props))
      (org-mcp--goto-heading target)

      (pcase-dolist (`(,key . ,val) after)
        (if val
            (progn
              (org-set-property key val)
              (push key set-props))
          (org-delete-property key)
          (push key deleted-props)))
      (setq set-props (nreverse set-props))
      (setq deleted-props (nreverse deleted-props)))))

(defun org-mcp--tool-node-set-scheduled (link &optional after files)
  "Update SCHEDULED timestamp on the headline LINK names.
AFTER is an ISO date string or nil/empty to remove.
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.

MCP Parameters:
  link - Link to the headline
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
  after - ISO date string (optional)
          Examples: \"2026-03-27\", \"2026-03-27 09:00\"
          nil or empty string removes the timestamp
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (let* ((target (org-mcp--link-target link files))
         (file-path (plist-get target :file))
         (previous-scheduled nil)
         (new-scheduled nil))

    (org-mcp--modify-and-save file-path "update scheduled"
                              `((before . ,previous-scheduled)
                                (after . ,new-scheduled))
      (org-mcp--goto-heading target)

      (setq previous-scheduled
            (or (org-entry-get (point) "SCHEDULED") ""))

      (if (or (null after) (equal after ""))
          ;; Remove scheduled
          (progn
            (org-schedule '(4))
            (setq new-scheduled ""))
        ;; Validate date format before calling org-schedule
        (org-mcp--validate-date-string after)
        (org-schedule nil after)
        (setq new-scheduled
              (or (org-entry-get (point) "SCHEDULED") ""))))))

(defun org-mcp--tool-node-set-deadline (link &optional after files)
  "Update DEADLINE timestamp on the headline LINK names.
AFTER is an ISO date string or nil/empty to remove.
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.

MCP Parameters:
  link - Link to the headline
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
  after - ISO date string (optional)
          Examples: \"2026-03-27\", \"2026-03-27 09:00\"
          nil or empty string removes the timestamp
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (let* ((target (org-mcp--link-target link files))
         (file-path (plist-get target :file))
         (previous-deadline nil)
         (new-deadline nil))

    (org-mcp--modify-and-save file-path "update deadline"
                              `((before . ,previous-deadline)
                                (after . ,new-deadline))
      (org-mcp--goto-heading target)

      (setq previous-deadline
            (or (org-entry-get (point) "DEADLINE") ""))

      (if (or (null after) (equal after ""))
          ;; Remove deadline
          (progn
            (org-deadline '(4))
            (setq new-deadline ""))
        ;; Validate date format before calling org-deadline
        (org-mcp--validate-date-string after)
        (org-deadline nil after)
        (setq new-deadline
              (or (org-entry-get (point) "DEADLINE") ""))))))

(defun org-mcp--tool-node-set-tags (link &optional after files)
  "Set tags on the headline LINK names.
AFTER is a string, a list of strings, or nil/empty to clear all tags.
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.

MCP Parameters:
  link - Link to the headline
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
  after - Tags to set (string or array, optional)
          Single tag: \"work\"
          Multiple tags: [\"work\", \"urgent\"]
          nil or empty to clear all tags
          Validated against org-tag-alist if configured
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (let* ((target (org-mcp--link-target link files))
         (file-path (plist-get target :file))
         (previous-tags nil)
         (new-tags nil))

    ;; Validate tags if provided
    (let ((tag-list
           (if (or (null after) (equal after "") (equal after []))
               nil
             (org-mcp--validate-and-normalize-tags after))))

      (org-mcp--modify-and-save file-path "set tags"
                                `((before . ,(or previous-tags []))
                                  (after . ,(or new-tags [])))
        (org-mcp--goto-heading target)

        (setq previous-tags (vconcat (org-get-tags nil t)))

        (org-set-tags tag-list)

        (setq new-tags (vconcat (org-get-tags nil t)))))))

(defun org-mcp--tool-node-set-priority (link &optional after files)
  "Set priority on the headline LINK names.
AFTER is a single-character string or nil/empty to remove.
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.

MCP Parameters:
  link - Link to the headline
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
  after - Priority character (string, optional)
          Must be within org-priority-highest to org-priority-lowest
          nil or empty string removes the priority
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  ;; Validate priority if provided
  (when (and after (not (equal after "")))
    (unless (= (length after) 1)
      (org-mcp--tool-validation-error
       "Priority must be a single character, got '%s'"
       after))
    (let ((char (string-to-char after)))
      (unless (and (>= char org-priority-highest)
                   (<= char org-priority-lowest))
        (org-mcp--tool-validation-error
         "Priority '%s' out of range ('%c' to '%c')"
         after org-priority-highest org-priority-lowest))))

  (let* ((target (org-mcp--link-target link files))
         (file-path (plist-get target :file))
         (previous-priority nil)
         (new-priority nil))

    (org-mcp--modify-and-save file-path "set priority"
                              `((before . ,previous-priority)
                                (after . ,new-priority))
      (org-mcp--goto-heading target)

      (setq previous-priority
            (let ((p
                   (org-element-property
                    :priority (org-element-at-point))))
              (if p
                  (char-to-string p)
                "")))

      (if (or (null after) (equal after ""))
          ;; Remove priority
          (progn
            (org-priority 'remove)
            (setq new-priority ""))
        ;; Set priority
        (org-priority (string-to-char after))
        (setq new-priority after)))))


(defun org-mcp--tool-node-add-note (link note &optional files)
  "Add a timestamped note to the LOGBOOK of the headline LINK names.
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.

MCP Parameters:
  link - Link to the headline
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
  note - Note text to add (string, required)
         Cannot be empty or whitespace-only
         Multi-line notes are indented properly in the LOGBOOK
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (when (or (null note)
            (string-empty-p note)
            (string-match-p "\\`[[:space:]]*\\'" note))
    (org-mcp--tool-validation-error
     "Note cannot be empty or whitespace-only"))

  (let* ((target (org-mcp--link-target link files))
         (file-path (plist-get target :file)))

    (org-mcp--modify-and-save file-path "add logbook note" nil
      (org-mcp--goto-heading target)
      (org-mcp--insert-log-note note 'note))))

;; org-ql integration

(defun org-mcp--ql-node-at-point (fields)
  "Return the node at point for the `:action' of `org-ql-select'.
It is the node a read returns, in the same shape, carrying FIELDS,
with the extra fields `org-mcp-ql-extra-properties' configures
appended.  Those are named by their own setting rather than by a
call, so they come with every match whatever FIELDS says."
  (append
   (org-mcp--node-at-point fields)
   (delq
    nil
    (mapcar
     (lambda (extra)
       (when-let* ((value (funcall (cdr extra))))
         (cons (car extra) value)))
     org-mcp-ql-extra-properties))))

(defun org-mcp--tool-query (query &optional fields files)
  "Search Org files using an org-ql QUERY expression.
QUERY is a string containing an org-ql query sexp.
FIELDS says how much of each matching node to return; see
`org-mcp--node-fields-given'.
FILES names the files and directories to search, replacing the
allowed files, see `org-mcp--with-file-set'; defaults to all
allowed files.

MCP Parameters:
  query - org-ql query sexp as string (e.g. \"(todo \\\"TODO\\\")\")
  fields - How much of each matching node to return (array of
          strings, or a string naming a configured list, optional);
          defaults to every field but content and children
  files - Files and directories to search, replacing the allowed
          files (array of strings, optional)"
  (when (or (not (stringp query)) (string-empty-p query))
    (org-mcp--tool-validation-error
     "Query must be a non-empty string"))
  (let ((query-sexp
         (condition-case nil
             (read query)
           (error
            (org-mcp--tool-validation-error
             "Failed to parse query: %s"
             query)))))
    (unless (consp query-sexp)
      (org-mcp--tool-validation-error "Query must be a list, got: %s"
                                      (type-of query-sexp)))
    (org-mcp--with-file-set files
      (let* ( ;; Resolved before the query runs, so a misspelled
             ;; field is refused rather than repeated per match.
             (node-fields
              (org-mcp--node-fields-given
               fields org-mcp--node-query-fields))
             (target-files org-agenda-files)
             (action
              (lambda () (org-mcp--ql-node-at-point node-fields)))
             (matches
              ;; Given no files, `org-ql-select' would search the
              ;; current buffer, which no call names.
              (when target-files
                (condition-case err
                    (org-ql-select
                     target-files
                     query-sexp
                     :action action)
                  (error
                   (org-mcp--tool-validation-error
                    "Org-ql query error: %s"
                    (error-message-string err)))))))
        (json-encode
         `((children . ,(vconcat matches))
           (total . ,(length matches))
           (files_searched . ,(length target-files))))))))

;; GTD query tools

(defun org-mcp--run-gtd-query (query-sexp)
  "Run QUERY-SEXP via `org-ql-select' with optional sorting.
A GTD query always runs over the allowed files: its tools take no
`files' parameter, and mcp-server-lib refuses a call passing one
with an \"Unexpected parameter\" error before any handler runs.
They take no `fields' parameter either, so every match carries
`org-mcp--node-query-fields', the fields org-query carries unasked.
Uses `org-mcp-query-sort-fn' for sorting when set.
Returns JSON-encoded results in the same format as org-query."
  (org-mcp--with-file-set nil
    (let*
        ((target-files org-agenda-files)
         (matches
          ;; Given no files, `org-ql-select' would search the
          ;; current buffer, which is not among the allowed files.
          (when target-files
            (condition-case err
                ;; Collect org-elements with the default action,
                ;; then sort.  We map `org-mcp--ql-node-at-point'
                ;; in a second pass because `org-ql-select' applies
                ;; :action before :sort — custom actions that
                ;; return non-element data would break sort
                ;; functions expecting org-elements.
                (let ((elements
                       (org-ql-select
                        target-files
                        query-sexp
                        :sort org-mcp-query-sort-fn)))
                  (mapcar
                   (lambda (el)
                     (with-current-buffer (org-element-property
                                           :buffer el)
                       ;; Widen: a heading outside the user's
                       ;; narrowing would be read at the wrong place.
                       (org-with-wide-buffer
                        (goto-char (org-element-property :begin el))
                        (org-mcp--ql-node-at-point
                         org-mcp--node-query-fields))))
                   elements))
              (error
               (org-mcp--tool-validation-error
                "Org-ql query error: %s"
                (error-message-string err)))))))
      (json-encode
       `((children . ,(vconcat matches))
         (total . ,(length matches))
         (files_searched . ,(length target-files)))))))

(defun org-mcp--tool-query-inbox ()
  "Query inbox items using the configured query function.

MCP Parameters: None

Returns: Same format as org-query tool, sorted by
`org-mcp-query-sort-fn' when configured."
  (org-mcp--run-gtd-query (funcall org-mcp-query-inbox-fn)))

(defun org-mcp--tool-query-next (&optional tag)
  "Query next action items, optionally filtered by TAG.

MCP Parameters:
  tag - Tag string to filter by (string, optional)

Returns: Same format as org-query tool, sorted by
`org-mcp-query-sort-fn' when configured."
  (let ((tag-filter
         (when (and tag (not (string-empty-p tag)))
           `(tags ,tag))))
    (org-mcp--run-gtd-query
     (funcall org-mcp-query-next-fn tag-filter))))

(defun org-mcp--tool-query-backlog (&optional tag)
  "Query backlog items, optionally filtered by TAG.

MCP Parameters:
  tag - Tag string to filter by (string, optional)

Returns: Same format as org-query tool, sorted by
`org-mcp-query-sort-fn' when configured."
  (let ((tag-filter
         (when (and tag (not (string-empty-p tag)))
           `(tags ,tag))))
    (org-mcp--run-gtd-query
     (funcall org-mcp-query-backlog-fn tag-filter))))

;; Read tools

(defun org-mcp--tool-node-read (link &optional fields files)
  "Tool handler for org-node-read.
LINK is a native Org link to a heading or a whole file.
FIELDS, when non-nil, says how much of the node to return; see
`org-mcp--node-fields-given'.
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.
Returns structured JSON.

MCP Parameters:
  link - Link to a heading or a file (string, required):
         - id:{id} (heading with that ID)
         - file:/path/to/file.org::#{custom-id} (heading with that
           CUSTOM_ID)
         - file:/path/to/file.org::*{title} (first heading with that
           title)
         - file:/path/to/file.org (whole file)
         - id:{id} of the file-level property drawer (whole file)
         - any of these bracketed, as [[link]] or [[link][description]]
  fields - How much of the node to return (array of strings, or a
          string naming a configured list, optional); defaults to
          every field but properties
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (org-mcp--read-structured link fields files))

(defun org-mcp--tool-read-outline (file)
  "Tool handler for org-read-outline.
FILE is the absolute path to an Org file, or a `file:' link to it with
no search part, such as file:/path/to/file.org.  Either way the file
must pass the scope gate, `org-mcp--find-allowed-file', as a file the
call names.  An `id:' link, even one to a file-level drawer, and a
`file:' link with a search part are refused without being looked up,
as parsed.  A string starting with `org://' is refused as no link, the
way the link tools refuse it.

MCP Parameters:
  file - Absolute path to an Org file, or a file: link to it with no
         search part"
  (json-encode
   (org-mcp--generate-outline
    (cond
     ((and (stringp file)
           (string-prefix-p "org://" (string-trim file)))
      (org-mcp--not-a-link-error file))
     ((org-mcp--link-written-p file)
      (let ((object (org-mcp--link-parse file)))
        (when (or (equal (org-element-property :type object) "id")
                  (org-element-property :search-option object))
          (org-mcp--tool-validation-error
           "org-read-outline takes a file's path or file: link, not an \
id: link or a search: %s"
           file))
        (plist-get (org-mcp--link-target file) :file)))
     (t
      (unless (and (stringp file) (file-name-absolute-p file))
        (org-mcp--tool-validation-error "Path must be absolute: %s"
                                        file))
      (expand-file-name
       (or (org-mcp--find-allowed-file file t)
           (org-mcp--tool-file-access-error file))))))))

(defun org-mcp--tool-node-text (link &optional files)
  "Tool handler for org-node-text.
LINK is a native Org link to a heading or a whole file.
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.
Returns plain text content.

MCP Parameters:
  link - Link to a heading or a file (string, required):
         - id:{id} (heading with that ID)
         - file:/path/to/file.org::#{custom-id} (heading with that
           CUSTOM_ID)
         - file:/path/to/file.org::*{title} (first heading with that
           title)
         - file:/path/to/file.org (returns entire file)
         - id:{id} of the file-level property drawer (returns entire
           file)
         - any of these bracketed, as [[link]] or [[link][description]]
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (org-mcp--read-link
   link #'org-mcp--node-text-at-point #'org-mcp--read-file
   files))

;; Clock tools

(defun org-mcp--tool-config-clock ()
  "Return the clock configuration.

MCP Parameters: None"
  (json-encode
   `((org_clock_into_drawer
      . ,(prin1-to-string org-clock-into-drawer))
     (org_clock_rounding_minutes . ,org-clock-rounding-minutes)
     (org_clock_continuously
      .
      ,(if org-clock-continuously
           t
         :json-false))
     (org_mcp_clock_continuous_threshold
      . ,org-mcp-clock-continuous-threshold))))

(defun org-mcp--tool-clock-dangling (&optional files)
  "Find all open (unclosed) clocks in a set of Org files.
The files are the ones FILES names, see `org-mcp--with-file-set',
or the allowed files when FILES is nil.
Uses `org-find-open-clocks' on each of them, in full even where the
user's buffer is narrowed; see `org-mcp--with-wide-clock-buffer'.
Reads each clock's timestamp via the Org element API.

MCP Parameters:
  files - Files and directories to search, replacing the allowed
          files (array of strings, optional)"
  (org-mcp--with-file-set files
    (let ((all-clocks nil))
      (dolist (file org-agenda-files)
        (org-mcp--with-wide-clock-buffer file
          (let ((open (org-find-open-clocks file)))
            (dolist (clock open)
              (let ((marker (car clock))
                    (clock-file (expand-file-name file)))
                (with-current-buffer (marker-buffer marker)
                  (save-excursion
                    (goto-char marker)
                    (let* ((el (org-element-at-point))
                           (start-str
                            (when (eq (org-element-type el) 'clock)
                              (org-mcp--clock-element-start-str el)))
                           (heading
                            (save-excursion
                              (org-back-to-heading t)
                              (org-mcp--title-at-point))))
                      (push `((file . ,clock-file)
                              (heading . ,heading)
                              (start . ,start-str)
                              (link . ,(org-mcp--link-at-point)))
                            all-clocks)))))))))
      (let ((total (length all-clocks)))
        (json-encode
         `((open_clocks . ,(vconcat (nreverse all-clocks)))
           (total . ,total)))))))

(defun org-mcp--tool-clock-active ()
  "Return the currently active clock entry, if any.

MCP Parameters: None"
  (let ((active (org-mcp--clock-find-active)))
    (if active
        (if (eq (alist-get 'allowed active) nil)
            (json-encode
             '((active . t) (in_allowed_file . :json-false)))
          (let ((marker (alist-get 'marker active)))
            (json-encode
             `((active . t)
               (file . ,(alist-get 'file active))
               (heading . ,(alist-get 'heading active))
               (start . ,(alist-get 'start active))
               (link
                .
                ,(with-current-buffer (marker-buffer marker)
                   (org-with-wide-buffer
                    (goto-char marker) (org-mcp--link-at-point))))))))
      (json-encode '((active . :json-false))))))

(defun org-mcp--tool-clock-in
    (link &optional start_time resolve files clock_out)
  "Clock in to the heading LINK names.
While a clock runs, CLOCK_OUT must name its heading, see
`org-mcp--clock-check-clock-out', and that clock is closed first, at
the new clock's start, which must not precede its own.  LINK,
START_TIME, RESOLVE and CLOCK_OUT are all checked before that, so a
refused call changes nothing.  Once that clock is closed, a save of it
that fails ends the call saying so, see `org-mcp--clock-save-closed'.
When `org-clock-continuously' is non-nil and no explicit START_TIME
is given, the new clock may start at the previous clock's end time
if it is within `org-mcp-clock-continuous-threshold' minutes.
When RESOLVE is true, the dangling CLOCK lines under the target
heading are deleted before clocking in: open lines other than the
running clock, which is closed, never deleted.
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.  It does not apply to CLOCK_OUT.

MCP Parameters:
  link - Link to the headline to clock in
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
  start_time - Optional ISO 8601 start time (e.g. 2026-03-23T14:30:00)
  resolve - true or \"true\" to delete dangling clocks before clocking
            in; false, \"false\" and null mean not to, and any other
            value is refused
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link
  clock_out - Link to the heading of the running clock, which is
              closed first; required while a clock runs, refused
              while none does"
  (let* ((target (org-mcp--link-target link files))
         (file-path (plist-get target :file))
         (resolve (org-mcp--boolean-param resolve "resolve"))
         (now (current-time))
         (explicit-start
          (when start_time
            (org-mcp--clock-parse-timestamp start_time)))
         ;; The running clock closes where the new one starts.
         (close-at
          (org-mcp--clock-round-time (or explicit-start now)))
         (active (org-mcp--clock-find-active))
         (running-start
          (and active
               (org-time-string-to-time (alist-get 'start active))))
         ;; Closing the running clock may edit another buffer that
         ;; already had unsaved edits; `saved' covers that edit too.
         (org-mcp--unsaved-change-p nil))
    ;; Every check runs before any clock is closed, so a refused call
    ;; changes nothing: a link that names no heading, such as
    ;; file:…::*Nope, is refused with the running clock intact.
    (org-mcp--with-org-file file-path
      (org-mcp--goto-heading target))
    (org-mcp--clock-check-clock-out active clock_out)
    (when (and active (time-less-p close-at running-start))
      (org-mcp--tool-validation-error
       "Start time %s is before the running clock's start %s"
       (org-mcp--clock-format-timestamp close-at)
       (org-mcp--clock-format-timestamp running-start)))
    (when active
      (let* ((marker (alist-get 'marker active))
             (buf (marker-buffer marker))
             (was-modified (buffer-modified-p buf))
             (tick (buffer-chars-modified-tick buf)))
        (org-clock-clock-out (cons marker running-start) t close-at)
        (org-mcp--clock-save-closed
         buf (alist-get 'file active) was-modified)
        ;; Only an edit that reached BUF can stay unsaved, and it has
        ;; not when a hook saved BUF.
        (when (and (/= tick (buffer-chars-modified-tick buf))
                   (buffer-modified-p buf))
          (setq org-mcp--unsaved-change-p t))))
    ;; Determine start time
    (let* ((continuous-start
            (when (and org-clock-continuously (not explicit-start))
              (let ((last-end (org-mcp--clock-find-last-closed)))
                (when last-end
                  (let ((elapsed
                         (float-time (time-subtract now last-end))))
                    (when (<= elapsed
                              (* 60
                                 org-mcp-clock-continuous-threshold))
                      last-end))))))
           (clock-start
            (org-mcp--clock-round-time
             (or explicit-start continuous-start now))))
      (let ((resolved-count 0))
        (org-mcp--modify-and-save file-path "clock-in"
                                  `((clocked_in . t)
                                    (start
                                     .
                                     ,(org-mcp--clock-format-timestamp
                                       clock-start))
                                    (heading
                                     . ,(org-mcp--title-at-point))
                                    ,@
                                    (when (> resolved-count 0)
                                      `((resolved
                                         . ,resolved-count))))
          (org-mcp--goto-heading target)
          (when resolve
            (setq resolved-count (org-mcp--clock-resolve-dangling)))
          (org-mcp--clock-insert-entry clock-start))))))

(defun org-mcp--tool-clock-out (&optional link end_time files)
  "Clock out the currently active clock.
A clock running in a file outside the allowed files is refused, as
clocking in refuses it: org-mcp writes no file outside them, and the
refusal names neither that file nor the heading and start of the clock
it holds, which `org-mcp--tool-clock-active' withholds too.
The clock is closed through Org, so Emacs's own clock stops with it
and Org's clock-out settings decide what the file ends up holding:
`org-clock-out-remove-zero-time-clocks' deletes a CLOCK line of no
length, and the drawer it empties, and `org-clock-out-switch-to-state'
rewrites the heading's TODO keyword.  The response reports neither; it
reports the close org-mcp asked for.
LINK, when not blank, must name a heading or file in the active
clock's file; see `org-mcp--link-given'.
END_TIME is an optional ISO 8601 end time (e.g. 2026-03-23T16:45:00).
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.  Without LINK it is not used.

MCP Parameters:
  link - Optional link to validate against active clock
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - file:{absolute-path}
           - any of these as [[link]] or [[link][description]]
  end_time - Optional ISO 8601 end time (e.g. 2026-03-23T16:45:00)
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (let ((active (org-mcp--clock-find-active)))
    (unless active
      (org-mcp--tool-conflict-error "No active clock to stop"))
    (unless (alist-get 'allowed active)
      (org-mcp--tool-validation-error
       "A clock is running in a file outside the allowed files.  Ask \
the user to clock out of it in Emacs"))
    (let* ((active-file (alist-get 'file active))
           (now (current-time))
           (end
            (if end_time
                (org-mcp--clock-round-time
                 (org-mcp--clock-parse-timestamp end_time))
              (org-mcp--clock-round-time now)))
           (start-str (alist-get 'start active))
           (start-parsed (org-parse-time-string start-str))
           (start-time (apply #'encode-time start-parsed)))
      ;; Validate end is after start
      (when (time-less-p end start-time)
        (org-mcp--tool-validation-error
         "End time %s is before start time %s"
         (org-mcp--clock-format-timestamp end)
         (format "[%s]" start-str)))
      ;; If a link is provided, validate it matches
      (when-let* ((link (org-mcp--link-given link)))
        (let ((link-file
               (plist-get (org-mcp--link-target link files) :file)))
          (unless (org-mcp--paths-equal-p link-file active-file)
            (org-mcp--tool-validation-error
             "Link file does not match active clock file"))))
      (let ((duration (float-time (time-subtract end start-time))))
        (org-mcp--modify-and-save active-file "clock-out"
                                  `((clocked_out . t)
                                    (heading
                                     . ,(alist-get 'heading active))
                                    (start . ,start-str)
                                    (end
                                     .
                                     ,(org-mcp--clock-format-timestamp
                                       end))
                                    (duration
                                     .
                                     ,(org-mcp--clock-duration-string
                                       duration)))
          ;; Org writes the close itself, so every CLOCK line it reads
          ;; as a clock is closed, its clock-out settings apply as they
          ;; do to an interactive clock-out, and the Emacs clock the
          ;; line belongs to stops: `org-clock-marker' is left unset
          ;; rather than pointing at a closed clock, which a later
          ;; clock-in would read as a clock still running.
          (let* ((marker (alist-get 'marker active))
                 ;; Org unsets the clock's marker as it closes it, so
                 ;; the heading is found while the marker still lives.
                 ;; It keeps its place: Org writes below it, and takes
                 ;; away no more than the CLOCK line and its drawer.
                 (heading
                  (save-excursion
                    (goto-char (marker-position marker))
                    (org-back-to-heading t)
                    (point))))
            (org-clock-clock-out (cons marker start-time) nil end)
            ;; The response links to the heading clocked out of.
            (goto-char heading)))))))

(defun org-mcp--tool-clock-add (link start end &optional files)
  "Add a completed clock entry to the heading LINK names.
START is ISO 8601 start time (e.g. 2026-03-23T14:30:00).
END is ISO 8601 end time (e.g. 2026-03-23T16:45:00).
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.

MCP Parameters:
  link - Link to the headline
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
  start - ISO 8601 start time (e.g. 2026-03-23T14:30:00)
  end - ISO 8601 end time (e.g. 2026-03-23T16:45:00)
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (let* ((target (org-mcp--link-target link files))
         (file-path (plist-get target :file))
         (start-time
          (org-mcp--clock-round-time
           (org-mcp--clock-parse-timestamp start)))
         (end-time
          (org-mcp--clock-round-time
           (org-mcp--clock-parse-timestamp end))))
    (when (time-less-p end-time start-time)
      (org-mcp--tool-validation-error
       "End time %s is before start time %s"
       (org-mcp--clock-format-timestamp end-time)
       (org-mcp--clock-format-timestamp start-time)))
    (org-mcp--modify-and-save file-path "clock-add"
                              `((added . t)
                                (start
                                 .
                                 ,(org-mcp--clock-format-timestamp
                                   start-time))
                                (end
                                 .
                                 ,(org-mcp--clock-format-timestamp
                                   end-time))
                                (duration
                                 .
                                 ,(org-mcp--clock-duration-string
                                   (float-time
                                    (time-subtract
                                     end-time start-time)))))
      (org-mcp--goto-heading target)
      (org-mcp--clock-insert-entry start-time end-time))))

(defun org-mcp--tool-clock-delete (link start &optional files)
  "Delete a clock entry from the heading LINK names.
START is the ISO 8601 start time of the clock entry to delete
\\(e.g., 2026-03-23T14:30:00).
FILES, when non-nil, names the files an `id:' LINK is looked up in;
see `org-mcp--link-target'.

MCP Parameters:
  link - Link to the headline
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
  start - ISO 8601 start time of the clock entry to delete
          (e.g. 2026-03-23T14:30:00)
  files - Files and directories to look up an id: link in, in order,
          instead of Emacs's ID index (array of strings, optional);
          refused with any other link"
  (let* ((target (org-mcp--link-target link files))
         (file-path (plist-get target :file))
         (start-time
          (org-mcp--clock-round-time
           (org-mcp--clock-parse-timestamp start)))
         (deleted-info nil))
    (org-mcp--modify-and-save file-path "clock-delete"
                              `((deleted . t) ,@deleted-info)
      (org-mcp--goto-heading target)
      (setq deleted-info (org-mcp--clock-delete-entry start-time))
      (unless deleted-info
        (org-mcp--tool-validation-error
         "No clock entry starting at %s found"
         (org-mcp--clock-format-timestamp start-time))))))

;; Tool description parts shared by several tools

(defconst org-mcp--heading-link-formats
  "         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - any of these as [[link]] or [[link][description]]
"
  "The link forms of a `link' parameter naming a heading.
Tool descriptions `concat' it after the parameter's first line.")

(defconst org-mcp--read-link-formats
  "         Formats:
         - id:{id} - heading with that ID
         - file:/path/to/file.org::#{custom-id} - heading with that
           CUSTOM_ID
         - file:/path/to/file.org::*{title} - first heading with that
           title
         - file:/path/to/file.org - whole file
         - id:{id} of the file-level property drawer - whole file
         - any of these as [[link]] or [[link][description]]
"
  "The link forms of a read tool's `link' parameter.
Tool descriptions `concat' it after the parameter's first line.")

(defconst org-mcp--files-set-description
  "          Each entry is an absolute path to an Org file or a
          directory; a relative path is refused.  A file outside the
          allowed files is accepted only as far as
          org-mcp-file-scope-override permits; see
          org-config-allowed-files.  A directory the setting permits is
          searched recursively for the Org files Org takes from a
          directory in org-agenda-files (by default every .org file,
          no archive), skipping hidden and unreadable directories,
          symlinked directories and anything not a regular file.
          Any other directory is not read: it stands for the allowed
          files under it, and is refused when there are none.  The
          buffers the call opens for these files are closed
          afterwards.
          null, false, \"\" and [] mean no files.
"
  "How the `files' parameter of a tool scanning a set of files works.
Tool descriptions `concat' it after the parameter's first lines.")

(defconst org-mcp--fields-description
  "          Either an array of the field names below, such as
          [\"title\", \"link\"], or the name of a list configured in
          org-mcp-node-field-lists, sent as a string.  An unknown
          field name and an unknown list name are both refused, and
          the refusal names the valid ones.
          null, false, \"\" and [] ask for the default.
"
  "How the `fields' parameter works, for every tool that takes one.
Each such tool names its own default before this text, because the
default is what that endpoint carries and not a property of the
parameter.")

(defconst org-mcp--node-description "
A node is a file or a heading, and both come back in one shape.  A
node carries the fields the call asked for, minus any it has no
value for: a key that is there has a value, a key that is missing
was either asked for and empty or never asked for, and the call
itself says which.  Nothing is ever sent as null.
  title - The heading's title, or a file's #+TITLE: and its own name
          when it sets none
  todo - TODO state
  priority - Priority letter
  tags - The tags in effect on the heading, inherited ones included
         as org-use-tag-inheritance and
         org-tags-exclude-from-inheritance direct, both of which
         org-config-tags reports
  local_tags - The tags written on the heading itself, which is the
         set org-node-set-tags replaces.  Identical to tags when
         inheritance is off
  scheduled - Scheduled timestamp
  deadline - Deadline timestamp
  closed - Closed timestamp
  file - Absolute path of the file the node lives in
  id - The ID the link names
  level - Heading level, 0 for a file
  link - Link naming this node again: id:{id} when it has an ID, else
         file:{path}::#{custom-id} when it has a CUSTOM_ID, else
         file:{path}::*{title}; a file without an ID is file:{path}
  content - Body text, or a file's preamble before its first heading
  properties - The Org property drawer
  children - The direct children, each a node carrying title, todo,
             level and link
"
  "How a node reads, for every tool description that returns one.
One shape serves a file, a heading, a child and a query result, so
the tools that return any of them share this text rather than each
describing the same thing differently.")

(defconst org-mcp--core-tool-specs
  (list
   (list
    #'org-mcp--tool-config-todo
    :id "org-config-todo"
    :description
    "Get the TODO keyword configuration from the current Emacs
Org-mode settings.  Returns information about task state sequences
and their semantics.

Parameters: None

Returns JSON object with two arrays:
  sequences - Array of TODO keyword sequences, each containing:
    - type: Sequence type (e.g., \"sequence\", \"type\")
    - keywords: Array of keywords including \"|\" separator between
active and done states
  semantics - Array of keyword semantics, each containing:
    - state: The TODO keyword (e.g., \"TODO\", \"DONE\")
    - isFinal: Whether this is a final (done) state (boolean)
    - sequenceType: The sequence type this keyword belongs to

The \"|\" separator in sequences marks the boundary between active
states (before) and done states (after).  If no \"|\" is present,
the last keyword is treated as the done state.

Use this tool to understand the available task states in the Org
configuration before creating or updating TODO items."
    :read-only t)
   (list
    #'org-mcp--tool-config-tags
    :id "org-config-tags"
    :description
    "Get tag-related configuration from the current Emacs Org-mode
settings.  Returns literal Elisp variable values as strings for tag
configuration introspection.

Parameters: None

Returns JSON object with literal Elisp expressions (as strings) for:
  org-use-tag-inheritance - Controls tag inheritance behavior
  org-tags-exclude-from-inheritance - Tags that don't inherit
  org-tag-alist - List of allowed tags with optional key bindings and
                  groups
  org-tag-persistent-alist - Additional persistent tags (or nil)

The org-tag-alist format includes:
  - Simple tags: (\"tagname\" . key-char)
  - Group markers: :startgroup, :endgroup for mutually exclusive tags
  - Grouptags: :startgrouptag, :grouptags, :endgrouptag for tag
hierarchies

Use this tool to understand:
  - Which tags are allowed
  - Tag inheritance rules
  - Mutually exclusive tag groups
  - Tag hierarchy relationships

This helps validate tag usage and understand tag semantics before
adding or modifying tags on TODO items."
    :read-only t)
   (list
    #'org-mcp--tool-config-tag-candidates
    :id "org-config-tag-candidates"
    :description
    (concat
     "Return all candidate tags the user might want to use across the
allowed files, or across the files named in `files'.

Mirrors Org's interactive tag completion (C-c C-q): the result is
the union of configured tags from `org-tag-alist' /
`org-tag-persistent-alist', any per-file `#+TAGS:' / `#+FILETAGS:'
keywords, and every tag actually present on a headline in any of
those files.  Group keywords like `:startgroup' are filtered out.

Parameters:
  files - Files and directories to collect tags from (array of
          strings, optional)
          Replaces the allowed files for this call; when omitted, all
          allowed files are used.
"
     org-mcp--files-set-description "
Returns JSON object with:
  tags - Sorted, deduplicated array of tag-name strings.

Use this when suggesting or completing tags rather than
`org-config-tags', which only exposes the static configuration.")
    :read-only t)
   (list
    #'org-mcp--tool-config-priority
    :id "org-config-priority"
    :description
    "Get priority configuration from the current Emacs Org-mode
settings.  Returns the priority range and default as single-character
strings.

Parameters: None

Returns JSON object with:
  highest - Highest priority character (e.g. \"A\")
  lowest - Lowest priority character (e.g. \"C\")
  default - Default priority character (e.g. \"B\")

Use this tool to understand the valid priority range before setting
or interpreting priorities on TODO items."
    :read-only t)
   (list
    #'org-mcp--tool-config-allowed-files
    :id "org-config-allowed-files"
    :description
    "Get the list of Org files accessible through the org-mcp
server, and whether a call may name Org files outside them.  Returns
the allowed files as configured in org-mcp-allowed-files (the agenda
files when unset), and the policy of org-mcp-file-scope-override.

Parameters: None

Returns JSON object containing:
  files (array of strings): Absolute paths of allowed Org files
  override_allowed (boolean): Whether a call may name an Org file
    outside the allowed files.  The permission lasts for that one
    call only.
  override_roots (array of strings, present only when overriding is
    limited to directories): Absolute paths of the directories under
    which a named Org file is permitted.  When override_allowed is
    true and override_roots is absent, any Org file is permitted.

Example response:
  {
    \"files\": [
      \"/home/user/org/tasks.org\",
      \"/home/user/org/projects.org\",
      \"/home/user/notes/daily.org\"
    ],
    \"override_allowed\": true,
    \"override_roots\": [\"/home/user/decisions\"]
  }

Empty configuration returns:
  {
    \"files\": [],
    \"override_allowed\": false
  }

Use cases:
  - Discovery: What Org files can I access through MCP?
  - Link Construction: I need to build a file: link - what's
    the exact path?
  - Access Troubleshooting: Why is my file access failing?
  - Configuration Verification: Did my org-mcp-allowed-files setting
    work correctly?"
    :read-only t)
   (list
    #'org-mcp--tool-node-set-todo
    :id "org-node-set-todo"
    :description
    (concat
     "Update the TODO state of an Org headline.  Changes the task state
while preserving the headline title, tags, and other properties.

Parameters:
  link - Link to the headline to update (string, required)
"
     org-mcp--heading-link-formats
     "  before - Expected current TODO state (string, optional)
           When provided, must match actual state or tool will error
           Omit to skip the state check
  after - New TODO state to set (string, required)
          Must be valid keyword from org-todo-keywords
  note - Optional note to attach to this state transition (string, optional)
         When provided, stored in LOGBOOK as part of the state change entry
         Empty or whitespace-only values are ignored
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  before - The TODO state the headline held (string, empty for none)
  after - The TODO state Org left it in (string): the one asked for,
          unless Org made another of it, as it does when it repeats
          a repeating entry instead of finishing it
  link - Link to the updated headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}")
    :read-only nil)
   (list
    #'org-mcp--tool-node-create
    :id "org-node-create"
    :description
    "Add a new TODO item to an Org file at a specified location.
Creates the headline with TODO state, optional tags, optional body
content, and optional properties.  No ID or CUSTOM_ID is created:
set one in properties to give the headline a stable link.

Parameters:
  title - Headline text without TODO state or tags (string, required)
          Cannot be empty or whitespace-only
          Cannot contain newlines
  todo - TODO keyword from org-todo-keywords (string, required)
  tags - Tags for the headline (string or array, optional)
         Single tag: \"urgent\"
         Multiple tags: [\"work\", \"urgent\"]
         Validated against org-tag-alist if configured
         Must follow Org tag rules (alphanumeric, _, @)
         Respects mutually exclusive tag groups
  content - Body content under the headline (string, optional)
            Cannot contain headlines at same or higher level as new
            item
            If #+BEGIN/#+END blocks are present, they must be balanced
  parent - Link to the parent (string, required)
           For top-level: file:{absolute-path}
                          or id:{id} of the file-level property drawer
           For child: id:{parent-id}
                      or file:{absolute-path}::#{custom-id}
                      or file:{absolute-path}::*{title} (first match)
           Links may be bracketed: [[link]] or [[link][description]]
  previous_sibling - Link to the sibling to insert after (string,
                     optional), in any form parent takes for a child:
                     a direct child of the parent, or a top-level
                     heading of the file when parent names the whole
                     file.  Its id: link is looked up in the parent's
                     file.  null, false and \"\" mean none.
                     If omitted, appends as last child of parent
  properties - Properties for the new headline (object, optional)
               e.g. {\"ID\": \"...\", \"CUSTOM_ID\": \"...\",
                     \"EFFORT\": \"1:00\"}
               Values are strings (numbers and booleans are
               accepted) on a single line, written as given and
               not otherwise checked; an ID is not added to Org's
               ID index
               true or false writes the text t or nil (false writes
               the property; null skips it)
               null or empty values are skipped
               Special properties (TODO, TAGS, PRIORITY, SCHEDULED,
               DEADLINE, etc.) are forbidden - use the other
               parameters and dedicated tools
               properties itself given as null, false, \"\" or {}
               means no properties
  files - Files and directories to look up an id: link of parent in
          (array of strings, optional); see org-node-read.  It
          applies to parent only, and is refused unless parent is an
          id: link.

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  link - Link to the new headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}
  file - Filename (not full path) where item was added
  title - The headline title that was created

Positioning behavior:
  - With parent only: Appends as last child of parent
  - With parent + previous_sibling: Inserts immediately after that
sibling and its subtree
  - Top-level (parent naming only the file): Adds after the
file's preamble (a file-level property drawer, keyword lines such as
#+TITLE and any text before the first heading), before every existing
heading
  - Top-level + previous_sibling: Inserts immediately after that
top-level heading and its subtree"
    :read-only nil)
   (list
    #'org-mcp--tool-node-set-title
    :id "org-node-set-title"
    :description
    (concat
     "Rename an Org headline's title while preserving its TODO state,
tags, properties, and body content.

Parameters:
  link - Link to the headline to rename (string, required)
"
     org-mcp--heading-link-formats
     "  before - Expected current title without TODO/tags (string,
           required)
           Must match actual title or tool will error
           Used to prevent race conditions
  after - New title without TODO state or tags (string, required)
          Cannot be empty or whitespace-only
          Cannot contain newlines
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  before - The previous headline title (string)
  after - The new title that was set (string)
  link - Link to the renamed headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}")
    :read-only nil)
   (list
    #'org-mcp--tool-node-set-content
    :id "org-node-set-content"
    :description
    (concat
     "Edit or append to the body content of an Org headline.  In replace
mode (default), finds and replaces a unique substring within the
headline's body text.  In append mode, inserts new content after
existing body content but before any child headlines.

Parameters:
  link - Link to the headline to edit (string, required)
"
     org-mcp--heading-link-formats
     "  before - Substring to find and replace (string, required in
           replace mode, ignored when append is true)
           Must appear exactly once in the body
           Use empty string \"\" only for adding to empty nodes
  after - Replacement or appended text (string, required)
          Cannot introduce headlines at same or higher level
          Must maintain balanced #+BEGIN/#+END blocks
          In append mode it is the content to append and cannot
          be empty or whitespace-only
  append - Append instead of replacing (optional, default false):
           true or \"true\" append; false, \"false\" and null
           replace; any other value is refused
           When true, after goes at the end of the body and before
           is ignored
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  link - Link to the edited headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}

Special behavior - Empty before (replace mode):
  An empty before asserts the node has no content:
  - It is how initial content reaches a node that has none
  - A node that already has content is refused, and the refusal
    asks for the part of the content to replace")
    :read-only nil)
   ;; Entry update tools
   (list
    #'org-mcp--tool-node-set-properties
    :id "org-node-set-properties"
    :description
    (concat
     "Set or delete properties on an Org headline.  Updates the
PROPERTIES drawer.  Setting ID or CUSTOM_ID gives the headline a
stable link; org-mcp creates neither itself.

Parameters:
  link - Link to the headline (string, required)
"
     org-mcp--heading-link-formats
     "  after - JSON object of property name-value pairs (required)
          String value (numbers and booleans are accepted):
          set the property; it must be a single line
          true or false writes the text t or nil (false keeps
          the property; null deletes it)
          null or empty string: delete the property
          ID and CUSTOM_ID can be set; values are written as
          given and not otherwise checked, and an ID is not
          added to Org's ID index
          Special properties (TODO, TAGS, PRIORITY, SCHEDULED,
          DEADLINE, etc.) are forbidden - use dedicated tools
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  properties_set - Array of property names that were set
  properties_deleted - Array of property names that were deleted
  link - Link to the headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}")
    :read-only nil)
   (list
    #'org-mcp--tool-node-set-scheduled
    :id "org-node-set-scheduled"
    :description
    (concat
     "Update the SCHEDULED timestamp on an Org headline.

Parameters:
  link - Link to the headline (string, required)
"
     org-mcp--heading-link-formats
     "  after - ISO date string (string, optional)
          Examples: \"2026-03-27\", \"2026-03-27 09:00\"
          Omit or empty string to remove the timestamp
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  before - Previous SCHEDULED value (string, empty if none)
  after - New SCHEDULED value (string, empty if removed)
  link - Link to the headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}")
    :read-only nil)
   (list
    #'org-mcp--tool-node-set-deadline
    :id "org-node-set-deadline"
    :description
    (concat
     "Update the DEADLINE timestamp on an Org headline.

Parameters:
  link - Link to the headline (string, required)
"
     org-mcp--heading-link-formats
     "  after - ISO date string (string, optional)
          Examples: \"2026-03-27\", \"2026-03-27 09:00\"
          Omit or empty string to remove the timestamp
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  before - Previous DEADLINE value (string, empty if none)
  after - New DEADLINE value (string, empty if removed)
  link - Link to the headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}")
    :read-only nil)
   (list
    #'org-mcp--tool-node-set-tags
    :id "org-node-set-tags"
    :description
    (concat
     "Set tags on an Org headline, replacing any existing tags.

Parameters:
  link - Link to the headline (string, required)
"
     org-mcp--heading-link-formats
     "  after - Tags to set (string or array, optional)
          Single tag: \"work\"
          Multiple tags: [\"work\", \"urgent\"]
          Omit or empty to clear all tags
          Validated against org-tag-alist if configured
          Must follow Org tag rules (alphanumeric, _, @)
          Respects mutually exclusive tag groups
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  before - Array of previous tags
  after - Array of new tags
  link - Link to the headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}")
    :read-only nil)
   (list
    #'org-mcp--tool-node-set-priority
    :id "org-node-set-priority"
    :description
    (concat
     "Set or remove priority on an Org headline.

Parameters:
  link - Link to the headline (string, required)
"
     org-mcp--heading-link-formats
     "  after - Priority character (string, optional)
          Must be in the configured range (default \"A\" to \"C\")
          Use org-config-priority to check the valid range
          Omit or empty string to remove priority
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  before - Previous priority (string, empty if none)
  after - New priority (string, empty if removed)
  link - Link to the headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}")
    :read-only nil)
   (list
    #'org-mcp--tool-node-add-note
    :id "org-node-add-note"
    :description
    (concat
     "Add a timestamped note to the LOGBOOK drawer of an Org headline.
Creates the LOGBOOK drawer if it doesn't exist.

Parameters:
  link - Link to the headline (string, required)
"
     org-mcp--heading-link-formats
     "  note - Note text to add (string, required)
         Cannot be empty or whitespace-only
         Multi-line notes are properly indented in the LOGBOOK
         Note is inserted at the top of the LOGBOOK drawer
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  link - Link to the headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}")
    :read-only nil)
   (list
    #'org-mcp--tool-node-read
    :id "org-node-read"
    :description
    (concat
     "Read an Org file or heading as a node.  Takes a native Org link.

Parameters:
  link - Link to a heading or a file (string, required)
"
     org-mcp--read-link-formats
     "         Any other string, such as a bare ID, a bare path or an
         org:// resource URI, is refused.
  fields - How much of the node to return (array of strings, or a
          string, optional)
          Defaults to every field below but properties.
"
     org-mcp--fields-description
     "  files - Files and directories to look up an id: link in (array of
          strings, optional)
          An id: link names no file, so without files it resolves
          only within the allowed files.  With files, the ID is
          looked up in these files instead, in the order given,
          rather than in Emacs's ID index: a heading in a file Emacs
          never indexed is found, and no index rescan runs.  Entries
          are checked as for org-query, so a file outside the
          allowed files is reached only as far as
          org-mcp-file-scope-override permits, and a directory is
          searched as that tool searches it.  An ID none of the files
          holds is an error.  Refused with any link but an id:
          link, such as a file: link, which names its file already.
          null, false, \"\" and [] mean no files.
          Every tool that names a heading takes files in the same
          way.

Returns: JSON object, the node the link names, carrying the fields
the call asked for.
"
     org-mcp--node-description "
File must be in the allowed files, or permitted by
org-mcp-file-scope-override.")
    :read-only t)
   (list
    #'org-mcp--tool-read-outline
    :id "org-read-outline"
    :description
    "Get hierarchical structure of Org file as JSON outline. Returns
   the titles of the top-level headlines and of their direct
   children; deeper headlines are left out. File must be in the
   allowed files, or permitted by org-mcp-file-scope-override.

Parameters:
  file - Absolute path to Org file, or a file: link to it with no
         search part, bare or bracketed, such as file:/path/to/file.org
         (string, required)
         An id: link, even one to a file-level drawer, and a file:
         link with a search part are refused without being looked
         up, and so is an org:// resource URI.

Returns: JSON object with hierarchical outline structure:
  children - Array of top-level headings, each a node carrying title,
             todo, level, link and its own children, the level-2
             headings; those carry no children of their own"
    :read-only t)
   (list
    #'org-mcp--tool-node-text
    :id "org-node-text"
    :description
    (concat
     "Read Org headline or file as plain text.  Takes a native Org link.
Returns headline with TODO state, tags, properties, body text, and all
nested subheadings.

Parameters:
  link - Link to a heading or a file (string, required)
"
     org-mcp--read-link-formats
     "         Any other string is refused, as in org-node-read.
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read

Returns: Plain text content of the headline and its subtree (or file)")
    :read-only t)
   (list
    #'org-mcp--tool-query
    :id "org-query"
    :description
    (concat
     "Search Org files using org-ql query expressions.  Supports
querying by TODO state, tags, priority, deadlines, properties, and
more.  Returns matched entries as JSON with Org links for follow-up
access.

Parameters:
  query - org-ql query sexp as string (string, required)
          Examples:
            (todo \"TODO\")
            (tags \"work\")
            (and (todo \"TODO\") (priority \"A\"))
            (deadline :to today)
  fields - How much of each matching node to return (array of
          strings, or a string, optional)
          Defaults to every field below but content and children,
          which a match list would read every matched subtree to
          fill; naming either asks for exactly that.
"
     org-mcp--fields-description
     "  files - Files and directories to search (array of strings, optional)
          Replaces the allowed files for this call; when omitted, all
          allowed files are searched.
"
     org-mcp--files-set-description "
Returns JSON object:
  children - Array of matching nodes, the shape org-node-read
             returns, each carrying the fields the call asked for.
  total - Number of matches (number)
  files_searched - Number of files searched (number)
"
     org-mcp--node-description)
    :read-only t))
  "Specs for the tools org-mcp registers on every `org-mcp-enable\='.
Each element is a `mcp-server-lib-register-server\=' `:tools\=' spec,
`(HANDLER :id STR :description STR [:read-only BOOL])\='.  The clock
tools live in `org-mcp--clock-tool-specs\=' and the tools that depend
on configuration in `org-mcp--gtd-tool-specs\='.")

(defun org-mcp--gtd-tool-specs ()
  "Return specs for the GTD query tools that are configured.
A tool is left out when its query function is nil, so a client
never sees a GTD tool that cannot answer.  The functions are
`org-mcp-query-inbox-fn', `org-mcp-query-next-fn' and
`org-mcp-query-backlog-fn'."
  (append
   (when org-mcp-query-inbox-fn
     (list
      (list
       #'org-mcp--tool-query-inbox
       :id "query-inbox"
       :description
       "Query inbox items using the configured GTD workflow.
Returns items matching the inbox query, sorted by rank when
a sort function is configured.  Always runs over the allowed files;
naming files is an error.  Use org-query to search other files.

Parameters: None

Returns: Same format as org-query tool"
       :read-only t)))
   (when org-mcp-query-next-fn
     (list
      (list
       #'org-mcp--tool-query-next
       :id "query-next"
       :description
       "Query next action items using the configured GTD workflow.
Returns actionable items sorted by rank when a sort function
is configured.  Always runs over the allowed files; naming files is
an error.  Use org-query to search other files.

Parameters:
  tag - Tag string to filter results (string, optional)

Returns: Same format as org-query tool"
       :read-only t)))
   (when org-mcp-query-backlog-fn
     (list
      (list
       #'org-mcp--tool-query-backlog
       :id "query-backlog"
       :description
       "Query backlog items (projects and standalone actions) using
the configured GTD workflow.  Returns items sorted by rank when
a sort function is configured.  Always runs over the allowed files;
naming files is an error.  Use org-query to search other files.

Parameters:
  tag - Tag string to filter results (string, optional)

Returns: Same format as org-query tool"
       :read-only t)))))

(defconst org-mcp--clock-tool-specs
  (list
   (list
    #'org-mcp--tool-config-clock
    :id "org-config-clock"
    :description
    "Get the clock configuration from the current Emacs Org-mode
settings.  Returns clock-related settings.

Parameters: None

Returns JSON object with:
  org_clock_into_drawer - Where to put clock entries (literal Elisp)
  org_clock_rounding_minutes - Rounding interval in minutes (number)
  org_clock_continuously - Whether continuous clocking is enabled
  org_mcp_clock_continuous_threshold - Max minutes for continuous
    clocking gap

Use this tool to understand clock settings before clocking
in or out."
    :read-only t)
   (list
    #'org-mcp--tool-clock-active
    :id "org-clock-active"
    :description
    "Get the currently active clock, if any.  Searches all allowed
files for an unclosed CLOCK entry.  Also detects native Emacs clocks
running in non-allowed files via `org-clock-is-active'.

Parameters: None

Returns JSON object:
  active - Whether a clock is active (boolean)
  in_allowed_file - false when clock is in a non-allowed file
    (only present in that case; file/heading/start/link are omitted)
  file - File path of active clock (string, only if active
    in allowed file)
  heading - Title of the heading with the active clock, the title a
    read reports (string, only if active in allowed file)
  start - Start timestamp string (string, only if active
    in allowed file)
  link - Link to the heading with the active clock (string, only if
    active in allowed file): id:{id} when it has an ID, else
    file:{path}::#{custom-id} when it has a CUSTOM_ID, else
    file:{path}::*{title}"
    :read-only t)
   (list
    #'org-mcp--tool-clock-in
    :id "org-clock-in"
    :description
    (concat
     "Clock in to the specified heading.

Only one clock runs at a time.  While one runs, the call must name it
in clock_out, and that clock is closed first, at the new clock's
start.  Without clock_out, or with one naming another heading, the
call is refused and changes nothing; the refusal names the running
clock's heading by title and link, and its start, so ask the user
before clocking out of it.  A clock running outside the allowed files
cannot be named: ask the user to clock out of it.

When org-clock-continuously is enabled and no explicit start_time
is given, the new clock may start at the previous clock's end time
if it is within the continuous threshold.

Rounding is applied per org-clock-rounding-minutes.

Parameters:
  link - Link to the headline to clock in (string, required)
"
     org-mcp--heading-link-formats
     "  start_time - ISO 8601 start time (string, optional)
               Example: 2026-03-23T14:30:00
               If omitted, uses current time (or continuous time)
               Must not be before the running clock's start
  resolve - true or \"true\" to delete the dangling (unclosed) CLOCK
            lines under the heading before clocking in (optional);
            the running clock is closed, never deleted; false,
            \"false\" and null mean not to; any other value is refused
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read.  Not used for clock_out
  clock_out - Link to the heading of the running clock (string);
              required while a clock runs, refused while none does.
              The link a refusal names for it is accepted as sent;
              an id: link is looked up in the running clock's file;
              null, false and \"\" mean no link

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when a change, including closing the running clock
          in its own file, is only in the user's open Emacs buffer,
          not on disk; tell the user it needs saving (boolean)
  clocked_in - Always true (boolean)
  start - Formatted start timestamp (string)
  heading - The heading's title, as a read reports it (string)
  link - Link to the headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}
  resolved - Number of dangling clocks deleted (integer, only if
             resolve was requested and dangling clocks were found)")
    :read-only nil)
   (list
    #'org-mcp--tool-clock-out
    :id "org-clock-out"
    :description
    "Clock out the currently active clock.

Closing the clock stops the Emacs clock it belongs to, so a clock-in
after it needs no clock_out.  Org's clock-out settings decide what the
file holds afterwards: org-clock-out-remove-zero-time-clocks deletes a
CLOCK line of no length, and the drawer it empties, and
org-clock-out-switch-to-state rewrites the heading's TODO keyword.
The response reports neither, so read the heading back when it matters.

A clock running outside the allowed files is refused: org-mcp writes
no file outside them and reports nothing about that clock, so ask the
user to clock out of it in Emacs.

Rounding is applied per org-clock-rounding-minutes.

Parameters:
  link - Optional link to validate against active clock (string)
         If provided, must name the file of the active clock or a
         heading in it; null, false and \"\" mean no link
         Formats:
           - id:{id}
           - file:{absolute-path}::#{custom-id}
           - file:{absolute-path}::*{title} (first match)
           - file:{absolute-path}
           - any of these as [[link]] or [[link][description]]
  end_time - ISO 8601 end time (string, optional)
             Example: 2026-03-23T16:45:00
             If omitted, uses current time
  files - Files and directories to look up an id: link in
          (array of strings, optional); see org-node-read

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  clocked_out - Always true (boolean)
  heading - The heading's title, as a read reports it (string)
  start - Start timestamp (string)
  end - End timestamp (string)
  duration - Duration as H:MM (string)
  link - Link to the headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}"
    :read-only nil)
   (list
    #'org-mcp--tool-clock-add
    :id "org-clock-add"
    :description
    (concat
     "Add a completed clock entry to a heading.  Creates a LOGBOOK
drawer if one doesn't exist.  New entries are inserted at the top
of the LOGBOOK.

Rounding is applied per org-clock-rounding-minutes.

Parameters:
  link - Link to the headline (string, required)
"
     org-mcp--heading-link-formats
     "  start - ISO 8601 start time (string, required)
          Example: 2026-03-23T14:30:00
  end - ISO 8601 end time (string, required)
        Example: 2026-03-23T16:45:00
        Must be after start time
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  added - Always true (boolean)
  start - Formatted start timestamp (string)
  end - Formatted end timestamp (string)
  duration - Duration as H:MM (string)
  link - Link to the headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}")
    :read-only nil)
   (list
    #'org-mcp--tool-clock-delete
    :id "org-clock-delete"
    :description
    (concat
     "Delete a clock entry from a heading.  Removes the LOGBOOK
drawer if it becomes empty after deletion.

Rounding is applied per org-clock-rounding-minutes.

Parameters:
  link - Link to the headline (string, required)
"
     org-mcp--heading-link-formats
     "  start - ISO 8601 start time of the clock entry to delete
          (string, required)
          Example: 2026-03-23T14:30:00
  files - Files and directories to look up an id: link in (array of
          strings, optional); see org-node-read

Returns JSON object:
  success - Always true on success (boolean)
  saved - False when the change is only in the user's open Emacs
          buffer, not on disk; tell the user it needs saving (boolean)
  deleted - Always true (boolean)
  start - Start timestamp of deleted entry (string)
  end - End timestamp of deleted entry (string, present if closed)
  duration - Duration as H:MM (string, present if closed)
  link - Link to the headline (string): id:{id} when it has
         an ID, else file:{path}::#{custom-id} when it has a
         CUSTOM_ID, else file:{path}::*{title}")
    :read-only nil)
   (list
    #'org-mcp--tool-clock-dangling
    :id "org-clock-dangling"
    :description
    (concat
     "Find all open (unclosed) clocks in allowed Org files, or in the
files named in `files'.  Searches for dangling CLOCK entries that
were never closed.  Uses Emacs native `org-find-open-clocks' on
each of those files.

Parameters:
  files - Files and directories to search (array of strings, optional)
          Replaces the allowed files for this call; when omitted, all
          allowed files are searched.
"
     org-mcp--files-set-description "
Returns JSON object:
  open_clocks - Array of open clocks, each with:
    file - File path (string)
    heading - The heading's title, as a read reports it (string)
    start - Start timestamp (string)
    link - Link to the heading (string): id:{id} when it has an ID,
           else file:{path}::#{custom-id} when it has a CUSTOM_ID,
           else file:{path}::*{title}
  total - Number of open clocks found (number)")
    :read-only t))
  "Specs for the clock tools, registered after the GTD query tools.
Same spec format as `org-mcp--core-tool-specs\='.")

(defconst org-mcp--resource-specs
  (list
   (list
    "org://{link}" #'org-mcp--handle-org-resource
    :name "Org resource (structured JSON)"
    :description
    "Read an Org file or heading as structured JSON.  The URI is
org:// followed by a native Org link, the same link the org-node-read
tool takes, percent-encoded as in any URI.

URI format: org://{link}
  link - A native Org link, bare or as [[link]] or
         [[link][description]]:
    - id:{id} - heading with that ID
    - file:/path/to/file.org::#{custom-id} - heading with that
      CUSTOM_ID
    - file:/path/to/file.org::*{title} - first heading with that
      title
    - file:/path/to/file.org - whole file
    - id:{id} of the file-level property drawer - whole file
    Encode at least % as %25, # as %23, ? as %3F, spaces, [ and ].
    The link is decoded exactly once, so a literal % in a title is
    sent as %25: *50%25%20Done reads the heading \"50% Done\".
  Any other string, such as a bare ID or path, is refused.

Examples:
  org://id:550e8400-e29b-41d4-a716-446655440000
  org://file:/home/user/org/projects.org::%23alpha
  org://file:/home/user/org/projects.org::*Project%20Alpha

Returns: the node the link names, exactly as the org-node-read tool
returns it.

A link resolves, and is refused, exactly as in the org-node-read tool.
The file must be in the allowed files, or permitted by
org-mcp-file-scope-override."
    :mime-type "application/json"))
  "Specs for the resources org-mcp registers.
Each element is a `mcp-server-lib-register-server\=' `:resources\=' spec,
`(URI HANDLER :name STR [:description STR] [:mime-type STR])\='.  The
`{link}\=' in the URI makes it a resource template.")

(defun org-mcp-enable ()
  "Enable the org-mcp server.
Registers every tool and the org:// resource template under
`org-mcp--server-id'.  Which GTD query tools are among them depends
on `org-mcp-query-inbox-fn' and its siblings at the time of the call.

Registrations are reference counted: a spec registered twice needs
two `org-mcp-disable' calls before it goes, and the second
registration keeps the properties of the first."
  (mcp-server-lib-register-server
   :id org-mcp--server-id
   :version org-mcp-version
   :tools
   (append
    org-mcp--core-tool-specs
    (org-mcp--gtd-tool-specs)
    org-mcp--clock-tool-specs)
   :resources org-mcp--resource-specs))


(defun org-mcp-disable ()
  "Disable the org-mcp server.
Drops one reference to everything registered under
`org-mcp--server-id', removing whatever reaches zero.  It works on
what is registered at the time of the call, not on what a particular
`org-mcp-enable' added: an inner enable that configured fewer GTD
query tools than an enclosing one takes the enclosing call's away
when it is undone."
  (mcp-server-lib-unregister-server org-mcp--server-id))


;;; Script Installation

(defun org-mcp--package-script-path ()
  "Return the path to org-mcp-stdio.sh in the package directory.
Returns nil if not found."
  (let* ((library-path (locate-library "org-mcp"))
         (package-dir
          (and library-path (file-name-directory library-path)))
         (script-path
          (and package-dir
               (expand-file-name "org-mcp-stdio.sh" package-dir))))
    (when (and script-path (file-exists-p script-path))
      script-path)))

(defun org-mcp--installed-script-path ()
  "Return the path where org-mcp-stdio.sh should be installed.
Reuses `mcp-server-lib-install-directory' so org-mcp-stdio.sh
lands next to emacs-mcp-stdio.sh, which it resolves relative to
its own directory."
  (expand-file-name "org-mcp-stdio.sh"
                    mcp-server-lib-install-directory))

;;;###autoload
(defun org-mcp-install ()
  "Install org-mcp-stdio.sh to `mcp-server-lib-install-directory'.
The wrapper script resolves emacs-mcp-stdio.sh relative to its
own directory, so installing both shims to the same directory
(the default behaviour, since org-mcp reuses mcp-server-lib's
install directory) lets MCP clients invoke org-mcp-stdio.sh with
no extra configuration."
  (interactive)
  (let ((source (org-mcp--package-script-path))
        (target (org-mcp--installed-script-path)))
    (unless source
      (error "Cannot find org-mcp-stdio.sh in package directory"))
    (when (file-exists-p target)
      (unless (yes-or-no-p
               (format "File already exists at %s. Overwrite? "
                       target))
        (user-error "Installation cancelled")))
    (make-directory (file-name-directory target) t)
    (copy-file source target t)
    (set-file-modes target #o755)
    (message "Script installed to: %s" target)))

;;;###autoload
(defun org-mcp-uninstall ()
  "Remove installed org-mcp-stdio.sh from `mcp-server-lib-install-directory'."
  (interactive)
  (let ((target (org-mcp--installed-script-path)))
    (unless (file-exists-p target)
      (user-error "No script found at: %s" target))
    (when (yes-or-no-p (format "Remove script at %s? " target))
      (delete-file target)
      (message "Script removed from: %s" target))))

(provide 'org-mcp)
;; scripts/format-elisp.el lays this file out for 70 columns.
;; Local Variables:
;; fill-column: 70
;; End:
;;; org-mcp.el ends here
