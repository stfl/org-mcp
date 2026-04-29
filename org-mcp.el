;;; org-mcp.el --- MCP server for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis, Stefan Lendl

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;;         Stefan Lendl <git@stfl.dev>
;; Keywords: convenience, files, matching, outlines
;; Version: 0.9.0
;; Package-Requires: ((emacs "30.1") (mcp-server-lib "0.2.0") (org-ql "0.9"))
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
of files to MCP."
  :type '(repeat file)
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
`org-mcp--ql-extract-match'.  Non-nil return values are included
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

(defun org-mcp--extract-uri-suffix (uri prefix)
  "Extract suffix from URI after PREFIX.
Returns the suffix string if URI starts with PREFIX, nil otherwise."
  (when (string-prefix-p prefix uri)
    (substring uri (length prefix))))

(defun org-mcp--reject-uri-prefix (uri)
  "Validate that URI is a bare identifier, not an `org://' URI.
Tool inputs accept the bare form only — a UUID, file path, or
`file#headline' descriptor.  The `org://' prefix is reserved for
the MCP resource layer and must not appear in tool parameters.
Throws a tool validation error on rejection; otherwise returns URI."
  (when (string-prefix-p "org://" uri)
    (org-mcp--tool-validation-error
     "URI must not include the `org://' prefix; pass the bare form: %s"
     uri))
  uri)

(defun org-mcp--extract-id-from-uri (uri)
  "Extract org ID from a bare URI.
URI must be a bare identifier (no `org://' prefix).
Returns the ID string if URI is ID-based, nil otherwise."
  (org-mcp--reject-uri-prefix uri)
  (when (eq (plist-get (org-mcp--detect-uri-type uri) :type) 'id)
    uri))

(defun org-mcp--uri-is-id-based (uri)
  "Return non-nil if URI is based on an Org ID."
  (not (null (org-mcp--extract-id-from-uri uri))))

;; Error handling helpers

(defun org-mcp--headline-not-found-error (headline-path)
  "Throw error for HEADLINE-PATH not found."
  (mcp-server-lib-tool-throw
   (format "Cannot find headline: %s"
           (mapconcat #'identity headline-path "/"))))

(defun org-mcp--id-not-found-error (id)
  "Throw error for ID not found."
  (mcp-server-lib-tool-throw (format "Cannot find ID '%s'" id)))

(defun org-mcp--tool-validation-error (message &rest args)
  "Throw validation error MESSAGE with ARGS for tool operations."
  (mcp-server-lib-tool-throw (apply #'format message args)))

(defun org-mcp--resource-validation-error (message &rest args)
  "Signal validation error MESSAGE with ARGS for resource operations."
  (mcp-server-lib-resource-signal-error
   mcp-server-lib-jsonrpc-error-invalid-params
   (apply #'format message args)))

(defun org-mcp--state-mismatch-error (expected found context)
  "Throw state mismatch error.
EXPECTED is the expected value, FOUND is the actual value,
CONTEXT describes what is being compared."
  (mcp-server-lib-tool-throw
   (format "%s mismatch: expected '%s', found '%s'"
           context expected found)))

(defun org-mcp--resource-not-found-error (resource-type identifier)
  "Signal resource not found error.
RESOURCE-TYPE is the type of resource,
IDENTIFIER is the resource identifier."
  (mcp-server-lib-resource-signal-error
   mcp-server-lib-jsonrpc-error-invalid-params
   (format "Cannot find %s: '%s'" resource-type identifier)))

(defun org-mcp--tool-file-access-error (locator)
  "Throw file access error for tool operations.
LOCATOR is the resource identifier (file path or ID) that was
denied access."
  (mcp-server-lib-tool-throw
   (format "'%s': the referenced file not in allowed list" locator)))

(defun org-mcp--resource-file-access-error (locator)
  "Signal file access error for resource operations.
LOCATOR is the resource identifier (file path or ID) that was
denied access."
  (mcp-server-lib-resource-signal-error
   mcp-server-lib-jsonrpc-error-invalid-params
   (format "'%s': the referenced file not in allowed list" locator)))

;; Helpers

(cl-defun
 org-mcp--file-buffer-context (file-path)
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

(defun org-mcp--expanded-allowed-files ()
  "Return the effective allowed-files list with each entry made absolute.
Pulls the source list from the function `org-mcp-allowed-files'
(which falls back to `org-agenda-files' when the variable
`org-mcp-allowed-files' is nil), then resolves relative entries
against `org-directory' exactly as `org-agenda-files' does.
Absolute entries pass through after tilde and environment variable
expansion."
  (mapcar
   (lambda (f) (expand-file-name f org-directory))
   (org-mcp-allowed-files)))

(defun org-mcp--find-allowed-file (filename)
  "Find FILENAME in `org-mcp-allowed-files'.
Compares against the absolute form of each allowed entry (relative
entries resolved against `org-directory').
Returns the expanded path if found, nil if not in the allowed list."
  (when-let* ((found
               (cl-find
                (file-truename filename)
                (org-mcp--expanded-allowed-files)
                :test #'org-mcp--paths-equal-p)))
    (expand-file-name found)))

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

(defun org-mcp--complete-and-save (response-alist)
  "Create ID if needed and return JSON.
Creates or gets an Org ID for the current headline and returns it.
RESPONSE-ALIST is an alist of response fields."
  (let ((id (org-id-get-create)))
    (json-encode
     (append
      `((success . t))
      response-alist
      `((uri . ,(org-mcp--build-org-uri-from-id id)))))))

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
of re-implementing the filter."
  (declare (indent 0) (debug (body)))
  `(let ((org-agenda-files
          (cl-remove-if-not
           #'file-exists-p (org-mcp--expanded-allowed-files))))
     ,@body))

(defmacro org-mcp--modify-and-save
    (file-path operation response-alist &rest body)
  "Execute BODY to modify Org file at FILE-PATH.
BODY runs in the canonical visited buffer for FILE-PATH. If the
buffer was already modified before BODY runs, org-mcp leaves it dirty
and unsaved. Otherwise it saves the buffer and refreshes other clean
visiting buffers afterward. OPERATION is retained for call-site
clarity and compatibility.
BODY can access FILE-PATH, OPERATION, and RESPONSE-ALIST as
variables."
  (declare (indent 3) (debug (form form form body)))
  `(let* ((ctx (org-mcp--file-buffer-context ,file-path))
          (buf (plist-get ctx :buffer))
          (preexisting-modified-p (plist-get ctx :modified-p))
          (result nil))
     (ignore ,operation)
     (with-current-buffer buf
       (save-restriction
         (widen)
         (setq result
               (save-mark-and-excursion
                 (save-match-data
                   (goto-char (point-min))
                   ,@body
                   (org-mcp--complete-and-save ,response-alist))))))
     (org-mcp--maybe-save-buffer
      buf ,file-path preexisting-modified-p)
     result))

(defun org-mcp--find-allowed-file-with-id (id)
  "Find an allowed file containing the Org ID.
First looks up in the org-id database, then validates the file is in
the allowed list.
Returns the expanded file path if found and allowed.
Throws a tool error if ID exists but file is not allowed, or if ID
is not found."
  (if-let* ((id-file (org-id-find-id-file id)))
    ;; ID found in database, check if file is allowed
    (if-let* ((allowed-file (org-mcp--find-allowed-file id-file)))
      allowed-file
      (org-mcp--tool-file-access-error id))
    ;; ID not in database - might not exist or DB is stale
    ;; Fall back to searching allowed files manually
    (let ((found-file nil))
      (dolist (allowed-file (org-mcp--expanded-allowed-files))
        (unless found-file
          (when (file-exists-p allowed-file)
            (org-mcp--with-org-file allowed-file
              (when (org-find-property "ID" id)
                (setq found-file (expand-file-name allowed-file)))))))
      (or found-file (org-mcp--id-not-found-error id)))))

(defmacro org-mcp--with-uri-dispatch (uri headline-body id-body)
  "Dispatch tool URI handling based on parsed URI type.
URI must be a bare identifier — no `org://' prefix is allowed in
tool inputs.  Accepted forms: a UUID, file path, or `file#headline'
descriptor.
HEADLINE-BODY is executed when URI refers to a headline,
with the URI bound to `headline'.
ID-BODY is executed when URI is ID-based, with the ID bound to `id'.
Throws a validation error if URI carries the `org://' prefix or its
type is not recognized."
  (declare (indent 1))
  `(let* ((bare (org-mcp--reject-uri-prefix ,uri))
          (parsed-type
           (plist-get (org-mcp--detect-uri-type bare) :type)))
     (cond
      ((eq parsed-type 'id)
       (let ((id bare))
         ,id-body))
      ((eq parsed-type 'headline)
       (let ((headline bare))
         ,headline-body))
      (t
       (org-mcp--tool-validation-error
        "URI does not refer to a headline: %s"
        ,uri)))))

(defun org-mcp--validate-file-access (filename)
  "Validate that FILENAME is in the allowed list.
FILENAME must be an absolute path.
Returns the full path if allowed, signals an error otherwise."
  (unless (file-name-absolute-p filename)
    (org-mcp--resource-validation-error "Path must be absolute: %s"
                                        filename))
  (let ((allowed-file (org-mcp--find-allowed-file filename)))
    (unless allowed-file
      (org-mcp--resource-file-access-error filename))
    allowed-file))

(defun org-mcp--extract-headings ()
  "Extract heading structure from current org buffer.
Returns a vector of level-1 heading alists.  Each level-1 heading
includes its immediate level-2 children; deeper levels are not
included."
  (vconcat
   (org-element-map
    (org-element-parse-buffer 'headline) 'headline
    (lambda (h)
      (when (= (org-element-property :level h) 1)
        `((title . ,(org-element-property :raw-value h))
          (level . 1)
          (children
           .
           ,(vconcat
             (org-element-map
              (org-element-contents h) 'headline
              (lambda (child)
                (when (= (org-element-property :level child) 2)
                  `((title . ,(org-element-property :raw-value child))
                    (level . 2)
                    (children . []))))
              nil nil 'headline))))))
    nil nil 'headline)))

(defun org-mcp--generate-outline (file-path)
  "Generate JSON outline structure for FILE-PATH."
  (org-mcp--with-org-file file-path
    (let ((headings (org-mcp--extract-headings)))
      `((headings . ,headings)))))

(defun org-mcp--decode-file-path (encoded-path)
  "Decode special characters from ENCODED-PATH.
Specifically decodes %23 back to #."
  (replace-regexp-in-string "%23" "#" encoded-path))

(defun org-mcp--build-headline-path ()
  "Build URL-encoded slash-separated headline path from point.
Returns a string suitable for use in org:// URIs."
  (mapconcat #'url-hexify-string (org-get-outline-path t) "/"))

(defun org-mcp--split-headline-uri (path-after-protocol)
  "Split PATH-AFTER-PROTOCOL into (file-path . headline-path).
PATH-AFTER-PROTOCOL is the part after the scheme prefix (e.g. `org://').
Returns (FILE . HEADLINE) where FILE is the decoded file path and
HEADLINE is the part after the fragment separator.
File paths with # characters should be encoded as %23."
  (if-let* ((hash-pos (string-match "#" path-after-protocol)))
    (cons
     (org-mcp--decode-file-path
      (substring path-after-protocol 0 hash-pos))
     (substring path-after-protocol (1+ hash-pos)))
    (cons (org-mcp--decode-file-path path-after-protocol) nil)))

(defun org-mcp--detect-uri-type (uri)
  "Detect URI type and return plist with parsed components.
URI is a string that can be:
- A UUID (8-4-4-4-12 hex format) → (:type id :uuid \"...\")
- A file path with # fragment → (:type headline :file \"/...\" :headline-path (\"H1\" \"H2\"))
- A file path starting with / → (:type file :file \"/...\")
- A plain UUID string (for backward compatibility) → (:type id :uuid \"...\")
Signals error if URI format is invalid."
  (let ((uri (string-trim uri)))
    (cond
     ;; UUID pattern: 8-4-4-4-12 hex digits
     ((string-match-p
       "\\`[0-9a-fA-F]\\{8\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{12\\}\\'"
       uri)
      `(:type id :uuid ,uri))
     ;; Contains # → headline path (file#headline)
     ((string-match "#" uri)
      (let* ((hash-pos (string-match "#" uri))
             (file
              (org-mcp--decode-file-path (substring uri 0 hash-pos)))
             (headline-str (substring uri (1+ hash-pos))))
        (if (string-empty-p file)
            (org-mcp--resource-validation-error
             "URI has empty file path: %s"
             uri)
          `(:type
            headline
            :file ,(expand-file-name file)
            :headline-path
            ,(mapcar
              #'url-unhex-string (split-string headline-str "/"))))))
     ;; Starts with / → file path
     ((string-prefix-p "/" uri)
      `(:type file :file ,(expand-file-name uri)))
     ;; Any other string → treat as ID (e.g., "test-id-123", "my-custom-id")
     ;; This allows org-id to validate and provide meaningful error messages
     (t
      `(:type id :uuid ,uri)))))

(defun org-mcp--parse-resource-uri (uri)
  "Parse URI and return (file-path . headline-path).
Validates file access and returns expanded file path."
  (let (file-path
        headline-path)
    (org-mcp--with-uri-dispatch
        uri
      ;; Handle headline URIs
      (let* ((split-result (org-mcp--split-headline-uri headline))
             (filename (car split-result))
             (headline-path-str (cdr split-result))
             (allowed-file (org-mcp--validate-file-access filename)))
        (setq file-path (expand-file-name allowed-file))
        (setq headline-path
              (when headline-path-str
                (mapcar
                 #'url-unhex-string
                 (split-string headline-path-str "/")))))
      ;; Handle ID-based URIs
      (progn
        (setq file-path (org-mcp--find-allowed-file-with-id id))
        (setq headline-path (list id))))
    (cons file-path headline-path)))

(defun org-mcp--navigate-to-headline (headline-path)
  "Navigate to headline in HEADLINE-PATH.
HEADLINE-PATH is a list of headline titles forming a path.
Returns t if found, nil otherwise.  Point is left at the headline."
  (when-let* ((marker
               (let ((case-fold-search nil))
                 (condition-case nil
                     (org-find-olp headline-path t)
                   (error
                    nil)))))
    (goto-char marker)
    (set-marker marker nil)
    t))

(defun org-mcp--extract-headline-content ()
  "Extract content of current headline including the headline itself.
Point should be at the headline."
  (let ((start (line-beginning-position)))
    (org-end-of-subtree t t)
    ;; Remove trailing newline if present
    (when (and (> (point) start) (= (char-before) ?\n))
      (backward-char))
    (buffer-substring-no-properties start (point))))

(defun org-mcp--build-org-uri-from-id (id)
  "Build an org:// URI from ID.
ID is the UUID string."
  (concat "org://" id))

(defun org-mcp--build-org-uri-from-position ()
  "Build an org:// URI for the heading at point.
Uses ID if available, otherwise builds path-based URI."
  (if-let* ((id (org-entry-get (point) "ID")))
    (org-mcp--build-org-uri-from-id id)
    ;; Build path-based URI from current position
    (let* ((file (buffer-file-name))
           (headline-path (org-mcp--build-headline-path)))
      (concat "org://" file "#" headline-path))))

(defun org-mcp--heading-metadata-at-point (&optional inherit-tags)
  "Return canonical heading metadata at point as a plist.

Reads from a single `org-element-at-point' call so callers do not have
to chain `org-entry-get'/`org-get-tags'/`org-get-todo-state'.

Returned plist keys:
  :title      string, with TODO/priority/tags/comment stripped
  :todo       string or nil
  :priority   one-character string or nil
  :tags       list of strings (heading-local by default)
  :level      integer
  :id         string or nil
  :scheduled  Org timestamp string or nil
  :deadline   Org timestamp string or nil
  :closed     Org timestamp string or nil

When INHERIT-TAGS is non-nil, :tags is the inherited tag list from
`org-get-tags'.  Otherwise it is the heading's own tags from the
parsed element.  Timestamps are returned as their `:raw-value' so the
result matches `org-entry-get' (canonical Org abbreviation, no
locale-dependent reformatting)."
  (let* ((el (org-element-at-point))
         (priority-char (org-element-property :priority el))
         (sched (org-element-property :scheduled el))
         (deadl (org-element-property :deadline el))
         (clsd (org-element-property :closed el)))
    (list
     :title (org-element-property :raw-value el)
     :todo (org-element-property :todo-keyword el)
     :priority (and priority-char (char-to-string priority-char))
     :tags
     (if inherit-tags
         (org-get-tags)
       (org-element-property :tags el))
     :level (org-element-property :level el)
     :id (org-element-property :ID el)
     :scheduled (and sched (org-element-property :raw-value sched))
     :deadline (and deadl (org-element-property :raw-value deadl))
     :closed (and clsd (org-element-property :raw-value clsd)))))

(defun org-mcp--extract-heading-child ()
  "Extract lightweight child entry at current heading.
Returns plist with title, todo, level, and uri.
Point should be at the heading. Does not recurse into children."
  (let* ((meta (org-mcp--heading-metadata-at-point))
         (title (plist-get meta :title))
         (todo (plist-get meta :todo))
         (level (plist-get meta :level))
         (id (plist-get meta :id))
         (uri
          (if id
              (org-mcp--build-org-uri-from-id id)
            (let ((file (buffer-file-name))
                  (headline-path (org-mcp--build-headline-path)))
              (concat "org://" file "#" headline-path)))))
    `((title . ,title)
      ,@
      (when todo
        `((todo . ,todo)))
      (level . ,level) (uri . ,uri))))

(defun org-mcp--extract-structured-heading ()
  "Extract full structured JSON for current heading.
Point should be at the heading.
Returns alist with all heading properties and lightweight children."
  (let*
      ((meta (org-mcp--heading-metadata-at-point t))
       (title (plist-get meta :title))
       (todo (plist-get meta :todo))
       (priority (plist-get meta :priority))
       (tags (plist-get meta :tags))
       (level (plist-get meta :level))
       (id (plist-get meta :id))
       (scheduled (plist-get meta :scheduled))
       (deadline (plist-get meta :deadline))
       (closed (plist-get meta :closed))
       (uri (org-mcp--build-org-uri-from-position))
       (children '())
       (content-end
        (save-excursion
          (org-end-of-subtree t t)
          (point)))
       ;; Get body content (before children).  Use `org-end-of-meta-data'
       ;; (with FULL=t) to skip planning lines, PROPERTIES, LOGBOOK, and
       ;; any other drawers in any order — matching the behaviour of
       ;; `org-mcp--tool-edit-body'.
       (body-content
        (save-excursion
          (org-end-of-meta-data t)
          (let ((body-start (point)))
            (if (re-search-forward "^\*" content-end t)
                (buffer-substring-no-properties
                 body-start (line-beginning-position))
              (buffer-substring-no-properties
               body-start content-end)))))
       ;; Extract direct children
       (child-level (1+ level)))
    ;; Collect direct children via sibling navigation.
    (save-excursion
      (org-back-to-heading t)
      (when (org-goto-first-child)
        (cl-loop
         do
         (when (= (org-current-level) child-level)
           (push (org-mcp--extract-heading-child) children))
         while (org-get-next-sibling))))
    ;; Build result alist
    `((title . ,title)
      ,@
      (when todo
        `((todo . ,todo)))
      ,@
      (when priority
        `((priority . ,priority)))
      ,@
      (when tags
        `((tags . ,(vconcat tags))))
      ,@
      (when scheduled
        `((scheduled . ,scheduled)))
      ,@
      (when deadline
        `((deadline . ,deadline)))
      ,@
      (when closed
        `((closed . ,closed)))
      ,@
      (when id
        `((id . ,id)))
      (level . ,level) (uri . ,uri) ,@
      (when (and body-content (not (string-blank-p body-content)))
        `((content . ,(string-trim body-content))))
      (children . ,(vconcat (nreverse children))))))

(defun org-mcp--extract-structured-file (file-path)
  "Extract structured JSON for FILE-PATH.
Returns alist with file path, preamble content, and top-level children."
  (org-mcp--with-org-file file-path
    (let ((preamble-end
           (save-excursion
             (if (re-search-forward "^\\* " nil t)
                 (line-beginning-position)
               (point-max))))
          (children '()))
      ;; Extract preamble (content before first heading)
      (let ((content
             (buffer-substring-no-properties
              (point-min) preamble-end)))
        (goto-char preamble-end)
        ;; Extract top-level headings
        (while (re-search-forward "^\\* " nil t)
          (push (org-mcp--extract-heading-child) children)
          (org-end-of-subtree t t))
        `((file . ,file-path)
          ,@
          (when (and content (not (string-blank-p content)))
            `((content . ,(string-trim content))))
          (children . ,(vconcat (nreverse children))))))))

(defun org-mcp--get-headline-content (file-path headline-path)
  "Get content for headline at HEADLINE-PATH in FILE-PATH.
HEADLINE-PATH is a list of headline titles to traverse.
Returns the content string or nil if not found."
  (org-mcp--with-org-file file-path
    (when (org-mcp--navigate-to-headline headline-path)
      (org-mcp--extract-headline-content))))

(defun org-mcp--goto-headline-from-uri (headline-path is-id)
  "Navigate to headline based on HEADLINE-PATH and IS-ID flag.
If IS-ID is non-nil, treats HEADLINE-PATH as containing an ID.
Otherwise, navigates using HEADLINE-PATH as title hierarchy."
  (if is-id
      ;; ID case - headline-path contains single ID
      (if-let* ((pos (org-find-property "ID" (car headline-path))))
        (goto-char pos)
        (org-mcp--id-not-found-error (car headline-path)))
    ;; Path case - headline-path contains title hierarchy
    (unless (org-mcp--navigate-to-headline headline-path)
      (org-mcp--headline-not-found-error headline-path))))

(defun org-mcp--get-content-by-id (file-path id)
  "Get content for org node with ID in FILE-PATH.
Returns the content string or nil if not found."
  (org-mcp--with-org-file file-path
    (when-let* ((pos (org-find-property "ID" id)))
      (goto-char pos)
      (org-mcp--extract-headline-content))))

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

(defun org-mcp--clock-find-active ()
  "Find the first open CLOCK across allowed files.
Returns alist with file, heading, start, allowed keys, or nil.
Uses `org-find-open-clocks' on allowed files.  Falls back to the
native Emacs clock marker for an unclosed clock in a non-allowed
file, reading the timestamp via the Org element API."
  (or (catch 'found
        (dolist (file (org-mcp--expanded-allowed-files))
          (when (file-exists-p file)
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
                            (org-get-heading t t t t))))
                    (throw 'found
                           (list
                            (cons 'file (expand-file-name file))
                            (cons 'heading heading)
                            (cons 'start start-str)
                            (cons 'allowed t)
                            (cons 'marker marker)))))))))
        nil)
      ;; Fallback: native Emacs clock marker in a non-allowed file.
      ;; Use `org-element-at-point' instead of a CLOCK regex.
      (when (org-clock-is-active)
        (let* ((buf (org-clock-is-active))
               (file (buffer-file-name buf)))
          (when (and file (not (org-mcp--find-allowed-file file)))
            (with-current-buffer buf
              (save-excursion
                (goto-char org-clock-marker)
                (let ((el (org-element-at-point)))
                  (when (eq (org-element-type el) 'clock)
                    (list
                     (cons 'file (expand-file-name file))
                     (cons 'heading nil)
                     (cons
                      'start (org-mcp--clock-element-start-str el))
                     (cons 'allowed nil)
                     (cons 'marker org-clock-marker)))))))))))

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
             (dur-str (org-mcp--clock-duration-string duration)))
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
`org-element-at-point'.
Throws an MCP tool error if unbalanced blocks are found."
  (with-temp-buffer
    (let ((org-inhibit-startup t))
      (delay-mode-hooks
        (org-mode)))
    (insert body)
    (goto-char (point-min))
    (while (re-search-forward
            "^#\\+\\(BEGIN\\|END\\|begin\\|end\\)_\\(\\S-+\\)"
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

(defun org-mcp--navigate-to-parent-or-top (parent-path parent-id)
  "Navigate to parent headline or top of file.
PARENT-PATH is a list of headline titles (or nil for top-level).
PARENT-ID is an ID string (or nil).
Returns parent level (integer) if parent exists, nil for top-level.
Assumes point is in an Org buffer."
  (if (or parent-path parent-id)
      (progn
        (org-mcp--goto-headline-from-uri
         (or (and parent-id (list parent-id)) parent-path) parent-id)
        ;; Save parent level before moving point
        ;; Ensure we're at the beginning of headline
        (org-back-to-heading t)
        (org-current-level))
    ;; No parent specified - top level
    ;; Skip past any header comments (#+TITLE, #+AUTHOR, etc.)
    (while (and (not (eobp)) (looking-at "^#\\+"))
      (forward-line))
    ;; Position correctly: if blank line after headers,
    ;; skip it; if headline immediately after, stay
    (when (and (not (eobp)) (looking-at "^[ \t]*$"))
      ;; On blank line after headers, skip
      (while (and (not (eobp)) (looking-at "^[ \t]*$"))
        (forward-line)))
    nil))

(defun org-mcp--position-for-new-child (after-uri parent-end)
  "Position point for inserting a new child under current heading.
AFTER-URI is an optional org:// URI of a sibling to insert after.
PARENT-END is the end position of the parent's subtree.
Assumes point is at parent heading.
If AFTER-URI is non-nil, positions after that sibling.
If nil, positions at end of parent's subtree.
Throws validation error if AFTER-URI is invalid or sibling not found."
  (if (and after-uri (not (string-empty-p after-uri)))
      (progn
        ;; Parse afterUri to get the ID
        (let ((after-id (org-mcp--extract-id-from-uri after-uri))
              (found nil))
          (unless after-id
            (org-mcp--tool-validation-error
             "Field after_uri must be a bare ID-based URI ({uuid}): %s"
             after-uri))
          ;; Find the sibling with the specified ID
          (org-back-to-heading t) ;; At parent
          ;; Search sibling in parent's subtree
          ;; Move to first child
          (if (org-goto-first-child)
              (progn
                ;; Now search among siblings
                (while (and (not found) (< (point) parent-end))
                  (let ((current-id (org-entry-get nil "ID")))
                    (when (string= current-id after-id)
                      (setq found t)
                      ;; Move to sibling end
                      (org-end-of-subtree t t)))
                  (unless found
                    ;; Move to next sibling
                    (unless (org-get-next-sibling)
                      ;; No more siblings
                      (goto-char parent-end)))))
            ;; No children
            (goto-char parent-end))
          (unless found
            (org-mcp--tool-validation-error
             "Sibling with ID %s not found under parent"
             after-id))))
    ;; No after_uri - insert at end of parent's subtree
    (org-end-of-subtree t t)
    ;; If we're at the start of a sibling, go back one char
    ;; to be at the end of parent's content
    (when (looking-at "^\\*+ ")
      (backward-char 1))))

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
      ;; at the end of a sibling subtree positioned via after_uri).
      ;; This is what avoids the "creates a sibling of the parent
      ;; instead of a child" pitfall of bare `org-insert-heading' when
      ;; the parent has no children.
      (progn
        (org-mcp--ensure-newline)
        (org-insert-heading nil nil (1+ parent-level))
        (insert title))
    ;; Top-level heading
    ;; Check if there are no headlines yet (empty buffer or only
    ;; headers before us)
    (let ((has-headline
           (save-excursion
             (goto-char (point-min))
             (re-search-forward "^\\*+ " nil t))))
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
OLD-BODY is the substring to replace.
NEW-BODY is the replacement text.
BODY-CONTENT is the current body content string.
BODY-BEGIN is the buffer position where body starts.
BODY-END is the buffer position where body ends."
  (let ((new-body-content
         (cond
          ;; Special case: empty oldBody with empty body
          ((and (string= old-body "")
                (string-match-p "\\`[[:space:]]*\\'" body-content))
           new-body)
          ;; Normal single replacement
          (t
           (let ((pos
                  (string-match
                   (regexp-quote old-body) body-content)))
             (if pos
                 (concat
                  (substring body-content 0 pos)
                  new-body
                  (substring body-content (+ pos (length old-body))))
               body-content))))))

    ;; Replace the body content
    (if (< body-begin body-end)
        (delete-region body-begin body-end)
      ;; Empty body - ensure we're at the right position
      (goto-char body-begin))
    (insert new-body-content)))

;; Tool handlers

(defun org-mcp--tool-get-todo-config ()
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

(defun org-mcp--tool-get-tag-config ()
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

(defun org-mcp--tool-get-tag-candidates ()
  "Return the union of all candidate tags across `org-mcp-allowed-files'.
Mirrors the set Org's interactive tag completion offers via
`org-global-tags-completion-table': configured tags from
`org-tag-alist' / `org-tag-persistent-alist', any per-file
`#+TAGS:' / `#+FILETAGS:', plus every tag actually present on
headlines in those files.  Group keywords (`:startgroup' etc.)
are filtered out.  Tags are returned sorted and deduplicated."
  (org-mcp--with-allowed-agenda-files
    (let* ((table
            (append
             (org-global-tags-completion-table)
             org-tag-alist
             org-tag-persistent-alist))
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

(defun org-mcp--tool-get-priority-config ()
  "Return the priority configuration."
  (json-encode
   `((highest . ,(char-to-string org-priority-highest))
     (lowest . ,(char-to-string org-priority-lowest))
     (default . ,(char-to-string org-priority-default)))))

(defun org-mcp--tool-get-allowed-files ()
  "Return the list of allowed Org files.
Each entry is returned as an absolute path; relative entries in
`org-mcp-allowed-files' are resolved against `org-directory'."
  (json-encode
   `((files . ,(vconcat (org-mcp--expanded-allowed-files))))))

(defun org-mcp--tool-update-todo-state
    (uri new_state &optional current_state note)
  "Update the TODO state of a headline at URI.
Creates an Org ID for the headline if one doesn't exist.
Returns the ID-based URI for the updated headline.
NEW_STATE is the new TODO state to set.
CURRENT_STATE, when provided, is checked against the actual state.
NOTE, when provided, is stored in LOGBOOK as part of the state change entry.

MCP Parameters:
  uri - URI of the headline
        Formats:
          - {absolute-path}#{headline-path}
          - {id}
  new_state - New TODO state (must be in `org-todo-keywords')
  current_state - Expected current TODO state (string, optional)
                  When provided, must match actual state or tool will error
                  Omit to skip the state check
  note - Optional note to attach to this state transition (string, optional)
         When provided, stored in LOGBOOK as part of the state change entry
         Empty or whitespace-only values are ignored"
  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed))
         (actual-prev nil))
    (org-mcp--modify-and-save file-path "update"
                              `((previous_state . ,actual-prev)
                                (new_state . ,new_state))
      ;; Validate inside the Org buffer so `org-todo-keywords-1'
      ;; reflects merged user-customization + per-file `#+TODO:'.
      (org-mcp--validate-todo-state new_state)
      (org-mcp--goto-headline-from-uri
       headline-path (org-mcp--uri-is-id-based uri))

      ;; Capture actual previous state
      (beginning-of-line)
      (setq actual-prev (or (org-get-todo-state) ""))

      ;; Check current state matches (only when caller provided it)
      (when current_state
        (unless (string= actual-prev current_state)
          (org-mcp--state-mismatch-error
           current_state
           (or (org-get-todo-state) "(no state)")
           "State")))

      ;; Update the state.  Bind `post-command-hook' to nil so that any
      ;; interactive log-note hook `org-todo' may schedule (e.g. when
      ;; `org-log-done' is set) cannot fire later -- we attach our own
      ;; note explicitly via `org-mcp--insert-log-note' below.
      (let ((post-command-hook nil))
        (org-todo new_state))

      ;; Add note to state transition if provided
      (when (and note (not (string-empty-p (string-trim note))))
        (org-mcp--insert-log-note note 'state
                                  new_state
                                  actual-prev)))))

(defun org-mcp--tool-add-todo
    (title todo_state body parent_uri &optional tags after_uri)
  "Add a new TODO item to an Org file.
Creates an Org ID for the new headline and returns its ID-based URI.
TITLE is the headline text.
TODO_STATE is the TODO state from `org-todo-keywords'.
BODY is optional body text.
PARENT_URI is the URI of the parent item.
TAGS is an optional single tag string or list of tag strings.
AFTER_URI is optional URI of sibling to insert after.

MCP Parameters:
  title - The headline text
  todo_state - TODO state from `org-todo-keywords'
  body - Optional body text content
  parent_uri - Parent item URI
               Formats:
                 - {absolute-path}#{headline-path}
                 - {id}
  tags - Tags to add (optional, single string or array of strings)
  after_uri - Sibling to insert after (optional)
              Formats:
                - {absolute-path}#{headline-path}
                - {id}"
  (org-mcp--validate-headline-title title)
  (let* ((tag-list (org-mcp--validate-and-normalize-tags tags))
         file-path
         parent-path
         parent-id)

    ;; Parse parent URI once to extract file-path and parent location
    (org-mcp--with-uri-dispatch
        parent_uri
      ;; Handle org:// URIs
      (let* ((split-result (org-mcp--split-headline-uri headline))
             (filename (car split-result))
             (path-str (cdr split-result))
             (allowed-file (org-mcp--validate-file-access filename)))
        (setq file-path (expand-file-name allowed-file))
        (when (and path-str (> (length path-str) 0))
          (setq parent-path
                (mapcar
                 #'url-unhex-string (split-string path-str "/")))))
      ;; Handle ID-based URIs
      (progn
        (setq file-path (org-mcp--find-allowed-file-with-id id))
        (setq parent-id id)))

    ;; Add the TODO item
    (org-mcp--modify-and-save file-path "add TODO"
                              `((file
                                 .
                                 ,(file-name-nondirectory file-path))
                                (title . ,title))
      ;; Validate inside the Org buffer so `org-todo-keywords-1'
      ;; reflects merged user-customization + per-file `#+TODO:'.
      (org-mcp--validate-todo-state todo_state)
      (let ((parent-level
             (org-mcp--navigate-to-parent-or-top
              parent-path parent-id)))

        ;; Handle positioning after navigation to parent
        (when (or parent-path parent-id)
          (let ((parent-end
                 (save-excursion
                   (org-end-of-subtree t t)
                   (point))))
            (org-mcp--position-for-new-child after_uri parent-end)))

        ;; Validate body before inserting heading
        ;; Calculate the target level for validation
        (let ((target-level
               (if (or parent-path parent-id)
                   ;; Child heading - parent level + 1
                   (1+ (or parent-level 0))
                 ;; Top-level heading
                 1)))

          ;; Validate body content if provided
          (when body
            (org-mcp--validate-body-no-headlines body target-level)
            (org-mcp--validate-body-no-unbalanced-blocks body)))

        ;; Insert the new heading
        (org-mcp--insert-heading title parent-level)

        (org-todo todo_state)

        (when tag-list
          (org-set-tags tag-list))

        ;; Add body if provided
        (if body
            (progn
              (end-of-line)
              (insert "\n" body)
              (unless (string-suffix-p "\n" body)
                (insert "\n"))
              ;; Move back to the heading for org-id-get-create
              ;; org-id-get-create requires point to be on a heading
              (org-back-to-heading t))
          ;; No body - ensure newline after heading
          (end-of-line)
          (unless (looking-at "\n")
            (insert "\n")))))))

;; Resource handlers

(defun org-mcp--handle-outline-resource (params)
  "Handler for org-outline://{filename} template.
PARAMS is an alist containing the filename parameter."
  (let* ((filename (alist-get "filename" params nil nil #'string=))
         (allowed-file (org-mcp--validate-file-access filename))
         (outline
          (org-mcp--generate-outline
           (expand-file-name allowed-file))))
    (json-encode outline)))

(defun org-mcp--handle-org-resource (params)
  "Handler for org://{uri} template with auto-detection.
PARAMS is an alist containing the uri parameter.
Returns structured JSON for files and headlines."
  (let* ((uri (alist-get "uri" params nil nil #'string=))
         (parsed (org-mcp--detect-uri-type uri)))
    (pcase (plist-get parsed :type)
      (`id
       (let* ((uuid (plist-get parsed :uuid))
              (file-path (org-id-find-id-file uuid)))
         (unless file-path
           (org-mcp--resource-not-found-error "ID" uuid))
         (let ((allowed-file (org-mcp--find-allowed-file file-path)))
           (unless allowed-file
             (org-mcp--resource-file-access-error uuid))
           (org-mcp--with-org-file allowed-file
             (when-let* ((pos (org-find-property "ID" uuid)))
               (goto-char pos)
               (json-encode
                (org-mcp--extract-structured-heading)))))))
      (`headline
       (let* ((file (plist-get parsed :file))
              (headline-path (plist-get parsed :headline-path))
              (allowed-file (org-mcp--validate-file-access file)))
         (org-mcp--with-org-file allowed-file
           (if (org-mcp--navigate-to-headline headline-path)
               (json-encode (org-mcp--extract-structured-heading))
             (org-mcp--resource-not-found-error
              "Headline" (mapconcat #'identity headline-path "/"))))))
      (`file
       (let ((allowed-file
              (org-mcp--validate-file-access
               (plist-get parsed :file))))
         (json-encode
          (org-mcp--extract-structured-file allowed-file)))))))

(defun org-mcp--handle-headline-resource (params)
  "Handler for org-read-headline tool with auto-detection.
PARAMS is an alist containing the uri parameter (can be file, file#path, or UUID).
Returns plain text content."
  (let* ((uri (alist-get "uri" params nil nil #'string=))
         (parsed (org-mcp--detect-uri-type uri)))
    (pcase (plist-get parsed :type)
      (`id
       (let* ((uuid (plist-get parsed :uuid))
              (file-path (org-id-find-id-file uuid)))
         (unless file-path
           (org-mcp--resource-not-found-error "ID" uuid))
         (let ((allowed-file (org-mcp--find-allowed-file file-path)))
           (unless allowed-file
             (org-mcp--resource-file-access-error uuid))
           (org-mcp--get-content-by-id allowed-file uuid))))
      (`headline
       (let* ((file (plist-get parsed :file))
              (headline-path (plist-get parsed :headline-path))
              (allowed-file (org-mcp--validate-file-access file)))
         (let ((content
                (org-mcp--get-headline-content
                 allowed-file headline-path)))
           (unless content
             (org-mcp--resource-not-found-error
              "Headline" (mapconcat #'identity headline-path "/")))
           content)))
      (`file
       (let ((allowed-file
              (org-mcp--validate-file-access
               (plist-get parsed :file))))
         (org-mcp--read-file allowed-file))))))

(defun org-mcp--tool-rename-headline (uri current_title new_title)
  "Rename headline title at URI from CURRENT_TITLE to NEW_TITLE.
Preserves the current TODO state and tags, creates an Org ID for the
headline if one doesn't exist.
Returns the ID-based URI for the renamed headline.

MCP Parameters:
  uri - URI of the headline
        Formats:
          - {absolute-path}#{headline-path}
          - {id}
  current_title - Current title without TODO state or tags
  new_title - New title without TODO state or tags"
  (org-mcp--validate-headline-title new_title)

  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed)))

    ;; Rename the headline in the file
    (org-mcp--modify-and-save file-path "rename"
                              `((previous_title . ,current_title)
                                (new_title . ,new_title))
      ;; Navigate to the headline
      (org-mcp--goto-headline-from-uri
       headline-path (org-mcp--uri-is-id-based uri))

      ;; Verify current title matches
      (beginning-of-line)
      (let ((actual-title (org-get-heading t t t t)))
        (unless (string= actual-title current_title)
          (org-mcp--state-mismatch-error
           current_title actual-title "Title")))

      (org-edit-headline new_title))))

(defun org-mcp--tool-edit-body
    (resource_uri old_body new_body &optional append)
  "Edit or append to body content of an Org node.
RESOURCE_URI is the URI of the node to edit.
OLD_BODY is the substring to search for (replace mode only).
NEW_BODY is the replacement or appended text.
APPEND if non-nil, append NEW_BODY to end of body instead of replacing.

MCP Parameters:
  resource_uri - URI of the node
                 Formats:
                   - {absolute-path}#{headline-path}
                   - {id}
  old_body - Substring to replace within the body (replace mode only).
             Must be unique.  Use \"\" to add to empty nodes.
             Ignored when append is true.
  new_body - Replacement or appended text
  append - Append to end of body instead of replacing (optional,
           default false)"
  ;; Normalize JSON false to nil for proper boolean handling
  ;; JSON false can arrive as :false (keyword) or "false" (string)
  (let ((append
         (cond
          ((eq append :false)
           nil)
          ((equal append "false")
           nil)
          (t
           append))))
    (if append
        ;; Append mode
        (progn
          (when (or (null new_body)
                    (string-empty-p new_body)
                    (string-match-p "\\`[[:space:]]*\\'" new_body))
            (org-mcp--tool-validation-error
             "new_body cannot be empty or whitespace-only"))

          (org-mcp--validate-body-no-unbalanced-blocks new_body)

          (let* ((parsed (org-mcp--parse-resource-uri resource_uri))
                 (file-path (car parsed))
                 (headline-path (cdr parsed)))

            (org-mcp--modify-and-save file-path "append body" nil
              (org-mcp--goto-headline-from-uri
               headline-path (org-mcp--uri-is-id-based resource_uri))

              (org-mcp--validate-body-no-headlines
               new_body (org-current-level))

              ;; Save heading position for org-id-get-create later
              (let ((heading-pos (point)))

                ;; Skip past headline and properties/planning
                (org-end-of-meta-data t)

                ;; Find end of body (before next headline or end of subtree)
                (let ((body-end nil))
                  (save-excursion
                    (if (org-goto-first-child)
                        (setq body-end (point))
                      (org-end-of-subtree t)
                      (setq body-end (point))))

                  (goto-char body-end)

                  ;; Ensure there's a newline before our content
                  (unless (or (= body-end (point-min))
                              (= (char-before body-end) ?\n))
                    (insert "\n"))

                  (insert new_body)

                  ;; Ensure trailing newline
                  (unless (= (char-before (point)) ?\n)
                    (insert "\n")))

                ;; Return to heading for org-id-get-create
                (goto-char heading-pos)))))

      ;; Replace mode
      (org-mcp--validate-body-no-unbalanced-blocks new_body)

      (let* ((parsed (org-mcp--parse-resource-uri resource_uri))
             (file-path (car parsed))
             (headline-path (cdr parsed)))

        (org-mcp--modify-and-save file-path "edit body" nil
          (org-mcp--goto-headline-from-uri
           headline-path (org-mcp--uri-is-id-based resource_uri))

          (org-mcp--validate-body-no-headlines
           new_body (org-current-level))

          ;; Skip past headline and properties
          (org-end-of-meta-data t)

          ;; Get body boundaries
          (let ((body-begin (point))
                (body-end nil)
                (body-content nil)
                (occurrence-count 0))

            ;; Find end of body (before next headline or end of subtree)
            (save-excursion
              (if (org-goto-first-child)
                  ;; Has children - body ends before first child
                  (setq body-end (point))
                ;; No children - body extends to end of subtree
                (org-end-of-subtree t)
                (setq body-end (point))))

            ;; Extract body content
            (setq body-content
                  (buffer-substring-no-properties
                   body-begin body-end))

            ;; Trim leading newline if present
            ;; (`org-end-of-meta-data' includes it)
            (when (and (> (length body-content) 0)
                       (= (aref body-content 0) ?\n))
              (setq body-content (substring body-content 1))
              (setq body-begin (1+ body-begin)))

            ;; Check if body is empty
            (when (string-match-p "\\`[[:space:]]*\\'" body-content)
              ;; Empty oldBody + empty body -> add content
              (if (string= old_body "")
                  ;; Treat as single replacement
                  (setq occurrence-count 1)
                (org-mcp--tool-validation-error
                 "Node has no body content")))

            ;; Count occurrences (unless already handled above)
            (unless (= occurrence-count 1)
              ;; Empty oldBody with non-empty body is an error
              (if (and (string= old_body "")
                       (not
                        (string-match-p
                         "\\`[[:space:]]*\\'" body-content)))
                  (org-mcp--tool-validation-error
                   "Cannot use empty old_body with non-empty body")
                ;; Normal occurrence counting
                (let ((case-fold-search nil)
                      (search-pos 0))
                  (while (string-match
                          (regexp-quote old_body) body-content
                          search-pos)
                    (setq occurrence-count (1+ occurrence-count))
                    (setq search-pos (match-end 0))))))

            ;; Validate occurrences
            (cond
             ((= occurrence-count 0)
              (org-mcp--tool-validation-error
               "Body text not found: %s"
               old_body))
             ((> occurrence-count 1)
              (org-mcp--tool-validation-error
               "Text appears %d times (must be unique)"
               occurrence-count)))

            ;; Perform replacement
            (org-mcp--replace-body-content
             old_body new_body body-content body-begin body-end)))))))

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
  "Org special properties that cannot be set via `org-set-properties'.")

(defun org-mcp--tool-set-properties (uri properties)
  "Set or delete properties on the headline at URI.
PROPERTIES is an alist of property name-value pairs.
String values set the property; null/empty values delete it.

MCP Parameters:
  uri - URI of the headline
        Formats:
          - {absolute-path}#{headline-path}
          - {id}
  properties - JSON object of property name-value pairs (required)
               String value: set property to that value
               null or empty string: delete the property
               Special properties (TODO, TAGS, PRIORITY, etc.) are
               forbidden"
  (unless (and properties (listp properties))
    (org-mcp--tool-validation-error
     "Properties must be a non-empty JSON object"))
  ;; Validate no special properties
  (dolist (pair properties)
    (let ((key
           (if (symbolp (car pair))
               (symbol-name (car pair))
             (car pair))))
      (when (member (upcase key) org-mcp--special-properties)
        (org-mcp--tool-validation-error
         "Cannot set special property '%s' - use the dedicated tool"
         key))))

  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed))
         (set-props nil)
         (deleted-props nil))

    (org-mcp--modify-and-save file-path "set properties"
                              `((properties_set . ,set-props)
                                (properties_deleted . ,deleted-props))
      (org-mcp--goto-headline-from-uri
       headline-path (org-mcp--uri-is-id-based uri))

      (dolist (pair properties)
        (let* ((key
                (if (symbolp (car pair))
                    (symbol-name (car pair))
                  (car pair)))
               (val (cdr pair)))
          (if (or (null val) (equal val ""))
              (progn
                (org-delete-property key)
                (push key deleted-props))
            (org-set-property
             key
             (if (stringp val)
                 val
               (format "%s" val)))
            (push key set-props))))
      (setq set-props (nreverse set-props))
      (setq deleted-props (nreverse deleted-props)))))

(defun org-mcp--tool-update-scheduled (uri &optional scheduled)
  "Update SCHEDULED timestamp on headline at URI.
SCHEDULED is an ISO date string or nil/empty to remove.

MCP Parameters:
  uri - URI of the headline
        Formats:
          - {absolute-path}#{headline-path}
          - {id}
  scheduled - ISO date string (optional)
              Examples: \"2026-03-27\", \"2026-03-27 09:00\"
              nil or empty string removes the timestamp"
  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed))
         (previous-scheduled nil)
         (new-scheduled nil))

    (org-mcp--modify-and-save file-path "update scheduled"
                              `((previous_scheduled
                                 . ,previous-scheduled)
                                (new_scheduled . ,new-scheduled))
      (org-mcp--goto-headline-from-uri
       headline-path (org-mcp--uri-is-id-based uri))

      (setq previous-scheduled
            (or (org-entry-get (point) "SCHEDULED") ""))

      (if (or (null scheduled) (equal scheduled ""))
          ;; Remove scheduled
          (progn
            (org-schedule '(4))
            (setq new-scheduled ""))
        ;; Validate date format before calling org-schedule
        (org-mcp--validate-date-string scheduled)
        (org-schedule nil scheduled)
        (setq new-scheduled
              (or (org-entry-get (point) "SCHEDULED") ""))))))

(defun org-mcp--tool-update-deadline (uri &optional deadline)
  "Update DEADLINE timestamp on headline at URI.
DEADLINE is an ISO date string or nil/empty to remove.

MCP Parameters:
  uri - URI of the headline
        Formats:
          - {absolute-path}#{headline-path}
          - {id}
  deadline - ISO date string (optional)
             Examples: \"2026-03-27\", \"2026-03-27 09:00\"
             nil or empty string removes the timestamp"
  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed))
         (previous-deadline nil)
         (new-deadline nil))

    (org-mcp--modify-and-save file-path "update deadline"
                              `((previous_deadline
                                 . ,previous-deadline)
                                (new_deadline . ,new-deadline))
      (org-mcp--goto-headline-from-uri
       headline-path (org-mcp--uri-is-id-based uri))

      (setq previous-deadline
            (or (org-entry-get (point) "DEADLINE") ""))

      (if (or (null deadline) (equal deadline ""))
          ;; Remove deadline
          (progn
            (org-deadline '(4))
            (setq new-deadline ""))
        ;; Validate date format before calling org-deadline
        (org-mcp--validate-date-string deadline)
        (org-deadline nil deadline)
        (setq new-deadline
              (or (org-entry-get (point) "DEADLINE") ""))))))

(defun org-mcp--tool-set-tags (uri &optional tags)
  "Set tags on headline at URI.
TAGS can be a string, list of strings, or nil/empty to clear all tags.

MCP Parameters:
  uri - URI of the headline
        Formats:
          - {absolute-path}#{headline-path}
          - {id}
  tags - Tags to set (string or array, optional)
         Single tag: \"work\"
         Multiple tags: [\"work\", \"urgent\"]
         nil or empty to clear all tags
         Validated against org-tag-alist if configured"
  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed))
         (previous-tags nil)
         (new-tags nil))

    ;; Validate tags if provided
    (let ((tag-list
           (if (or (null tags) (equal tags "") (equal tags []))
               nil
             (org-mcp--validate-and-normalize-tags tags))))

      (org-mcp--modify-and-save file-path "set tags"
                                `((previous_tags
                                   .
                                   ,(or previous-tags []))
                                  (new_tags . ,(or new-tags [])))
        (org-mcp--goto-headline-from-uri
         headline-path (org-mcp--uri-is-id-based uri))

        (setq previous-tags (vconcat (org-get-tags nil t)))

        (org-set-tags tag-list)

        (setq new-tags (vconcat (org-get-tags nil t)))))))

(defun org-mcp--tool-set-priority (uri &optional priority)
  "Set priority on headline at URI.
PRIORITY is a single-character string or nil/empty to remove.

MCP Parameters:
  uri - URI of the headline
        Formats:
          - {absolute-path}#{headline-path}
          - {id}
  priority - Priority character (string, optional)
             Must be within org-priority-highest to org-priority-lowest
             nil or empty string removes the priority"
  ;; Validate priority if provided
  (when (and priority (not (equal priority "")))
    (unless (= (length priority) 1)
      (org-mcp--tool-validation-error
       "Priority must be a single character, got '%s'"
       priority))
    (let ((char (string-to-char priority)))
      (unless (and (>= char org-priority-highest)
                   (<= char org-priority-lowest))
        (org-mcp--tool-validation-error
         "Priority '%s' out of range ('%c' to '%c')"
         priority org-priority-highest org-priority-lowest))))

  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed))
         (previous-priority nil)
         (new-priority nil))

    (org-mcp--modify-and-save file-path "set priority"
                              `((previous_priority
                                 . ,previous-priority)
                                (new_priority . ,new-priority))
      (org-mcp--goto-headline-from-uri
       headline-path (org-mcp--uri-is-id-based uri))

      (setq previous-priority
            (let ((p
                   (org-element-property
                    :priority (org-element-at-point))))
              (if p
                  (char-to-string p)
                "")))

      (if (or (null priority) (equal priority ""))
          ;; Remove priority
          (progn
            (org-priority ?\s)
            (setq new-priority ""))
        ;; Set priority
        (org-priority (string-to-char priority))
        (setq new-priority priority)))))


(defun org-mcp--tool-add-logbook-note (uri note)
  "Add a timestamped note to the LOGBOOK drawer of headline at URI.

MCP Parameters:
  uri - URI of the headline
        Formats:
          - {absolute-path}#{headline-path}
          - {id}
  note - Note text to add (string, required)
         Cannot be empty or whitespace-only
         Multi-line notes are indented properly in the LOGBOOK"
  (when (or (null note)
            (string-empty-p note)
            (string-match-p "\\`[[:space:]]*\\'" note))
    (org-mcp--tool-validation-error
     "Note cannot be empty or whitespace-only"))

  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed)))

    (org-mcp--modify-and-save file-path "add logbook note" nil
      (org-mcp--goto-headline-from-uri
       headline-path (org-mcp--uri-is-id-based uri))
      (org-mcp--insert-log-note note 'note))))

;; org-ql integration

(defun org-mcp--ql-extract-match ()
  "Extract match data at point for `org-ql-select' :action.
Returns an alist with headline metadata suitable for JSON encoding.
Extra properties from `org-mcp-ql-extra-properties' are appended."
  (let* ((meta (org-mcp--heading-metadata-at-point))
         (title (plist-get meta :title))
         (level (plist-get meta :level))
         (file (buffer-file-name))
         (todo (plist-get meta :todo))
         (priority (plist-get meta :priority))
         (tags (plist-get meta :tags))
         (scheduled (plist-get meta :scheduled))
         (deadline (plist-get meta :deadline))
         (closed (plist-get meta :closed))
         (uri (org-mcp--build-org-uri-from-position))
         (props
          (cl-remove-if
           (lambda (pair)
             (member
              (car pair)
              '("ALLTAGS"
                "BLOCKED"
                "CATEGORY"
                "CLOCKSUM"
                "CLOCKSUM_T"
                "CLOSED"
                "DEADLINE"
                "FILE"
                "ITEM"
                "PRIORITY"
                "SCHEDULED"
                "TAGS"
                "TIMESTAMP"
                "TIMESTAMP_IA"
                "TODO")))
           (org-entry-properties nil 'standard)))
         (result `((title . ,title) (level . ,level) (file . ,file))))
    (when todo
      (push `(todo . ,todo) result))
    (when priority
      (push `(priority . ,priority) result))
    (when tags
      (push `(tags . ,(vconcat tags)) result))
    (when scheduled
      (push `(scheduled . ,scheduled) result))
    (when deadline
      (push `(deadline . ,deadline) result))
    (when closed
      (push `(closed . ,closed) result))
    (push `(uri . ,uri) result)
    (when props
      (let ((props-alist
             (mapcar (lambda (p) (cons (car p) (cdr p))) props)))
        (push `(properties . ,props-alist) result)))
    (dolist (extra org-mcp-ql-extra-properties)
      (let ((val (funcall (cdr extra))))
        (when val
          (push (cons (car extra) val) result))))
    (nreverse result)))

(defun org-mcp--tool-ql-query (query &optional files)
  "Search Org files using an org-ql QUERY expression.
QUERY is a string containing an org-ql query sexp.
FILES is an optional list of file paths to search (must be in
`org-mcp-allowed-files'); defaults to all allowed files.

MCP Parameters:
  query - org-ql query sexp as string (e.g. \"(todo \\\"TODO\\\")\")
  files - Array of file paths to search (optional)"
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
    (org-mcp--with-allowed-agenda-files
      (let* ((target-files
              (if files
                  (mapcar
                   (lambda (f)
                     (or (org-mcp--find-allowed-file f)
                         (org-mcp--tool-file-access-error f)))
                   (append files nil))
                org-agenda-files))
             (action #'org-mcp--ql-extract-match)
             (matches
              (condition-case err
                  (org-ql-select
                   target-files
                   query-sexp
                   :action action)
                (error
                 (org-mcp--tool-validation-error
                  "Org-ql query error: %s"
                  (error-message-string err))))))
        (json-encode
         `((matches . ,(vconcat matches))
           (total . ,(length matches))
           (files_searched . ,(length target-files))))))))

;; GTD query tools

(defun org-mcp--run-gtd-query (query-sexp)
  "Run QUERY-SEXP via `org-ql-select' with optional sorting.
Uses `org-mcp-query-sort-fn' for sorting when set.
Returns JSON-encoded results in the same format as org-ql-query."
  (org-mcp--with-allowed-agenda-files
    (let* ((target-files org-agenda-files)
           (matches
            (condition-case err
                ;; Collect org-elements with the default action, then
                ;; sort.  We map `org-mcp--ql-extract-match' in a
                ;; second pass because `org-ql-select' applies :action
                ;; before :sort — custom actions that return
                ;; non-element data would break sort functions
                ;; expecting org-elements.
                (let ((elements
                       (org-ql-select
                        target-files
                        query-sexp
                        :sort org-mcp-query-sort-fn)))
                  (mapcar
                   (lambda (el)
                     (with-current-buffer (org-element-property
                                           :buffer el)
                       (save-excursion
                         (goto-char (org-element-property :begin el))
                         (org-mcp--ql-extract-match))))
                   elements))
              (error
               (org-mcp--tool-validation-error
                "Org-ql query error: %s"
                (error-message-string err))))))
      (json-encode
       `((matches . ,(vconcat matches))
         (total . ,(length matches))
         (files_searched . ,(length target-files)))))))

(defun org-mcp--tool-query-inbox ()
  "Query inbox items using the configured query function.

MCP Parameters: None

Returns: Same format as org-ql-query tool, sorted by
`org-mcp-query-sort-fn' when configured."
  (org-mcp--run-gtd-query (funcall org-mcp-query-inbox-fn)))

(defun org-mcp--tool-query-next (&optional tag)
  "Query next action items, optionally filtered by TAG.

MCP Parameters:
  tag - Tag string to filter by (string, optional)

Returns: Same format as org-ql-query tool, sorted by
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

Returns: Same format as org-ql-query tool, sorted by
`org-mcp-query-sort-fn' when configured."
  (let ((tag-filter
         (when (and tag (not (string-empty-p tag)))
           `(tags ,tag))))
    (org-mcp--run-gtd-query
     (funcall org-mcp-query-backlog-fn tag-filter))))

;; Tools duplicating resource templates

(defun org-mcp--tool-read (uri)
  "Tool handler for org-read.
URI must be a bare identifier — no `org://' prefix.
Returns structured JSON.

MCP Parameters:
  uri - URI string (string, required). Accepted forms (no `org://'
        prefix; the prefix is reserved for the MCP resource layer):
        - /path/to/file.org (file path)
        - /path/to/file.org#Headline/Subhead (headline path)
        - UUID (8-4-4-4-12 format)"
  (org-mcp--reject-uri-prefix uri)
  (org-mcp--handle-org-resource `(("uri" . ,uri))))

(defun org-mcp--tool-read-outline (file)
  "Tool wrapper for org-outline://{filename} resource template.
FILE is the absolute path to an Org file.

MCP Parameters:
  file - Absolute path to an Org file"
  (org-mcp--handle-outline-resource `(("filename" . ,file))))

(defun org-mcp--tool-read-headline (uri)
  "Tool handler for org-read-headline.
URI must be a bare identifier — no `org://' prefix.
Returns plain text content.

MCP Parameters:
  uri - URI string (string, required). Accepted forms (no `org://'
        prefix; the prefix is reserved for the MCP resource layer):
        - /path/to/file.org (returns entire file)
        - /path/to/file.org#Headline/Subhead (headline path)
        - UUID (8-4-4-4-12 format)
        Headline paths use URL encoding for special chars."
  (org-mcp--reject-uri-prefix uri)
  (org-mcp--handle-headline-resource `(("uri" . ,uri))))

;; Clock tools

(defun org-mcp--tool-get-clock-config ()
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

(defun org-mcp--tool-clock-find-dangling ()
  "Find all open (unclosed) clocks in allowed Org files.
Uses `org-find-open-clocks' on each file in `org-mcp-allowed-files'.
Reads each clock's timestamp via the Org element API.

MCP Parameters: None"
  (org-mcp--with-allowed-agenda-files
    (let ((all-clocks nil))
      (dolist (file org-agenda-files)
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
                            (org-get-heading t t t t))))
                    (push `((file . ,clock-file)
                            (heading . ,heading)
                            (start . ,start-str))
                          all-clocks))))))))
      (let ((total (length all-clocks)))
        (json-encode
         `((open_clocks . ,(vconcat (nreverse all-clocks)))
           (total . ,total)))))))

(defun org-mcp--tool-clock-get-active ()
  "Return the currently active clock entry, if any.

MCP Parameters: None"
  (let ((active (org-mcp--clock-find-active)))
    (if active
        (if (eq (alist-get 'allowed active) nil)
            (json-encode
             '((active . t) (in_allowed_file . :json-false)))
          (json-encode
           `((active . t)
             (file . ,(alist-get 'file active))
             (heading . ,(alist-get 'heading active))
             (start . ,(alist-get 'start active)))))
      (json-encode '((active . :json-false))))))

(defun org-mcp--tool-clock-in (uri &optional start_time resolve)
  "Clock in to the heading at URI.
If another clock is active, it is closed first.
When `org-clock-continuously' is non-nil and no explicit START_TIME
is given, the new clock may start at the previous clock's end time
if it is within `org-mcp-clock-continuous-threshold' minutes.
When RESOLVE is \"true\", dangling (unclosed) CLOCK lines under
the target heading are deleted before clocking in.

MCP Parameters:
  uri - URI of the headline to clock in
        Formats:
          - {absolute-path}#{headline-path}
          - {id}
  start_time - Optional ISO 8601 start time (e.g. 2026-03-23T14:30:00)
  resolve - When \"true\", delete dangling clocks before clocking in"
  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed))
         (is-id (org-mcp--uri-is-id-based uri))
         (now (current-time))
         (explicit-start
          (when start_time
            (org-mcp--clock-parse-timestamp start_time)))
         ;; Check for active clock and close it if needed
         ;; Check for active clock and close it if needed
         (active (org-mcp--clock-find-active)))
    ;; Close active clock if exists.  When resolve is requested and the
    ;; active clock is in the same file, skip auto-close — resolve will
    ;; delete the dangling clock in the modify-and-save body instead.
    (when (and active
               (not
                (and (equal resolve "true")
                     (org-mcp--paths-equal-p
                      (alist-get 'file active) file-path))))
      (let* ((active-file (alist-get 'file active))
             (close-at
              (org-mcp--clock-round-time
               (if explicit-start
                   explicit-start
                 now)))
             (marker (alist-get 'marker active)))
        (if (alist-get 'allowed active)
            ;; Allowed file: close via org-clock-out using canonical buffer
            (progn
              (let* ((buf (org-mcp--get-file-buffer active-file))
                     (was-modified
                      (with-current-buffer buf
                        (buffer-modified-p)))
                     (start-str (alist-get 'start active)))
                (when start-str
                  (with-current-buffer buf
                    (save-mark-and-excursion
                      (save-match-data
                        (goto-char (point-min))
                        (when (re-search-forward
                               (concat
                                "^\\([ \t]*CLOCK: \\["
                                (regexp-quote start-str)
                                "\\]\\)[ \t]*$")
                               nil t)
                          (goto-char (match-beginning 1))
                          (let ((el (org-element-at-point)))
                            (when (eq (org-element-type el) 'clock)
                              (move-marker
                               org-clock-marker
                               (org-element-property :begin el))
                              (move-marker
                               org-clock-hd-marker
                               (save-excursion
                                 (org-back-to-heading t)
                                 (point)))
                              (org-clock-out nil t close-at)))))))
                  (org-mcp--maybe-save-buffer
                   buf active-file was-modified))))
          ;; Non-allowed file: close via org-clock-out
          (let ((buf (marker-buffer org-clock-marker)))
            (when buf
              (let ((was-modified
                     (with-current-buffer buf
                       (buffer-modified-p))))
                (with-current-buffer buf
                  (org-clock-out nil t close-at))
                (unless was-modified
                  (with-current-buffer buf
                    (when (buffer-modified-p)
                      (save-buffer))))))))))
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
                                     . ,(org-get-heading t t t t))
                                    ,@
                                    (when (> resolved-count 0)
                                      `((resolved
                                         . ,resolved-count))))
          (org-mcp--goto-headline-from-uri headline-path is-id)
          (when (equal resolve "true")
            (setq resolved-count (org-mcp--clock-resolve-dangling)))
          (org-mcp--clock-insert-entry clock-start))))))

(defun org-mcp--tool-clock-out (&optional uri end_time)
  "Clock out the currently active clock.
If URI is provided, validates it matches the active clock's heading.
URI is an optional URI to validate against active clock.
END_TIME is an optional ISO 8601 end time (e.g. 2026-03-23T16:45:00).

MCP Parameters:
  uri - Optional URI to validate against active clock
        Formats:
          - {absolute-path}#{headline-path}
          - {id}
  end_time - Optional ISO 8601 end time (e.g. 2026-03-23T16:45:00)"
  (let ((active (org-mcp--clock-find-active)))
    (unless active
      (org-mcp--tool-validation-error "No active clock to stop"))
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
      ;; If URI provided, validate it matches
      (when uri
        (let* ((parsed (org-mcp--parse-resource-uri uri))
               (uri-file (car parsed)))
          (unless (org-mcp--paths-equal-p uri-file active-file)
            (org-mcp--tool-validation-error
             "URI file does not match active clock file"))))
      (let* ((duration (float-time (time-subtract end start-time)))
             (close-text
              (format "--%s => %s"
                      (org-mcp--clock-format-timestamp end)
                      (org-mcp--clock-duration-string duration))))
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
          ;; Find the active clock line by its exact start timestamp
          (when (re-search-forward (concat
                                    "^\\([ \t]*CLOCK: \\["
                                    (regexp-quote start-str)
                                    "\\]\\)[ \t]*$")
                                   nil t)
            (goto-char (match-end 1))
            (insert close-text)
            ;; Navigate to heading for complete-and-save
            (org-back-to-heading t)))))))

(defun org-mcp--tool-clock-add (uri start end)
  "Add a completed clock entry to the heading at URI.
START is ISO 8601 start time (e.g. 2026-03-23T14:30:00).
END is ISO 8601 end time (e.g. 2026-03-23T16:45:00).

MCP Parameters:
  uri - URI of the headline
        Formats:
          - {absolute-path}#{headline-path}
          - {id}
  start - ISO 8601 start time (e.g. 2026-03-23T14:30:00)
  end - ISO 8601 end time (e.g. 2026-03-23T16:45:00)"
  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed))
         (is-id (org-mcp--uri-is-id-based uri))
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
      (org-mcp--goto-headline-from-uri headline-path is-id)
      (org-mcp--clock-insert-entry start-time end-time))))

(defun org-mcp--tool-clock-delete (uri start)
  "Delete a clock entry from the heading at URI.
START is the ISO 8601 start time of the clock entry to delete
\\(e.g., 2026-03-23T14:30:00).

MCP Parameters:
  uri - URI of the headline
        Formats:
          - {absolute-path}#{headline-path}
          - {id}
  start - ISO 8601 start time of the clock entry to delete
          (e.g. 2026-03-23T14:30:00)"
  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed))
         (is-id (org-mcp--uri-is-id-based uri))
         (start-time
          (org-mcp--clock-round-time
           (org-mcp--clock-parse-timestamp start)))
         (deleted-info nil))
    (org-mcp--modify-and-save file-path "clock-delete"
                              `((deleted . t) ,@deleted-info)
      (org-mcp--goto-headline-from-uri headline-path is-id)
      (setq deleted-info (org-mcp--clock-delete-entry start-time))
      (unless deleted-info
        (org-mcp--tool-validation-error
         "No clock entry starting at %s found"
         (org-mcp--clock-format-timestamp start-time))))))

(defun org-mcp-enable ()
  "Enable the org-mcp server."
  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-todo-config
   :id "org-get-todo-config"
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
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-tag-config
   :id "org-get-tag-config"
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
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-tag-candidates
   :id "org-get-tag-candidates"
   :description
   "Return all candidate tags the user might want to use across the
files in `org-mcp-allowed-files'.

Mirrors Org's interactive tag completion (C-c C-q): the result is
the union of configured tags from `org-tag-alist' /
`org-tag-persistent-alist', any per-file `#+TAGS:' / `#+FILETAGS:'
keywords, and every tag actually present on a headline in any
allowed file.  Group keywords like `:startgroup' are filtered out.

Parameters: None

Returns JSON object with:
  tags - Sorted, deduplicated array of tag-name strings.

Use this when suggesting or completing tags rather than
`org-get-tag-config', which only exposes the static configuration."
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-priority-config
   :id "org-get-priority-config"
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
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-allowed-files
   :id "org-get-allowed-files"
   :description
   "Get the list of Org files accessible through the org-mcp
server.  Returns the configured allowed files exactly as specified in
org-mcp-allowed-files.

Parameters: None

Returns JSON object containing:
  files (array of strings): Absolute paths of allowed Org files

Example response:
  {
    \"files\": [
      \"/home/user/org/tasks.org\",
      \"/home/user/org/projects.org\",
      \"/home/user/notes/daily.org\"
    ]
  }

Empty configuration returns:
  {
    \"files\": []
  }

Use cases:
  - Discovery: What Org files can I access through MCP?
  - URI Construction: I need to build an org:// URI - what's
    the exact path?
  - Access Troubleshooting: Why is my file access failing?
  - Configuration Verification: Did my org-mcp-allowed-files setting
    work correctly?"
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-update-todo-state
   :id "org-update-todo-state"
   :description
   "Update the TODO state of an Org headline.  Changes the task state
while preserving the headline title, tags, and other properties.
Creates an Org ID property for the headline if one doesn't exist.

Parameters:
  uri - URI of the headline to update (string, required)
        Formats:
          - {absolute-path}#{url-encoded-path}
          - {uuid}
  current_state - Expected current TODO state (string, optional)
                  When provided, must match actual state or tool will error
                  Omit to skip the state check
  new_state - New TODO state to set (string, required)
              Must be valid keyword from org-todo-keywords
  note - Optional note to attach to this state transition (string, optional)
         When provided, stored in LOGBOOK as part of the state change entry
         Empty or whitespace-only values are ignored

Returns JSON object:
  success - Always true on success (boolean)
  previous_state - The previous TODO state (string, empty for none)
  new_state - The new TODO state that was set (string)
  uri - org:// URI (org://{uuid}) for the updated headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-add-todo
   :id "org-add-todo"
   :description
   "Add a new TODO item to an Org file at a specified location.
Creates the headline with TODO state, optional tags, and optional body content.
Automatically creates an Org ID property for the new headline.

Parameters:
  title - Headline text without TODO state or tags (string, required)
          Cannot be empty or whitespace-only
          Cannot contain newlines
  todo_state - TODO keyword from org-todo-keywords (string, required)
  tags - Tags for the headline (string or array, optional)
         Single tag: \"urgent\"
         Multiple tags: [\"work\", \"urgent\"]
         Validated against org-tag-alist if configured
         Must follow Org tag rules (alphanumeric, _, @)
         Respects mutually exclusive tag groups
  body - Body content under the headline (string, optional)
         Cannot contain headlines at same or higher level as new item
         If #+BEGIN/#+END blocks are present, they must be balanced
  parent_uri - Parent location (string, required). Pass the bare form
               (no `org://' prefix).
               For top-level: {absolute-path}
               For child: {path}#{parent-path}
                         or {parent-uuid}
  after_uri - Sibling to insert after (string, optional). Pass the
              bare form (no `org://' prefix).
              Must be {uuid} format
              If omitted, appends as last child of parent

Returns JSON object:
  success - Always true on success (boolean)
  uri - org:// URI (org://{uuid}) for the new headline
  file - Filename (not full path) where item was added
  title - The headline title that was created

Positioning behavior:
  - With parent_uri only: Appends as last child of parent
  - With parent_uri + after_uri: Inserts immediately after specified
sibling
  - Top-level (parent_uri with no fragment): Adds at end of file."
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-rename-headline
   :id "org-rename-headline"
   :description
   "Rename an Org headline's title while preserving its TODO state,
tags, properties, and body content.  Creates an Org ID property for
the headline if one doesn't exist.

Parameters:
  uri - URI of the headline to rename (string, required)
        Formats:
          - {absolute-path}#{url-encoded-path}
          - {uuid}
  current_title - Expected current title without TODO/tags (string,
required)
                  Must match actual title or tool will error
                  Used to prevent race conditions
  new_title - New title without TODO state or tags (string, required)
              Cannot be empty or whitespace-only
              Cannot contain newlines

Returns JSON object:
  success - Always true on success (boolean)
  previous_title - The previous headline title (string)
  new_title - The new title that was set (string)
  uri - org:// URI (org://{uuid}) for the renamed headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-edit-body
   :id "org-edit-body"
   :description
   "Edit or append to the body content of an Org headline.  In replace
mode (default), finds and replaces a unique substring within the
headline's body text.  In append mode, inserts new content after
existing body content but before any child headlines.  Creates an
Org ID property for the headline if one doesn't exist.

Parameters:
  resource_uri - URI of the headline to edit (string, required)
                 Formats:
                   - {absolute-path}#{url-encoded-path}
                   - {uuid}
  old_body - Substring to find and replace (string, required in
             replace mode, ignored when append is true)
             Must appear exactly once in the body
             Use empty string \"\" only for adding to empty nodes
  new_body - Replacement or appended text (string, required)
             Cannot introduce headlines at same or higher level
             Must maintain balanced #+BEGIN/#+END blocks
  append - Append new_body to end of body instead of replacing
           (boolean, optional, default false)
           When true, old_body is ignored

Returns JSON object:
  success - Always true on success (boolean)
  uri - org:// URI (org://{uuid}) for the edited headline

Special behavior - Empty old_body (replace mode):
  When old_body is \"\", the tool adds content to empty nodes:
  - Only works if node body is empty or whitespace-only
  - Error if node already has content
  - Useful for adding initial content to newly created headlines"
   :read-only nil
   :server-id org-mcp--server-id)

  ;; Entry update tools
  (mcp-server-lib-register-tool
   #'org-mcp--tool-set-properties
   :id "org-set-properties"
   :description
   "Set or delete properties on an Org headline.  Updates the
PROPERTIES drawer.  Creates an Org ID for the headline if one
doesn't exist.

Parameters:
  uri - URI of the headline (string, required)
        Formats:
          - {absolute-path}#{url-encoded-path}
          - {uuid}
  properties - JSON object of property name-value pairs (required)
               String value: set the property
               null or empty string: delete the property
               Special properties (TODO, TAGS, PRIORITY, SCHEDULED,
               DEADLINE, etc.) are forbidden - use dedicated tools

Returns JSON object:
  success - Always true on success (boolean)
  properties_set - Array of property names that were set
  properties_deleted - Array of property names that were deleted
  uri - org:// URI (org://{uuid}) for the headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-update-scheduled
   :id "org-update-scheduled"
   :description
   "Update the SCHEDULED timestamp on an Org headline.  Creates an
Org ID for the headline if one doesn't exist.

Parameters:
  uri - URI of the headline (string, required)
        Formats:
          - {absolute-path}#{url-encoded-path}
          - {uuid}
  scheduled - ISO date string (string, optional)
              Examples: \"2026-03-27\", \"2026-03-27 09:00\"
              Omit or empty string to remove the timestamp

Returns JSON object:
  success - Always true on success (boolean)
  previous_scheduled - Previous SCHEDULED value (string, empty if none)
  new_scheduled - New SCHEDULED value (string, empty if removed)
  uri - org:// URI (org://{uuid}) for the headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-update-deadline
   :id "org-update-deadline"
   :description
   "Update the DEADLINE timestamp on an Org headline.  Creates an
Org ID for the headline if one doesn't exist.

Parameters:
  uri - URI of the headline (string, required)
        Formats:
          - {absolute-path}#{url-encoded-path}
          - {uuid}
  deadline - ISO date string (string, optional)
             Examples: \"2026-03-27\", \"2026-03-27 09:00\"
             Omit or empty string to remove the timestamp

Returns JSON object:
  success - Always true on success (boolean)
  previous_deadline - Previous DEADLINE value (string, empty if none)
  new_deadline - New DEADLINE value (string, empty if removed)
  uri - org:// URI (org://{uuid}) for the headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-set-tags
   :id "org-set-tags"
   :description
   "Set tags on an Org headline, replacing any existing tags.
Creates an Org ID for the headline if one doesn't exist.

Parameters:
  uri - URI of the headline (string, required)
        Formats:
          - {absolute-path}#{url-encoded-path}
          - {uuid}
  tags - Tags to set (string or array, optional)
         Single tag: \"work\"
         Multiple tags: [\"work\", \"urgent\"]
         Omit or empty to clear all tags
         Validated against org-tag-alist if configured
         Must follow Org tag rules (alphanumeric, _, @)
         Respects mutually exclusive tag groups

Returns JSON object:
  success - Always true on success (boolean)
  previous_tags - Array of previous tags
  new_tags - Array of new tags
  uri - org:// URI (org://{uuid}) for the headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-set-priority
   :id "org-set-priority"
   :description
   "Set or remove priority on an Org headline.  Creates an Org ID
for the headline if one doesn't exist.

Parameters:
  uri - URI of the headline (string, required)
        Formats:
          - {absolute-path}#{url-encoded-path}
          - {uuid}
  priority - Priority character (string, optional)
             Must be in the configured range (default \"A\" to \"C\")
             Use org-get-priority-config to check the valid range
             Omit or empty string to remove priority

Returns JSON object:
  success - Always true on success (boolean)
  previous_priority - Previous priority (string, empty if none)
  new_priority - New priority (string, empty if removed)
  uri - org:// URI (org://{uuid}) for the headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-add-logbook-note
   :id "org-add-logbook-note"
   :description
   "Add a timestamped note to the LOGBOOK drawer of an Org headline.
Creates the LOGBOOK drawer if it doesn't exist.  Creates an Org ID
for the headline if one doesn't exist.

Parameters:
  uri - URI of the headline (string, required)
        Formats:
          - {absolute-path}#{url-encoded-path}
          - {uuid}
  note - Note text to add (string, required)
         Cannot be empty or whitespace-only
         Multi-line notes are properly indented in the LOGBOOK
         Note is inserted at the top of the LOGBOOK drawer

Returns JSON object:
  success - Always true on success (boolean)
  uri - org:// URI (org://{uuid}) for the headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-read
   :id "org-read"
   :description
   "Read Org file or headline with structured JSON output. Auto-detects
URI format and returns structured data including children, properties,
and timestamps.

Parameters:
  uri - URI (string, required). Pass the bare form (no `org://'
        prefix; that prefix is reserved for the MCP resource layer).
        Formats:
        - /path/to/file.org - file path (returns file with children)
        - /path/to/file.org#Headline/Subhead - headline path
        - UUID (8-4-4-4-12 format) - ID-based lookup

Returns: JSON object with structured data:
  For files:
    file - File path
    content - Preamble text before first heading (if any)
    children - Array of top-level headings (title, todo, level, uri)
  For headlines:
    title - Headline text
    todo - TODO state (if present)
    priority - Priority letter (if present)
    tags - Array of tags (if present)
    scheduled - Scheduled timestamp (if present)
    deadline - Deadline timestamp (if present)
    closed - Closed timestamp (if present)
    id - Org ID (if present)
    level - Heading level
    uri - org:// URI for this heading
    content - Body text (if present)
    children - Array of direct children (title, todo, level, uri)

File must be in org-mcp-allowed-files."
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-read-outline
   :id "org-read-outline"
   :description
   "Get hierarchical structure of Org file as JSON outline. Returns
   all headline titles and nesting relationships at full depth. File
   must be in org-mcp-allowed-files.

Parameters:
  file - Absolute path to Org file (string, required)

Returns: JSON object with hierarchical outline structure"
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-read-headline
   :id "org-read-headline"
   :description
   "Read Org headline or file as plain text. Auto-detects URI format.
Returns headline with TODO state, tags, properties, body text, and all
nested subheadings.

Parameters:
  uri - URI (string, required). Pass the bare form (no `org://'
        prefix; that prefix is reserved for the MCP resource layer).
        Formats:
        - /path/to/file.org - returns entire file
        - /path/to/file.org#Headline/Subhead - headline path
        - UUID (8-4-4-4-12 format) - ID-based lookup
        Headline paths use URL encoding for special chars.

Returns: Plain text content of the headline and its subtree (or file)"
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-ql-query
   :id "org-ql-query"
   :description
   "Search Org files using org-ql query expressions.  Supports
querying by TODO state, tags, priority, deadlines, properties, and
more.  Returns matched entries as JSON with URIs for follow-up access.

Parameters:
  query - org-ql query sexp as string (string, required)
          Examples:
            (todo \"TODO\")
            (tags \"work\")
            (and (todo \"TODO\") (priority \"A\"))
            (deadline :to today)
  files - Subset of allowed files to search (array of strings, optional)
          Defaults to all org-mcp-allowed-files

Returns JSON object:
  matches - Array of matched entries, each with:
    title - Headline text (string)
    level - Headline level (number)
    file - Absolute file path (string)
    todo - TODO state (string, omitted if none)
    priority - Priority letter (string, omitted if none)
    tags - Local tags (array, omitted if none)
    id - Org ID (string, omitted if none)
    uri - org:// URI (string)
    properties - Standard properties (object, omitted if none)
  total - Number of matches (number)
  files_searched - Number of files searched (number)"
   :read-only t
   :server-id org-mcp--server-id)

  ;; GTD query tools (registered only when configured)
  (when org-mcp-query-inbox-fn
    (mcp-server-lib-register-tool
     #'org-mcp--tool-query-inbox
     :id "query-inbox"
     :description
     "Query inbox items using the configured GTD workflow.
Returns items matching the inbox query, sorted by rank when
a sort function is configured.

Parameters: None

Returns: Same format as org-ql-query tool"
     :read-only t
     :server-id org-mcp--server-id))

  (when org-mcp-query-next-fn
    (mcp-server-lib-register-tool
     #'org-mcp--tool-query-next
     :id "query-next"
     :description
     "Query next action items using the configured GTD workflow.
Returns actionable items sorted by rank when a sort function
is configured.

Parameters:
  tag - Tag string to filter results (string, optional)

Returns: Same format as org-ql-query tool"
     :read-only t
     :server-id org-mcp--server-id))

  (when org-mcp-query-backlog-fn
    (mcp-server-lib-register-tool
     #'org-mcp--tool-query-backlog
     :id "query-backlog"
     :description
     "Query backlog items (projects and standalone actions) using
the configured GTD workflow.  Returns items sorted by rank when
a sort function is configured.

Parameters:
  tag - Tag string to filter results (string, optional)

Returns: Same format as org-ql-query tool"
     :read-only t
     :server-id org-mcp--server-id))

  ;; Clock tools
  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-clock-config
   :id "org-get-clock-config"
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
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-clock-get-active
   :id "org-clock-get-active"
   :description
   "Get the currently active clock, if any.  Searches all allowed
files for an unclosed CLOCK entry.  Also detects native Emacs clocks
running in non-allowed files via `org-clock-is-active'.

Parameters: None

Returns JSON object:
  active - Whether a clock is active (boolean)
  in_allowed_file - false when clock is in a non-allowed file
    (only present in that case; file/heading/start are omitted)
  file - File path of active clock (string, only if active
    in allowed file)
  heading - Heading title with active clock (string, only if active
    in allowed file)
  start - Start timestamp string (string, only if active
    in allowed file)"
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-clock-in
   :id "org-clock-in"
   :description
   "Clock in to the specified heading.  If another clock is active,
it is automatically closed first.

When org-clock-continuously is enabled and no explicit start_time
is given, the new clock may start at the previous clock's end time
if it is within the continuous threshold.

Rounding is applied per org-clock-rounding-minutes.

Parameters:
  uri - URI of the headline to clock in (string, required)
        Formats:
          - {absolute-path}#{headline-path}
          - {uuid}
  start_time - ISO 8601 start time (string, optional)
               Example: 2026-03-23T14:30:00
               If omitted, uses current time (or continuous time)
  resolve - When 'true', delete dangling (unclosed) CLOCK lines
            under the heading before clocking in (string, optional)

Returns JSON object:
  success - Always true on success (boolean)
  clocked_in - Always true (boolean)
  start - Formatted start timestamp (string)
  heading - The heading title (string)
  uri - org:// URI (org://{uuid}) for the headline
  resolved - Number of dangling clocks deleted (integer, only if
             resolve was requested and dangling clocks were found)"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-clock-out
   :id "org-clock-out"
   :description
   "Clock out the currently active clock.

Rounding is applied per org-clock-rounding-minutes.

Parameters:
  uri - Optional URI to validate against active clock (string)
        If provided, must match the file of the active clock
        Formats:
          - {absolute-path}#{headline-path}
          - {uuid}
  end_time - ISO 8601 end time (string, optional)
             Example: 2026-03-23T16:45:00
             If omitted, uses current time

Returns JSON object:
  success - Always true on success (boolean)
  clocked_out - Always true (boolean)
  heading - The heading title (string)
  start - Start timestamp (string)
  end - End timestamp (string)
  duration - Duration as H:MM (string)
  uri - org:// URI (org://{uuid}) for the headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-clock-add
   :id "org-clock-add"
   :description
   "Add a completed clock entry to a heading.  Creates a LOGBOOK
drawer if one doesn't exist.  New entries are inserted at the top
of the LOGBOOK.

Rounding is applied per org-clock-rounding-minutes.

Parameters:
  uri - URI of the headline (string, required)
        Formats:
          - {absolute-path}#{headline-path}
          - {uuid}
  start - ISO 8601 start time (string, required)
          Example: 2026-03-23T14:30:00
  end - ISO 8601 end time (string, required)
        Example: 2026-03-23T16:45:00
        Must be after start time

Returns JSON object:
  success - Always true on success (boolean)
  added - Always true (boolean)
  start - Formatted start timestamp (string)
  end - Formatted end timestamp (string)
  duration - Duration as H:MM (string)
  uri - org:// URI (org://{uuid}) for the headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-clock-delete
   :id "org-clock-delete"
   :description
   "Delete a clock entry from a heading.  Removes the LOGBOOK
drawer if it becomes empty after deletion.

Rounding is applied per org-clock-rounding-minutes.

Parameters:
  uri - URI of the headline (string, required)
        Formats:
          - {absolute-path}#{headline-path}
          - {uuid}
  start - ISO 8601 start time of the clock entry to delete
          (string, required)
          Example: 2026-03-23T14:30:00

Returns JSON object:
  success - Always true on success (boolean)
  deleted - Always true (boolean)
  start - Start timestamp of deleted entry (string)
  end - End timestamp of deleted entry (string, present if closed)
  duration - Duration as H:MM (string, present if closed)
  uri - org:// URI (org://{uuid}) for the headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-clock-find-dangling
   :id "org-clock-find-dangling"
   :description
   "Find all open (unclosed) clocks in allowed Org files.
Searches for dangling CLOCK entries that were never closed.
Uses Emacs native `org-find-open-clocks' on each allowed file.

Parameters: None

Returns JSON object:
  open_clocks - Array of open clocks, each with:
    file - File path (string)
    heading - Heading title (string)
    start - Start timestamp (string)
  total - Number of open clocks found (number)"
   :read-only t
   :server-id org-mcp--server-id)

  ;; Register template resources for org files
  (mcp-server-lib-register-resource
   "org://{uri}" #'org-mcp--handle-org-resource
   :name "Org resource (structured JSON)"
   :description
   "Access Org file or headline with structured JSON output.
Auto-detects URI format and returns structured data.

URI format: org://{uri}
  uri - Can be one of:
    - /path/to/file.org - file path
    - /path/to/file.org#Headline/Subhead - headline path
    - UUID (8-4-4-4-12 format) - ID-based lookup

Returns: JSON object with structured data:
  For files:
    file - File path
    content - Preamble text before first heading (if any)
    children - Array of top-level headings (title, todo, level, uri)
  For headlines:
    title - Headline text
    todo - TODO state (if present)
    priority - Priority letter (if present)
    tags - Array of tags (if present)
    scheduled - Scheduled timestamp (if present)
    deadline - Deadline timestamp (if present)
    closed - Closed timestamp (if present)
    id - Org ID (if present)
    level - Heading level
    uri - org:// URI for this heading
    content - Body text (if present)
    children - Array of direct children (title, todo, level, uri)

File must be in org-mcp-allowed-files."
   :mime-type "application/json"
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-resource
   "org-outline://{filename}" #'org-mcp--handle-outline-resource
   :name "Org file outline"
   :description
   "Get the hierarchical structure of an Org file as a JSON
outline.  Extracts headline titles and their nesting relationships up
to 2 levels deep.

URI format: org-outline://{filename}
  filename - Absolute path to the Org file (required)

Returns: JSON object with structure:
  {
    \"headings\": [
      {
        \"title\": \"Top-level heading\",
        \"level\": 1,
        \"children\": [
          {
            \"title\": \"Subheading\",
            \"level\": 2,
            \"children\": []
          }
        ]
      }
    ]
  }

Depth limitation:
  - Level 1 headings (top-level) are extracted
  - Level 2 headings (direct children) are included
  - Deeper levels are not included (children arrays are empty)

Example URIs:
  org-outline:///home/user/notes/tasks.org
  org-outline:///Users/name/Documents/projects.org

Use this resource to:
  - Get document structure overview
  - Understand file organization without reading full content"
   :mime-type "application/json"
   :server-id org-mcp--server-id))

(defun org-mcp-disable ()
  "Disable the org-mcp server."
  (mcp-server-lib-unregister-tool
   "org-get-todo-config" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-get-tag-config" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-get-tag-candidates" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-get-priority-config" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-get-allowed-files" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-update-todo-state" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-add-todo" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-rename-headline" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-edit-body" org-mcp--server-id)
  ;; Entry update tools
  (mcp-server-lib-unregister-tool
   "org-set-properties" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-update-scheduled" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-update-deadline" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-set-tags" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-set-priority" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-add-logbook-note" org-mcp--server-id)
  ;; Unregister workaround tools
  (mcp-server-lib-unregister-tool "org-read" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-read-outline" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-read-headline" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-ql-query" org-mcp--server-id)
  ;; GTD query tools (ignore errors if they weren't registered)
  (ignore-errors
    (mcp-server-lib-unregister-tool "query-inbox" org-mcp--server-id))
  (ignore-errors
    (mcp-server-lib-unregister-tool "query-next" org-mcp--server-id))
  (ignore-errors
    (mcp-server-lib-unregister-tool
     "query-backlog" org-mcp--server-id))
  ;; Clock tools
  (mcp-server-lib-unregister-tool
   "org-get-clock-config" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-clock-get-active" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-clock-in" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-clock-out" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-clock-add" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-clock-delete" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-clock-find-dangling" org-mcp--server-id)
  ;; Unregister template resources
  (mcp-server-lib-unregister-resource
   "org://{uri}" org-mcp--server-id)
  (mcp-server-lib-unregister-resource
   "org-outline://{filename}" org-mcp--server-id))

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
;;; org-mcp.el ends here
