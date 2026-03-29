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
  "List of absolute paths to Org files that can be accessed via MCP."
  :type '(repeat file)
  :group 'org-mcp)

(defcustom org-mcp-stored-queries-file nil
  "Path to file storing named org-ql queries.
When nil, stored query functionality is disabled."
  :type '(choice (const :tag "Disabled" nil) (file :tag "File path"))
  :group 'org-mcp)

(defcustom org-mcp-clock-continuous-threshold 30
  "Max minutes since last clock-out for continuous clocking.
When `org-clock-continuously' is non-nil and a new clock-in occurs
within this many minutes of the last clock-out, the new clock starts
at the previous clock's end time."
  :type 'integer
  :group 'org-mcp)

(defvar org-mcp--stored-queries 'unloaded
  "In-memory alist of stored org-ql queries.
Each entry is (KEY . ((query . QUERY-STRING) (description . DESC))).
Symbol `unloaded' before first access.")

(defconst org-mcp--server-id "org-mcp"
  "Server ID for org-mcp MCP server registration.")

(defconst org-mcp--uri-headline-prefix "org-headline://"
  "URI prefix for headline resources.")

(defun org-mcp--extract-uri-suffix (uri prefix)
  "Extract suffix from URI after PREFIX.
Returns the suffix string if URI starts with PREFIX, nil otherwise."
  (when (string-prefix-p prefix uri)
    (substring uri (length prefix))))

(defun org-mcp--extract-id-from-uri (uri)
  "Extract org ID from an org:// URI.
Returns the ID string if URI is ID-based, nil otherwise."
  (when (string-prefix-p "org://" uri)
    (let ((suffix (substring uri (length "org://"))))
      (when (eq
             (plist-get (org-mcp--detect-uri-type suffix) :type) 'id)
        suffix))))

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

(defun org-mcp--read-file (file-path)
  "Read and return the contents of FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun org-mcp--paths-equal-p (path1 path2)
  "Return t if PATH1 and PATH2 refer to the same file.
Handles symlinks and path variations by normalizing both paths."
  (string= (file-truename path1) (file-truename path2)))

(defun org-mcp--find-allowed-file (filename)
  "Find FILENAME in `org-mcp-allowed-files'.
Returns the expanded path if found, nil if not in the allowed list."
  (when-let* ((found
               (cl-find
                (file-truename filename)
                org-mcp-allowed-files
                :test #'org-mcp--paths-equal-p)))
    (expand-file-name found)))

(defun org-mcp--refresh-file-buffers (file-path)
  "Refresh all buffers visiting FILE-PATH.
Preserves narrowing state across the refresh operation."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when-let* ((buf-file (buffer-file-name)))
        (when (string= buf-file file-path)
          (let ((was-narrowed (buffer-narrowed-p))
                (narrow-start nil)
                (narrow-end nil))
            ;; Save narrowing markers if narrowed
            (when was-narrowed
              (setq narrow-start (point-min-marker))
              (setq narrow-end (point-max-marker)))
            (condition-case err
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
                    (narrow-to-region narrow-start narrow-end)))
              (error
               (org-mcp--tool-validation-error
                "Failed to refresh buffer for file %s: %s. \
Check your Emacs hooks (`before-revert-hook', \
`after-revert-hook', `revert-buffer-function')"
                file-path (error-message-string err))))))))))

(defun org-mcp--complete-and-save (file-path response-alist)
  "Create ID if needed, save FILE-PATH, return JSON.
Creates or gets an Org ID for the current headline and returns it.
FILE-PATH is the visited file path; used only for buffer refresh.
RESPONSE-ALIST is an alist of response fields."
  (let ((id (org-id-get-create)))
    (save-buffer)
    (org-mcp--refresh-file-buffers file-path)
    (json-encode
     (append
      `((success . t))
      response-alist
      `((uri . ,(org-mcp--build-org-uri-from-id id)))))))

(defun org-mcp--fail-if-modified (file-path operation)
  "Check if FILE-PATH has unsaved change in any buffer.
OPERATION is a string describing the operation for error messages."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (string= (buffer-file-name) file-path)
                 (buffer-modified-p))
        (org-mcp--tool-validation-error
         "Cannot %s: file has unsaved changes in buffer"
         operation)))))

(defmacro org-mcp--with-org-file (file-path &rest body)
  "Execute BODY in a temp Org buffer with file at FILE-PATH."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert-file-contents ,file-path)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defmacro org-mcp--modify-and-save
    (file-path operation response-alist &rest body)
  "Execute BODY to modify Org file at FILE-PATH, then save result.
First validates that FILE-PATH has no unsaved changes (using
OPERATION for error messages).  Then executes BODY in a temp buffer
set up for the Org file.  After BODY executes, creates an Org ID if
needed, saves the buffer, refreshes any visiting buffers, and
returns the result of `org-mcp--complete-and-save' with FILE-PATH
and RESPONSE-ALIST.
BODY can access FILE-PATH, OPERATION, and RESPONSE-ALIST as
variables."
  (declare (indent 3) (debug (form form form body)))
  `(progn
     (org-mcp--fail-if-modified ,file-path ,operation)
     (with-temp-buffer
       (set-visited-file-name ,file-path t)
       (insert-file-contents ,file-path)
       (org-mode)
       (goto-char (point-min))
       ,@body
       (org-mcp--complete-and-save ,file-path ,response-alist))))

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
      (dolist (allowed-file org-mcp-allowed-files)
        (unless found-file
          (when (file-exists-p allowed-file)
            (org-mcp--with-org-file allowed-file
              (when (org-find-property "ID" id)
                (setq found-file (expand-file-name allowed-file)))))))
      (or found-file (org-mcp--id-not-found-error id)))))

(defmacro org-mcp--with-uri-prefix-dispatch
    (uri headline-body id-body)
  "Dispatch tool URI handling based on prefix.
URI is the URI string to dispatch on.
HEADLINE-BODY is executed when URI starts with
`org-mcp--uri-headline-prefix', with the URI after the prefix bound
to `headline'.
ID-BODY is executed when URI is an ID-based org:// URI,
with the ID bound to `id'.
Throws an error if URI format is not recognized."
  (declare (indent 1))
  `(cond
    ;; Handle org:// URIs (auto-detect by content)
    ((string-prefix-p "org://" ,uri)
     (let* ((suffix (substring ,uri (length "org://")))
            (parsed-type
             (plist-get (org-mcp--detect-uri-type suffix) :type)))
       (cond
        ((eq parsed-type 'id)
         (let ((id suffix))
           ,id-body))
        ((eq parsed-type 'headline)
         (let ((headline suffix))
           ,headline-body))
        (t
         (org-mcp--tool-validation-error
          "URI does not refer to a headline: %s"
          ,uri)))))
    ;; Handle org-headline:// URIs
    ((string-prefix-p org-mcp--uri-headline-prefix ,uri)
     (let ((headline
            (org-mcp--extract-uri-suffix
             ,uri org-mcp--uri-headline-prefix)))
       ,headline-body))
    (t
     (org-mcp--tool-validation-error "Invalid resource URI format: %s"
                                     ,uri))))

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

(defun org-mcp--extract-children (target-level)
  "Extract children at TARGET-LEVEL until next lower level heading."
  (let ((children '()))
    (save-excursion
      (while (and (re-search-forward "^\\*+ " nil t)
                  (>= (org-current-level) target-level))
        (when (= (org-current-level) target-level)
          (let* ((title (org-get-heading t t t t))
                 (child
                  `((title . ,title)
                    (level . ,target-level)
                    (children . []))))
            (push child children)))))
    (vconcat (nreverse children))))

(defun org-mcp--extract-headings ()
  "Extract heading structure from current org buffer."
  (let ((result '()))
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t) ; Find level 1 headings
      (let* ((title (org-get-heading t t t t))
             ;; Get level 2 children
             (children (org-mcp--extract-children 2))
             (heading
              `((title . ,title) (level . 1) (children . ,children))))
        (push heading result)))
    (vconcat (nreverse result))))

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
Returns a string suitable for use in org-headline:// URIs."
  (let ((components '()))
    (save-excursion
      (push (url-hexify-string (org-get-heading t t t t)) components)
      (while (org-up-heading-safe)
        (push
         (url-hexify-string (org-get-heading t t t t)) components)))
    (mapconcat #'identity components "/")))

(defun org-mcp--split-headline-uri (path-after-protocol)
  "Split PATH-AFTER-PROTOCOL into (file-path . headline-path).
PATH-AFTER-PROTOCOL is the part after `org-headline://'.
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
    (org-mcp--with-uri-prefix-dispatch
        uri
      ;; Handle org-headline:// URIs
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
  (catch 'not-found
    (let ((search-start (point-min))
          (search-end (point-max))
          (current-level 0)
          (found nil)
          (path-index 0))
      (dolist (target-title headline-path)
        (setq found nil)
        (goto-char search-start)
        (while (and (not found)
                    (re-search-forward "^\\*+ " search-end t))
          (let ((title (org-get-heading t t t t))
                (level (org-current-level)))
            (when (and (string= title target-title)
                       (or (= current-level 0)
                           (= level (1+ current-level))))
              (setq found t)
              (setq current-level level)
              ;; Limit search to this subtree for nesting
              (when (< (1+ path-index) (length headline-path))
                (setq search-start (point))
                (setq search-end
                      (save-excursion
                        (org-end-of-subtree t t)
                        (point)))))))
        (unless found
          (throw 'not-found nil))
        (setq path-index (1+ path-index))))
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

(defun org-mcp--extract-heading-child ()
  "Extract lightweight child entry at current heading.
Returns plist with title, todo, level, and uri.
Point should be at the heading. Does not recurse into children."
  (let* ((title (org-get-heading t t t t))
         (todo (org-get-todo-state))
         (level (org-current-level))
         (id (org-entry-get (point) "ID"))
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
  (let* ((title (org-get-heading t t t t))
         (todo (org-get-todo-state))
         (priority (org-entry-get (point) "PRIORITY"))
         (tags (org-get-tags))
         (level (org-current-level))
         (id (org-entry-get (point) "ID"))
         (scheduled (org-entry-get (point) "SCHEDULED"))
         (deadline (org-entry-get (point) "DEADLINE"))
         (closed (org-entry-get (point) "CLOSED"))
         (props (org-entry-properties))
         (uri (org-mcp--build-org-uri-from-position))
         (content-start
          (save-excursion
            (forward-line 1)
            (point)))
         (children '())
         (content-end
          (save-excursion
            (org-end-of-subtree t t)
            (point)))
         ;; Get body content (before children)
         (body-content
          (save-excursion
            (goto-char content-start)
            ;; Skip property drawer if present
            (when (looking-at "^[ \t]*:PROPERTIES:")
              (re-search-forward "^[ \t]*:END:" nil t)
              (forward-line 1))
            ;; Skip LOGBOOK drawer if present
            (when (looking-at "^[ \t]*:LOGBOOK:")
              (re-search-forward "^[ \t]*:END:" nil t)
              (forward-line 1))
            ;; Collect body until next heading
            (let ((body-start (point)))
              (if (re-search-forward "^\*" content-end t)
                  (buffer-substring-no-properties
                   body-start (line-beginning-position))
                (buffer-substring-no-properties
                 body-start content-end)))))
         ;; Extract direct children
         (child-level (1+ level)))
    ;; Collect direct children
    (save-excursion
      (goto-char content-start)
      (while (and (re-search-forward (format "^\\*\\{%d\\} "
                                             child-level)
                                     content-end t)
                  (= (org-current-level) child-level))
        (push (org-mcp--extract-heading-child) children)
        (org-end-of-subtree t t)))
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
TIME is an Emacs time value.  Returns rounded time."
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
  "Format TIME as Org clock timestamp `[YYYY-MM-DD Day HH:MM]'."
  (format-time-string "[%Y-%m-%d %a %H:%M]" time))

(defun org-mcp--clock-parse-timestamp (str)
  "Parse ISO timestamp STR to Emacs time.
STR should be in ISO 8601 format like 2026-03-23T14:30:00."
  (let ((parsed (parse-time-string str)))
    (unless (and (nth 0 parsed)
                 (nth 1 parsed)
                 (nth 2 parsed)
                 (nth 3 parsed)
                 (nth 4 parsed)
                 (nth 5 parsed))
      (org-mcp--tool-validation-error "Cannot parse timestamp: '%s'"
                                      str))
    (encode-time parsed)))

(defun org-mcp--clock-duration-string (seconds)
  "Format SECONDS as clock duration string `H:MM'."
  (let* ((minutes (round seconds 60))
         (hours (/ minutes 60))
         (mins (% minutes 60)))
    (format "%d:%02d" hours mins)))

(defun org-mcp--clock-find-active ()
  "Search allowed files for unclosed CLOCK line.
Returns alist with file, heading, start keys, or nil."
  (or
   (catch 'found
     (dolist (file org-mcp-allowed-files)
       (when (file-exists-p file)
         (org-mcp--with-org-file file
           (while
               (re-search-forward
                "^[ \t]*CLOCK: \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{2,3\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\)\\][ \t]*$"
                nil t)
             (let ((start-str (match-string 1)))
               (save-excursion
                 (org-back-to-heading t)
                 (throw 'found
                        (list
                         (cons 'file (expand-file-name file))
                         (cons 'heading (org-get-heading t t t t))
                         (cons 'start start-str)
                         (cons 'allowed t)))))))))
     nil)
   ;; Fallback: check native Emacs clock marker in non-allowed file
   (when (org-clock-is-active)
     (let* ((buf (org-clock-is-active))
            (file (buffer-file-name buf)))
       (when (and file (not (org-mcp--find-allowed-file file)))
         (with-current-buffer buf
           (save-excursion
             (goto-char org-clock-marker)
             (forward-line 0)
             (when
                 (looking-at
                  "^[ \t]*CLOCK: \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{2,3\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\)\\][ \t]*$")
               (list
                (cons 'file (expand-file-name file))
                (cons 'heading nil)
                (cons 'start (match-string 1))
                (cons 'allowed nil))))))))))

(defun org-mcp--clock-find-last-closed ()
  "Find most recent closed clock end time across allowed files.
Returns Emacs time of the most recent clock end, or nil."
  (let ((latest nil))
    (dolist (file org-mcp-allowed-files)
      (when (file-exists-p file)
        (org-mcp--with-org-file file
          (while
              (re-search-forward
               "^[ \t]*CLOCK: \\[[^]]+\\]--\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{2,3\\} \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\)\\]"
               nil t)
            (let* ((end-str (match-string 1))
                   (end-time (org-parse-time-string end-str)))
              (when (or (not latest)
                        (time-less-p
                         (car latest) (apply #'encode-time end-time)))
                (setq latest
                      (cons
                       (apply #'encode-time end-time) end-str))))))))
    (when latest
      (car latest))))

(defun org-mcp--clock-ensure-logbook ()
  "Create or find LOGBOOK drawer at current heading.
Point must be at a heading.  Inserts LOGBOOK after PROPERTIES
drawer and planning lines if it doesn't exist.
Returns point at start of LOGBOOK content (after :LOGBOOK: line)."
  (org-back-to-heading t)
  (let ((end
         (save-excursion
           (org-end-of-subtree t t)
           (point)))
        (logbook-start nil))
    ;; Look for existing LOGBOOK drawer
    (save-excursion
      (forward-line 1)
      (while
          (and
           (< (point) end)
           (not logbook-start)
           (looking-at
            "^[ \t]*\\(:\\|#\\+\\|SCHEDULED\\|DEADLINE\\|CLOSED\\)"))
        (if (looking-at "^[ \t]*:LOGBOOK:[ \t]*$")
            (progn
              (forward-line 1)
              (setq logbook-start (point)))
          (forward-line 1))))
    (if logbook-start
        (goto-char logbook-start)
      ;; Create LOGBOOK drawer
      (forward-line 1)
      ;; Skip past PROPERTIES drawer
      (when (looking-at "^[ \t]*:PROPERTIES:")
        (re-search-forward "^[ \t]*:END:" end t)
        (forward-line 1))
      ;; Skip past planning lines
      (while (and (< (point) end)
                  (looking-at
                   "^[ \t]*\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):"))
        (forward-line 1))
      (insert ":LOGBOOK:\n:END:\n")
      (forward-line -2)
      (forward-line 1)
      (point))))

(defun org-mcp--clock-insert-entry (start &optional end)
  "Insert CLOCK line in LOGBOOK at current heading.
START is the clock start time.  END is optional clock end time.
If END is provided, inserts a closed clock entry with duration.
New entries are inserted at the top of the LOGBOOK drawer."
  (let ((logbook-pos (org-mcp--clock-ensure-logbook)))
    (goto-char logbook-pos)
    (if end
        (let* ((duration (float-time (time-subtract end start)))
               (dur-str (org-mcp--clock-duration-string duration)))
          (insert
           (format "CLOCK: %s--%s => %s\n"
                   (org-mcp--clock-format-timestamp start)
                   (org-mcp--clock-format-timestamp end)
                   dur-str)))
      (insert
       (format "CLOCK: %s\n"
               (org-mcp--clock-format-timestamp start))))))

(defun org-mcp--clock-resolve-dangling ()
  "Delete unclosed CLOCK lines in LOGBOOK at current heading.
Point must be at a heading.  Returns count of deleted lines."
  (let ((count 0)
        (end
         (save-excursion
           (org-end-of-subtree t t)
           (point))))
    (save-excursion
      (forward-line 1)
      (while (re-search-forward "^[ \t]*CLOCK: \\[[^]]+\\][ \t]*$"
                                end
                                t)
        (forward-line 0)
        (let ((line-end (line-beginning-position 2)))
          (delete-region (point) line-end)
          (setq end (- end (- line-end (point))))
          (cl-incf count))))
    count))

(defun org-mcp--clock-remove-empty-logbook ()
  "Remove LOGBOOK drawer at current heading if it is empty.
Point must be at a heading."
  (save-excursion
    (org-back-to-heading t)
    (let ((end
           (save-excursion
             (org-end-of-subtree t t)
             (point))))
      (forward-line 1)
      (when (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" end t)
        (forward-line 0)
        (let ((drawer-start (point)))
          (forward-line 1)
          (when (looking-at "^[ \t]*:END:[ \t]*$")
            (forward-line 1)
            (delete-region drawer-start (point))))))))

(defun org-mcp--clock-start-time-regex (start-time)
  "Build regex matching an Org clock timestamp for START-TIME.
Matches the date and time exactly but allows any day-of-week
abbreviation, making the match locale-independent."
  (format-time-string "\\[%Y-%m-%d [A-Za-z]\\{2,3\\} %H:%M\\]"
                      start-time))

(defun org-mcp--clock-delete-entry (start-time)
  "Delete CLOCK line matching START-TIME in LOGBOOK at current heading.
Point must be at a heading.  START-TIME is an Emacs time value.
Removes LOGBOOK drawer if it becomes empty.
Returns an alist with deleted entry info, or nil if not found."
  (let* ((start-regex (org-mcp--clock-start-time-regex start-time))
         (end
          (save-excursion
            (org-end-of-subtree t t)
            (point)))
         (clock-regex
          (concat
           "^[ \t]*CLOCK: "
           start-regex
           "\\(?:--\\(\\[[^]]+\\]\\)"
           " => +\\([0-9]+:[0-9]+\\)\\)?[ \t]*$"))
         (found nil))
    (save-excursion
      (forward-line 1)
      (when (re-search-forward clock-regex end t)
        (let ((end-str (match-string 1))
              (duration-str (match-string 2)))
          (setq found
                `((start
                   .
                   ,(org-mcp--clock-format-timestamp start-time))
                  ,@
                  (when end-str
                    `((end . ,end-str)))
                  ,@
                  (when duration-str
                    `((duration . ,duration-str)))))
          (forward-line 0)
          (delete-region (point) (line-beginning-position 2)))))
    (when found
      (org-mcp--clock-remove-empty-logbook))
    found))

(defun org-mcp--validate-todo-state (state)
  "Validate STATE is a valid TODO keyword."
  (let ((valid-states
         (delete
          "|"
          (org-remove-keyword-keys
           (apply #'append (mapcar #'cdr org-todo-keywords))))))
    (unless (member state valid-states)
      (org-mcp--tool-validation-error
       "Invalid TODO state: '%s' - valid states: %s"
       state (mapconcat #'identity valid-states ", ")))))

(defun org-mcp--validate-and-normalize-tags (tags)
  "Validate and normalize TAGS.
TAGS can be a single tag string or list of tag strings.
Returns normalized tag list.
Validates:
- Tag names follow Org rules (alphanumeric, underscore, at-sign)
- Tags are in configured tag alist (if configured)
- Tags don't violate mutual exclusivity groups
Signals error for invalid tags."
  (let ((tag-list (org-mcp--normalize-tags-to-list tags))
        (allowed-tags
         (append
          (mapcar
           #'org-mcp--extract-tag-from-alist-entry org-tag-alist)
          (mapcar
           #'org-mcp--extract-tag-from-alist-entry
           org-tag-persistent-alist))))
    ;; Remove special keywords like :startgroup
    (setq allowed-tags
          (cl-remove-if
           #'org-mcp--is-tag-group-keyword-p allowed-tags))
    ;; If tag alists are configured, validate against them
    (when allowed-tags
      (dolist (tag tag-list)
        (unless (member tag allowed-tags)
          (org-mcp--tool-validation-error
           "Tag not in configured tag alist: %s"
           tag))))
    ;; Always validate tag names follow Org's rules
    (dolist (tag tag-list)
      (unless (string-match "^[[:alnum:]_@]+$" tag)
        (org-mcp--tool-validation-error
         "Invalid tag name (must be alphanumeric, _, or @): %s"
         tag)))
    ;; Validate mutual exclusivity if tag-alist is configured
    (when org-tag-alist
      (org-mcp--validate-mutex-tag-groups tag-list org-tag-alist))
    (when org-tag-persistent-alist
      (org-mcp--validate-mutex-tag-groups
       tag-list org-tag-persistent-alist))
    tag-list))

(defun org-mcp--extract-tag-from-alist-entry (entry)
  "Extract tag name from an `org-tag-alist' ENTRY.
ENTRY can be a string or a cons cell (tag . key)."
  (if (consp entry)
      (car entry)
    entry))

(defun org-mcp--is-tag-group-keyword-p (tag)
  "Check if symbol TAG is a special keyword like :startgroup."
  (and (symbolp tag) (string-match "^:" (symbol-name tag))))

(defun org-mcp--parse-mutex-tag-groups (tag-alist)
  "Parse mutually exclusive tag groups from TAG-ALIST.
Returns a list of lists, where each inner list contains tags
that are mutually exclusive with each other."
  (let ((groups '())
        (current-group nil)
        (in-group nil))
    (dolist (entry tag-alist)
      (cond
       ;; Start of a mutex group
       ((eq entry :startgroup)
        (setq in-group t)
        (setq current-group '()))
       ;; End of a mutex group
       ((eq entry :endgroup)
        (when (and in-group current-group)
          (push current-group groups))
        (setq in-group nil)
        (setq current-group nil))
       ;; Inside a group - collect tags
       (in-group
        (let ((tag (org-mcp--extract-tag-from-alist-entry entry)))
          (when (and tag (not (org-mcp--is-tag-group-keyword-p tag)))
            (push tag current-group))))))
    groups))

(defun org-mcp--validate-mutex-tag-groups (tags tag-alist)
  "Validate that TAGS don't violate mutex groups in TAG-ALIST.
TAGS is a list of tag strings.
Errors if multiple tags from same mutex group."
  (let ((mutex-groups (org-mcp--parse-mutex-tag-groups tag-alist)))
    (dolist (group mutex-groups)
      (let ((tags-in-group
             (cl-intersection tags group :test #'string=)))
        (when (> (length tags-in-group) 1)
          (org-mcp--tool-validation-error
           "Tags %s are mutually exclusive (cannot use together)"
           (mapconcat (lambda (tag) (format "'%s'" tag)) tags-in-group
                      ", ")))))))

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
Uses a state machine: tracks if we're in a block, and which one.
Text inside blocks is literal and doesn't start/end other blocks.
Throws an MCP tool error if unbalanced blocks are found."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (let
        ((current-block nil)) ; Current block type or nil
      ;; Scan forward for all block markers
      ;; Block names can be any non-whitespace chars
      (while (re-search-forward
              "^#\\+\\(BEGIN\\|END\\|begin\\|end\\)_\\(\\S-+\\)"
              nil t)
        (let ((marker-type (upcase (match-string 1)))
              (block-type (upcase (match-string 2))))
          (cond
           ;; Found BEGIN
           ((string= marker-type "BEGIN")
            (if current-block
                ;; Already in block - BEGIN is literal
                nil
              ;; Not in a block - enter this block
              (setq current-block block-type)))
           ;; Found END
           ((string= marker-type "END")
            (cond
             ;; Not in any block - this END is orphaned
             ((null current-block)
              (org-mcp--tool-validation-error
               "Orphaned END_%s without BEGIN_%s"
               block-type block-type))
             ;; In matching block - exit the block
             ((string= current-block block-type)
              (setq current-block nil))
             ;; In different block - this END is just literal text
             (t
              nil))))))
      ;; After scanning, check if we're still in a block
      (when current-block
        (org-mcp--tool-validation-error
         "Body contains unclosed %s block"
         current-block)))))

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
             "Field after_uri must be an ID-based URI (org://{uuid}): %s"
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
      ;; We're inside a parent
      (progn
        (org-mcp--ensure-newline)
        ;; Insert heading manually at parent level + 1
        ;; We don't use `org-insert-heading' because when parent has
        ;; no children, it creates a sibling of the parent instead of
        ;; a child
        (let ((heading-start (point)))
          (insert (make-string (1+ parent-level) ?*) " " title "\n")
          ;; Set point to heading for `org-todo' and `org-set-tags'
          (goto-char heading-start)
          (end-of-line)))
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
    (old-body new-body body-content replace-all body-begin body-end)
  "Replace body content in the current buffer.
OLD-BODY is the substring to replace.
NEW-BODY is the replacement text.
BODY-CONTENT is the current body content string.
REPLACE-ALL if non-nil, replace all occurrences.
BODY-BEGIN is the buffer position where body starts.
BODY-END is the buffer position where body ends."
  (let ((new-body-content
         (cond
          ;; Special case: empty oldBody with empty body
          ((and (string= old-body "")
                (string-match-p "\\`[[:space:]]*\\'" body-content))
           new-body)
          ;; Normal replacement with replaceAll
          (replace-all
           (replace-regexp-in-string
            (regexp-quote old-body) new-body body-content
            t t))
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
  "Return the TODO keyword configuration."
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

(defun org-mcp--tool-get-priority-config ()
  "Return the priority configuration."
  (json-encode
   `((highest . ,(char-to-string org-priority-highest))
     (lowest . ,(char-to-string org-priority-lowest))
     (default . ,(char-to-string org-priority-default)))))

(defun org-mcp--tool-get-allowed-files ()
  "Return the list of allowed Org files."
  (json-encode `((files . ,(vconcat org-mcp-allowed-files)))))

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
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
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
    (org-mcp--validate-todo-state new_state)
    (org-mcp--modify-and-save file-path "update"
                              `((previous_state . ,actual-prev)
                                (new_state . ,new_state))
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

      ;; Update the state
      (org-todo new_state)

      ;; Add note to state transition if provided
      (when (and note (not (string-empty-p (string-trim note))))
        ;; Discard any interactive log hook org-todo may have set up
        (remove-hook 'post-command-hook 'org-add-log-note)
        ;; Set up state-type note at current headline position
        (org-add-log-setup 'state new_state actual-prev 'note)
        ;; Remove hook again; we handle the note ourselves
        (remove-hook 'post-command-hook 'org-add-log-note)
        ;; Initialize the variables that org-add-log-note (the interactive
        ;; hook we bypassed) would normally set before org-store-log-note
        (move-marker org-log-note-return-to (point))
        (setq org-log-note-window-configuration
              (current-window-configuration))
        ;; Store note text and insert into buffer via marker
        (save-current-buffer
          (set-buffer (get-buffer-create "*Org Note*"))
          (erase-buffer)
          (insert note)
          (org-store-log-note))))))

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
                 - org-headline://{absolute-path}#{headline-path}
                 - org://{id}
  tags - Tags to add (optional, single string or array of strings)
  after_uri - Sibling to insert after (optional)
              Formats:
                - org-headline://{absolute-path}#{headline-path}
                - org://{id}"
  (org-mcp--validate-headline-title title)
  (org-mcp--validate-todo-state todo_state)
  (let* ((tag-list (org-mcp--validate-and-normalize-tags tags))
         file-path
         parent-path
         parent-id)

    ;; Parse parent URI once to extract file-path and parent location
    (org-mcp--with-uri-prefix-dispatch
        parent_uri
      ;; Handle org-headline:// URIs
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
  "Handler for org-headline://{uri} template with auto-detection.
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
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
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
    (resource_uri old_body new_body replace_all)
  "Edit body content of an Org node using partial string replacement.
RESOURCE_URI is the URI of the node to edit.
OLD_BODY is the substring to search for within the node's body.
         Use empty string \"\" to add content to an empty node.
NEW_BODY is the replacement text.
REPLACE_ALL if non-nil, replace all occurrences.

MCP Parameters:
  resource_uri - URI of the node
                 Formats:
                   - org-headline://{absolute-path}#{headline-path}
                   - org://{id}
  old_body - Substring to replace within the body (must be unique
             unless replace_all).  Use \"\" to add to empty nodes
  new_body - Replacement text
  replace_all - Replace all occurrences (optional, default false)"
  ;; Normalize JSON false to nil for proper boolean handling
  ;; JSON false can arrive as :false (keyword) or "false" (string)
  (let ((replace_all
         (cond
          ((eq replace_all :false)
           nil)
          ((equal replace_all "false")
           nil)
          (t
           replace_all))))
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
                (buffer-substring-no-properties body-begin body-end))

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
            (org-mcp--tool-validation-error "Body text not found: %s"
                                            old_body))
           ((and (> occurrence-count 1) (not replace_all))
            (org-mcp--tool-validation-error
             (concat "Text appears %d times (use replace_all)")
             occurrence-count)))

          ;; Perform replacement
          (org-mcp--replace-body-content
           old_body
           new_body
           body-content
           replace_all
           body-begin
           body-end))))))

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
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
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
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
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
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
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
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
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
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
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

(defun org-mcp--tool-append-body (uri content)
  "Append CONTENT to the body of headline at URI.
Inserts after existing body content but before any child headlines.

MCP Parameters:
  uri - URI of the headline
        Formats:
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
  content - Text to append (string, required)
            Cannot be empty or whitespace-only
            Cannot contain headlines at same or higher level
            Must have balanced #+BEGIN/#+END blocks"
  (when (or (null content)
            (string-empty-p content)
            (string-match-p "\\`[[:space:]]*\\'" content))
    (org-mcp--tool-validation-error
     "Content cannot be empty or whitespace-only"))

  (org-mcp--validate-body-no-unbalanced-blocks content)

  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed)))

    (org-mcp--modify-and-save file-path "append body" nil
      (org-mcp--goto-headline-from-uri
       headline-path (org-mcp--uri-is-id-based uri))

      (org-mcp--validate-body-no-headlines
       content (org-current-level))

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

          (insert content)

          ;; Ensure trailing newline
          (unless (= (char-before (point)) ?\n)
            (insert "\n")))

        ;; Return to heading for org-id-get-create
        (goto-char heading-pos)))))

(defun org-mcp--tool-add-logbook-note (uri note)
  "Add a timestamped note to the LOGBOOK drawer of headline at URI.

MCP Parameters:
  uri - URI of the headline
        Formats:
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
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

      (let ((logbook-pos (org-mcp--clock-ensure-logbook))
            (timestamp
             (format-time-string (org-time-stamp-format t t))))
        (goto-char logbook-pos)
        ;; Format note lines: first line after timestamp, rest indented
        (let* ((lines (split-string note "\n"))
               (first-line (car lines))
               (rest-lines (cdr lines)))
          (insert (format "- Note taken on %s \\\\\n" timestamp))
          (insert (format "  %s\n" first-line))
          (dolist (line rest-lines)
            (insert (format "  %s\n" line))))))))

;; org-ql integration

(defun org-mcp--ql-extract-match ()
  "Extract match data at point for `org-ql-select' :action.
Returns an alist with headline metadata suitable for JSON encoding."
  (let* ((title (org-get-heading t t t t))
         (level (org-current-level))
         (file (buffer-file-name))
         (todo (org-get-todo-state))
         (priority
          (org-element-property :priority (org-element-at-point)))
         (tags (org-get-tags nil t))
         (id (org-entry-get nil "ID"))
         (uri (org-mcp--build-org-uri-from-position))
         (parent-priority
          (save-excursion
            (when (org-up-heading-safe)
              (org-element-property
               :priority (org-element-at-point)))))
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
      (push `(priority . ,(char-to-string priority)) result))
    (when parent-priority
      (push
       `(parent-priority . ,(char-to-string parent-priority)) result))
    (when tags
      (push `(tags . ,(vconcat tags)) result))
    (when id
      (push `(id . ,id) result))
    (push `(uri . ,uri) result)
    (when props
      (let ((props-alist
             (mapcar (lambda (p) (cons (car p) (cdr p))) props)))
        (push `(properties . ,props-alist) result)))
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
    (let ((target-files
           (if files
               (mapcar
                (lambda (f)
                  (or (org-mcp--find-allowed-file f)
                      (org-mcp--tool-file-access-error f)))
                (append files nil))
             (cl-remove-if-not
              #'file-exists-p org-mcp-allowed-files))))
      (let* ((action #'org-mcp--ql-extract-match)
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

;; Stored org-ql queries

(defun org-mcp--stored-queries-ensure-loaded ()
  "Ensure stored queries are loaded from disk.
Signals an error if `org-mcp-stored-queries-file' is nil."
  (unless org-mcp-stored-queries-file
    (org-mcp--tool-validation-error
     "Stored queries not configured: %s is nil"
     "org-mcp-stored-queries-file"))
  (when (eq org-mcp--stored-queries 'unloaded)
    (if (file-exists-p org-mcp-stored-queries-file)
        (progn
          (load org-mcp-stored-queries-file nil t t)
          ;; If file didn't set the variable, initialize to nil
          (when (eq org-mcp--stored-queries 'unloaded)
            (setq org-mcp--stored-queries nil)))
      (setq org-mcp--stored-queries nil))))

(defun org-mcp--stored-queries-save ()
  "Save stored queries to disk as pretty-printed Elisp."
  (with-temp-file org-mcp-stored-queries-file
    (insert
     ";;; org-mcp-stored-queries.el --- "
     "Stored org-ql queries "
     "-*- lexical-binding: t -*-\n"
     ";; Auto-generated by org-mcp. Do not edit by hand.\n\n"
     "(setq org-mcp--stored-queries\n"
     "      '"
     (pp-to-string org-mcp--stored-queries)
     ")\n"))
  ;; Refresh any buffer visiting this file
  (let ((buf (find-buffer-visiting org-mcp-stored-queries-file)))
    (when buf
      (with-current-buffer buf
        (revert-buffer t t t)))))

(defun org-mcp--validate-stored-query-key (key)
  "Validate KEY for stored queries.
Must be a non-empty string matching [a-zA-Z0-9_-]+."
  (when (or (not (stringp key)) (string-empty-p key))
    (org-mcp--tool-validation-error "Key must be a non-empty string"))
  (unless (string-match-p "\\`[a-zA-Z0-9_-]+\\'" key)
    (org-mcp--tool-validation-error
     "Key must contain only alphanumeric characters, %s"
     "hyphens, and underscores")))

(defun org-mcp--tool-ql-list-stored-queries ()
  "List all stored org-ql queries.

MCP Parameters: none"
  (org-mcp--stored-queries-ensure-loaded)
  (let ((queries
         (mapcar
          (lambda (entry)
            (let ((key (car entry))
                  (data (cdr entry)))
              `((key . ,key)
                (query . ,(alist-get 'query data))
                (description . ,(alist-get 'description data)))))
          org-mcp--stored-queries)))
    (json-encode
     `((queries . ,(vconcat queries)) (total . ,(length queries))))))

(defun org-mcp--tool-ql-save-stored-query
    (key query &optional description)
  "Save a named org-ql query.
KEY is the query identifier.
QUERY is the org-ql query sexp as a string.
DESCRIPTION is an optional human-readable description.

MCP Parameters:
  key - Query identifier (string, required)
  query - org-ql query sexp as string (string, required)
  description - Human-readable description (string, optional)"
  (org-mcp--validate-stored-query-key key)
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
                                      (type-of query-sexp))))
  (org-mcp--stored-queries-ensure-loaded)
  (let* ((existing (assoc key org-mcp--stored-queries))
         (action
          (if existing
              "updated"
            "created"))
         (data
          `((query . ,query) (description . ,(or description "")))))
    (if existing
        (setcdr existing data)
      (setq org-mcp--stored-queries
            (append org-mcp--stored-queries (list (cons key data)))))
    (org-mcp--stored-queries-save)
    (json-encode `((success . t) (action . ,action) (key . ,key)))))

(defun org-mcp--tool-ql-delete-stored-query (key)
  "Delete a stored org-ql query by KEY.

MCP Parameters:
  key - Query identifier to delete (string, required)"
  (org-mcp--validate-stored-query-key key)
  (org-mcp--stored-queries-ensure-loaded)
  (unless (assoc key org-mcp--stored-queries)
    (org-mcp--tool-validation-error "Stored query not found: %s" key))
  (setq org-mcp--stored-queries
        (assoc-delete-all key org-mcp--stored-queries))
  (org-mcp--stored-queries-save)
  (json-encode `((success . t) (key . ,key))))

(defun org-mcp--tool-ql-run-stored-query (key &optional files)
  "Run a stored org-ql query by KEY.
FILES is an optional list of file paths to search.

MCP Parameters:
  key - Query identifier to run (string, required)
  files - Subset of allowed files to search
          (array of strings, optional)"
  (org-mcp--validate-stored-query-key key)
  (org-mcp--stored-queries-ensure-loaded)
  (let ((entry (assoc key org-mcp--stored-queries)))
    (unless entry
      (org-mcp--tool-validation-error "Stored query not found: %s"
                                      key))
    (org-mcp--tool-ql-query (alist-get 'query (cdr entry)) files)))

;; Tools duplicating resource templates

(defun org-mcp--tool-read (uri)
  "Tool wrapper for org://{uri} resource template.
URI can be a file path, file#headline-path, or UUID.
Returns structured JSON.

MCP Parameters:
  uri - URI string. Formats:
        - /path/to/file.org (file path)
        - /path/to/file.org#Headline/Subhead (headline path)
        - UUID string (8-4-4-4-12 format)"
  (org-mcp--handle-org-resource `(("uri" . ,uri))))

(defun org-mcp--tool-read-outline (file)
  "Tool wrapper for org-outline://{filename} resource template.
FILE is the absolute path to an Org file.

MCP Parameters:
  file - Absolute path to an Org file"
  (org-mcp--handle-outline-resource `(("filename" . ,file))))

(defun org-mcp--tool-read-headline (uri)
  "Tool wrapper for org-headline://{uri} resource template.
URI can be a file path, file#headline-path, or UUID.
Returns plain text content.

MCP Parameters:
  uri - URI string. Formats:
        - /path/to/file.org (returns entire file)
        - /path/to/file.org#Headline/Subhead (headline path)
        - UUID string (8-4-4-4-12 format)
        Headline paths use URL encoding for special chars."
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

MCP Parameters: None"
  (let ((all-clocks nil))
    (dolist (file org-mcp-allowed-files)
      (when (file-exists-p file)
        (let ((open (org-find-open-clocks file)))
          (dolist (clock open)
            (let* ((marker (car clock))
                   (clock-file (expand-file-name file))
                   (start-str
                    (with-current-buffer (marker-buffer marker)
                      (save-excursion
                        (goto-char marker)
                        (forward-line 0)
                        (when (looking-at
                               "[ \t]*CLOCK: \\[\\([^]]+\\)\\]")
                          (match-string 1)))))
                   (heading
                    (with-current-buffer (marker-buffer marker)
                      (save-excursion
                        (goto-char marker)
                        (org-back-to-heading t)
                        (org-get-heading t t t t)))))
              (push `((file . ,clock-file)
                      (heading . ,heading)
                      (start . ,start-str))
                    all-clocks))))))
    (json-encode
     `((open_clocks . ,(vconcat (nreverse all-clocks)))
       (total . ,(length all-clocks))))))

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
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
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
         (active (org-mcp--clock-find-active))
         (close-time nil))
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
             (start-str (alist-get 'start active))
             (start-parsed (org-parse-time-string start-str))
             (start-time (apply #'encode-time start-parsed))
             (duration
              (float-time (time-subtract close-at start-time)))
             (close-text
              (format "--%s => %s"
                      (org-mcp--clock-format-timestamp close-at)
                      (org-mcp--clock-duration-string duration))))
        (setq close-time close-at)
        (if (alist-get 'allowed active)
            ;; Allowed file: close via text replacement
            (progn
              (org-mcp--fail-if-modified active-file "clock-in")
              (with-temp-buffer
                (set-visited-file-name active-file t)
                (insert-file-contents active-file)
                (goto-char (point-min))
                (when (re-search-forward (concat
                                          "^\\([ \t]*CLOCK: \\["
                                          (regexp-quote
                                           start-str)
                                          "\\]\\)[ \t]*$")
                                         nil t)
                  (goto-char (match-end 1))
                  (insert close-text))
                (save-buffer)
                (org-mcp--refresh-file-buffers active-file)))
          ;; Non-allowed file: close via buffer edit and clear markers
          (let ((buf (marker-buffer org-clock-marker)))
            (when buf
              (with-current-buffer buf
                (save-excursion
                  (goto-char org-clock-marker)
                  (forward-line 0)
                  (when (looking-at
                         "^\\([ \t]*CLOCK: \\[[^]]+\\]\\)[ \t]*$")
                    (goto-char (match-end 1))
                    (insert close-text)
                    (save-buffer))))
              (move-marker org-clock-marker nil)
              (move-marker org-clock-hd-marker nil))))))
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
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
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
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
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
          - org-headline://{absolute-path}#{headline-path}
          - org://{id}
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
  - URI Construction: I need to build an org-headline:// URI - what's
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
          - org-headline://{absolute-path}#{url-encoded-path}
          - org://{uuid}
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
  parent_uri - Parent location (string, required)
               For top-level: org-headline://{absolute-path}
               For child: org-headline://{path}#{parent-path}
                         or org://{parent-uuid}
  after_uri - Sibling to insert after (string, optional)
              Must be org://{uuid} format
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
          - org-headline://{absolute-path}#{url-encoded-path}
          - org://{uuid}
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
   "Edit the body content of an Org headline using partial string
replacement.  Finds and replaces a substring within the headline's
body text.  Creates an Org ID property for the headline if one doesn't
exist.

Parameters:
  resource_uri - URI of the headline to edit (string, required)
                 Formats:
                   - org-headline://{absolute-path}#{url-encoded-path}
                   - org://{uuid}
  old_body - Substring to find and replace (string, required)
             Must appear exactly once unless replace_all is true
             Use empty string \"\" only for adding to empty nodes
  new_body - Replacement text (string, required)
             Cannot introduce headlines at same or higher level
             Must maintain balanced #+BEGIN/#+END blocks
  replace_all - Replace all occurrences (boolean, optional, default
                false). When false, old_body must be unique in the
                body.

Returns JSON object:
  success - Always true on success (boolean)
  uri - org:// URI (org://{uuid}) for the edited headline

Special behavior - Empty old_body:
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
          - org-headline://{absolute-path}#{url-encoded-path}
          - org://{uuid}
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
          - org-headline://{absolute-path}#{url-encoded-path}
          - org://{uuid}
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
          - org-headline://{absolute-path}#{url-encoded-path}
          - org://{uuid}
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
          - org-headline://{absolute-path}#{url-encoded-path}
          - org://{uuid}
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
          - org-headline://{absolute-path}#{url-encoded-path}
          - org://{uuid}
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
   #'org-mcp--tool-append-body
   :id "org-append-body"
   :description
   "Append content to the body of an Org headline.  Inserts after
existing body content but before any child headlines.  Creates an
Org ID for the headline if one doesn't exist.

Parameters:
  uri - URI of the headline (string, required)
        Formats:
          - org-headline://{absolute-path}#{url-encoded-path}
          - org://{uuid}
  content - Text to append (string, required)
            Cannot be empty or whitespace-only
            Cannot introduce headlines at same or higher level
            Must maintain balanced #+BEGIN/#+END blocks

Returns JSON object:
  success - Always true on success (boolean)
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
          - org-headline://{absolute-path}#{url-encoded-path}
          - org://{uuid}
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
  uri - URI string (string, required). Formats:
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
  uri - URI string (string, required). Formats:
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

  (mcp-server-lib-register-tool
   #'org-mcp--tool-ql-list-stored-queries
   :id "org-ql-list-stored-queries"
   :description
   "List all stored org-ql queries.  Returns named queries that have
been saved for reuse across sessions.

Parameters: none

Returns JSON object:
  queries - Array of stored queries, each with:
    key - Query identifier (string)
    query - org-ql query sexp (string)
    description - Human-readable description (string)
  total - Number of stored queries (number)"
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-ql-save-stored-query
   :id "org-ql-save-stored-query"
   :description
   "Save a named org-ql query for reuse.  Creates a new stored query
or updates an existing one with the same key.

Parameters:
  key - Query identifier, alphanumeric with hyphens/underscores
        (string, required)
  query - org-ql query sexp as string (string, required)
          Examples:
            (todo \"TODO\")
            (and (todo \"TODO\") (priority \"A\"))
  description - Human-readable description (string, optional)

Returns JSON object:
  success - Always true on success (boolean)
  action - \"created\" or \"updated\" (string)
  key - The query key (string)"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-ql-delete-stored-query
   :id "org-ql-delete-stored-query"
   :description
   "Delete a stored org-ql query by key.

Parameters:
  key - Query identifier to delete (string, required)

Returns JSON object:
  success - Always true on success (boolean)
  key - The deleted query key (string)"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-ql-run-stored-query
   :id "org-ql-run-stored-query"
   :description
   "Run a previously saved org-ql query by key.  Looks up the stored
query and executes it against Org files.

Parameters:
  key - Query identifier to run (string, required)
  files - Subset of allowed files to search
          (array of strings, optional)
          Defaults to all org-mcp-allowed-files

Returns: Same as org-ql-query tool"
   :read-only t
   :server-id org-mcp--server-id)

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
          - org-headline://{absolute-path}#{headline-path}
          - org://{uuid}
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
          - org-headline://{absolute-path}#{headline-path}
          - org://{uuid}
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
          - org-headline://{absolute-path}#{headline-path}
          - org://{uuid}
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
          - org-headline://{absolute-path}#{headline-path}
          - org://{uuid}
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
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-resource
   "org-headline://{uri}" #'org-mcp--handle-headline-resource
   :name "Org headline or file (plain text)"
   :description
   "Access Org headline or file as plain text. Auto-detects URI format.
Returns complete subtree with all children.

URI format: org-headline://{uri}
  uri - Can be one of:
    - /path/to/file.org - returns entire file
    - /path/to/file.org#Headline/Subhead - headline path
    - UUID (8-4-4-4-12 format) - ID-based lookup
    Headline paths use URL encoding for special chars.

Returns: Plain text content including:
  - The headline itself with TODO state and tags
  - All properties drawer content
  - Body text
  - All nested subheadings (complete subtree)

Example URIs:
  org-headline:///home/user/tasks.org
    → Entire file

  org-headline:///home/user/tasks.org#Project%20Alpha
    → Top-level \"Project Alpha\" heading

  org-headline:///home/user/tasks.org#Project%20Alpha/Planning
    → \"Planning\" subheading under \"Project Alpha\"

  org-headline://550e8400-e29b-41d4-a716-446655440000
    → Headline with that ID property

Use this resource to:
  - Read specific sections of an Org file
  - Access headline content by hierarchical path or ID
  - Get complete subtree including all children"
   :mime-type "text/plain"
   :server-id org-mcp--server-id))

(defun org-mcp-disable ()
  "Disable the org-mcp server."
  (mcp-server-lib-unregister-tool
   "org-get-todo-config" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-get-tag-config" org-mcp--server-id)
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
   "org-append-body" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-add-logbook-note" org-mcp--server-id)
  ;; Unregister workaround tools
  (mcp-server-lib-unregister-tool "org-read" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-read-outline" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-read-headline" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-ql-query" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-ql-list-stored-queries" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-ql-save-stored-query" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-ql-delete-stored-query" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-ql-run-stored-query" org-mcp--server-id)
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
  (setq org-mcp--stored-queries 'unloaded)
  ;; Unregister template resources
  (mcp-server-lib-unregister-resource
   "org://{uri}" org-mcp--server-id)
  (mcp-server-lib-unregister-resource
   "org-outline://{filename}" org-mcp--server-id)
  (mcp-server-lib-unregister-resource
   "org-headline://{uri}" org-mcp--server-id))

(provide 'org-mcp)
;;; org-mcp.el ends here
