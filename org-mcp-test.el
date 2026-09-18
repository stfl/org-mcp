;;; org-mcp-test.el --- Tests for org-mcp -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for org-mcp package.

;;; Code:

(require 'ert)
(require 'org-mcp)
(require 'mcp-server-lib-commands)
(require 'mcp-server-lib-ert)
(require 'json)

(setq mcp-server-lib-ert-server-id "org-mcp")

;;; Test Data Constants

;; Initial content strings for various test scenarios

(defconst org-mcp-test--content-empty ""
  "Empty org file content.")

(defconst org-mcp-test--content-with-id-id
  "550e8400-e29b-41d4-a716-446655440000"
  "ID value for org-mcp-test--content-with-id.")

(defconst org-mcp-test--content-with-id-uri
  org-mcp-test--content-with-id-id
  "Bare URI form for org-mcp-test--content-with-id (tool input).")

(defconst org-mcp-test--content-with-id-resource-uri
  (format "org://%s" org-mcp-test--content-with-id-id)
  "Resource URI form for org-mcp-test--content-with-id (tool output).")

(defconst org-mcp-test--content-nested-siblings-parent-id
  "nested-siblings-parent-id-002"
  "ID for Parent Task in org-mcp-test--content-nested-siblings.")

(defconst org-mcp-test--content-nested-siblings
  (format
   "#+TITLE: My Org Document

* Parent Task
:PROPERTIES:
:ID:       %s
:END:
Some parent content.
** First Child 50%% Complete
First child content.
It spans multiple lines.
** Second Child
:PROPERTIES:
:ID:       %s
:END:
Second child content.
** Third Child #3"
   org-mcp-test--content-nested-siblings-parent-id
   org-mcp-test--content-with-id-id)
  "Parent with multiple child tasks and doc file header.")

(defconst org-mcp-test--childless-parent-id
  "childless-parent-id-003"
  "ID for the parent in org-mcp-test--content-childless-parent.")

(defconst org-mcp-test--content-childless-parent
  (format
   "* Parent Task
:PROPERTIES:
:ID:       %s
:END:
Some parent content."
   org-mcp-test--childless-parent-id)
  "Top-level parent with body but no child headings.")

(defconst org-mcp-test--level2-parent-level3-sibling-id
  "level2-parent-level3-sibling-id-001"
  "ID for Review org-mcp.el in level2-parent-level3-children.")

(defconst org-mcp-test--content-level2-parent-level3-children
  (format
   "* Top Level
** Review the package
*** Review org-mcp.el
:PROPERTIES:
:ID:       %s
:END:
Main package file"
   org-mcp-test--level2-parent-level3-sibling-id)
  "Level 2 parent with level 3 children - matches emacs.org structure.")

(defconst org-mcp-test--content-simple-todo
  "* TODO Original Task
First line of body.
Second line of body.
Third line of body."
  "Simple TODO task with three-line body.")

(defconst org-mcp-test--content-with-id-todo
  (format
   "* TODO Task with ID
:PROPERTIES:
:ID:       %s
:END:
First line of content.
Second line of content.
Third line of content."
   org-mcp-test--content-with-id-id)
  "Task with an Org ID property, TODO state, and multiline content.")


(defconst org-mcp-test--timestamp-id "20240101T120000"
  "Timestamp-format ID value.")

(defconst org-mcp-test--content-timestamp-id
  (format
   "* TODO Task with timestamp ID
:PROPERTIES:
:ID:       %s
:END:
Task content."
   org-mcp-test--timestamp-id)
  "Task with a timestamp-format ID property.")

(defconst org-mcp-test--content-with-id-no-body
  (format
   "* TODO Task with ID but no body
:PROPERTIES:
:ID:       %s
:END:"
   org-mcp-test--timestamp-id)
  "Task with an ID property but no body content.")

(defconst org-mcp-test--body-text-multiline
  (concat
   "This is the body text.\n"
   "It has multiple lines.\n"
   "With some content.")
  "Multi-line body text for testing TODO items with content.")

(defconst org-mcp-test--other-child-id "A1B2C3D4-E5F6-7890-ABCD-EF1234567890"
  "ID value for Other Child in afterUri-not-sibling test.")

(defconst org-mcp-test--content-wrong-levels
  (format
   "* First Parent
Some content in first parent.
* Second Parent
** Other Child
:PROPERTIES:
:ID:       %s
:END:
*** Target Headline
This should NOT be found via First Parent/Target Headline path.
* Third Parent
** Target Headline
This is actually a child of Third Parent, not First Parent!"
   org-mcp-test--other-child-id)
  "Test content with same headline names at different levels.")

(defconst org-mcp-test--content-todo-with-tags
  "* TODO Task with Tags :work:urgent:\nTask description."
  "TODO task with tags and body.")

(defconst org-mcp-test--content-slash-not-nested-before
  "* Parent
** Real Child
Content here.
* Parent/Child
This is a single headline with a slash, not nested under Parent."
  "Content with Parent having a child and separate Parent/Child headline.")

(defconst org-mcp-test--content-with-id-repeated-text
  "* Test Heading
:PROPERTIES:
:ID: test-id
:END:
First occurrence of pattern.
Some other text.
Second occurrence of pattern.
More text.
Third occurrence of pattern."
  "Heading with ID and repeated text patterns.")

(defconst org-mcp-test--content-duplicate-headlines-before
  "* Team Updates
** Project Review
First review content.
* Development Tasks
** Project Review
Second review content.
* Planning
** Project Review
Third review content."
  "Content with duplicate 'Project Review' headlines under different parents.")

(defconst org-mcp-test--content-hierarchy-before
  "* First Section
** Target
Some content.
* Second Section
** Other Item
More content.
** Target
This Target is under Second Section, not First Section."
  "Content with duplicate 'Target' headlines under different parents.")

(defconst org-mcp-test--content-todo-keywords-before
  "* Project Management
** TODO Review Documents
This task needs to be renamed
** DONE Review Code
This is already done"
  "Parent with TODO and DONE children for testing keyword handling.")

;; Expected patterns and validation regexes
;;
;; Note on property drawer patterns: The patterns use ` *` (zero or more
;; spaces) before :PROPERTIES:, :ID:, and :END: lines to maintain compatibility
;; across Emacs versions. Emacs 27.2 indents property drawers with 3 spaces,
;; while Emacs 28+ does not add indentation.

(defconst org-mcp-test--expected-parent-task-from-nested-siblings
  (format
   "* Parent Task
:PROPERTIES:
:ID:       nested-siblings-parent-id-002
:END:
Some parent content.
** First Child 50%% Complete
First child content.
It spans multiple lines.
** Second Child
:PROPERTIES:
:ID:       %s
:END:
Second child content.
** Third Child #3"
   org-mcp-test--content-with-id-id)
  "Expected content when extracting Parent Task from nested-siblings.")

(defconst org-mcp-test--regex-after-sibling-level3
  (concat "\\`\\* Top Level\n"
          "\\*\\* Review the package\n"
          "\\*\\*\\* Review org-mcp\\.el\n"
          " *:PROPERTIES:\n"
          " *:ID: +" org-mcp-test--level2-parent-level3-sibling-id "\n"
          " *:END:\n"
          "Main package file\n"
          "\\*\\*\\* TODO Review org-mcp-test\\.el +.*:internet:.*\n"
          " *:PROPERTIES:\n"
          " *:ID: +[a-fA-F0-9-]+\n"
          " *:END:\n\\'")
  "Expected pattern after adding TODO after level 3 sibling.")

(defconst org-mcp-test--expected-regex-renamed-second-child
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    ":PROPERTIES:\n"
    ":ID: +nested-siblings-parent-id-002\n"
    ":END:\n"
    "Some parent content\\.\n"
    "\\*\\* First Child 50%% Complete\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Renamed Second Child\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\n?\\'")
   org-mcp-test--content-with-id-id)
  "Regex matching complete buffer after renaming Second Child.")

(defconst org-mcp-test--expected-regex-todo-to-in-progress-with-id
  (format
   (concat
    "\\`"
    "\\* IN-PROGRESS Task with ID\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "First line of content\\.\n"
    "Second line of content\\.\n"
    "Third line of content\\."
    "\\'")
   org-mcp-test--content-with-id-id)
  "Expected regex for TODO to IN-PROGRESS state change with ID.")

(defconst org-mcp-test--expected-timestamp-id-done-regex
  (concat
   "\\`\\* DONE Task with timestamp ID"
   "\\(?:\n:PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\\)?"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching complete buffer after updating timestamp ID task to DONE.")

(defconst org-mcp-test--expected-task-one-in-progress-regex
  (concat
   "\\`\\* IN-PROGRESS Task One"
   "\\(?:\n:PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\\)?"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching complete buffer with Task One in IN-PROGRESS state.")

(defconst org-mcp-test--expected-task-with-id-in-progress-regex
  (concat
   "\\`\\* IN-PROGRESS Task with ID"
   "\\(?:\n:PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\\)?"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching complete buffer with Task with ID in IN-PROGRESS state.")

(defconst org-mcp-test--expected-regex-top-level-with-header
  (concat
   "\\`#\\+TITLE: My Org Document\n"
   "\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   "\\(?: *:PROPERTIES:\n"
   " *:ID: +[^\n]+\n"
   " *:END:\n\\)?"
   "\n?"
   "\\* Parent Task\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-nested-siblings-parent-id "\n"
   ":END:\n"
   "Some parent content\\.\n"
   "\\*\\* First Child 50% Complete\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "\\*\\* Second Child\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-with-id-id "\n"
   ":END:\n"
   "Second child content\\.\n"
   "\\*\\* Third Child #3\n?\\'")
  "Regex matching complete buffer after adding top-level TODO with headers.")

(defconst org-mcp-test--regex-child-under-parent
  (format
   (concat
    "^\\* Parent Task\n"
    "\\(?: *:PROPERTIES:\n *:ID: +nested-siblings-parent-id-002\n *:END:\n\\)?"
    "Some parent content\\.\n"
    "\\*\\* First Child 50%% Complete\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    "\\(?: *:PROPERTIES:\n *:ID: +%s\n *:END:\n\\)?"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\n"
    "\\*\\* TODO Child Task +.*:work:.*\n"
    "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?")
   org-mcp-test--content-with-id-id)
  "Pattern for child TODO (level 2) added under parent (level 1) with existing child (level 2).")

(defconst org-mcp-test--regex-child-into-childless-parent
  (concat
   "\\`\\* Parent Task\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--childless-parent-id "\n"
   ":END:\n"
   "Some parent content\\.\n"
   "\\*\\* TODO Only Child +.*:work:.*\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?\\'")
  "Pattern for first child (level 2) added under a previously-childless parent (level 1).")

(defconst org-mcp-test--regex-second-child-same-level
  (concat
   "\\`\\* Top Level\n"
   "\\*\\* Review the package\n"
   "\\*\\*\\* Review org-mcp\\.el\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?"  ; Review org-mcp.el has ID
   "Main package file\n"
   "\\*\\*\\* TODO Second Child +.*:work:.*\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?\\'")  ; Second Child may have ID
  "Pattern for second child (level 3) added at same level as first child (level 3) under parent (level 2).")

(defconst org-mcp-test--regex-todo-with-body
  (concat
   "^\\* TODO Task with Body +:[^\n]*\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?" ; Optional properties
   (regexp-quote org-mcp-test--body-text-multiline)
   "\n?$")
  "Pattern for TODO with body text.")

(defconst org-mcp-test--regex-todo-after-sibling
  (concat
   "^#\\+TITLE: My Org Document\n\n"
   "\\* Parent Task\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-nested-siblings-parent-id "\n"
   ":END:\n"
   "Some parent content\\.\n"
   "\\*\\* First Child 50% Complete\n"
   ":PROPERTIES:\n"
   ":ID: +[^\n]+\n"
   ":END:\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n\n?"
   "\\*\\* TODO New Task After First +:[^\n]*\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?"
   "\\*\\* Second Child\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-with-id-id "\n"
   ":END:\n"
   "Second child content\\.\n"
   "\\*\\* Third Child #3\n?\\'")
  "Pattern for TODO added after specific sibling.")

(defconst org-mcp-test--regex-todo-without-tags
  (concat
   "^\\* TODO Task Without Tags *\n" ; No tags, optional spaces
   "\\(?: *:PROPERTIES:\n" " *:ID: +[^\n]+\n" " *:END:\n\\)?$")
  "Pattern for TODO item without any tags.")

(defconst org-mcp-test--pattern-add-todo-parent-id-uri
  (concat
   "^\\* Parent Task\n"
   "\\(?: *:PROPERTIES:\n"
   " *:ID: +[^\n]+\n"
   " *:END:\n\\)?"
   "Some parent content\\.\n"
   "\\*\\* First Child 50% Complete\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "\\*\\* Second Child\n"
   "\\(?: *:PROPERTIES:\n"
   " *:ID: +[^\n]+\n"
   " *:END:\n\\)?"
   "Second child content\\.\n"
   "\\*\\* Third Child #3\n"
   "\\*\\* TODO Child via ID +:work:\n"
   "\\(?: *:PROPERTIES:\n"
   " *:ID: +[^\n]+\n"
   " *:END:\n\\)?$")
  "Pattern for TODO added via parent ID URI.")

(defconst org-mcp-test--client-id
  "client-set-id-001"
  "ID a client writes through a properties parameter.")

(defconst org-mcp-test--pattern-add-todo-with-id-property
  (concat
   "\\`#\\+TITLE: My Org Document\n\n"
   "\\* Parent Task\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-nested-siblings-parent-id "\n"
   ":END:\n"
   "Some parent content\\.\n"
   "\\*\\* First Child 50% Complete\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "\\*\\* Second Child\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-with-id-id "\n"
   ":END:\n"
   "Second child content\\.\n"
   "\\*\\* Third Child #3\n"
   "\\*\\* TODO Client ID Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +" org-mcp-test--client-id "\n"
   " *:END:\n\\'")
  "Pattern for a child TODO created with a client-set ID.
The new heading carries that ID and no other.")

(defconst org-mcp-test--pattern-add-todo-with-custom-id-property
  (concat
   "\\`\\* TODO Custom ID Task\n"
   " *:PROPERTIES:\n"
   " *:CUSTOM_ID: +custom-id-task\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n\\'")
  "Pattern for a TODO created with a client-set CUSTOM_ID.")

(defconst org-mcp-test--pattern-add-todo-with-properties
  (concat
   "\\`\\* TODO Task with Properties +:work:\n"
   " *:PROPERTIES:\n"
   " *:EFFORT: +1:00\n"
   " *:OWNER: +alice\n"
   " *:ESTIMATE: +3\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   (regexp-quote org-mcp-test--body-text-multiline)
   "\n\\'")
  "Pattern for a TODO created with tags, a body and properties.
The drawer sits between the heading and the body, a number is written
as its text, and a property sent as null is not written.")

(defconst org-mcp-test--pattern-renamed-simple-todo
  (concat
   "\\`\\* TODO Updated Task\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "First line of body\\.\n"
   "Second line of body\\.\n"
   "Third line of body\\.\n?\\'")
  "Pattern for renamed simple TODO with generated ID.")

(defconst org-mcp-test--pattern-renamed-todo-with-tags
  (concat
   "^\\* TODO Renamed Task[ \t]+:work:urgent:\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task description\\.$")
  "Pattern for renamed TODO task preserving tags.")

(defconst org-mcp-test--pattern-renamed-headline-no-todo
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    "\\(?: *:PROPERTIES:\n *:ID: +nested-siblings-parent-id-002\n *:END:\n\\)?"
    "Some parent content\\.\n"
    "\\*\\* Updated Child\n"
    " *:PROPERTIES:\n"
    " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
    " *:END:\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    "\\(?: *:PROPERTIES:\n *:ID: +%s\n *:END:\n\\)?"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\n?"
    "\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for renamed headline without TODO state.")

(defconst org-mcp-test--pattern-renamed-headline-with-id
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    "\\(?: *:PROPERTIES:\n *:ID: +nested-siblings-parent-id-002\n *:END:\n\\)?"
    "Some parent content\\.\n"
    "\\*\\* First Child 50%% Complete\n"
    "\\(?: *:PROPERTIES:\n *:ID:[ \t]+[A-Fa-f0-9-]+\n *:END:\n\\)?"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    "\\(?: *:PROPERTIES:\n *:ID: +%s\n *:END:\n\\)?"
    "Second child content\\.\n"
    "\\*\\* Renamed Child\n"
    " *:PROPERTIES:\n"
    " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
    " *:END:\n?\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for headline renamed with ID creation.")

(defconst org-mcp-test--pattern-renamed-slash-headline
  (concat
   "\\`\\* Parent\n"
   "\\*\\* Real Child\n"
   "Content here\\.\n"
   "\\* Parent/Child Renamed\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "This is a single headline with a slash, not nested under Parent\\.\n?\\'")
  "Pattern for renamed headline containing slash character.")

(defconst org-mcp-test--regex-slash-not-nested-after
  (concat
   "\\`\\* Parent\n"
   "\\*\\* Real Child\n"
   "Content here\\.\n"
   "\\* Parent-Child Renamed\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "This is a single headline with a slash, not nested under Parent\\.\n?\\'")
  "Regex for slash-not-nested test after renaming Parent/Child.")

(defconst org-mcp-test--regex-percent-after
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "Some parent content\\.\n"
    "\\*\\* First Child 75%% Complete\n"
    " *:PROPERTIES:\n"
    " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
    " *:END:\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\n?\\'")
   org-mcp-test--content-nested-siblings-parent-id
   org-mcp-test--content-with-id-id)
  "Expected pattern after renaming headline with percent sign.")

(defconst org-mcp-test--regex-duplicate-first-renamed
  (concat
   "\\`\\* Team Updates\n"
   "\\*\\* Q1 Review\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "First review content\\.\n"
   "\\* Development Tasks\n"
   "\\*\\* Project Review\n"
   "Second review content\\.\n"
   "\\* Planning\n"
   "\\*\\* Project Review\n"
   "Third review content\\.\n?\\'")
  "Regex for duplicate headlines after renaming first occurrence.")

(defconst org-mcp-test--regex-hierarchy-second-target-renamed
  (concat
   "\\`\\* First Section\n"
   "\\*\\* Target\n"
   "Some content\\.\n"
   "\\* Second Section\n"
   "\\*\\* Other Item\n"
   "More content\\.\n"
   "\\*\\* Renamed Target\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "This Target is under Second Section, not First Section\\.\n?\\'")
  "Regex for hierarchy test after renaming second Target.")

(defconst org-mcp-test--regex-add-todo-with-mutex-tags
  (concat
   "\\`#\\+TITLE: Test Org File\n"
   "\n"
   "\\* TODO Test Task[ \t]+\\(:[^:\n]+\\)+:\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n?\\'")
  "Regex for add-todo test accepting any tag order.")

(defconst org-mcp-test--regex-todo-keywords-after
  (concat
   "\\`\\* Project Management\n"
   "\\*\\* TODO Q1 Planning Review\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "This task needs to be renamed\n"
   "\\*\\* DONE Review Code\n"
   "This is already done\n?\\'")
  "Regex for todo-keywords test after renaming TODO headline.")

(defconst org-mcp-test--pattern-edit-body-single-line
  (format (concat
           "\\`#\\+TITLE: My Org Document\n"
           "\n"
           "\\* Parent Task\n"
           ":PROPERTIES:\n"
           ":ID: +nested-siblings-parent-id-002\n"
           ":END:\n"
           "Some parent content\\.\n"
           "\\*\\* First Child 50%% Complete\n"
           "First child content\\.\n"
           "It spans multiple lines\\.\n"
           "\\*\\* Second Child\n"
           ":PROPERTIES:\n"
           ":ID: +%s\n"
           ":END:\n"
           "Updated second child content\\.\n"
           "\\*\\* Third Child #3\n"
           "?\\'")
          org-mcp-test--content-with-id-id)
  "Pattern for single-line edit-body test result.")

(defconst org-mcp-test--pattern-edit-body-multiline
  (format (concat
           "\\`\\* TODO Task with ID\n"
           ":PROPERTIES:\n"
           ":ID: +%s\n"
           ":END:\n"
           "First line of content\\.\n"
           "This has been replaced\n"
           "with new multiline\n"
           "content here\\.\n"
           "Third line of content\\.\n"
           "?\\'")
          org-mcp-test--content-with-id-id)
  "Pattern for multiline edit-body test result.")


(defconst org-mcp-test--pattern-edit-body-nested-headlines
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    "\\(?: *:PROPERTIES:\n *:ID: +nested-siblings-parent-id-002\n *:END:\n\\)?"
    "Updated parent content\n"
    "\\*\\* First Child 50%% Complete\n"
    "\\(?: *:PROPERTIES:\n *:ID:[ \t]+[A-Fa-f0-9-]+\n *:END:\n\\)?"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    "\\(?: *:PROPERTIES:\n *:ID: +%s\n *:END:\n\\)?"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\n?"
    "\\(?: *:PROPERTIES:\n *:ID:[ \t]+[A-Fa-f0-9-]+\n *:END:\n\\)?"
    "\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for nested headlines edit-body test result.")

(defconst org-mcp-test--pattern-edit-body-empty
  (concat
   "\\*\\* Third Child #3New content added\\.\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:")
  "Pattern for edit-body test with empty body adding content.")

(defconst org-mcp-test--pattern-edit-body-empty-with-props
  (format (concat
           " *:PROPERTIES:\n"
           " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
           " *:END:\n"
           " *:PROPERTIES:\n"
           " *:ID: +%s\n"
           " *:END:Content added after properties\\.")
          org-mcp-test--timestamp-id)
  "Pattern for edit-body with existing properties adding content.")

(defconst org-mcp-test--pattern-edit-body-accept-lower-level
  (concat
   "\\* Parent Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +nested-siblings-parent-id-002\n"
   " *:END:\n"
   "Some parent content\\.\n"
   "\\*\\* First Child 50% Complete\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "\\*\\* Second Child\n"
   " *:PROPERTIES:\n"
   " *:ID: +"
   org-mcp-test--content-with-id-id
   "\n"
   " *:END:\n"
   "some text\n"
   "\\*\\*\\* Subheading content\n"
   "\\(?: *:PROPERTIES:\n" ; Subheading gets ID
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n\\)?"
   "\\*\\* Third Child #3")
  "Pattern for edit-body accepting lower-level headlines.")

(defconst org-mcp-test--pattern-tool-read-headline-single
  (concat
   "\\`\\* Parent/Child\n"
   "This is a single headline with a slash, not nested under Parent\\.\n"
   "?\\'")
  "Pattern for org-read-headline tool single-level path result.")

(defconst org-mcp-test--pattern-tool-read-headline-nested
  (concat
   "\\`\\*\\* First Child 50% Complete\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "?\\'")
  "Pattern for org-read-headline tool nested path result.")

(defconst org-mcp-test--pattern-tool-read-by-id
  (format
   (concat
    "\\`\\*\\* Second Child\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "Second child content\\.\n"
    "?\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for org-read-by-id tool result.")

(defconst org-mcp-test--content-id-resource
  (concat
   "* Section with ID\n"
   ":PROPERTIES:\n"
   ":ID: 12345678-abcd-efgh-ijkl-1234567890ab\n"
   ":END:\n"
   "Content of section with ID.")
  "Content for ID resource tests.")


(defconst org-mcp-test--clock-task-content
  "* TODO Task One\n"
  "Initial Org file content for clock tool tests.")

(defconst org-mcp-test--clock-task-with-open-clock
  "* TODO Task One\n:LOGBOOK:\nCLOCK: [2026-01-01 Thu 10:00]\n:END:\n"
  "Org file content with an unclosed CLOCK entry, used for clock-out tests.")

(defconst org-mcp-test--clock-add-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] => 1:00\n"
   ":END:\n"
   "\\'")
  "Regex matching the complete file after org-clock-add adds a closed CLOCK entry.")

(defconst org-mcp-test--clock-in-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\n"
   ":END:\n"
   "\\'")
  "Regex matching the complete file after org-clock-in inserts an open CLOCK entry.")

(defconst org-mcp-test--clock-out-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] => 1:00\n"
   ":END:\n"
   "\\'")
  "Regex matching the complete file after org-clock-out closes the CLOCK entry.")

(defconst org-mcp-test--before-save-hook-initial-content
  "* Headline\n\nOriginal body\n"
  "Initial org file content for before-save-hook tests.")

(defconst org-mcp-test--scope-task-content "* TODO Task\nBody\n"
  "File holding one TODO heading, the before image for scope tests.")

(defconst org-mcp-test--scope-task-done-regex
  (concat
   "\\`\\* DONE Task\n"
   ":PROPERTIES:\n"
   ":ID:[ \t]+[A-Fa-f0-9-]+\n"
   ":END:\n"
   "Body\n"
   "\\'")
  "Regex matching the complete scope-test file after Task becomes DONE.")

(defconst org-mcp-test--scope-task-with-id-content
  (format "* TODO Task\n:PROPERTIES:\n:ID:       %s\n:END:\nBody\n"
          org-mcp-test--content-with-id-id)
  "Scope-test file whose heading carries an ID.")

(defconst org-mcp-test--scope-tagged-content "* TODO Inner :innertag:\n"
  "File holding one heading with a tag no other scope-test file uses.")

(defconst org-mcp-test--file-set-template
  "* TODO %s :%s:\n:LOGBOOK:\nCLOCK: [2026-01-01 Thu 10:00]\n:END:\n"
  "Content of a file-set test file, formatted with its title twice.
The title doubles as the heading's tag, and the heading holds an open
clock, so each tool taking `files' reports the file by that title.")

(defconst org-mcp-test--remote-prefix "/org-mcp-remote:host:"
  "Prefix of the file names the fake remote method claims.")

;; Test helpers

(defun org-mcp-test--read-file (file)
  "Read and return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-mcp-test--verify-file-matches (test-file expected-pattern)
  "Verify TEST-FILE content matches EXPECTED-PATTERN regexp."
  (should (string-match-p expected-pattern (org-mcp-test--read-file test-file))))

(defun org-mcp-test--verify-buffer-matches (buffer expected-pattern)
  "Verify the complete contents of BUFFER match EXPECTED-PATTERN regexp."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (should (string-match-p expected-pattern (buffer-string))))))

(defmacro org-mcp-test--assert-error-and-file (test-file error-form)
  "Assert that ERROR-FORM throws an error and TEST-FILE remains unchanged."
  (declare (indent 1) (debug t))
  `(let ((original-content (org-mcp-test--read-file ,test-file)))
     (should-error ,error-form :type 'mcp-server-lib-tool-error)
     (should (string= (org-mcp-test--read-file ,test-file) original-content))))

(defmacro org-mcp-test--with-enabled (&rest body)
  "Run BODY with org-mcp enabled, ensuring cleanup."
  (declare (indent defun) (debug t))
  `(progn
     (org-mcp-enable)
     (unwind-protect
         (mcp-server-lib-ert-with-server :tools t :resources t ,@body)
       (org-mcp-disable))))

(defmacro org-mcp-test--with-temp-org-files (file-specs &rest body)
  "Create temporary Org files, execute BODY, and ensure cleanup.
FILE-SPECS is a list of file specifications.
Each spec is (VAR CONTENT [FILENAME-PREFIX]).
VAR is the variable to bind the temp file path to.
CONTENT is the initial content to write to the file.
FILENAME-PREFIX is optional, defaults to \"org-mcp-test\".
All created files are automatically added to `org-mcp-allowed-files'.
BODY is executed with org-mcp enabled."
  (declare (indent 1))
  (let* ((vars (mapcar #'car file-specs))
         (temp-vars (mapcar (lambda (v) (gensym (symbol-name v)))
                            vars))
         (bindings (cl-mapcar
                    (lambda (var temp-var)
                      `(,var ,temp-var))
                    vars temp-vars))
         (inits (cl-mapcar
                 (lambda (temp-var spec)
                   (let ((content (nth 1 spec))
                         (filename (or (nth 2 spec) "org-mcp-test")))
                     `(setq ,temp-var
                            (make-temp-file ,filename nil ".org" ,content))))
                 temp-vars file-specs))
         (cleanups (mapcar
                    (lambda (temp-var)
                      `(when ,temp-var
                         (delete-file ,temp-var)))
                    temp-vars)))
    `(let (,@temp-vars)
       (unwind-protect
           (progn
             ,@inits
             (let (,@bindings
                   (org-mcp-allowed-files (list ,@temp-vars)))
               (org-mcp-test--with-enabled
                 ,@body)))
         ,@cleanups))))

(defmacro org-mcp-test--with-id-tracking
    (allowed-files id-locations &rest body)
  "Set up org-id tracking with ID-LOCATIONS and run BODY.
ALLOWED-FILES is the list of files to bind to `org-mcp-allowed-files'.
ID-LOCATIONS is a list of (ID . FILE) cons cells to register.
Sets up `org-id-track-globally' and `org-id-locations-file',
then registers each ID location."
  (declare (indent 2) (debug t))
  `(let ((org-id-track-globally t)
         (org-id-locations-file nil) ; Prevent saving to disk
         (org-id-locations nil)
         (org-mcp-allowed-files ,allowed-files))
     (dolist (id-loc ,id-locations)
       (org-id-add-location (car id-loc) (cdr id-loc)))
     ,@body))

(defmacro org-mcp-test--with-id-setup (file-var initial-content ids &rest body)
  "Create temp file, set up org-id tracking with IDS, run BODY.
FILE-VAR is the variable to bind the temp file path to.
INITIAL-CONTENT is the initial content to write to the file.
IDS is a list of ID strings to register.
Sets up `org-id-track-globally' and `org-id-locations-file',
then registers each ID location and enables MCP for BODY.
The created temp file is automatically added to `org-mcp-allowed-files'."
  (declare (indent 2) (debug t))
  `(org-mcp-test--with-temp-org-files
       ((,file-var ,initial-content))
     (org-mcp-test--with-id-tracking
      (list ,file-var)
      (mapcar (lambda (id) (cons id ,file-var)) ,ids)
      ,@body)))

;; Helper functions for reading MCP resources

(defun org-mcp-test--verify-resource-read (uri text)
  "Verify MCP resource at URI being TEXT."
  (mcp-server-lib-ert-verify-resource-read
   uri `((uri . ,uri)
         (text . ,text)
         (mimeType . "text/plain"))))

;; Helper functions for testing org-get-todo-config MCP tool

(defun org-mcp-test--check-todo-config-sequence
    (seq expected-type expected-keywords)
  "Check sequence SEQ has EXPECTED-TYPE and EXPECTED-KEYWORDS."
  (should (= (length seq) 2))
  (should (equal (alist-get 'type seq) expected-type))
  (should (equal (alist-get 'keywords seq) expected-keywords)))

(defun org-mcp-test--check-todo-config-semantic
    (sem expected-state expected-final expected-type)
  "Check semantic SEM properties.
EXPECTED-STATE is the TODO keyword.
EXPECTED-FINAL is whether it's a final state.
EXPECTED-TYPE is the sequence type."
  (should (= (length sem) 3))
  (should (equal (alist-get 'state sem) expected-state))
  (should (equal (alist-get 'isFinal sem) expected-final))
  (should (equal (alist-get 'sequenceType sem) expected-type)))

(defmacro org-mcp-test--with-get-todo-config-result (keywords &rest body)
  "Call get-todo-config tool with KEYWORDS and run BODY with result bindings.
Sets `org-todo-keywords' to KEYWORDS, calls the get-todo-config MCP tool,
and binds `sequences' and `semantics' from the result for use in BODY."
  (declare (indent 1) (debug t))
  `(let ((org-todo-keywords ,keywords))
     (org-mcp-test--with-enabled
      (let ((result (json-read-from-string
                     (mcp-server-lib-ert-call-tool "org-get-todo-config" nil))))
        (should (= (length result) 2))
        (let ((sequences (cdr (assoc 'sequences result)))
              (semantics (cdr (assoc 'semantics result))))
          ,@body)))))

;; Helper functions for testing org-get-tag-config MCP tool

(defmacro org-mcp-test--get-tag-config-and-check
    (expected-alist expected-persistent expected-inheritance expected-exclude)
  "Call org-get-tag-config tool and check result against expected values.
EXPECTED-ALIST is the expected value for org-tag-alist (string).
EXPECTED-PERSISTENT is the expected value for org-tag-persistent-alist (string).
EXPECTED-INHERITANCE is the expected value for org-use-tag-inheritance (string).
EXPECTED-EXCLUDE is the expected value for
org-tags-exclude-from-inheritance (string)."
  (declare (indent defun) (debug t))
  `(org-mcp-test--with-enabled
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool "org-get-tag-config" nil))))
      (should (= (length result) 4))
      (should (equal (alist-get 'org-tag-alist result) ,expected-alist))
      (should (equal (alist-get 'org-tag-persistent-alist result)
                     ,expected-persistent))
      (should (equal (alist-get 'org-use-tag-inheritance result)
                     ,expected-inheritance))
      (should (equal (alist-get 'org-tags-exclude-from-inheritance result)
                     ,expected-exclude)))))

;; Helper functions for testing org-get-allowed-files MCP tool

(defun org-mcp-test--call-get-allowed-files ()
  "Call org-get-allowed-files tool and return the parsed result."
  (json-read-from-string
   (mcp-server-lib-ert-call-tool "org-get-allowed-files" nil)))

(defun org-mcp-test--get-allowed-files-and-check (allowed-files expected-files)
  "Call org-get-allowed-files tool and verify the result.
ALLOWED-FILES is the value to bind to org-mcp-allowed-files.
EXPECTED-FILES is a list of expected file paths.
The scope override stays at its default, refusal."
  (let ((org-mcp-allowed-files allowed-files)
        (org-mcp-file-scope-override nil))
    (org-mcp-test--with-enabled
     (let ((result (org-mcp-test--call-get-allowed-files)))
       (should (= (length result) 2))
       (should (eq (alist-get 'override_allowed result) :json-false))
       (let ((files (cdr (assoc 'files result))))
         (should (vectorp files))
         (should (= (length files) (length expected-files)))
         (dotimes (i (length expected-files))
           (should (string= (aref files i) (nth i expected-files)))))))))

;; Helper functions for testing org-add-todo MCP tool

(defun org-mcp-test--call-add-todo-expecting-error
    (test-file title todoState tags body parentUri &optional afterUri
               properties)
  "Call org-add-todo MCP tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
TITLE is the headline text.
TODOSTATE is the TODO state.
TAGS is a list of tag strings or nil.
BODY is the body text or nil.
PARENTURI is the URI of the parent item.
AFTERURI is optional URI of sibling to insert after.
PROPERTIES is an optional alist sent as the properties parameter."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((params
           `((title . ,title)
             (todo_state . ,todoState)
             (tags . ,tags)
             (body . ,body)
             (parent_uri . ,parentUri)
             (after_uri . ,afterUri)
             ,@(when properties `((properties . ,properties)))))
          (request
            (mcp-server-lib-create-tools-call-request
             "org-add-todo" nil params))
          (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
          (result (mcp-server-lib-ert-process-tool-response response)))
     ;; If we get here, the tool succeeded when we expected failure
     (error "Expected error but got success: %s" result))))

(defun org-mcp-test--add-todo-and-check
    (title todoState tags body parentUri afterUri
           basename test-file expected-pattern &optional properties)
  "Add TODO item, verify the result and return the parsed response.
TITLE is the headline text.
TODOSTATE is the TODO state.
TAGS is a list of tag strings or nil.
BODY is the body text or nil.
PARENTURI is the URI of the parent item.
AFTERURI is optional URI of sibling to insert after.
BASENAME is the expected file basename.
TEST-FILE is the path to the file to check.
EXPECTED-PATTERN is a regexp that the file content should match.
PROPERTIES is an optional alist sent as the properties parameter."
  (let* ((params
          `((title . ,title)
            (todo_state . ,todoState)
            (tags . ,tags)
            (body . ,body)
            (parent_uri . ,parentUri)
            (after_uri . ,afterUri)
            ,@(when properties `((properties . ,properties)))))
         (result-text (mcp-server-lib-ert-call-tool "org-add-todo" params))
         (result (json-read-from-string result-text)))
    ;; Check result structure
    (should (= (length result) 5))
    (should (equal (alist-get 'success result) t))
    (should (eq (alist-get 'saved result) t))
    (should (string-match-p "\\`org://.+" (alist-get 'uri result)))
    (should (equal (alist-get 'file result) basename))
    (should (equal (alist-get 'title result) title))
    (org-mcp-test--verify-file-matches test-file expected-pattern)
    result))

(defun org-mcp-test--id-registered-p (id)
  "Return non-nil when ID has an entry in `org-id-locations'."
  (and (hash-table-p org-id-locations) (gethash id org-id-locations)))

(defun org-mcp-test--verify-no-modified-buffer (file)
  "Verify no buffer visiting FILE holds unsaved changes."
  (let ((buffer (find-buffer-visiting file)))
    (should-not (and buffer (buffer-modified-p buffer)))))

(defun org-mcp-test--call-set-properties-expecting-error
    (test-file uri properties)
  "Call org-set-properties expecting an error, verify nothing changed.
TEST-FILE is the file that must stay unchanged on disk and in any
buffer visiting it.  URI is the headline URI.  PROPERTIES is the
alist sent as the properties parameter."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((request
           (mcp-server-lib-create-tools-call-request
            "org-set-properties" nil
            `((uri . ,uri) (properties . ,properties))))
          (response
           (mcp-server-lib-process-jsonrpc-parsed
            request mcp-server-lib-ert-server-id))
          (result (mcp-server-lib-ert-process-tool-response response)))
     (error "Expected error but got success: %s" result)))
  (org-mcp-test--verify-no-modified-buffer test-file))

;; Helper functions for testing org-update-todo-state MCP tool

(defun org-mcp-test--call-update-todo-state (uri new-state &optional current-state note)
  "Call org-update-todo-state tool via JSON-RPC and return the result.
URI is the headline URI, NEW-STATE is the new TODO state to set.
CURRENT-STATE, when provided, is the expected current TODO state.
NOTE, when provided, is a note to attach to the state transition."
  (let* ((params
          `((uri . ,uri)
            (new_state . ,new-state)
            ,@(when current-state `((current_state . ,current-state)))
            ,@(when note `((note . ,note)))))
         (result-text
          (mcp-server-lib-ert-call-tool "org-update-todo-state" params)))
    (json-read-from-string result-text)))

(defun org-mcp-test--call-update-todo-state-expecting-error
    (test-file resource-uri current-state new-state)
  "Call org-update-todo-state tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
RESOURCE-URI is the URI to update.
CURRENT-STATE is the expected current TODO state (nil to omit).
NEW-STATE is the new TODO state to set."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((request
            (mcp-server-lib-create-tools-call-request
             "org-update-todo-state" 1
             `((uri . ,resource-uri)
               ,@(when current-state `((current_state . ,current-state)))
               (new_state . ,new-state))))
          (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
          (result (mcp-server-lib-ert-process-tool-response response)))
     ;; If we get here, the tool succeeded when we expected failure
     (error "Expected error but got success: %s" result))))

(defun org-mcp-test--update-todo-state-and-check
    (resource-uri old-state new-state test-file expected-content-regex)
  "Update TODO state and verify the result via MCP JSON-RPC.
RESOURCE-URI is the URI to update.
OLD-STATE is the current TODO state to update from.
NEW-STATE is the new TODO state to update to.
TEST-FILE is the file to verify content after update.
EXPECTED-CONTENT-REGEX is an anchored regex that matches the complete buffer."
  (let ((result
         (org-mcp-test--call-update-todo-state
          resource-uri new-state old-state)))
    (should (= (length result) 5))
    (should (equal (alist-get 'success result) t))
    (should (eq (alist-get 'saved result) t))
    (should (equal (alist-get 'previous_state result) old-state))
    (should (equal (alist-get 'new_state result) new-state))
    (should (stringp (alist-get 'uri result)))
    (should (string-prefix-p "org://" (alist-get 'uri result)))
    ;; For ID-based URIs, returned URI is the canonical `org://{uuid}'
    ;; resource form even though the input was bare.
    (when (org-mcp--uri-is-id-based resource-uri)
      (should (equal (alist-get 'uri result)
                     (concat "org://" resource-uri))))
    (org-mcp-test--verify-file-matches test-file expected-content-regex)))

;; Helper functions for testing org-rename-headline MCP tool

(defun org-mcp-test--call-rename-headline-and-check
    (uri current-title new-title test-file expected-content-regex)
  "Call org-rename-headline tool via JSON-RPC and verify the result.
URI is the headline URI.
CURRENT-TITLE is the expected current title.
NEW-TITLE is the new title to set.
TEST-FILE is the file to verify content after rename.
EXPECTED-CONTENT-REGEX is an anchored regex that matches the complete buffer."
  (let* ((params
          `((uri . ,uri)
            (current_title . ,current-title)
            (new_title . ,new-title)))
         (result-text
          (mcp-server-lib-ert-call-tool "org-rename-headline" params))
         (result (json-read-from-string result-text))
         (result-uri (alist-get 'uri result)))
    (should (= (length result) 5))
    (should (equal (alist-get 'success result) t))
    (should (eq (alist-get 'saved result) t))
    (should (equal (alist-get 'previous_title result) current-title))
    (should (equal (alist-get 'new_title result) new-title))
    (should (stringp result-uri))
    (should (string-prefix-p "org://" result-uri))
    ;; If input URI was ID-based, result URI is the canonical
    ;; `org://{uuid}' resource form even though the input was bare.
    (when (org-mcp--uri-is-id-based uri)
      (should (equal result-uri (concat "org://" uri))))
    (org-mcp-test--verify-file-matches test-file expected-content-regex)))

(defun org-mcp-test--call-rename-headline-expecting-error
    (test-file uri current-title new-title)
  "Call org-rename-headline tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
URI is the URI to rename.
CURRENT-TITLE is the current title for validation.
NEW-TITLE is the new title to set."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((params
           `((uri . ,uri)
             (current_title . ,current-title)
             (new_title . ,new-title)))
          (request
            (mcp-server-lib-create-tools-call-request
             "org-rename-headline" 1 params))
          (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
          (result (mcp-server-lib-ert-process-tool-response response)))
     ;; If we get here, the tool succeeded when we expected failure
     (error "Expected error but got success: %s" result))))

(defun org-mcp-test--read-resource-expecting-error
    (uri expected-error-message)
  "Read resource at URI expecting an error with EXPECTED-ERROR-MESSAGE."
  (let* ((request (mcp-server-lib-create-resources-read-request uri))
         (response-json (mcp-server-lib-process-jsonrpc request mcp-server-lib-ert-server-id))
         (response
          (json-parse-string response-json :object-type 'alist)))
    (unless (assoc 'error response)
      (error "Expected error but got success for URI: %s" uri))
    (mcp-server-lib-ert-check-error-object
     response
     mcp-server-lib-jsonrpc-error-invalid-params
     expected-error-message)))

;; Helper functions for testing org-edit-body MCP tool

(defun org-mcp-test--call-edit-body-and-check
    (test-file resource-uri old-body new-body expected-pattern
               &optional append expected-id)
  "Call org-edit-body tool and check result structure and file content.
TEST-FILE is the path to the file to check.
RESOURCE-URI is the URI of the node to edit.
OLD-BODY is the substring to search for within the node's body.
NEW-BODY is the replacement text.
EXPECTED-PATTERN is a regexp that the file content should match.
APPEND if true, append new-body to end of body (default: nil).
EXPECTED-ID if provided, check the returned URI has this exact ID."
  (let* ((params
          `((resource_uri . ,resource-uri)
            (old_body . ,old-body)
            (new_body . ,new-body)
            (append . ,append)))
         (result-text (mcp-server-lib-ert-call-tool "org-edit-body" params))
         (result (json-read-from-string result-text)))
    (should (= (length result) 3))
    (should (equal (alist-get 'success result) t))
    (should (eq (alist-get 'saved result) t))
    (let ((uri (alist-get 'uri result)))
      (if expected-id
          (should (equal uri (concat "org://" expected-id)))
        (should (string-prefix-p "org://" uri))))
    (org-mcp-test--verify-file-matches test-file expected-pattern)))

(defun org-mcp-test--call-edit-body-expecting-error
    (test-file resource-uri old-body new-body)
  "Call org-edit-body tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
RESOURCE-URI is the URI of the node to edit.
OLD-BODY is the substring to search for within the node's body.
NEW-BODY is the replacement text."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((params
           `((resource_uri . ,resource-uri)
             (old_body . ,old-body)
             (new_body . ,new-body)))
          (request
            (mcp-server-lib-create-tools-call-request
             "org-edit-body" 1 params))
          (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
          (result (mcp-server-lib-ert-process-tool-response response)))
     ;; If we get here, the tool succeeded when we expected failure
     (error "Expected error but got success: %s" result))))

;; Helper functions for testing org-read MCP tool

(defun org-mcp-test--call-read (uri)
  "Call org-read tool via JSON-RPC and return the result.
URI must be a bare {path}, {path}#{headline}, or {uuid} (no `org://' prefix)."
  (let ((params `((uri . ,uri))))
    (mcp-server-lib-ert-call-tool "org-read" params)))

;; Helper functions for testing org-read-outline MCP tool

(defun org-mcp-test--call-read-outline (file)
  "Call org-read-outline tool via JSON-RPC and return the result.
FILE is the file path to read the outline from."
  (let* ((params `((file . ,file)))
         (result-json
          (mcp-server-lib-ert-call-tool "org-read-outline" params)))
    (json-parse-string result-json :object-type 'alist)))

;; Helper functions for testing org-read-headline MCP tool

(defun org-mcp-test--call-read-headline (uri)
  "Call org-read-headline tool via JSON-RPC and return the result.
URI must be a bare {path}, {path}#{headline}, or {uuid} (no `org://' prefix)."
  (let ((params `((uri . ,uri))))
    (mcp-server-lib-ert-call-tool "org-read-headline" params)))

;; Helper functions for testing clock MCP tools

(defun org-mcp-test--call-clock-add (uri start end)
  "Call org-clock-add tool via JSON-RPC and return the parsed result.
URI is the headline URI, START and END are ISO 8601 timestamps."
  (let* ((params `((uri . ,uri) (start . ,start) (end . ,end)))
         (result-text
          (mcp-server-lib-ert-call-tool "org-clock-add" params)))
    (json-read-from-string result-text)))

(defun org-mcp-test--call-clock-in (uri &optional start-time resolve)
  "Call org-clock-in tool via JSON-RPC and return the parsed result.
URI is the headline URI.  START-TIME is an optional ISO 8601 timestamp.
RESOLVE when non-nil is passed as the `resolve' parameter (e.g. \"true\")."
  (let* ((params
          (append
           `((uri . ,uri))
           (when start-time `((start_time . ,start-time)))
           (when resolve `((resolve . ,resolve)))))
         (result-text
          (mcp-server-lib-ert-call-tool "org-clock-in" params)))
    (json-read-from-string result-text)))

(defun org-mcp-test--call-clock-out (&optional end-time)
  "Call org-clock-out tool via JSON-RPC and return the parsed result.
END-TIME is an optional ISO 8601 end timestamp."
  (let* ((params (if end-time `((end_time . ,end-time)) '()))
         (result-text
          (mcp-server-lib-ert-call-tool "org-clock-out" params)))
    (json-read-from-string result-text)))

(defun org-mcp-test--call-clock-get-active ()
  "Call org-clock-get-active tool via JSON-RPC and return the parsed result."
  (let ((result-text
         (mcp-server-lib-ert-call-tool "org-clock-get-active" nil)))
    (json-read-from-string result-text)))

(defun org-mcp-test--call-clock-delete (uri start)
  "Call org-clock-delete tool via JSON-RPC and return the parsed result.
URI is the headline URI.  START is the ISO 8601 start timestamp of the
clock entry to delete."
  (let* ((params `((uri . ,uri) (start . ,start)))
         (result-text
          (mcp-server-lib-ert-call-tool "org-clock-delete" params)))
    (json-read-from-string result-text)))

(defun org-mcp-test--call-clock-find-dangling ()
  "Call org-clock-find-dangling tool via JSON-RPC and return the parsed result."
  (let ((result-text
         (mcp-server-lib-ert-call-tool
          "org-clock-find-dangling" nil)))
    (json-read-from-string result-text)))


;;; Tests

(ert-deftest org-mcp-test-tool-get-todo-config-empty ()
  "Test org-get-todo-config with empty `org-todo-keywords'."
  (org-mcp-test--with-get-todo-config-result
   nil
   (should (assoc 'sequences result))
   (should (assoc 'semantics result))
   (should (equal sequences []))
   (should (equal semantics []))))

(ert-deftest org-mcp-test-tool-get-todo-config-default ()
  "Test org-get-todo-config with default `org-todo-keywords'."
  (org-mcp-test--with-get-todo-config-result '((sequence "TODO(t!)" "DONE(d!)"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "sequence" ["TODO(t!)" "|" "DONE(d!)"])
    (should (= (length semantics) 2))
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "TODO" nil "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "DONE" t "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-single-keyword ()
  "Test org-get-todo-config with single keyword."
  (org-mcp-test--with-get-todo-config-result '((sequence "DONE"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "sequence" ["|" "DONE"])
    (should (= (length semantics) 1))
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "DONE" t "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-explicit-bar ()
  "Test org-get-todo-config with explicit | and multiple states."
  (org-mcp-test--with-get-todo-config-result '((sequence
                                "TODO" "NEXT" "|" "DONE" "CANCELLED"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0)
     "sequence"
     ["TODO" "NEXT" "|" "DONE" "CANCELLED"])
    (should (= (length semantics) 4))
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "TODO" nil "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "NEXT" nil "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 2) "DONE" t "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 3) "CANCELLED" t "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-type ()
  "Test org-get-todo-config with type keywords."
  (org-mcp-test--with-get-todo-config-result '((type "Fred" "Sara" "Lucy" "|" "DONE"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "type" ["Fred" "Sara" "Lucy" "|" "DONE"])
    (should (= (length semantics) 4))
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "Fred" nil "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "Sara" nil "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 2) "Lucy" nil "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 3) "DONE" t "type")))

(ert-deftest org-mcp-test-tool-get-todo-config-multiple-sequences ()
  "Test org-get-todo-config with multiple sequences."
  (org-mcp-test--with-get-todo-config-result '((sequence "TODO" "|" "DONE")
                               (type "BUG" "FEATURE" "|" "FIXED"))
    (should (= (length sequences) 2))
    ;; First sequence
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "sequence" ["TODO" "|" "DONE"])
    ;; Second sequence
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 1) "type" ["BUG" "FEATURE" "|" "FIXED"])
    (should (= (length semantics) 5))
    ;; Semantics from first sequence
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "TODO" nil "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "DONE" t "sequence")
    ;; Semantics from second sequence
    (org-mcp-test--check-todo-config-semantic (aref semantics 2) "BUG" nil "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 3) "FEATURE" nil "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 4) "FIXED" t "type")))

(ert-deftest org-mcp-test-tool-get-todo-config-no-done-states ()
  "Test org-get-todo-config with no done states."
  (org-mcp-test--with-get-todo-config-result '((sequence "TODO" "NEXT" "|"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "sequence" ["TODO" "NEXT" "|"])
    (should (= (length semantics) 2))
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "TODO" nil "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "NEXT" nil "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-type-no-separator ()
  "Test org-get-todo-config with type keywords and no separator."
  (org-mcp-test--with-get-todo-config-result '((type "BUG" "FEATURE" "ENHANCEMENT"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "type" ["BUG" "FEATURE" "|" "ENHANCEMENT"])
    (should (= (length semantics) 3))
    (org-mcp-test--check-todo-config-semantic (aref semantics 0) "BUG" nil "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "FEATURE" nil "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 2) "ENHANCEMENT" t "type")))

(ert-deftest org-mcp-test-tool-get-tag-config-empty ()
  "Test org-get-tag-config with empty `org-tag-alist'."
  (let ((org-tag-alist nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t))
    (org-mcp-test--get-tag-config-and-check "nil" "nil" "t" "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-simple ()
  "Test org-get-tag-config with simple tags."
  (let ((org-tag-alist '("work" "personal" "urgent"))
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t)
        (org-tags-exclude-from-inheritance nil))
    (org-mcp-test--get-tag-config-and-check
     "(\"work\" \"personal\" \"urgent\")" "nil" "t" "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-with-keys ()
  "Test org-get-tag-config with fast selection keys."
  (let ((org-tag-alist
         '(("work" . ?w) ("personal" . ?p) "urgent" ("@home" . ?h)))
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t))
    (org-mcp-test--get-tag-config-and-check
     "((\"work\" . 119) (\"personal\" . 112) \"urgent\" (\"@home\" . 104))"
     "nil"
     "t"
     "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-with-groups ()
  "Test org-get-tag-config with tag groups."
  (let ((org-tag-alist
         '((:startgroup)
           ("@office" . ?o)
           ("@home" . ?h)
           ("@errand" . ?e)
           (:endgroup)
           "laptop"
           (:startgrouptag)
           ("project")
           (:grouptags)
           ("proj_a")
           ("proj_b")
           (:endgrouptag)))
        (org-tag-persistent-alist nil))
    (org-mcp-test--get-tag-config-and-check
     "((:startgroup) (\"@office\" . 111) (\"@home\" . 104) (\"@errand\" . 101) (:endgroup) \"laptop\" (:startgrouptag) (\"project\") (:grouptags) (\"proj_a\") (\"proj_b\") (:endgrouptag))"
     "nil"
     "t"
     "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-persistent ()
  "Test org-get-tag-config with persistent tags."
  (let ((org-tag-alist '(("work" . ?w)))
        (org-tag-persistent-alist '(("important" . ?i) "recurring"))
        (org-tags-exclude-from-inheritance nil))
    (org-mcp-test--get-tag-config-and-check
     "((\"work\" . 119))" "((\"important\" . 105) \"recurring\")"
     "t"
     "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance-enabled ()
  "Test org-get-tag-config with inheritance enabled."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t))
    (org-mcp-test--get-tag-config-and-check
     "(\"work\" \"personal\")" "nil" "t" "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance-disabled ()
  "Test org-get-tag-config with inheritance disabled."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance nil))
    (org-mcp-test--get-tag-config-and-check
     "(\"work\" \"personal\")" "nil" "nil" "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance-selective ()
  "Test org-get-tag-config with selective inheritance (list)."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance '("work")))
    (org-mcp-test--get-tag-config-and-check
     "(\"work\" \"personal\")" "nil" "(\"work\")"
     "nil")))

(defun org-mcp-test--call-get-tag-candidates ()
  "Call org-get-tag-candidates and return the parsed `tags' vector."
  (let* ((result-text
          (mcp-server-lib-ert-call-tool "org-get-tag-candidates" nil))
         (result (json-read-from-string result-text)))
    (alist-get 'tags result)))

(ert-deftest org-mcp-test-tool-get-tag-candidates-empty ()
  "No allowed files and no configured tags returns an empty vector."
  (let ((org-tag-alist nil)
        (org-tag-persistent-alist nil)
        (org-mcp-allowed-files nil))
    (org-mcp-test--with-enabled
     (should
      (equal (org-mcp-test--call-get-tag-candidates) [])))))

(ert-deftest org-mcp-test-tool-get-tag-candidates-config-only ()
  "Without allowed files, returns configured tags from both alists."
  (let ((org-tag-alist '(("work" . ?w) "personal"))
        (org-tag-persistent-alist '(("important" . ?i)))
        (org-mcp-allowed-files nil))
    (org-mcp-test--with-enabled
     (should
      (equal
       (org-mcp-test--call-get-tag-candidates)
       ["important" "personal" "work"])))))

(ert-deftest org-mcp-test-tool-get-tag-candidates-from-files ()
  "Returns the union of configured tags and tags present in headlines."
  (org-mcp-test--with-temp-org-files
      ((file-a
        (concat
         "#+TITLE: A\n#+TAGS: filetag_a\n\n"
         "* TODO Task A1                                       :alpha:\n"
         "* TODO Task A2                                       :beta:\n"))
       (file-b
        (concat
         "#+TITLE: B\n\n"
         "* DONE Task B1                                 :gamma:alpha:\n")))
    (let ((org-tag-alist '(("configured" . ?c)))
          (org-tag-persistent-alist nil))
      (let ((tags (org-mcp-test--call-get-tag-candidates)))
        (should (vectorp tags))
        (should (member "configured" (append tags nil)))
        (should (member "filetag_a" (append tags nil)))
        (should (member "alpha" (append tags nil)))
        (should (member "beta" (append tags nil)))
        (should (member "gamma" (append tags nil)))
        ;; sorted, deduplicated
        (should (equal (append tags nil)
                       (delete-dups (sort (append tags nil) #'string<))))))))

(ert-deftest org-mcp-test-tool-get-tag-candidates-filters-group-keywords ()
  "Group keywords like `:startgroup' do not appear in the result."
  (let ((org-tag-alist
         '((:startgroup)
           ("@office" . ?o)
           ("@home" . ?h)
           (:endgroup)
           "laptop"
           (:startgrouptag)
           ("project")
           (:grouptags)
           ("proj_a")
           ("proj_b")
           (:endgrouptag)))
        (org-tag-persistent-alist nil)
        (org-mcp-allowed-files nil))
    (org-mcp-test--with-enabled
     (let ((tags (append (org-mcp-test--call-get-tag-candidates) nil)))
       (should (member "@office" tags))
       (should (member "@home" tags))
       (should (member "laptop" tags))
       (should (member "project" tags))
       (should (member "proj_a" tags))
       (should (member "proj_b" tags))
       (dolist (kw '(":startgroup" ":endgroup"
                     ":startgrouptag" ":grouptags" ":endgrouptag"))
         (should-not (member kw tags)))))))

(ert-deftest org-mcp-test-tool-get-allowed-files-empty ()
  "Test org-get-allowed-files with empty configuration."
  (org-mcp-test--get-allowed-files-and-check nil nil))

(ert-deftest org-mcp-test-tool-get-allowed-files-single ()
  "Test org-get-allowed-files with single file."
  (org-mcp-test--get-allowed-files-and-check
   '("/home/user/tasks.org")
   '("/home/user/tasks.org")))

(ert-deftest org-mcp-test-tool-get-allowed-files-multiple ()
  "Test org-get-allowed-files with multiple files."
  (org-mcp-test--get-allowed-files-and-check
   '("/home/user/tasks.org"
     "/home/user/projects.org"
     "/home/user/notes.org")
   '("/home/user/tasks.org"
     "/home/user/projects.org"
     "/home/user/notes.org")))

(ert-deftest org-mcp-test-tool-get-allowed-files-relative-paths ()
  "Relative entries are returned expanded against `org-directory'."
  (let ((org-directory "/home/user/org/"))
    (org-mcp-test--get-allowed-files-and-check
     '("tasks.org" "subdir/projects.org")
     '("/home/user/org/tasks.org"
       "/home/user/org/subdir/projects.org"))))

(ert-deftest org-mcp-test-tool-get-allowed-files-mixed-paths ()
  "Absolute and relative entries coexist; relative resolved against `org-directory'."
  (let ((org-directory "/home/user/org/"))
    (org-mcp-test--get-allowed-files-and-check
     '("/etc/global.org" "tasks.org" "subdir/projects.org")
     '("/etc/global.org"
       "/home/user/org/tasks.org"
       "/home/user/org/subdir/projects.org"))))

(ert-deftest org-mcp-test-tool-get-allowed-files-tilde-expansion ()
  "Tilde-prefixed entries are expanded to the user's home directory."
  (let ((org-directory "/home/user/org/"))
    (org-mcp-test--get-allowed-files-and-check
     '("~/notes.org")
     (list (expand-file-name "~/notes.org")))))

(ert-deftest org-mcp-test-find-allowed-file-relative-path ()
  "`org-mcp--find-allowed-file' resolves a relative entry against `org-directory'."
  (org-mcp-test--with-temp-org-files
      ((real-file "* Heading\n"))
    (let* ((dir (file-name-directory real-file))
           (name (file-name-nondirectory real-file))
           (org-directory dir)
           (org-mcp-allowed-files (list name)))
      ;; Looking up by absolute path must find the relative entry.
      (should (org-mcp--find-allowed-file real-file))
      (should (org-mcp--paths-equal-p
               (org-mcp--find-allowed-file real-file)
               real-file)))))

(ert-deftest org-mcp-test-find-allowed-file-absolute-path-with-org-directory ()
  "Absolute entries still match when `org-directory' is unrelated."
  (org-mcp-test--with-temp-org-files
      ((real-file "* Heading\n"))
    (let ((org-directory "/some/other/place/")
          (org-mcp-allowed-files (list real-file)))
      (should (org-mcp--paths-equal-p
               (org-mcp--find-allowed-file real-file)
               real-file)))))

(ert-deftest org-mcp-test-find-allowed-file-relative-and-absolute-interchangeable ()
  "The same file is found whether the allowed-list entry is absolute or relative."
  (org-mcp-test--with-temp-org-files
      ((real-file "* Heading\n"))
    (let* ((dir (file-name-directory real-file))
           (name (file-name-nondirectory real-file))
           (org-directory dir))
      ;; Configured as absolute path
      (let ((org-mcp-allowed-files (list real-file)))
        (should (org-mcp--paths-equal-p
                 (org-mcp--find-allowed-file real-file)
                 real-file)))
      ;; Configured as relative path
      (let ((org-mcp-allowed-files (list name)))
        (should (org-mcp--paths-equal-p
                 (org-mcp--find-allowed-file real-file)
                 real-file))))))

(ert-deftest org-mcp-test-with-allowed-agenda-files-expands-relative ()
  "`org-mcp--with-allowed-agenda-files' binds `org-agenda-files' to absolute paths."
  (org-mcp-test--with-temp-org-files
      ((real-file "* Heading\n"))
    (let* ((dir (file-name-directory real-file))
           (name (file-name-nondirectory real-file))
           (org-directory dir)
           (org-mcp-allowed-files (list name)))
      (org-mcp--with-allowed-agenda-files
        (should (= (length org-agenda-files) 1))
        (should (org-mcp--paths-equal-p
                 (car org-agenda-files)
                 real-file))))))

(ert-deftest org-mcp-test-uri-relative-allowed-absolute-uri ()
  "Path URI with absolute path resolves when allowed list uses relative path."
  (org-mcp-test--with-temp-org-files
      ((real-file "* Heading\nBody"))
    (let* ((dir (file-name-directory real-file))
           (name (file-name-nondirectory real-file))
           (org-directory dir)
           (org-mcp-allowed-files (list name))
           (uri (format "%s#Heading" real-file))
           (result (org-mcp-test--call-read-headline uri)))
      (should (string= result "* Heading\nBody")))))

(ert-deftest org-mcp-test-uri-absolute-allowed-absolute-uri ()
  "Path URI with absolute path resolves when allowed list uses absolute path."
  (org-mcp-test--with-temp-org-files
      ((real-file "* Heading\nBody"))
    ;; with-temp-org-files already binds allowed-files to (list real-file).
    (let* ((uri (format "%s#Heading" real-file))
           (result (org-mcp-test--call-read-headline uri)))
      (should (string= result "* Heading\nBody")))))

(ert-deftest org-mcp-test-uri-interchangeable-paths-same-result ()
  "Same file is reachable whether the allowed entry is absolute or relative."
  (org-mcp-test--with-temp-org-files
      ((real-file "* Heading\nBody"))
    (let* ((dir (file-name-directory real-file))
           (name (file-name-nondirectory real-file))
           (uri (format "%s#Heading" real-file)))
      ;; Configured as absolute path
      (let ((org-mcp-allowed-files (list real-file)))
        (should (string= (org-mcp-test--call-read-headline uri)
                         "* Heading\nBody")))
      ;; Configured as relative path (same physical file)
      (let ((org-directory dir)
            (org-mcp-allowed-files (list name)))
        (should (string= (org-mcp-test--call-read-headline uri)
                         "* Heading\nBody"))))))

(ert-deftest org-mcp-test-uri-relative-allowed-rejects-other-file ()
  "Relative allowed entry does not accidentally allow a sibling file."
  (org-mcp-test--with-temp-org-files
      ((allowed-file "* Allowed\n")
       (forbidden-file "* Forbidden\n"))
    (let* ((dir (file-name-directory allowed-file))
           (allowed-name (file-name-nondirectory allowed-file))
           (org-directory dir)
           ;; Only the relative allowed-name is in the list.
           (org-mcp-allowed-files (list allowed-name))
           (uri (format "%s#Forbidden" forbidden-file)))
      (should-error (org-mcp-test--call-read-headline uri)))))

(ert-deftest org-mcp-test-allowed-files-fn-returns-configured ()
  "`org-mcp-allowed-files' (function) returns the variable when set."
  (let ((org-mcp-allowed-files '("a.org" "b.org"))
        (org-agenda-files '("never-used.org")))
    (should (equal (org-mcp-allowed-files) '("a.org" "b.org")))))

(ert-deftest org-mcp-test-allowed-files-fn-falls-back-to-agenda ()
  "`org-mcp-allowed-files' (function) falls back to `org-agenda-files' when nil."
  (org-mcp-test--with-temp-org-files
      ((agenda-file "* Heading\n"))
    (let ((org-mcp-allowed-files nil)
          (org-agenda-files (list agenda-file)))
      (should (equal (org-mcp-allowed-files) (list agenda-file))))))

(ert-deftest org-mcp-test-fallback-uri-tool-uses-agenda-files ()
  "URI tool calls work when allowed list is nil and `org-agenda-files' is set."
  (org-mcp-test--with-temp-org-files
      ((agenda-file "* Heading\nBody"))
    (let ((org-mcp-allowed-files nil)
          (org-agenda-files (list agenda-file)))
      (let* ((uri (format "%s#Heading" agenda-file))
             (result (org-mcp-test--call-read-headline uri)))
        (should (string= result "* Heading\nBody"))))))

(ert-deftest org-mcp-test-fallback-respects-relative-agenda-entries ()
  "Fallback to `org-agenda-files' resolves relative entries against `org-directory'."
  (org-mcp-test--with-temp-org-files
      ((agenda-file "* Heading\nBody"))
    (let* ((dir (file-name-directory agenda-file))
           (name (file-name-nondirectory agenda-file))
           (org-mcp-allowed-files nil)
           (org-directory dir)
           (org-agenda-files (list name)))
      (let* ((uri (format "%s#Heading" agenda-file))
             (result (org-mcp-test--call-read-headline uri)))
        (should (string= result "* Heading\nBody"))))))

(ert-deftest org-mcp-test-fallback-explicit-overrides-agenda ()
  "When `org-mcp-allowed-files' is set, `org-agenda-files' is ignored."
  (org-mcp-test--with-temp-org-files
      ((allowed "* Allowed\n")
       (agenda-only "* AgendaOnly\n"))
    (let ((org-mcp-allowed-files (list allowed))
          (org-agenda-files (list allowed agenda-only)))
      ;; Allowed file is reachable
      (let ((uri (format "%s#Allowed" allowed)))
        (should (string= (org-mcp-test--call-read-headline uri)
                         "* Allowed")))
      ;; A file only in `org-agenda-files' must NOT be reachable.
      (let ((uri (format "%s#AgendaOnly" agenda-only)))
        (should-error (org-mcp-test--call-read-headline uri))))))

;; Scope override

(defun org-mcp-test--write-file (dir name content)
  "Write CONTENT to file NAME in DIR and return the file's path.
File name handlers are bypassed, so a `.gpg' name is written as
plain text rather than encrypted."
  (let ((file (expand-file-name name dir))
        (file-name-handler-alist nil))
    (make-directory (file-name-directory file) t)
    (write-region content nil file nil 'silent)
    file))

(defun org-mcp-test--read-file-raw (file)
  "Return the contents of FILE, bypassing file name handlers.
A `.gpg' file is read as written, without an attempt to decrypt it."
  (let ((file-name-handler-alist nil))
    (org-mcp-test--read-file file)))

(defmacro org-mcp-test--with-scope-dirs (override &rest body)
  "Run BODY with org-mcp enabled, two temporary directories and OVERRIDE.
Binds `root' and `outside' to fresh directories, as directory
names, and `allowed' to an Org file in neither of them, which is
the only allowed file.  OVERRIDE is evaluated with these bindings
in place and becomes `org-mcp-file-scope-override'.  Everything is
deleted afterwards."
  (declare (indent 1) (debug t))
  `(let* ((root
           (file-name-as-directory
            (make-temp-file "org-mcp-root-" t)))
          (outside
           (file-name-as-directory
            (make-temp-file "org-mcp-outside-" t)))
          (allowed
           (make-temp-file "org-mcp-allowed-" nil ".org" "* Allowed\n")))
     (unwind-protect
         (let ((org-mcp-allowed-files (list allowed))
               (org-mcp-file-scope-override ,override))
           (org-mcp-test--with-enabled
             ,@body))
       (let ((file-name-handler-alist nil))
         (delete-directory root t)
         (delete-directory outside t)
         (delete-file allowed)))))

(defmacro org-mcp-test--with-remote-probe (ops-var &rest body)
  "Run BODY with a fake remote method, collecting file operations in OPS-VAR.
File names starting with `org-mcp-test--remote-prefix' are remote:
the handler answers `file-remote-p' and records every other
operation in OPS-VAR before failing it, since that is where a TRAMP
method could open a connection.  Like TRAMP, it expands an absolute
local name itself, which Emacs routes to it only because
`default-directory' is remote."
  (declare (indent 1) (debug t))
  `(let* ((,ops-var nil)
          (file-name-handler-alist
           (cons
            (cons
             (concat "\\`" (regexp-quote org-mcp-test--remote-prefix))
             (lambda (operation &rest args)
               (cond
                ((eq operation 'file-remote-p)
                 org-mcp-test--remote-prefix)
                ((and (eq operation 'expand-file-name)
                      (file-name-absolute-p (car args))
                      (not
                       (string-prefix-p
                        org-mcp-test--remote-prefix (car args))))
                 (let ((default-directory "/"))
                   (expand-file-name (car args) "/")))
                (t
                 (push operation ,ops-var)
                 (error "Remote file operation: %s" operation)))))
            file-name-handler-alist)))
     ,@body))

(defun org-mcp-test--call-tool-refused
    (tool-name params expected-message &optional file)
  "Call TOOL-NAME with PARAMS and assert it is refused.
The refusal arrives as a tool error or, from the resource-style
validation, as a JSON-RPC error; either way its message must match
the regexp EXPECTED-MESSAGE.  When FILE is non-nil, it must be
byte-for-byte unchanged afterwards."
  (let* ((before (and file (org-mcp-test--read-file-raw file)))
         (response
          (mcp-server-lib-process-jsonrpc-parsed
           (mcp-server-lib-create-tools-call-request tool-name 1 params)
           mcp-server-lib-ert-server-id))
         (result (alist-get 'result response))
         (message
          (if (eq (alist-get 'isError result) t)
              (alist-get 'text (aref (alist-get 'content result) 0))
            (alist-get 'message (alist-get 'error response)))))
    (should (stringp message))
    (should (string-match-p expected-message message))
    (when file
      (should (string= (org-mcp-test--read-file-raw file) before)))))

(defun org-mcp-test--assert-scope-refused (file)
  "Assert that reading and writing the Task heading in FILE is refused.
FILE holds `org-mcp-test--scope-task-content' and stays unchanged."
  (let ((uri (format "%s#Task" file)))
    (org-mcp-test--call-tool-refused
     "org-read-headline" `((uri . ,uri)) "not in allowed list")
    (org-mcp-test--call-tool-refused
     "org-update-todo-state"
     `((uri . ,uri) (current_state . "TODO") (new_state . "DONE"))
     "not in allowed list"
     file)))

(defun org-mcp-test--assert-scope-permitted (file)
  "Assert that the Task heading in FILE can be read and then written.
FILE holds `org-mcp-test--scope-task-content'; afterwards its Task
is DONE."
  (let ((uri (format "%s#Task" file)))
    (should
     (string= (org-mcp-test--call-read-headline uri) "* TODO Task\nBody"))
    (let ((result
           (org-mcp-test--call-update-todo-state uri "DONE" "TODO")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'new_state result) "DONE")))
    (org-mcp-test--verify-file-matches
     file org-mcp-test--scope-task-done-regex)))

(ert-deftest org-mcp-test-scope-override-nil-refuses-read-and-write ()
  "With the default refusal, a file outside the allowed files is unreachable."
  (org-mcp-test--with-scope-dirs nil
    (let ((out (org-mcp-test--write-file
                outside "out.org" org-mcp-test--scope-task-content))
          (in (org-mcp-test--write-file
               root "in.org" org-mcp-test--scope-task-content)))
      (org-mcp-test--assert-scope-refused out)
      (org-mcp-test--assert-scope-refused in)
      (org-mcp-test--call-tool-refused
       "org-ql-query" `((query . "(todo)") (files . ,(vector out)))
       "not in allowed list"))))

(ert-deftest org-mcp-test-scope-override-roots-permits-under-root ()
  "Under a root list, a file below a root is readable and writable."
  (org-mcp-test--with-scope-dirs (list root)
    (let ((in (org-mcp-test--write-file
               root "sub/in.org" org-mcp-test--scope-task-content)))
      (org-mcp-test--assert-scope-permitted in)
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-ql-query"
               `((query . "(done)") (files . ,(vector in)))))))
        (should (= (alist-get 'files_searched result) 1))
        (should (= (alist-get 'total result) 1)))
      ;; Nothing carries over: the allowed files are as configured.
      (should
       (equal
        (alist-get 'files (org-mcp-test--call-get-allowed-files))
        (vector allowed)))
      (should
       (= (alist-get
           'files_searched (org-mcp-test--call-ql-query "(todo)"))
          1)))))

(ert-deftest org-mcp-test-scope-override-roots-refuses-outside-root ()
  "Under a root list, a file outside every root is refused."
  (org-mcp-test--with-scope-dirs (list root)
    (let ((out (org-mcp-test--write-file
                outside "out.org" org-mcp-test--scope-task-content)))
      (org-mcp-test--assert-scope-refused out)
      (org-mcp-test--call-tool-refused
       "org-ql-query" `((query . "(todo)") (files . ,(vector out)))
       "not in allowed list"))))

(ert-deftest org-mcp-test-scope-override-roots-refuses-symlink-out-of-root ()
  "A symlink below a root that points outside every root is refused."
  (org-mcp-test--with-scope-dirs (list root)
    (let ((out (org-mcp-test--write-file
                outside "out.org" org-mcp-test--scope-task-content))
          (escape (expand-file-name "escape.org" root)))
      (make-symbolic-link out escape)
      (org-mcp-test--assert-scope-refused escape)
      (should
       (string= (org-mcp-test--read-file out)
                org-mcp-test--scope-task-content)))))

(ert-deftest org-mcp-test-scope-override-roots-resolves-symlinked-root ()
  "A root that is itself a symlink permits the files of its target."
  (let ((link-root
         (make-temp-name
          (expand-file-name "org-mcp-link-root-"
                            temporary-file-directory))))
    (unwind-protect
        (org-mcp-test--with-scope-dirs (list link-root)
          (make-symbolic-link (directory-file-name root) link-root)
          (org-mcp-test--assert-scope-permitted
           (org-mcp-test--write-file
            root "in.org" org-mcp-test--scope-task-content)))
      (delete-file link-root))))

(ert-deftest org-mcp-test-scope-override-t-permits-any-org-file ()
  "With t, an Org file anywhere is readable and writable."
  (org-mcp-test--with-scope-dirs t
    (org-mcp-test--assert-scope-permitted
     (org-mcp-test--write-file
      outside "out.org" org-mcp-test--scope-task-content))))

(ert-deftest org-mcp-test-scope-override-t-extensions ()
  "With t, `.org_archive' is reachable; `.txt', `.org.gpg' and directories not."
  (org-mcp-test--with-scope-dirs t
    (let ((archive (org-mcp-test--write-file
                    outside "old.org_archive"
                    org-mcp-test--scope-task-content))
          (txt (org-mcp-test--write-file
                outside "notes.txt" org-mcp-test--scope-task-content))
          (gpg (org-mcp-test--write-file
                outside "secret.org.gpg"
                org-mcp-test--scope-task-content))
          (dir (expand-file-name "dir.org" outside)))
      (make-directory dir)
      (should
       (string= (org-mcp-test--call-read-headline archive)
                org-mcp-test--scope-task-content))
      (org-mcp-test--assert-scope-refused txt)
      (org-mcp-test--assert-scope-refused gpg)
      (org-mcp-test--call-tool-refused
       "org-read-headline" `((uri . ,dir)) "not in allowed list"))))

(defun org-mcp-test--remote-spellings ()
  "Return names of one remote file under the fake remote method.
The first is remote as written; the others become remote only once
`..', `.' or `~' are expanded, or once the `/:' quote is removed."
  (let ((method-and-host (substring org-mcp-test--remote-prefix 1))
        (home-depth
         (length (split-string (expand-file-name "~") "/" t))))
    (list
     (concat org-mcp-test--remote-prefix "~/x.org")
     (concat "/tmp/../" method-and-host "/x.org")
     (concat "/./" method-and-host "/x.org")
     (concat "/:" org-mcp-test--remote-prefix "/x.org")
     (concat
      "~/" (apply #'concat (make-list home-depth "../"))
      method-and-host "/x.org"))))

(ert-deftest org-mcp-test-scope-override-refuses-remote-path ()
  "A remote path is refused before any operation on it, whatever the setting.
This holds for every spelling in `org-mcp-test--remote-spellings',
through a read, a write, the path#outline form and a query."
  (dolist (override '(nil t))
    (org-mcp-test--with-scope-dirs override
      (org-mcp-test--with-remote-probe ops
        (dolist (remote (org-mcp-test--remote-spellings))
          (let ((outline (concat remote "#Task")))
            (when (string-prefix-p "/" remote)
              (org-mcp-test--call-tool-refused
               "org-read-headline" `((uri . ,remote))
               "Remote or quoted paths are not supported"))
            (org-mcp-test--call-tool-refused
             "org-read-headline" `((uri . ,outline))
             "Remote or quoted paths are not supported")
            (org-mcp-test--call-tool-refused
             "org-update-todo-state"
             `((uri . ,outline) (new_state . "DONE"))
             "Remote or quoted paths are not supported")
            (org-mcp-test--call-tool-refused
             "org-read-outline" `((file . ,remote)) "not in allowed list")
            (org-mcp-test--assert-files-refused
             (vector remote) "not in allowed list")))
        ;; A remote directory is never walked.
        (org-mcp-test--assert-files-refused
         (vector (concat org-mcp-test--remote-prefix "/dir/"))
         "not in allowed list")
        ;; A relative name inherits a remote `default-directory'.
        (let ((default-directory
               (concat org-mcp-test--remote-prefix "/dir/")))
          (org-mcp-test--assert-files-refused
           ["x.org"] "not in allowed list")
          (org-mcp-test--assert-files-refused ["."] "not in allowed list"))
        (should (null ops))))))

(ert-deftest org-mcp-test-scope-override-refuses-symlink-to-remote ()
  "A local symlink whose target is remote is refused without following it."
  (dolist (kind '(nil roots t))
    (org-mcp-test--with-scope-dirs (if (eq kind 'roots)
                                       (list root)
                                     kind)
      (let ((link (expand-file-name "link.org" root)))
        (let ((file-name-handler-alist nil))
          (make-symbolic-link
           (concat org-mcp-test--remote-prefix "/x.org") link))
        (org-mcp-test--with-remote-probe ops
          (org-mcp-test--call-tool-refused
           "org-read-headline" `((uri . ,link)) "not in allowed list")
          (org-mcp-test--call-tool-refused
           "org-update-todo-state"
           `((uri . ,(concat link "#Task")) (new_state . "DONE"))
           "not in allowed list")
          (org-mcp-test--call-tool-refused
           "org-ql-query" `((query . "(todo)") (files . ,(vector link)))
           "not in allowed list")
          (should (null ops)))))))

(ert-deftest org-mcp-test-scope-override-checks-quoted-name-as-visited ()
  "A `/:'-quoted name is checked as the file it opens.
With t, a `.org' symlink to a `.txt' or an `.org.gpg' file is
refused for a read and for writes when its name is quoted, when it
becomes quoted only once `..' is expanded, and when another link
reaches it through a quoted target.  A quoted name of a real `.org'
file stays reachable."
  (org-mcp-test--with-scope-dirs t
    (dolist (name '("rc.txt" "secret.org.gpg"))
      (let* ((target
              (org-mcp-test--write-file
               outside name org-mcp-test--scope-task-content))
             (link (expand-file-name (concat name ".org") root))
             (via (expand-file-name (concat "via-" name ".org") root)))
        (let ((file-name-handler-alist nil))
          (make-symbolic-link target link)
          (make-symbolic-link (concat "/:" link) via))
        (pcase-dolist (`(,path . ,message)
                       `((,(concat "/:" link) . "not in allowed list")
                         (,(concat "/tmp/../:" link)
                          . "Remote or quoted paths are not supported")
                         (,via . "not in allowed list")))
          (org-mcp-test--call-tool-refused
           "org-read-headline" `((uri . ,path)) message target)
          (org-mcp-test--call-tool-refused
           "org-update-todo-state"
           `((uri . ,(concat path "#Task"))
             (current_state . "TODO")
             (new_state . "DONE"))
           message target)
          (org-mcp-test--call-tool-refused
           "org-add-todo"
           `((title . "New")
             (todo_state . "TODO")
             (body . nil)
             (parent_uri . ,(concat path "#")))
           message target))))
    (org-mcp-test--assert-scope-permitted
     (concat
      "/:"
      (org-mcp-test--write-file
       root "real.org" org-mcp-test--scope-task-content)))))

(ert-deftest org-mcp-test-scope-override-ignores-remote-roots ()
  "A remote root, or a relative one under a remote `org-directory', permits nothing."
  (org-mcp-test--with-scope-dirs (list
                                  (concat
                                   org-mcp-test--remote-prefix "/srv/")
                                  "notes")
    (let ((out (org-mcp-test--write-file
                outside "out.org" org-mcp-test--scope-task-content)))
      (org-mcp-test--with-remote-probe ops
        (let ((org-directory
               (concat org-mcp-test--remote-prefix "/org/")))
          (should
           (equal
            (org-mcp-test--call-get-allowed-files)
            `((files . ,(vector allowed))
              (override_allowed . :json-false))))
          (org-mcp-test--call-tool-refused
           "org-read-headline" `((uri . ,(concat out "#Task")))
           "not in allowed list"))
        (should (null ops))))))

(ert-deftest org-mcp-test-scope-override-bare-id-stays-in-allowed-files ()
  "An ID alone never takes the override; naming its file does."
  (org-mcp-test--with-scope-dirs t
    (let ((out (org-mcp-test--write-file
                outside "out.org"
                org-mcp-test--scope-task-with-id-content)))
      (org-mcp-test--with-id-tracking
          (list allowed)
          `((,org-mcp-test--content-with-id-id . ,out))
        (org-mcp-test--call-tool-refused
         "org-read-headline" `((uri . ,org-mcp-test--content-with-id-id))
         "not in allowed list")
        (should
         (string-prefix-p
          "* TODO Task"
          (org-mcp-test--call-read-headline (format "%s#Task" out))))))))

(ert-deftest org-mcp-test-allowed-files-directory-entry-does-not-widen ()
  "A directory among the allowed files makes no file under it reachable."
  (org-mcp-test--with-scope-dirs nil
    (let* ((inner (org-mcp-test--write-file
                   root "inner.org" org-mcp-test--scope-tagged-content))
           (org-mcp-allowed-files (list allowed root))
           (org-tag-alist nil)
           (org-tag-persistent-alist nil))
      (should
       (equal
        (alist-get 'files (org-mcp-test--call-get-allowed-files))
        (vector allowed)))
      (should-not
       (member "innertag"
               (append (org-mcp-test--call-get-tag-candidates) nil)))
      (let ((result (org-mcp-test--call-ql-query "(todo)")))
        (should (= (alist-get 'files_searched result) 1))
        (should (= (alist-get 'total result) 0)))
      (org-mcp-test--call-tool-refused
       "org-read-headline" `((uri . ,inner)) "not in allowed list")
      (org-mcp-test--call-tool-refused
       "org-read-headline" `((uri . ,root)) "not in allowed list"))))

(ert-deftest org-mcp-test-tool-get-allowed-files-reports-override-policy ()
  "org-get-allowed-files reports whether overriding is permitted and where."
  (let ((org-mcp-allowed-files nil)
        (org-agenda-files nil)
        (org-directory "/home/user/org/"))
    (org-mcp-test--with-enabled
      (let ((org-mcp-file-scope-override nil))
        (should
         (equal
          (org-mcp-test--call-get-allowed-files)
          '((files . []) (override_allowed . :json-false)))))
      (let ((org-mcp-file-scope-override t))
        (should
         (equal
          (org-mcp-test--call-get-allowed-files)
          '((files . []) (override_allowed . t)))))
      ;; Neither t nor a list: the gate refuses, and so does the report.
      (let ((org-mcp-file-scope-override "~/decisions"))
        (should
         (equal
          (org-mcp-test--call-get-allowed-files)
          '((files . []) (override_allowed . :json-false)))))
      (let ((org-mcp-file-scope-override '("/srv/decisions" "notes")))
        (should
         (equal
          (org-mcp-test--call-get-allowed-files)
          '((files . [])
            (override_allowed . t)
            (override_roots
             .
             ["/srv/decisions" "/home/user/org/notes"]))))))))

;; Query tools taking a file set

(defun org-mcp-test--write-set-file (dir name title)
  "Write file NAME in DIR from `org-mcp-test--file-set-template'.
TITLE is the heading's title and tag.  Returns the file's path."
  (org-mcp-test--write-file
   dir name (format org-mcp-test--file-set-template title title)))

(defun org-mcp-test--call-with-files (tool params files)
  "Call TOOL with PARAMS and return the parsed result.
FILES, when non-nil, is added to PARAMS as the `files' parameter."
  (json-read-from-string
   (mcp-server-lib-ert-call-tool
    tool (append params (and files `((files . ,files)))))))

(defun org-mcp-test--scan-files (&optional files)
  "Return the sorted titles the tools taking `files' find in FILES.
FILES, when non-nil, is the `files' parameter of org-ql-query,
org-get-tag-candidates and org-clock-find-dangling.  Every file
searched holds one `org-mcp-test--file-set-template' heading, so the
three tools must agree: the titles the query matches, one per file
searched, the tags beyond the configured ones, and the headings
holding an open clock."
  (let* ((org-tag-alist nil)
         (org-tag-persistent-alist nil)
         (query
          (org-mcp-test--call-with-files
           "org-ql-query" '((query . "(todo)")) files))
         (titles
          (sort (mapcar (lambda (match) (alist-get 'title match))
                        (alist-get 'matches query))
                #'string<))
         (tags
          (org-mcp-test--call-with-files
           "org-get-tag-candidates" nil files))
         (clocks
          (org-mcp-test--call-with-files
           "org-clock-find-dangling" nil files)))
    (should (= (alist-get 'files_searched query) (length titles)))
    (should (equal (append (alist-get 'tags tags) nil) titles))
    (should
     (equal (sort (mapcar (lambda (clock) (alist-get 'heading clock))
                          (alist-get 'open_clocks clocks))
                  #'string<)
            titles))
    titles))

(defun org-mcp-test--assert-files-refused (files message)
  "Assert that each tool taking `files' refuses FILES.
The refusal of org-ql-query, org-get-tag-candidates and
org-clock-find-dangling must match the regexp MESSAGE."
  (pcase-dolist (`(,tool . ,params)
                 '(("org-ql-query" (query . "(todo)"))
                   ("org-get-tag-candidates")
                   ("org-clock-find-dangling")))
    (org-mcp-test--call-tool-refused
     tool (append params `((files . ,files))) message)))

(defun org-mcp-test--refused-path-regexp (path)
  "Return a regexp matching the whole refusal message naming PATH."
  (concat "\\`'" (regexp-quote path)
          "': the referenced file not in allowed list\\'"))

(defmacro org-mcp-test--with-scope-dirs-and-gtd (override &rest body)
  "Run BODY as `org-mcp-test--with-scope-dirs' does, OVERRIDE included.
The GTD query tools are registered as well, each matching every
TODO heading."
  (declare (indent 1) (debug t))
  `(let ((org-mcp-query-inbox-fn (lambda () '(todo)))
         (org-mcp-query-next-fn (lambda (&optional _tag-filter) '(todo)))
         (org-mcp-query-backlog-fn
          (lambda (&optional _tag-filter) '(todo)))
         (org-mcp-query-sort-fn nil))
     (org-mcp-test--with-scope-dirs ,override
       ,@body)))

(defun org-mcp-test--gtd-titles ()
  "Return the sorted titles each GTD query tool matches.
The three tools match every TODO heading, so they must agree."
  (let ((results
         (mapcar
          (lambda (tool)
            (sort (mapcar
                   (lambda (match) (alist-get 'title match))
                   (alist-get
                    'matches
                    (json-read-from-string
                     (mcp-server-lib-ert-call-tool tool nil))))
                  #'string<))
          '("query-inbox" "query-next" "query-backlog"))))
    (should (equal (nth 1 results) (car results)))
    (should (equal (nth 2 results) (car results)))
    (car results)))

(ert-deftest org-mcp-test-file-set-narrows-allowed-files ()
  "A named set inside the allowed files narrows each tool to it."
  (org-mcp-test--with-scope-dirs nil
    (let* ((alpha (org-mcp-test--write-set-file outside "alpha.org" "alpha"))
           (beta (org-mcp-test--write-set-file outside "beta.org" "beta"))
           (org-mcp-allowed-files (list alpha beta)))
      (should (equal (org-mcp-test--scan-files) '("alpha" "beta")))
      (should (equal (org-mcp-test--scan-files (vector alpha)) '("alpha")))
      (should (equal (org-mcp-test--scan-files (vector beta)) '("beta"))))))

(ert-deftest org-mcp-test-file-set-replaces-allowed-files ()
  "A named set replaces the allowed files rather than adding to them."
  (org-mcp-test--with-scope-dirs t
    (let* ((alpha (org-mcp-test--write-set-file root "alpha.org" "alpha"))
           (beta (org-mcp-test--write-set-file outside "beta.org" "beta"))
           (org-mcp-allowed-files (list alpha)))
      (should (equal (org-mcp-test--scan-files) '("alpha")))
      (should (equal (org-mcp-test--scan-files (vector beta)) '("beta")))
      (should
       (equal (org-mcp-test--scan-files (vector beta alpha))
              '("alpha" "beta")))
      ;; Named directly and through its directory, a file counts once.
      (should
       (equal (org-mcp-test--scan-files (vector beta outside))
              '("beta"))))))

(ert-deftest org-mcp-test-file-set-override-nil-refuses-outside ()
  "With the default refusal, a named set reaches nothing outside the allowed files."
  (org-mcp-test--with-scope-dirs nil
    (let ((in (org-mcp-test--write-set-file root "in.org" "in"))
          (out (org-mcp-test--write-set-file outside "out.org" "out")))
      (org-mcp-test--assert-files-refused (vector out) "not in allowed list")
      (org-mcp-test--assert-files-refused (vector in) "not in allowed list")
      ;; One refused entry refuses the whole set.
      (org-mcp-test--assert-files-refused
       (vector allowed out) (org-mcp-test--refused-path-regexp out))
      ;; A directory holding no allowed file is refused unread.
      (org-mcp-test--assert-files-refused
       (vector root) (org-mcp-test--refused-path-regexp root)))))

(ert-deftest org-mcp-test-file-set-override-roots ()
  "Under a root list, a named file or directory must lie under a root.
A file found under a root that links out of every root fails the
call, and the refusal names it by the path the call reaches it by,
not by the file it resolves to."
  (org-mcp-test--with-scope-dirs (list root)
    (let ((in (org-mcp-test--write-set-file root "sub/in.org" "in"))
          (out (org-mcp-test--write-set-file outside "out.org" "out"))
          (link (expand-file-name "link" outside)))
      (should (equal (org-mcp-test--scan-files (vector in)) '("in")))
      (should (equal (org-mcp-test--scan-files (vector root)) '("in")))
      (should
       (equal (org-mcp-test--scan-files (vector (file-name-directory in)))
              '("in")))
      (org-mcp-test--assert-files-refused
       (vector out) (org-mcp-test--refused-path-regexp out))
      ;; A directory outside every root holding no allowed file.
      (org-mcp-test--assert-files-refused
       (vector outside) (org-mcp-test--refused-path-regexp outside))
      ;; A directory holding a root does not lie under it, so it
      ;; stands for the allowed files under it and is not walked.
      (let ((result
             (org-mcp-test--call-with-files
              "org-ql-query" '((query . "(todo)"))
              (vector (file-name-directory (directory-file-name root))))))
        (should (= (alist-get 'files_searched result) 1))
        (should (= (alist-get 'total result) 0)))
      ;; A symlink to a root is walked as that root.
      (make-symbolic-link (directory-file-name root) link)
      (should (equal (org-mcp-test--scan-files (vector link)) '("in")))
      (make-symbolic-link out (expand-file-name "sub/escape.org" root))
      (org-mcp-test--assert-files-refused
       (vector link)
       (org-mcp-test--refused-path-regexp (concat link "/sub/escape.org"))))))

(ert-deftest org-mcp-test-file-set-directory-without-override ()
  "A directory the setting does not permit stands for its allowed files.
It is not read, so an Org file under it that is not allowed stays
out of the set, and it is refused when no allowed file lies under
it.  This holds under nil and, for a directory outside every root,
under a root list."
  (dolist (override '(nil roots))
    (org-mcp-test--with-scope-dirs (and (eq override 'roots)
                                        (list root))
      (let* ((alpha (org-mcp-test--write-set-file outside "alpha.org" "alpha"))
             (beta (org-mcp-test--write-set-file outside "sub/beta.org" "beta"))
             (gamma (org-mcp-test--write-set-file root "gamma.org" "gamma"))
             (org-mcp-allowed-files (list alpha beta gamma))
             (empty (file-name-as-directory
                     (expand-file-name "empty" outside))))
        (org-mcp-test--write-set-file outside "stray.org" "stray")
        (make-directory empty)
        (should (equal (org-mcp-test--scan-files (vector outside))
                       '("alpha" "beta")))
        (should (equal (org-mcp-test--scan-files
                        (vector (file-name-directory beta)))
                       '("beta")))
        (org-mcp-test--assert-files-refused
         (vector empty) (org-mcp-test--refused-path-regexp empty))
        ;; An unreadable directory is not read either.
        (set-file-modes outside #o300)
        (unwind-protect
            (should (equal (org-mcp-test--scan-files (vector outside))
                           '("alpha" "beta")))
          (set-file-modes outside #o700))))))

(ert-deftest org-mcp-test-file-set-walks-directory-recursively ()
  "A named directory is searched recursively for Org files.
Hidden files and directories are skipped, and so are an archive and
other files, as Org skips them in a directory of `org-agenda-files'.
A directory named like an Org file is searched, not read as a file.
Symlinks to directories, one of them forming a cycle, are not
followed.  A subdirectory that cannot be read is skipped, and so is
an Org name that is no regular file: a dangling symlink, a FIFO."
  (org-mcp-test--with-scope-dirs t
    (let ((tree (file-name-as-directory (expand-file-name "tree" outside)))
          (locked (expand-file-name "locked" outside)))
      (org-mcp-test--write-set-file tree "top.org" "top")
      (org-mcp-test--write-set-file tree "a/b/deep.org" "deep")
      (org-mcp-test--write-set-file tree "old.org_archive" "archive")
      (org-mcp-test--write-set-file tree "dir.org/inner.org" "inner")
      (org-mcp-test--write-set-file tree ".hidden/secret.org" "secret")
      (org-mcp-test--write-set-file tree "notes.txt" "notes")
      (org-mcp-test--write-set-file tree "locked/shut.org" "shut")
      (org-mcp-test--write-set-file root "elsewhere.org" "elsewhere")
      (let ((file-name-handler-alist nil))
        ;; An Emacs lock file: hidden, and a dangling symlink.
        (make-symbolic-link
         "user@host.1234:1" (expand-file-name "a/.#deep.org" tree))
        (make-symbolic-link
         "missing.org" (expand-file-name "dangling.org" tree))
        (make-symbolic-link
         (directory-file-name tree) (expand-file-name "a/loop" tree))
        (make-symbolic-link
         (directory-file-name root) (expand-file-name "linked" tree)))
      (should
       (= 0
          (call-process
           "mkfifo" nil nil nil (expand-file-name "pipe.org" tree))))
      (set-file-modes (expand-file-name "locked" tree) #o000)
      (unwind-protect
          (should
           (equal (org-mcp-test--scan-files (vector tree))
                  '("deep" "inner" "top")))
        (set-file-modes (expand-file-name "locked" tree) #o700))
      ;; A named directory that cannot be read is refused by the name
      ;; the call gave it.
      (make-directory locked)
      (set-file-modes locked #o000)
      (unwind-protect
          (org-mcp-test--assert-files-refused
           (vector locked)
           (concat "\\`Cannot read directory: " (regexp-quote locked) "\\'"))
        (set-file-modes locked #o700)))))

(defun org-mcp-test--cap-regexp (cap)
  "Return a regexp matching the refusal of a set over CAP."
  (format "more than %d files and directories.*org-mcp-max-files" cap))

(ert-deftest org-mcp-test-file-set-cap-is-an-error ()
  "A named set over `org-mcp-max-files' is an error, never a partial result.
The limit counts every file in the set and every directory searched
for it, the named ones included."
  (org-mcp-test--with-scope-dirs t
    (let* ((tree (file-name-as-directory (expand-file-name "tree" outside)))
           (a (org-mcp-test--write-set-file tree "a.org" "a"))
           (sub (file-name-as-directory (expand-file-name "sub" tree))))
      (org-mcp-test--write-set-file sub "b.org" "b")
      (org-mcp-test--write-set-file sub "c.org" "c")
      ;; tree, a.org, sub, b.org and c.org.
      (let ((org-mcp-max-files 5))
        (should
         (equal (org-mcp-test--scan-files (vector tree)) '("a" "b" "c")))
        ;; However often it is named, a file counts once.
        (should
         (equal (org-mcp-test--scan-files (vector a tree a))
                '("a" "b" "c"))))
      (let ((org-mcp-max-files 4))
        (org-mcp-test--assert-files-refused
         (vector tree) (org-mcp-test--cap-regexp 4)))
      ;; sub, b.org and c.org; the count runs across entries.
      (let ((org-mcp-max-files 3))
        (should (equal (org-mcp-test--scan-files (vector sub)) '("b" "c")))
        (org-mcp-test--assert-files-refused
         (vector a sub) (org-mcp-test--cap-regexp 3))))))

(ert-deftest org-mcp-test-file-set-cap-counts-empty-directories ()
  "Directories count toward `org-mcp-max-files' even when they hold no file.
The walk stops as soon as the count passes the limit: a file it would
refuse, sorting after the directories, is then never reached."
  (org-mcp-test--with-scope-dirs (list root)
    (let ((out (org-mcp-test--write-set-file outside "out.org" "out"))
          (escape (expand-file-name "zz-escape.org" root)))
      (dotimes (i 30)
        (make-directory (expand-file-name (format "d%02d" i) root)))
      ;; root and its 30 subdirectories.
      (let ((org-mcp-max-files 31))
        (should (equal (org-mcp-test--scan-files (vector root)) nil)))
      (let ((org-mcp-max-files 30))
        (org-mcp-test--assert-files-refused
         (vector root) (org-mcp-test--cap-regexp 30)))
      (make-symbolic-link out escape)
      (let ((org-mcp-max-files 31))
        (org-mcp-test--assert-files-refused
         (vector root) (org-mcp-test--refused-path-regexp escape)))
      (let ((org-mcp-max-files 30))
        (org-mcp-test--assert-files-refused
         (vector root) (org-mcp-test--cap-regexp 30))))))

(ert-deftest org-mcp-test-file-set-ignores-agenda-restriction ()
  "An agenda restriction never widens what the set-scanning tools reach.
While the agenda is restricted, as by `C-c a <', the function
`org-agenda-files' returns the file of the restriction.  No tool
working on a set of files reaches it, whether the call names files,
names an empty set or names none, and no GTD query does."
  (org-mcp-test--with-scope-dirs-and-gtd t
    (let* ((alpha (org-mcp-test--write-set-file root "alpha.org" "alpha"))
           (beta (org-mcp-test--write-set-file root "beta.org" "beta"))
           (out (org-mcp-test--write-set-file outside "out.org" "out"))
           (empty (file-name-as-directory (expand-file-name "empty" root)))
           (org-mcp-allowed-files (list alpha))
           (restriction (get 'org-agenda-files 'org-restrict)))
      (make-directory empty)
      (unwind-protect
          (progn
            (put 'org-agenda-files 'org-restrict (list out))
            (should (equal (org-mcp-test--scan-files) '("alpha")))
            (should
             (equal (org-mcp-test--scan-files (vector beta)) '("beta")))
            (should (equal (org-mcp-test--scan-files (vector empty)) nil))
            (should (equal (org-mcp-test--gtd-titles) '("alpha"))))
        (put 'org-agenda-files 'org-restrict restriction)))))

(ert-deftest org-mcp-test-file-set-does-not-carry-over ()
  "A named set lasts for its call only, also when the call fails.
Later calls naming no files, GTD queries included, run over the
allowed files, and org-get-allowed-files reports them unchanged."
  (org-mcp-test--with-scope-dirs-and-gtd t
    (let* ((alpha (org-mcp-test--write-set-file root "alpha.org" "alpha"))
           (beta (org-mcp-test--write-set-file outside "beta.org" "beta"))
           (org-mcp-allowed-files (list alpha))
           (check
            (lambda ()
              ;; First the tool that binds no set of its own.
              (should
               (equal
                (alist-get 'files (org-mcp-test--call-get-allowed-files))
                (vector alpha)))
              (should (equal (org-mcp-test--gtd-titles) '("alpha")))
              (should (equal (org-mcp-test--scan-files) '("alpha"))))))
      (should (equal (org-mcp-test--scan-files (vector beta)) '("beta")))
      (funcall check)
      ;; A call failing while its set is in force.
      (org-mcp-test--call-tool-refused
       "org-ql-query"
       `((query . "(no-such-predicate)") (files . ,(vector beta)))
       "Org-ql query error")
      (funcall check)
      ;; A call failing while its set is built.
      (let ((org-mcp-max-files 0))
        (org-mcp-test--assert-files-refused
         (vector beta) "org-mcp-max-files"))
      (funcall check))))

(ert-deftest org-mcp-test-gtd-queries-refuse-files ()
  "The GTD queries and the clock state tool refuse a `files' parameter.
They declare none, and mcp-server-lib refuses a parameter a tool does
not declare before the tool runs."
  (org-mcp-test--with-scope-dirs-and-gtd t
    (let* ((alpha (org-mcp-test--write-set-file root "alpha.org" "alpha"))
           (beta (org-mcp-test--write-set-file outside "beta.org" "beta"))
           (org-mcp-allowed-files (list alpha)))
      (dolist (tool '("query-inbox" "query-next" "query-backlog"
                      "org-clock-get-active"))
        (org-mcp-test--call-tool-refused
         tool `((files . ,(vector beta))) "Unexpected parameter: files"))
      (should (equal (org-mcp-test--gtd-titles) '("alpha"))))))

(ert-deftest org-mcp-test-query-tools-never-search-current-buffer ()
  "An empty set of files searches nothing, not the current buffer.
Given no files, `org-ql-select' searches the current buffer.  This
covers a named directory holding no Org file, and allowed files of
which none exists, for the GTD queries too."
  (org-mcp-test--with-scope-dirs-and-gtd t
    (let ((buffer (get-buffer-create "org-mcp-test-current")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (insert (format org-mcp-test--file-set-template "here" "here"))
            (should (equal (org-mcp-test--scan-files (vector outside)) nil))
            (let ((org-mcp-allowed-files
                   (list (expand-file-name "missing.org" outside))))
              (should (equal (org-mcp-test--scan-files) nil))
              (should (equal (org-mcp-test--gtd-titles) nil))))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-file-set-validates-parameter ()
  "`files' is a non-empty array of absolute paths, or a single path."
  (org-mcp-test--with-scope-dirs t
    (let ((beta (org-mcp-test--write-set-file outside "beta.org" "beta")))
      (should (equal (org-mcp-test--scan-files beta) '("beta")))
      (org-mcp-test--assert-files-refused [] "non-empty array of paths")
      (org-mcp-test--assert-files-refused [1] "non-empty array of paths")
      (let ((default-directory outside))
        (org-mcp-test--assert-files-refused
         ["beta.org"] "not in allowed list")
        (org-mcp-test--assert-files-refused ["."] "not in allowed list")))))

(defmacro org-mcp-test--with-add-todo-setup
    (file-var initial-content &rest body)
  "Helper for org-add-todo test.
Sets up FILE-VAR with INITIAL-CONTENT and standard org configuration.
Executes BODY with org-mcp enabled and standard variables set."
  (declare (indent 2))
  `(org-mcp-test--with-temp-org-files
       ((,file-var ,initial-content))
     (let ((org-todo-keywords
            '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
           (org-tag-alist '("work" "personal" "urgent"))
           (org-id-locations-file nil))
       ,@body)))

(defun org-mcp-test--assert-add-todo-rejects-body-headline
    (initial-content parent-headline body-with-headline)
  "Test that adding TODO with BODY-WITH-HEADLINE is rejected.
INITIAL-CONTENT is the initial file content.
PARENT-HEADLINE is the parent headline path (empty string for top-level).
BODY-WITH-HEADLINE is the body containing invalid headline."
  (org-mcp-test--with-add-todo-setup test-file initial-content
    (let ((parent-uri
           (format "%s#%s" test-file parent-headline)))
      (org-mcp-test--call-add-todo-expecting-error
       test-file "Test Task" "TODO" '("work") body-with-headline parent-uri))))

(ert-deftest org-mcp-test-file-resource-template-in-list ()
  "The only resource template is org://{link}; an org-outline:// URI is not found."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (should
     (equal
      (mapcar
       (lambda (template) (alist-get 'uriTemplate template))
       (append (mcp-server-lib-ert-get-resource-templates-list) nil))
      '("org://{link}")))
    (let ((uri (concat "org-outline://" test-file)))
      (org-mcp-test--read-resource-expecting-error
       uri (format "Resource not found: %s" uri)))))

(defun org-mcp-test--assert-add-todo-invalid-title (invalid-title)
  "Assert that adding TODO with INVALID-TITLE throws an error.
Tests that the given title is rejected when creating a TODO."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "%s#" test-file)))
      (org-mcp-test--call-add-todo-expecting-error
       test-file invalid-title "TODO" nil nil parent-uri))))

(defun org-mcp-test--assert-rename-headline-rejected
    (initial-content headline-title new-title)
  "Assert renaming headline to NEW-TITLE is rejected.
INITIAL-CONTENT is the Org content to test with.
HEADLINE-TITLE is the current headline to rename.
NEW-TITLE is the invalid new title that should be rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file initial-content))
    (let ((resource-uri
           (format "%s#%s"
                   test-file
                   (url-hexify-string headline-title))))
      (org-mcp-test--call-rename-headline-expecting-error
       test-file resource-uri headline-title new-title))))

(ert-deftest org-mcp-test-file-resource-not-in-list-after-disable ()
  "Test that resources are unregistered after `org-mcp-disable'."
  (let ((org-mcp-allowed-files '("test.org")))
    ;; Enable then disable
    (org-mcp-enable)
    (org-mcp-disable)
    ;; Start server and check resources
    (mcp-server-lib-ert-with-server
     :tools nil
     :resources nil
     (let ((resources (mcp-server-lib-ert-get-resource-list)))
       ;; Check that the resource list is empty
       (should (= (length resources) 0))))))

(ert-deftest org-mcp-test-file-resource-read ()
  "Test that reading org:// resource returns structured JSON."
  (let ((test-content "* Test Heading\nThis is test content."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let* ((uri (format "org://%s" test-file))
             (request (mcp-server-lib-create-resources-read-request uri))
             (response-json (mcp-server-lib-process-jsonrpc request mcp-server-lib-ert-server-id))
             (response (json-parse-string response-json :object-type 'alist))
             (result (alist-get 'result response))
             (contents (alist-get 'contents result)))
        (when (alist-get 'error response)
          (error "Resource request failed: %s"
                 (alist-get 'message (alist-get 'error response))))
        (let* ((json-text (alist-get 'text (aref contents 0)))
               (data (json-parse-string json-text :object-type 'alist))
               (file (alist-get 'file data))
               (children (alist-get 'children data)))
          (should (equal file test-file))
          (should (= (length children) 1))
          (let ((child (aref children 0)))
            (should (equal (alist-get 'title child) "Test Heading"))
            (should (= (alist-get 'level child) 1))))))))

(defconst org-mcp-test--content-outline-depth
  "* First Section
Some content here.
** Subsection 1.1
More content.
** Subsection 1.2
Even more content.
* Second Section
Content of second section.
*** Deep subsection
Very deep content."
  "Two top-level sections, one with level-2 children, one with a level 3.")

(ert-deftest org-mcp-test-tool-read-outline-depth ()
  "org-read-outline returns top-level headings and their level-2 children."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-outline-depth))
    (let ((headings
           (alist-get
            'headings (org-mcp-test--call-read-outline test-file))))
      ;; Check we have the right number of top-level headings
      (should (= (length headings) 2))
      ;; Check first heading
      (let ((first (aref headings 0)))
        (should (equal (alist-get 'title first) "First Section"))
        (should (= (alist-get 'level first) 1))
        ;; Check children of first heading
        (let ((children (alist-get 'children first)))
          (should (= (length children) 2))
          (should
           (equal (alist-get 'title (aref children 0)) "Subsection 1.1"))
          (should
           (equal
            (alist-get 'title (aref children 1)) "Subsection 1.2"))))
      ;; Check second heading
      (let ((second (aref headings 1)))
        (should (equal (alist-get 'title second) "Second Section"))
        (should (= (alist-get 'level second) 1))
        ;; Deep subsection is left out (level 3 under level 1)
        (should (= (length (alist-get 'children second)) 0))))))

(ert-deftest org-mcp-test-file-not-in-allowed-list-returns-error ()
  "Test that reading a file not in allowed list returns an error."
  (org-mcp-test--with-temp-org-files
      ((allowed-file "Allowed content")
       (forbidden-file "Forbidden content"))
    (let ((org-mcp-allowed-files (list allowed-file)))
      ;; Try to read the forbidden file
      (let ((uri (format "org://%s" forbidden-file)))
        (org-mcp-test--read-resource-expecting-error
         uri
         (format "'%s': the referenced file not in allowed list" forbidden-file))))))

(ert-deftest org-mcp-test-headline-resource-not-found ()
  "Test org-read-headline tool error for non-existent headline."
  (let ((test-content "* Existing Section\nSome content."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (should-error
       (org-mcp-test--call-read-headline
        (format "%s#Nonexistent" test-file))))))

(ert-deftest org-mcp-test-headline-resource-file-with-hash ()
  "Test org-read-headline tool with # in filename."
  (org-mcp-test--with-temp-org-files
      ((file org-mcp-test--content-nested-siblings "org-mcp-test-file#"))
    ;; Test accessing the file with # encoded as %23
    (let* ((encoded-path (replace-regexp-in-string "#" "%23" file))
           (uri
            (format "%s#Parent%%20Task/First%%20Child%%2050%%25%%20Complete"
                    encoded-path))
           (result (org-mcp-test--call-read-headline uri)))
      (should
       (string=
        result
        "** First Child 50% Complete\nFirst child content.\nIt spans multiple lines.")))))

(ert-deftest org-mcp-test-headline-resource-headline-with-hash ()
  "Test org-read-headline tool with # in headline title."
  (org-mcp-test--with-temp-org-files
      ((file org-mcp-test--content-nested-siblings))
    ;; Test accessing headline with # encoded as %23
    (let* ((uri
            (format "%s#Parent%%20Task/Third%%20Child%%20%%233" file))
           (result (org-mcp-test--call-read-headline uri)))
      (should (string= result "** Third Child #3")))))

(ert-deftest
    org-mcp-test-headline-resource-file-and-headline-with-hash
    ()
  "Test org-read-headline tool with # in both filename and headline."
  (org-mcp-test--with-temp-org-files
      ((file org-mcp-test--content-nested-siblings "org-mcp-test-file#"))
    ;; Test with both file and headline containing #
    (let* ((encoded-path (replace-regexp-in-string "#" "%23" file))
           (uri
            (format "%s#Parent%%20Task/Third%%20Child%%20%%233"
                    encoded-path))
           (result (org-mcp-test--call-read-headline uri)))
      (should (string= result "** Third Child #3")))))

(ert-deftest org-mcp-test-headline-resource-path-traversal ()
  "Test that path traversal with ../ in org:// URIs is rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    ;; Test with ../ in the filename part - path won't be absolute
    (should-error
     (org-mcp-test--call-read-headline
      (format "org://../%s#Parent%%20Task"
              (file-name-nondirectory test-file))))))

(ert-deftest org-mcp-test-headline-resource-encoded-path-traversal ()
  "Test that URL-encoded path traversal in org:// URIs is rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    ;; Test with URL-encoded ../ (%2E%2E%2F) in the filename part
    ;; The encoding is NOT decoded, so %2E%2E%2F remains literal
    (should-error
     (org-mcp-test--call-read-headline
      (format "org://%%2E%%2E%%2F%s#Parent%%20Task"
              (file-name-nondirectory test-file))))))

(ert-deftest org-mcp-test-headline-resource-navigation ()
  "Test that headline navigation respects structure."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-wrong-levels))
   ;; Test accessing "Target Headline" under "First Parent"
   ;; Should get the level-2 headline, NOT the level-3 one
   ;; This SHOULD throw an error because First Parent has no such child
   (should-error
    (org-mcp-test--call-read-headline
     (format "%s#First%%20Parent/Target%%20Headline" test-file)))))

(ert-deftest org-mcp-test-headline-resource-deep-nested-path ()
  "Test navigation through a 3-level outline path."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-wrong-levels))
    (let ((result
           (org-mcp-test--call-read-headline
            (format
             "%s#Second%%20Parent/Other%%20Child/Target%%20Headline"
             test-file))))
      (should
       (string=
        result
        (concat
         "*** Target Headline\n"
         "This should NOT be found via First Parent/Target Headline path."))))))

(ert-deftest org-mcp-test-headline-resource-bare-non-toplevel-rejected ()
  "Test that a bare single-element path that only exists below level 1 errors.
`org-find-olp' resolves a single-element path at the top level only; a
title that only exists at deeper levels cannot be reached without its
parent."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-wrong-levels))
    (should-error
     (org-mcp-test--call-read-headline
      (format "%s#Target%%20Headline" test-file)))))

(ert-deftest org-mcp-test-id-resource-not-found ()
  "Test org-read-headline tool error for non-existent ID."
  (let ((test-content "* Section without ID\nNo ID here."))
    (org-mcp-test--with-id-setup test-file test-content '()
      (should-error
       (org-mcp-test--call-read-headline "org://nonexistent-id-12345")))))

(ert-deftest org-mcp-test-id-resource-file-not-allowed ()
  "Test org-read-headline tool validates file is in allowed list."
  ;; Create two files - one allowed, one not
  (org-mcp-test--with-temp-org-files
      ((allowed-file "* Allowed\n")
       (other-file
        (concat
         "* Section with ID\n"
         ":PROPERTIES:\n"
         ":ID: test-id-789\n"
         ":END:\n"
         "This file is not in allowed list.")))
    (org-mcp-test--with-id-tracking
        (list allowed-file)
        `(("test-id-789" . ,other-file))
      (should-error
       (org-mcp-test--call-read-headline "org://test-id-789")))))

(ert-deftest org-mcp-test-update-todo-state-success ()
  "Test successful TODO state update."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "|" "DONE(d!)"))))
        ;; Update TODO to IN-PROGRESS
        (let ((resource-uri
               (format "%s#Task%%20One" test-file)))
          (org-mcp-test--update-todo-state-and-check
           resource-uri "TODO" "IN-PROGRESS"
           test-file org-mcp-test--expected-task-one-in-progress-regex))))))

(ert-deftest org-mcp-test-update-todo-state-mismatch ()
  "Test TODO state update fails on state mismatch."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        ;; Try to update with wrong current state
        (let ((resource-uri
               (format "%s#Task%%20One" test-file)))
          (org-mcp-test--call-update-todo-state-expecting-error
           test-file resource-uri "IN-PROGRESS" "DONE"))))))

(ert-deftest org-mcp-test-update-todo-with-timestamp-id ()
  "Test updating TODO state using timestamp-format ID (not UUID)."
  (let ((test-content org-mcp-test--content-timestamp-id))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
      (org-mcp-test--with-id-setup test-file test-content
          `("20240101T120000")
        (let ((uri "20240101T120000"))
          (org-mcp-test--update-todo-state-and-check
           uri "TODO" "DONE"
           test-file
           org-mcp-test--expected-timestamp-id-done-regex))))))

(ert-deftest org-mcp-test-update-todo-state-empty-newstate-invalid ()
  "Test that empty string for newState is rejected."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        ;; Try to set empty state
        (let ((resource-uri
               (format "%s#Task%%20One" test-file)))
          (org-mcp-test--call-update-todo-state-expecting-error
           test-file resource-uri "TODO" ""))))))

(ert-deftest org-mcp-test-update-todo-state-invalid ()
  "Test TODO state update fails for invalid new state."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        ;; Try to update to invalid state
        (let ((resource-uri
               (format "%s#Task%%20One" test-file)))
          (org-mcp-test--call-update-todo-state-expecting-error
           test-file resource-uri "TODO" "INVALID-STATE"))))))

(ert-deftest org-mcp-test-update-todo-state-with-open-buffer ()
  "Test TODO state update works when file is open in a clean buffer.
When the visited buffer was clean, org-mcp edits it and auto-saves to disk."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        ;; Open the file in a buffer (clean — not modified)
        (let ((buffer (find-file-noselect test-file)))
          (unwind-protect
              (progn
                ;; Buffer is clean before the update
                (with-current-buffer buffer
                  (should-not (buffer-modified-p)))
                ;; Update TODO state while buffer is open
                (let ((resource-uri
                       (format "%s#Task%%20One"
                               test-file)))
                  (org-mcp-test--update-todo-state-and-check
                   resource-uri "TODO" "IN-PROGRESS"
                   test-file org-mcp-test--expected-task-one-in-progress-regex)
                  ;; Verify the buffer was also updated
                  (with-current-buffer buffer
                    (goto-char (point-min))
                    (should
                     (re-search-forward "^\\* IN-PROGRESS Task One"
                                        nil t)))))
            ;; Clean up: kill the buffer
            (kill-buffer buffer)))))))

(defconst org-mcp-test--content-two-todo-tasks
  "* TODO Task One
Task description.
* TODO Task Two
Another task description."
  "Org file content with two TODO tasks, used for modified-buffer tests.")

(defconst org-mcp-test--expected-modified-buffer-task-one-in-progress-regex
  (concat
   "\\`\\* IN-PROGRESS Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   "Task description\\.\n"
   "\\* TODO Task Two\n"
   "Another task description\\.\n"
   "\\* TODO Task Three\n"
   "Added in buffer\\."
   "\\'")
  "Regex matching the complete buffer after updating Task One to IN-PROGRESS.
The buffer also keeps the unsaved Task Three edit made before the update.")

(ert-deftest org-mcp-test-update-todo-state-with-modified-buffer ()
  "Test TODO state update succeeds on a pre-modified buffer without auto-saving.
When the visited buffer was already dirty before org-mcp writes, the
edit is applied in-buffer only — the file on disk must remain unchanged
and the response must report `saved' as false."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-two-todo-tasks))
    (let ((org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
      ;; Open the file in a buffer and modify it elsewhere
      (let ((buffer (find-file-noselect test-file)))
        (unwind-protect
            (progn
              ;; Make a modification at an unrelated location
              (with-current-buffer buffer
                (goto-char (point-max))
                (insert "\n* TODO Task Three\nAdded in buffer.")
                ;; Buffer is now modified but not saved
                (should (buffer-modified-p)))

              ;; Update TODO state — should succeed without auto-save
              (let ((resource-uri
                     (format "%s#Task%%20One"
                             test-file)))
                (let ((result
                       (org-mcp-test--call-update-todo-state
                        resource-uri "IN-PROGRESS" "TODO")))
                  (should (equal (alist-get 'success result) t))
                  (should (eq (alist-get 'saved result) :json-false))
                  (should (equal (alist-get 'new_state result) "IN-PROGRESS")))
                ;; Buffer must still be modified (never auto-saved)
                (with-current-buffer buffer
                  (should (buffer-modified-p)))
                ;; Buffer content reflects the TODO change and the user edit
                (org-mcp-test--verify-buffer-matches
                 buffer
                 org-mcp-test--expected-modified-buffer-task-one-in-progress-regex)
                ;; Disk must still have the *original* content — no auto-save
                (should (string= (org-mcp-test--read-file test-file)
                                 org-mcp-test--content-two-todo-tasks))))
          ;; Clean up: kill the buffer
          (kill-buffer buffer))))))

(ert-deftest org-mcp-test-update-todo-state-nonexistent-id ()
  "Test TODO state update fails for non-existent UUID."
  (let ((test-content "* TODO Task One\nTask description."))
    (let ((org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
      (org-mcp-test--with-id-setup test-file test-content '()
        ;; Try to update a non-existent ID
        (let ((resource-uri "org://nonexistent-uuid-12345"))
          (org-mcp-test--call-update-todo-state-expecting-error
           test-file resource-uri "TODO" "IN-PROGRESS"))))))

(ert-deftest org-mcp-test-update-todo-state-by-id ()
  "Test updating TODO state using org:// URI."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (let ((org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
      (org-mcp-test--with-id-setup test-file test-content
          `(,org-mcp-test--content-with-id-id)
        (org-mcp-test--update-todo-state-and-check
         org-mcp-test--content-with-id-uri "TODO" "IN-PROGRESS"
         test-file
         org-mcp-test--expected-task-with-id-in-progress-regex)))))

(ert-deftest org-mcp-test-update-todo-state-rejects-org-prefix ()
  "Test that `org-update-todo-state' rejects an `org://'-prefixed URI."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (let ((org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
      (org-mcp-test--with-id-setup test-file test-content
          `(,org-mcp-test--content-with-id-id)
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file
         org-mcp-test--content-with-id-resource-uri
         "TODO" "IN-PROGRESS")))))

(ert-deftest org-mcp-test-update-todo-state-nonexistent-headline ()
  "Test TODO state update fails for non-existent headline path."
  (let ((test-content
         "* TODO Task One
Task description.
* TODO Task Two
Another task."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        ;; Try to update a non-existent headline
        (let ((resource-uri
               (format "%s#Nonexistent%%20Task"
                       test-file)))
          (org-mcp-test--call-update-todo-state-expecting-error
           test-file resource-uri "TODO" "IN-PROGRESS"))))))

(ert-deftest org-mcp-test-update-todo-state-without-current-state ()
  "Test TODO state update without providing current_state."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "|" "DONE(d!)"))))
        (let ((resource-uri
               (format "%s#Task%%20One" test-file)))
          (let ((result
                 (org-mcp-test--call-update-todo-state
                  resource-uri "IN-PROGRESS")))
            (should (= (length result) 5))
            (should (equal (alist-get 'success result) t))
            (should (equal (alist-get 'previous_state result) "TODO"))
            (should (equal (alist-get 'new_state result) "IN-PROGRESS"))
            (should (stringp (alist-get 'uri result)))
            (should (string-prefix-p "org://" (alist-get 'uri result)))
            (org-mcp-test--verify-file-matches
             test-file org-mcp-test--expected-task-one-in-progress-regex)))))))

(ert-deftest org-mcp-test-update-todo-state-without-current-state-no-state ()
  "Test TODO state update without current_state on headline with no TODO state."
  (let ((test-content "* Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO(t!)" "|" "DONE(d!)"))))
        (let ((resource-uri
               (format "%s#Task%%20One" test-file)))
          (let ((result
                 (org-mcp-test--call-update-todo-state
                  resource-uri "TODO")))
            (should (= (length result) 5))
            (should (equal (alist-get 'success result) t))
            (should (equal (alist-get 'previous_state result) ""))
            (should (equal (alist-get 'new_state result) "TODO"))
            (should (stringp (alist-get 'uri result)))
            (should (string-prefix-p "org://" (alist-get 'uri result)))))))))

(defconst org-mcp-test--expected-task-one-done-with-note-regex
  (concat
   "\\`\\* DONE Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "- State \"DONE\"[ \t]+from \"TODO\"[ \t]+\\[.*\\] \\\\\\\\\n"
   "  Test note\n"
   ":END:\n"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching buffer after updating Task One to DONE with a note.")

(defconst org-mcp-test--expected-task-one-done-with-note-no-drawer-regex
  (concat
   "\\`\\* DONE Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   "- State \"DONE\"[ \t]+from \"TODO\"[ \t]+\\[.*\\] \\\\\\\\\n"
   "  Test note\n"
   "\\(?:.\\|\n\\)*\\'")
  "Regex after marking Task One DONE with a note and no LOGBOOK drawer.")

(defconst org-mcp-test--content-task-scheduled-repeat
  "* TODO Weekly Task\nSCHEDULED: <2026-01-01 Thu +1w>"
  "Task with a +1w SCHEDULED repeater.")

(defconst org-mcp-test--content-task-scheduled-repeat-to-state
  (concat "* TODO Weekly Task\n"
          ":PROPERTIES:\n"
          ":REPEAT_TO_STATE: NEXT\n"
          ":END:\n"
          "SCHEDULED: <2026-01-01 Thu +1w>")
  "Task with +1w SCHEDULED repeater and REPEAT_TO_STATE: NEXT.")

;; After marking DONE, repeat reverts to TODO, SCHEDULED advances
(defconst org-mcp-test--expected-weekly-task-repeat-triggered-regex
  (concat
   "\\`\\* TODO Weekly Task\n"
   "\\(?::PROPERTIES:\n"
   "\\(?::REPEAT_TO_STATE:[ \t]+\\S-+\n\\)?"
   ":ID:[ \t]+[A-Fa-f0-9-]+\n"
   "\\(?::LAST_REPEAT:[ \t]+\\[.*\\]\n\\)?"
   ":END:\n\\)?"
   "SCHEDULED: <[0-9]+-[0-9]+-[0-9]+[^>]*\\+1w[^>]*>\n"
   "\\(?:.\\|\n\\)*\\'")
  "Regex: Weekly Task after repeat triggered (state back to TODO, date advanced).")

;; After marking DONE, repeat reverts to NEXT (REPEAT_TO_STATE)
(defconst org-mcp-test--expected-weekly-task-repeat-to-state-regex
  (concat
   "\\`\\* NEXT Weekly Task\n"
   "\\(?::PROPERTIES:\n"
   "\\(?::REPEAT_TO_STATE:[ \t]+NEXT\n\\)?"
   ":ID:[ \t]+[A-Fa-f0-9-]+\n"
   "\\(?::LAST_REPEAT:[ \t]+\\[.*\\]\n\\)?"
   ":END:\n\\)?"
   "SCHEDULED: <[0-9]+-[0-9]+-[0-9]+[^>]*\\+1w[^>]*>\n"
   "\\(?:.\\|\n\\)*\\'")
  "Regex: Weekly Task after repeat with REPEAT_TO_STATE reverts to NEXT.")

;;; Test data for CRUD entry tools

(defconst org-mcp-test--content-bare-todo
  "* TODO Simple Task\nTask body text."
  "Bare TODO task without properties for CRUD tests.")

(defconst org-mcp-test--content-todo-with-props
  "* TODO Task with Properties
:PROPERTIES:
:EFFORT:   1:00
:CATEGORY: work
:END:
Some body."
  "TODO task with existing user properties.")

(defconst org-mcp-test--content-todo-with-scheduled
  "* TODO Scheduled Task
SCHEDULED: <2026-03-01 Sun>
Task body."
  "TODO task with a SCHEDULED timestamp.")

(defconst org-mcp-test--content-todo-with-deadline
  "* TODO Deadline Task
DEADLINE: <2026-03-15 Sun>
Task body."
  "TODO task with a DEADLINE timestamp.")

(defconst org-mcp-test--content-todo-with-priority
  "* TODO [#B] Priority Task\nTask body."
  "TODO task with priority B.")

(defconst org-mcp-test--content-todo-with-children
  "* TODO Parent Task
Parent body.
** Child One
Child body."
  "TODO task with a child heading for append-body tests.")

(defconst org-mcp-test--content-todo-empty-body
  "* TODO Empty Body Task"
  "TODO task with no body content.")

(defconst org-mcp-test--content-todo-with-logbook
  "* TODO Task with Logbook
:LOGBOOK:
CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:00] =>  1:00
:END:
Task body."
  "TODO task with an existing LOGBOOK drawer.")

;; Patterns for CRUD tool tests

(defconst org-mcp-test--pattern-set-properties-new
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:EFFORT: +2:00\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after setting EFFORT property on bare task.")

(defconst org-mcp-test--pattern-set-properties-update
  (concat
   "\\`\\* TODO Task with Properties\n"
   " *:PROPERTIES:\n"
   " *:EFFORT: +2:30\n"
   " *:CATEGORY: +work\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Some body\\.\n?\\'")
  "Pattern after updating EFFORT property.")

(defconst org-mcp-test--pattern-set-properties-delete
  (concat
   "\\`\\* TODO Task with Properties\n"
   " *:PROPERTIES:\n"
   " *:CATEGORY: +work\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Some body\\.\n?\\'")
  "Pattern after deleting EFFORT property.")

(defconst org-mcp-test--pattern-set-properties-id-and-custom-id
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +" org-mcp-test--client-id "\n"
   " *:CUSTOM_ID: +simple-task\n"
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after a client sets ID and CUSTOM_ID on a bare task.
The heading carries the client's ID and no other.")

(defconst org-mcp-test--pattern-scheduled-set
  (concat
   "\\`\\* TODO Simple Task\n"
   "SCHEDULED: <2026-03-27 .*>\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after setting SCHEDULED on bare task.")

(defconst org-mcp-test--pattern-scheduled-update
  (concat
   "\\`\\* TODO Scheduled Task\n"
   "SCHEDULED: <2026-04-15 .*>\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task body\\.\n?\\'")
  "Pattern after updating existing SCHEDULED.")

(defconst org-mcp-test--pattern-scheduled-remove
  (concat
   "\\`\\* TODO Scheduled Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task body\\.\n?\\'")
  "Pattern after removing SCHEDULED.")

(defconst org-mcp-test--pattern-deadline-set
  (concat
   "\\`\\* TODO Simple Task\n"
   "DEADLINE: <2026-03-27 .*>\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after setting DEADLINE on bare task.")

(defconst org-mcp-test--pattern-deadline-update
  (concat
   "\\`\\* TODO Deadline Task\n"
   "DEADLINE: <2026-04-15 .*>\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task body\\.\n?\\'")
  "Pattern after updating existing DEADLINE.")

(defconst org-mcp-test--pattern-deadline-remove
  (concat
   "\\`\\* TODO Deadline Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task body\\.\n?\\'")
  "Pattern after removing DEADLINE.")

(defconst org-mcp-test--pattern-tags-set
  (concat
   "\\`\\* TODO Simple Task[ \t]+:work:urgent:\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after setting tags on bare task.")

(defconst org-mcp-test--pattern-tags-replace
  (concat
   "\\`\\* TODO Task with Tags[ \t]+:personal:\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task description\\.\n?\\'")
  "Pattern after replacing tags.")

(defconst org-mcp-test--pattern-tags-clear
  (concat
   "\\`\\* TODO Task with Tags\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task description\\.\n?\\'")
  "Pattern after clearing all tags.")

(defconst org-mcp-test--pattern-priority-set
  (concat
   "\\`\\* TODO \\[#A\\] Simple Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after setting priority A on bare task.")

(defconst org-mcp-test--pattern-priority-change
  (concat
   "\\`\\* TODO \\[#C\\] Priority Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task body\\.\n?\\'")
  "Pattern after changing priority from B to C.")

(defconst org-mcp-test--pattern-priority-remove
  (concat
   "\\`\\* TODO Priority Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task body\\.\n?\\'")
  "Pattern after removing priority.")

(defconst org-mcp-test--pattern-append-body
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task body text\\.\n"
   "Appended line\\.\n?\\'")
  "Pattern after appending to body.")

(defconst org-mcp-test--pattern-append-body-empty
  (concat
   "\\`\\* TODO Empty Body Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "New body content\\.\n?\\'")
  "Pattern after appending to empty body.")

(defconst org-mcp-test--pattern-append-body-with-children
  (concat
   "\\`\\* TODO Parent Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Parent body\\.\n"
   "Appended text\\.\n"
   "\\*\\* Child One\n"
   "Child body\\.\n?\\'")
  "Pattern after appending body before children.")

(defconst org-mcp-test--pattern-logbook-note-new
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   ":LOGBOOK:\n"
   "- Note taken on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  This is my note\\.\n"
   ":END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after adding logbook note to task without LOGBOOK.")

(defconst org-mcp-test--pattern-logbook-note-existing
  (concat
   "\\`\\* TODO Task with Logbook\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   ":LOGBOOK:\n"
   "- Note taken on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  Another note\\.\n"
   "CLOCK: \\[2026-01-01 Thu 10:00\\]--\\[2026-01-01 Thu 11:00\\] =>  1:00\n"
   ":END:\n"
   "Task body\\.\n?\\'")
  "Pattern after adding logbook note to task with existing LOGBOOK.")

(defconst org-mcp-test--pattern-logbook-note-multiline
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   ":LOGBOOK:\n"
   "- Note taken on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  First line\\.\n"
   "  Second line\\.\n"
   ":END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after adding multiline logbook note.")

(defconst org-mcp-test--pattern-logbook-note-special-chars
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   ":LOGBOOK:\n"
   "- Note taken on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  Quotes \"like this\", backslash \\\\, percent %, asterisk \\*\\.\n"
   ":END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after adding logbook note with special characters.")

(defconst org-mcp-test--pattern-logbook-note-no-drawer
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "- Note taken on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  Plain note\\.\n"
   "Task body text\\.\n?\\'")
  "Pattern after adding logbook note with `org-log-into-drawer' nil.")

(defconst org-mcp-test--pattern-logbook-note-custom-heading
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   ":LOGBOOK:\n"
   "- Custom note prefix \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  My note\\.\n"
   ":END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after adding logbook note with custom `org-log-note-headings'.")

(defconst org-mcp-test--crud-test-id
  "crud-test-id-001"
  "ID for CRUD test entries.")

(defconst org-mcp-test--content-todo-with-test-id
  (format
   "* TODO ID Task
:PROPERTIES:
:ID:       %s
:END:
Task body."
   org-mcp-test--crud-test-id)
  "TODO task with known ID for CRUD tests.")

(ert-deftest org-mcp-test-update-todo-state-with-note ()
  "Test TODO state update with an attached note."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
            (org-log-into-drawer t))
        (let ((resource-uri
               (format "%s#Task%%20One" test-file)))
          (let* ((params `((uri . ,resource-uri)
                           (new_state . "DONE")
                           (note . "Test note")))
                 (result-text (mcp-server-lib-ert-call-tool
                               "org-update-todo-state" params))
                 (result (json-read-from-string result-text)))
            (should (equal (alist-get 'success result) t))
            (should (equal (alist-get 'previous_state result) "TODO"))
            (should (equal (alist-get 'new_state result) "DONE"))
            (org-mcp-test--verify-file-matches
             test-file
             org-mcp-test--expected-task-one-done-with-note-regex)))))))

(ert-deftest org-mcp-test-update-todo-state-with-note-no-drawer ()
  "Test TODO state update with note when `org-log-into-drawer' is nil.
The note is inserted directly under the heading rather than in a
LOGBOOK drawer."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
            (org-log-into-drawer nil))
        (let ((resource-uri
               (format "%s#Task%%20One" test-file)))
          (let* ((params `((uri . ,resource-uri)
                           (new_state . "DONE")
                           (note . "Test note")))
                 (result-text (mcp-server-lib-ert-call-tool
                               "org-update-todo-state" params))
                 (result (json-read-from-string result-text)))
            (should (equal (alist-get 'success result) t))
            (should (equal (alist-get 'previous_state result) "TODO"))
            (should (equal (alist-get 'new_state result) "DONE"))
            (org-mcp-test--verify-file-matches
             test-file
             org-mcp-test--expected-task-one-done-with-note-no-drawer-regex)))))))

(ert-deftest org-mcp-test-update-todo-state-triggers-repeat ()
  "Test that marking DONE on a task with a repeater triggers the repeat."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-task-scheduled-repeat))
    (let ((org-log-repeat nil)
          (org-todo-keywords '((sequence "TODO" "|" "DONE"))))
      (let* ((resource-uri (format "%s#Weekly%%20Task" test-file))
             (result (org-mcp-test--call-update-todo-state resource-uri "DONE")))
        ;; Response fields
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'previous_state result) "TODO"))
        (should (equal (alist-get 'new_state result) "DONE"))
        ;; File: repeat fired — state reverted to TODO, SCHEDULED advanced
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--expected-weekly-task-repeat-triggered-regex)))))

(ert-deftest org-mcp-test-update-todo-state-repeat-to-state ()
  "Test that REPEAT_TO_STATE is respected when repeat triggers."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-task-scheduled-repeat-to-state))
    (let ((org-log-repeat nil)
          (org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE"))))
      (let* ((resource-uri (format "%s#Weekly%%20Task" test-file))
             (result (org-mcp-test--call-update-todo-state resource-uri "DONE")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'previous_state result) "TODO"))
        (should (equal (alist-get 'new_state result) "DONE"))
        ;; File: state reverted to NEXT (from REPEAT_TO_STATE)
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--expected-weekly-task-repeat-to-state-regex)))))

(ert-deftest org-mcp-test-add-todo-top-level ()
  "Test adding a top-level TODO item."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "%s#" test-file)))
      (org-mcp-test--add-todo-and-check
       "New Task"
       "TODO"
       '("work" "urgent")
       nil ; no body
       parent-uri
       nil ; no afterUri
       (file-name-nondirectory test-file)
       test-file
       (concat
        "^\\* TODO New Task +:.*work.*urgent.*:\n"
        "\\(?: *:PROPERTIES:\n"
        " *:ID: +[^\n]+\n"
        " *:END:\n\\)?$")))))

(ert-deftest org-mcp-test-add-todo-top-level-with-header ()
  "Test adding top-level TODO after header comments."
  (let ((initial-content org-mcp-test--content-nested-siblings))
    (org-mcp-test--with-add-todo-setup test-file initial-content
      (let ((parent-uri (format "%s#" test-file)))
        (org-mcp-test--add-todo-and-check
         "New Top Task"
         "TODO"
         '("urgent")
         nil ; no body
         parent-uri
         nil ; no afterUri
         (file-name-nondirectory test-file)
         test-file
         org-mcp-test--expected-regex-top-level-with-header)))))

(ert-deftest org-mcp-test-add-todo-invalid-state ()
  "Test that adding TODO with invalid state throws error."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "%s#" test-file)))
      (org-mcp-test--call-add-todo-expecting-error
       test-file
       "New Task"
       "INVALID-STATE" ; Not in org-todo-keywords
       '("work")
       nil
       parent-uri))))

(ert-deftest org-mcp-test-add-todo-invalid-state-error-lists-valid-states ()
  "Invalid TODO state error lists every valid keyword from `org-todo-keywords-1'."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-empty))
    (let ((org-todo-keywords
           '((sequence "TODO" "NEXT" "|" "DONE" "CANCELED")))
          (org-tag-alist '("work")))
      (let* ((parent-uri (format "%s#" test-file))
             (params
              `((title . "New Task")
                (todo_state . "BOGUS")
                (tags . ("work"))
                (body . nil)
                (parent_uri . ,parent-uri)))
             (request
              (mcp-server-lib-create-tools-call-request
               "org-add-todo" nil params))
             (response
              (mcp-server-lib-process-jsonrpc-parsed
               request mcp-server-lib-ert-server-id))
             (err
              (should-error
               (mcp-server-lib-ert-process-tool-response response)
               :type 'mcp-server-lib-tool-error))
             (msg (error-message-string err)))
        (should (string-match-p "Invalid TODO state: 'BOGUS'" msg))
        (dolist (kw '("TODO" "NEXT" "DONE" "CANCELED"))
          (should (string-match-p (regexp-quote kw) msg)))
        (should-not (string-match-p "|" msg))))))

(ert-deftest org-mcp-test-add-todo-valid-state-with-fast-access-and-log-spec ()
  "Adding TODO accepts a bare keyword name when the sequence
declares fast-access keys plus state-logging specs (e.g. `(c!)' =
fast key `c' + log timestamp on entry)."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-empty))
    (let ((org-todo-keywords
           '((sequence "TODO(t!)" "|" "DONE(d!)" "CANCELED(c!)")))
          (org-tag-alist '("work"))
          (org-id-locations-file nil))
      (let ((parent-uri (format "%s#" test-file)))
        (org-mcp-test--add-todo-and-check
         "Cancel Me"
         "CANCELED"
         '("work")
         nil
         parent-uri
         nil
         (file-name-nondirectory test-file)
         test-file
         "^\\* CANCELED Cancel Me +:work:")))))

(ert-deftest org-mcp-test-add-todo-rejects-raw-fast-access-form ()
  "TODO state passed in raw `KEYWORD(key+logspec)' form is rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-empty))
    (let ((org-todo-keywords
           '((sequence "TODO(t!)" "|" "DONE(d!)")))
          (org-tag-alist '("work")))
      (let ((parent-uri (format "%s#" test-file)))
        (org-mcp-test--call-add-todo-expecting-error
         test-file
         "New Task"
         "TODO(t!)" ; selection-key form is not the state name
         '("work")
         nil
         parent-uri)))))

(ert-deftest org-mcp-test-add-todo-valid-state-multiple-sequences ()
  "Adding TODO accepts a keyword drawn from a non-first sequence."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-empty))
    (let ((org-todo-keywords
           '((sequence "TODO" "|" "DONE")
             (type "BUG" "FEATURE" "|" "FIXED")))
          (org-tag-alist '("work"))
          (org-id-locations-file nil))
      (let ((parent-uri (format "%s#" test-file)))
        (org-mcp-test--add-todo-and-check
         "File Bug"
         "BUG"
         '("work")
         nil
         parent-uri
         nil
         (file-name-nondirectory test-file)
         test-file
         "^\\* BUG File Bug +:work:")))))

(ert-deftest org-mcp-test-add-todo-empty-title ()
  "Test that adding TODO with empty title throws error."
  (org-mcp-test--assert-add-todo-invalid-title ""))

(ert-deftest org-mcp-test-add-todo-spaces-only-title ()
  "Test that adding TODO with spaces-only title throws error."
  (org-mcp-test--assert-add-todo-invalid-title "   "))

(ert-deftest org-mcp-test-add-todo-mixed-whitespace-title ()
  "Test that adding TODO with mixed whitespace title throws error."
  (org-mcp-test--assert-add-todo-invalid-title "	  	"))

(ert-deftest org-mcp-test-add-todo-unicode-nbsp-title ()
  "Test that adding TODO with Unicode non-breaking space throws error."
  ;; U+00A0 is the non-breaking space character
  (org-mcp-test--assert-add-todo-invalid-title "\u00A0"))

(ert-deftest org-mcp-test-add-todo-embedded-newline-title ()
  "Test that adding TODO with embedded newline in title throws error."
  (org-mcp-test--assert-add-todo-invalid-title
   "First Line\nSecond Line"))

(ert-deftest org-mcp-test-add-todo-tag-free-form-with-alist ()
  "Free-form tags are accepted even when `org-tag-alist' is configured.
Org permits free-form tags in headlines, so we only enforce
`org-tag-re' here, not membership in the configured alist."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "%s#" test-file)))
      (org-mcp-test--add-todo-and-check
       "Task1"
       "TODO"
       '("freeform")
       nil
       parent-uri
       nil
       (file-name-nondirectory test-file)
       test-file
       (concat
        "^\\* TODO Task1 +:freeform:\n"
        "\\(?: *:PROPERTIES:\n"
        " *:ID: +[^\n]+\n"
        " *:END:\n\\)?$")))))

(ert-deftest org-mcp-test-add-todo-tag-accept-valid-with-alist ()
  "Test that tags in `org-tag-alist' are accepted."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "%s#" test-file)))
      ;; Should accept tags in org-tag-alist (work, personal, urgent)
      (org-mcp-test--add-todo-and-check
       "ValidTask"
       "TODO"
       '("work")
       nil
       parent-uri
       nil
       (file-name-nondirectory test-file)
       test-file
       (concat
        "^\\* TODO ValidTask +:work:\n"
        "\\(?: *:PROPERTIES:\n"
        " *:ID: +[^\n]+\n"
        " *:END:\n\\)?$")))))

(ert-deftest org-mcp-test-add-todo-tag-validation-without-alist ()
  "Test valid tag names are accepted when `org-tag-alist' is empty."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((org-tag-alist nil)
          (org-tag-persistent-alist nil))
      (let ((parent-uri (format "%s#" test-file)))
        ;; Should accept valid tag names (alphanumeric, _, @)
        (org-mcp-test--add-todo-and-check
         "Task1"
         "TODO"
         '("validtag" "tag123" "my_tag" "@home")
         nil
         parent-uri
         nil
         (file-name-nondirectory test-file)
         test-file
         (concat
          "^\\* TODO Task1 +:"
          ".*validtag.*tag123.*my_tag.*@home.*:\n"
          "\\(?: *:PROPERTIES:\n"
          " *:ID: +[^\n]+\n"
          " *:END:\n\\)?$"))))))

(ert-deftest org-mcp-test-add-todo-tag-invalid-characters ()
  "Test that tags with characters outside `org-tag-re' are rejected."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((org-tag-alist nil)
          (org-tag-persistent-alist nil))
      (let ((parent-uri (format "%s#" test-file)))
        ;; Reject tags containing characters outside `org-tag-re'
        ;; (which permits [:alnum:], `_', `@', `#', `%').  Note that
        ;; Emacs's [:alnum:] is Unicode-aware, so e.g. \"café\" is a
        ;; legal tag and is therefore not tested here.
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" '("invalid-tag!") nil parent-uri)
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" '("tag-with-dash") nil parent-uri)
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" '("tag with space") nil parent-uri)
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" '("tag:colon") nil parent-uri)
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" '("tag.dot") nil parent-uri)
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" '("tag~tilde") nil parent-uri)))))

(ert-deftest org-mcp-test-add-todo-tag-org-tag-re-extras ()
  "Test that `#' and `%' are accepted (per `org-tag-re')."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((org-tag-alist nil)
          (org-tag-persistent-alist nil))
      (let ((parent-uri (format "%s#" test-file)))
        (org-mcp-test--add-todo-and-check
         "Task1"
         "TODO"
         '("tag#hash" "pct%tag")
         nil
         parent-uri
         nil
         (file-name-nondirectory test-file)
         test-file
         (concat
          "^\\* TODO Task1 +:"
          ".*tag#hash.*pct%tag.*:\n"
          "\\(?: *:PROPERTIES:\n"
          " *:ID: +[^\n]+\n"
          " *:END:\n\\)?$"))))))

(ert-deftest org-mcp-test-add-todo-grouptags-children-allowed ()
  "Tags inside `:startgrouptag'/`:grouptags'/`:endgrouptag' are allowed."
  (org-mcp-test--with-temp-org-files
      ((test-file "#+TITLE: Test Org File\n\n"))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-id-locations-file nil)
          (org-tag-alist
           '((:startgrouptag)
             ("project")
             (:grouptags)
             ("proj_a")
             ("proj_b")
             (:endgrouptag))))
      (let ((parent-uri (format "%s#" test-file)))
        ;; Both the umbrella tag and a child tag are valid.
        (org-mcp-test--add-todo-and-check
         "Task1"
         "TODO"
         '("project" "proj_a")
         nil
         parent-uri
         nil
         (file-name-nondirectory test-file)
         test-file
         (concat
          "^\\* TODO Task1 +:"
          "\\(?:project:proj_a\\|proj_a:project\\):\n"
          "\\(?: *:PROPERTIES:\n"
          " *:ID: +[^\n]+\n"
          " *:END:\n\\)?$"))))))

(ert-deftest org-mcp-test-add-todo-mutex-tags-from-persistent-alist ()
  "Mutex group declared in `org-tag-persistent-alist' is enforced."
  (org-mcp-test--with-temp-org-files
      ((test-file "#+TITLE: Test Org File\n\n"))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-id-locations-file nil)
          (org-tag-alist nil)
          (org-tag-persistent-alist
           '(:startgroup
             ("@office" . ?o)
             ("@home" . ?h)
             :endgroup)))
      (let ((parent-uri (format "%s#" test-file)))
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO"
         ["@office" "@home"]
         nil parent-uri)))))

(ert-deftest org-mcp-test-add-todo-child-under-parent ()
  "Test adding a child TODO under an existing parent."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-nested-siblings
    (let ((parent-uri
           (format "%s#Parent%%20Task" test-file)))
      (org-mcp-test--add-todo-and-check
       "Child Task"
       "TODO"
       '("work")
       nil ; no body
       parent-uri
       nil ; no afterUri
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-child-under-parent))))

(ert-deftest org-mcp-test-add-todo-child-empty-after-uri ()
  "Test adding a child TODO with empty string for after_uri.
Empty string should be treated as nil - append as last child."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-nested-siblings
    (let ((parent-uri
           (format "%s#Parent%%20Task" test-file)))
      (org-mcp-test--add-todo-and-check
       "Child Task"
       "TODO"
       '("work")
       nil ; no body
       parent-uri
       "" ; empty string after_uri
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-child-under-parent))))

(ert-deftest org-mcp-test-add-todo-child-into-childless-parent ()
  "Test adding the first child under a parent that has no existing children.
This guards against the historical pitfall where bare `org-insert-heading'
would create a sibling of the parent instead of a child when the parent
had no children."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-childless-parent
    (let ((parent-uri
           (format "%s" org-mcp-test--childless-parent-id)))
      (org-mcp-test--add-todo-and-check
       "Only Child"
       "TODO"
       '("work")
       nil ; no body
       parent-uri
       nil ; no afterUri
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-child-into-childless-parent))))

(ert-deftest org-mcp-test-add-todo-second-child-same-level ()
  "Test that adding a second child creates it at the same level as first child.
This tests the bug where the second child was created at level 4 instead of level 3."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-level2-parent-level3-children
    (let ((parent-uri
           (format "%s#Top%%20Level/Review%%20the%%20package"
                   test-file)))
      (org-mcp-test--add-todo-and-check
       "Second Child"
       "TODO"
       '("work")
       nil  ; no body
       parent-uri
       nil ; no after_uri
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-second-child-same-level))))

(ert-deftest org-mcp-test-add-todo-with-after-uri ()
  "Test adding TODO after a sibling using after_uri.
Tests that adding after a level 3 sibling correctly creates level 3.
Reproduces the emacs.org scenario: level 2 parent (via path),
level 3 sibling (via ID)."
  (let ((initial-content org-mcp-test--content-level2-parent-level3-children))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-tag-alist '("internet")))
      (org-mcp-test--with-id-setup test-file initial-content
          `(,org-mcp-test--level2-parent-level3-sibling-id)
        (let ((parent-uri
               (format "%s#Top%%20Level/Review%%20the%%20package"
                       test-file))
              (after-uri (format "%s"
                                 org-mcp-test--level2-parent-level3-sibling-id)))
          ;; BUG: org-insert-heading creates level 1 (*) instead of level 3 (***)
          (org-mcp-test--add-todo-and-check
           "Review org-mcp-test.el"
           "TODO"
           '("internet")
           nil
           parent-uri
           after-uri
           (file-name-nondirectory test-file)
           test-file
           org-mcp-test--regex-after-sibling-level3))))))

(ert-deftest org-mcp-test-add-todo-with-body ()
  "Test adding TODO with body text."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "%s#" test-file))
          (body-text org-mcp-test--body-text-multiline))
      (org-mcp-test--add-todo-and-check
       "Task with Body"
       "TODO"
       '("work")
       body-text
       parent-uri
       nil
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-todo-with-body))))

(ert-deftest org-mcp-test-add-todo-body-with-same-level-headline ()
  "Test that adding TODO with body containing same-level headline is rejected."
  (org-mcp-test--assert-add-todo-rejects-body-headline
   org-mcp-test--content-empty
   "" ; top-level parent
   "Some initial text.\n* Another headline\nMore text."))

(ert-deftest org-mcp-test-add-todo-body-with-higher-level-headline ()
  "Test that adding TODO with body containing higher-level headline is rejected."
  (org-mcp-test--assert-add-todo-rejects-body-headline
   "* Parent\n"
   "Parent"
   "Some initial text.\n* Top level headline\nMore text."))

(ert-deftest org-mcp-test-add-todo-body-with-headline-at-eof ()
  "Test that adding TODO with body ending in headline at EOF is rejected."
  (org-mcp-test--assert-add-todo-rejects-body-headline
   org-mcp-test--content-empty
   "" ; top-level parent
   "Some initial text.\n* Headline at EOF"))

(ert-deftest org-mcp-test-add-todo-body-with-asterisk-only-at-eof ()
  "Test that body ending with just asterisk at EOF is correctly accepted.
A single asterisk without space is not a valid Org headline."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "%s#" test-file))
          (body-with-asterisk "Some initial text.\n*"))
      ;; Should succeed since * without space is not a headline
      (org-mcp-test--add-todo-and-check
       "Task"
       "TODO"
       '("work")
       body-with-asterisk
       parent-uri
       nil
       (file-name-nondirectory test-file)
       test-file
       (concat
        "^\\* TODO Task +:work:\n"
        "\\(?: *:PROPERTIES:\n"
        " *:ID: +[^\n]+\n"
        " *:END:\n\\)?"
        "Some initial text\\.\n"
        "\\*$")))))

(ert-deftest org-mcp-test-add-todo-body-with-unbalanced-block ()
  "Test that adding TODO with body containing unbalanced block is rejected.
Unbalanced blocks like #+BEGIN_EXAMPLE without #+END_EXAMPLE should be
rejected in TODO body content."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let
        ((parent-uri (format "%s#" test-file))
         (body-with-unbalanced-block
          "Here's an example:\n#+BEGIN_EXAMPLE\nsome code\nMore text after block"))
      ;; Should reject unbalanced blocks
      (org-mcp-test--call-add-todo-expecting-error
       test-file
       "Task with unbalanced block"
       "TODO"
       '("work")
       body-with-unbalanced-block
       parent-uri))))

(ert-deftest org-mcp-test-add-todo-body-with-unbalanced-end-block ()
  "Test that adding TODO with body containing unbalanced END block is rejected.
An #+END_EXAMPLE without matching #+BEGIN_EXAMPLE should be rejected."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "%s#" test-file))
          (body-with-unbalanced-end
           "Some text before\n#+END_EXAMPLE\nMore text after"))
      ;; Should reject unbalanced END blocks
      (org-mcp-test--call-add-todo-expecting-error
       test-file
       "Task with unbalanced END block"
       "TODO"
       '("work")
       body-with-unbalanced-end
       parent-uri))))

(ert-deftest org-mcp-test-add-todo-body-with-literal-block-end ()
  "Test that TODO body with END_SRC inside EXAMPLE block is accepted.
#+END_SRC inside an EXAMPLE block is literal text, not a block delimiter.
This is valid Org-mode syntax and should be allowed."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "%s#" test-file))
          (body-with-literal-end
           "Example of source block:\n#+BEGIN_EXAMPLE\n#+END_SRC\n#+END_EXAMPLE\nText after."))
      ;; Should succeed - #+END_SRC is just literal text inside EXAMPLE block
      (org-mcp-test--add-todo-and-check
       "Task with literal END_SRC"
       "TODO"
       '("work")
       body-with-literal-end
       parent-uri
       nil
       (file-name-nondirectory test-file)
       test-file
       (concat
        "^\\* TODO Task with literal END_SRC +:work:\n"
        "\\(?: *:PROPERTIES:\n"
        " *:ID: +[^\n]+\n"
        " *:END:\n\\)?"
        "Example of source block:\n"
        "#\\+BEGIN_EXAMPLE\n"
        "#\\+END_SRC\n"
        "#\\+END_EXAMPLE\n"
        "Text after\\.$")))))

(ert-deftest org-mcp-test-add-todo-after-sibling ()
  "Test adding TODO after a specific sibling."
  (let ((initial-content org-mcp-test--content-nested-siblings))
    (org-mcp-test--with-temp-org-files
        ((test-file initial-content))
     (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
           (org-tag-alist '("work")))
       ;; First add ID to First Child 50% Complete so we can reference it
       (let ((first-id nil))
         (with-temp-buffer
           (set-visited-file-name test-file t)
           (insert-file-contents test-file)
           (org-mode)
           (goto-char (point-min))
           ;; Add ID to First Child 50% Complete
           (re-search-forward "^\\*\\* First Child 50% Complete")
           (org-id-get-create)
           (setq first-id (org-id-get))
           (write-region (point-min) (point-max) test-file))
         ;; Kill any buffer visiting the test file
         (let ((buf (find-buffer-visiting test-file)))
           (when buf
             (kill-buffer buf)))

         (org-mcp-test--with-id-tracking
          (list test-file)
          `((,first-id . ,test-file))
          (let ((parent-uri
                 (format "%s#Parent%%20Task"
                         test-file))
                (after-uri (format "%s" first-id)))
            (org-mcp-test--add-todo-and-check
             "New Task After First"
             "TODO"
             '("work")
             nil
             parent-uri
             after-uri
             (file-name-nondirectory test-file)
             test-file
             org-mcp-test--regex-todo-after-sibling))))))))

(ert-deftest org-mcp-test-add-todo-afterUri-not-sibling ()
  "Test error when afterUri is not a child of parentUri."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (org-tag-alist '("work")))
    (org-mcp-test--with-id-setup
     test-file
     org-mcp-test--content-wrong-levels
     `(,org-mcp-test--other-child-id)
     (let* ((parent-uri
             (format "%s#First%%20Parent"
                     test-file))
            (after-uri
             (format "%s" org-mcp-test--other-child-id)))
       ;; Error: Other Child is not a child of First Parent
       (org-mcp-test--call-add-todo-expecting-error
        test-file "New Task" "TODO" '("work") nil parent-uri
        after-uri)))))

(ert-deftest org-mcp-test-add-todo-parent-id-uri ()
  "Test adding TODO with parent specified as org:// URI."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((org-todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)")))
          (org-tag-alist '("work"))
          (org-id-locations-file nil))
      ;; Use org:// for parent
      (let ((parent-uri
             (format "%s"
                     org-mcp-test--content-nested-siblings-parent-id)))
        (org-mcp-test--add-todo-and-check
         "Child via ID"
         "TODO"
         '("work")
         nil
         parent-uri
         nil
         (file-name-nondirectory test-file)
         test-file
         org-mcp-test--pattern-add-todo-parent-id-uri)))))

(ert-deftest org-mcp-test-add-todo-mutex-tags-error ()
  "Test that mutually exclusive tags are rejected."
  (let ((initial-content "#+TITLE: Test Org File\n\n"))
    (org-mcp-test--with-temp-org-files
        ((test-file initial-content))
      (let ((org-id-track-globally nil)
            (org-id-locations-file nil)
            (org-todo-keywords '((sequence "TODO" "|" "DONE")))
            ;; Configure mutex tag groups
            (org-tag-alist
             '(("work" . ?w)
               :startgroup
               ("@office" . ?o)
               ("@home" . ?h)
               :endgroup)))
        ;; Try to add TODO with conflicting tags - should error
        (let ((parent-uri (format "%s#" test-file)))
          (org-mcp-test--call-add-todo-expecting-error
           test-file
           "Test Task"
           "TODO"
           ["work" "@office" "@home"] ; conflicting tags
           nil
           parent-uri
           nil))))))

(ert-deftest org-mcp-test-add-todo-mutex-tags-valid ()
  "Test that non-conflicting tags from mutex groups are accepted."
  (let ((initial-content "#+TITLE: Test Org File\n\n"))
    (org-mcp-test--with-temp-org-files
        ((test-file initial-content))
      (let ((org-id-track-globally nil)
            (org-id-locations-file nil)
            (org-todo-keywords '((sequence "TODO" "|" "DONE")))
            ;; Configure mutex tag groups
            (org-tag-alist
             '(("work" . ?w)
               :startgroup
               ("@office" . ?o)
               ("@home" . ?h)
               :endgroup ("project" . ?p))))
        ;; Add TODO with non-conflicting tags
        (let ((parent-uri (format "%s#" test-file)))
          (org-mcp-test--add-todo-and-check
           "Test Task"
           "TODO"
           ["work" "@office" "project"] ; no conflict
           nil
           parent-uri
           nil
           (file-name-nondirectory test-file)
           test-file
           org-mcp-test--regex-add-todo-with-mutex-tags))))))

(ert-deftest org-mcp-test-add-todo-nil-tags ()
  "Test that adding TODO with nil tags creates headline without tags."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "%s#" test-file)))
      (org-mcp-test--add-todo-and-check
       "Task Without Tags"
       "TODO"
       nil ; nil for tags
       nil ; no body
       parent-uri
       nil ; no afterUri
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-todo-without-tags))))

(ert-deftest org-mcp-test-add-todo-empty-list-tags ()
  "Test that adding TODO with empty list tags creates headline without tags."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "%s#" test-file)))
      (org-mcp-test--add-todo-and-check
       "Task Without Tags"
       "TODO"
       '() ; empty list for tags
       nil ; no body
       parent-uri
       nil ; no afterUri
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-todo-without-tags))))

(ert-deftest org-mcp-test-add-todo-with-id-property ()
  "Test a client sets the ID of a new TODO in the create call.
The heading carries that ID and no other, the response addresses it
by that ID, and the ID is not added to `org-id-locations'."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-nested-siblings
   `(,org-mcp-test--content-nested-siblings-parent-id)
   (let* ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (result
           (org-mcp-test--add-todo-and-check
            "Client ID Task"
            "TODO"
            nil
            nil
            org-mcp-test--content-nested-siblings-parent-id
            nil
            (file-name-nondirectory test-file)
            test-file
            org-mcp-test--pattern-add-todo-with-id-property
            `((ID . ,org-mcp-test--client-id)))))
     (should
      (equal
       (alist-get 'uri result) (concat "org://" org-mcp-test--client-id)))
     (should
      (org-mcp-test--id-registered-p
       org-mcp-test--content-nested-siblings-parent-id))
     (should-not (org-mcp-test--id-registered-p org-mcp-test--client-id)))))

(ert-deftest org-mcp-test-add-todo-with-custom-id-property ()
  "Test a client sets the CUSTOM_ID of a new TODO in the create call."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (org-mcp-test--add-todo-and-check
     "Custom ID Task"
     "TODO"
     nil
     nil
     (format "%s#" test-file)
     nil
     (file-name-nondirectory test-file)
     test-file
     org-mcp-test--pattern-add-todo-with-custom-id-property
     '((CUSTOM_ID . "custom-id-task")))))

(ert-deftest org-mcp-test-add-todo-with-properties ()
  "Test a new TODO gets arbitrary properties next to tags and a body."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (org-mcp-test--add-todo-and-check
     "Task with Properties"
     "TODO"
     '("work")
     org-mcp-test--body-text-multiline
     (format "%s#" test-file)
     nil
     (file-name-nondirectory test-file)
     test-file
     org-mcp-test--pattern-add-todo-with-properties
     '((EFFORT . "1:00") (OWNER . "alice") (ESTIMATE . 3) (SKIPPED)))))

(ert-deftest org-mcp-test-add-todo-properties-forbid-special ()
  "Test a create call with a special property fails, file unchanged."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (org-mcp-test--call-add-todo-expecting-error
     test-file "Task" "TODO" nil nil (format "%s#" test-file) nil
     '((SCHEDULED . "<2026-01-01>")))))

(ert-deftest org-mcp-test-add-todo-properties-invalid-name ()
  "Test a create call with an invalid property name edits nothing.
The call fails before the heading is inserted, so neither the file
nor a buffer visiting it changes.  The call comes from a buffer with
Emacs Lisp syntax, where a line break is not whitespace, and a name
with one is refused all the same."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (with-syntax-table emacs-lisp-mode-syntax-table
      (dolist (name '("BAD NAME" "BAD\nNAME"))
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" nil nil (format "%s#" test-file) nil
         `((,name . "value")))
        (org-mcp-test--verify-no-modified-buffer test-file)))))

(ert-deftest org-mcp-test-add-todo-properties-multiline-value ()
  "Test a create call with a line break in a property value fails.
The break would inject a heading, so the call edits nothing."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (org-mcp-test--call-add-todo-expecting-error
     test-file "Task" "TODO" nil nil (format "%s#" test-file) nil
     '((FOO . "x\n* Injected heading")))
    (org-mcp-test--verify-no-modified-buffer test-file)))

(ert-deftest org-mcp-test-properties-refuse-non-string-values ()
  "Test booleans and arrays are refused as property values.
A create call with a boolean and a set call with an array both fail
and leave the file unchanged."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-bare-todo
    (org-mcp-test--call-add-todo-expecting-error
     test-file "Task" "TODO" nil nil (format "%s#" test-file) nil
     '((FLAG . :json-false)))
    (org-mcp-test--call-set-properties-expecting-error
     test-file (format "%s#Simple%%20Task" test-file)
     '((ITEMS . ["a" "b"])))))

(ert-deftest org-mcp-test-rename-headline-simple ()
  "Test renaming a simple TODO headline."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-simple-todo))
    (let ((org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
          (resource-uri
           (format "%s#Original%%20Task"
                   test-file)))
      ;; Rename the headline
      (org-mcp-test--call-rename-headline-and-check
       resource-uri
       "Original Task"
       "Updated Task"
       test-file
       org-mcp-test--pattern-renamed-simple-todo))))

(ert-deftest org-mcp-test-rename-headline-title-mismatch ()
  "Test that rename fails when current title doesn't match."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-simple-todo))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
      ;; Try to rename with wrong current title
      (let* ((resource-uri
              (format "%s#Original%%20Task"
                      test-file)))
        (org-mcp-test--call-rename-headline-expecting-error
         test-file resource-uri "Wrong Title" "Updated Task")))))

(ert-deftest org-mcp-test-rename-headline-preserve-tags ()
  "Test that renaming preserves tags."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-tag-alist '("work" "urgent" "personal")))
      ;; Rename the headline
      (let ((resource-uri
             (format "%s#Task%%20with%%20Tags"
                     test-file)))
        (org-mcp-test--call-rename-headline-and-check
         resource-uri
         "Task with Tags"
         "Renamed Task"
         test-file
         org-mcp-test--pattern-renamed-todo-with-tags)))))

(ert-deftest org-mcp-test-rename-headline-no-todo ()
  "Test renaming a regular headline without TODO state."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    ;; Rename the headline
    (let ((resource-uri
           (format "%s#Parent%%20Task/First%%20Child%%2050%%25%%20Complete"
                   test-file)))
      (org-mcp-test--call-rename-headline-and-check
       resource-uri
       "First Child 50% Complete"
       "Updated Child"
       test-file
       org-mcp-test--pattern-renamed-headline-no-todo))))

(ert-deftest org-mcp-test-rename-headline-nested-path-navigation ()
  "Test correct headline path navigation in nested structures.
Verifies that the implementation correctly navigates nested headline
paths and only matches headlines at the appropriate hierarchy level."
  (let ((initial-content org-mcp-test--content-wrong-levels))
    (org-mcp-test--with-temp-org-files
        ((test-file initial-content))
      ;; Try to rename "First Parent/Target Headline"
      ;; But there's no Target Headline under First Parent!
      ;; The function should fail, but it might incorrectly
      ;; find Third Parent's Target Headline
      (let* ((resource-uri
              (format "%s#First%%20Parent/Target%%20Headline"
                      test-file)))
        ;; This should throw an error because First Parent has no Target Headline
        (org-mcp-test--call-rename-headline-expecting-error
         test-file
         resource-uri
         "Target Headline"
         "Renamed Target Headline")))))

(ert-deftest org-mcp-test-rename-headline-by-id ()
  "Test renaming a headline accessed by org-id URI."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((org-id-track-globally t)
          (org-id-locations-file nil)
          (org-id-locations nil))
      ;; Register the ID without file scanning
      (org-id-add-location org-mcp-test--content-with-id-id test-file)
      ;; Rename using ID-based URI
      (org-mcp-test--call-rename-headline-and-check
       org-mcp-test--content-with-id-uri
       "Second Child"
       "Renamed Second Child"
       test-file
       org-mcp-test--expected-regex-renamed-second-child))))

(ert-deftest org-mcp-test-rename-headline-id-not-found ()
  "Test error when ID doesn't exist."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((org-id-track-globally nil)
          (org-id-locations-file nil))
      ;; Try to rename non-existent ID
      (org-mcp-test--call-rename-headline-expecting-error
       test-file
       "org://non-existent-id-12345"
       "Whatever"
       "Should Fail"))))

(ert-deftest org-mcp-test-rename-headline-with-slash ()
  "Test renaming a headline containing a slash character.
Slashes must be properly URL-encoded to avoid path confusion."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-slash-not-nested-before))
    ;; The slash should be encoded as %2F in the URI
    (let ((resource-uri
           (format
            "%s#Parent%%2FChild"
            test-file)))
      (org-mcp-test--call-rename-headline-and-check
       resource-uri
       "Parent/Child"
       "Parent/Child Renamed"
       test-file
       org-mcp-test--pattern-renamed-slash-headline))))

(ert-deftest org-mcp-test-rename-headline-slash-not-nested ()
  "Test that headline with slash is not treated as nested path.
Verifies that 'Parent/Child' is treated as a single headline,
not as Child under Parent."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-slash-not-nested-before))
    ;; Try to rename the "Parent/Child" headline
    (let ((resource-uri
           ;; Slash encoded as %2F to indicate single headline
           (format "%s#Parent%%2FChild"
                   test-file)))
      (org-mcp-test--call-rename-headline-and-check
       resource-uri
       "Parent/Child"
       "Parent-Child Renamed"
       test-file
       org-mcp-test--regex-slash-not-nested-after))))

(ert-deftest org-mcp-test-rename-headline-with-percent ()
  "Test renaming a headline containing a percent sign.
Percent signs must be properly URL-encoded to avoid double-encoding issues."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    ;; The percent should be encoded as %25 in the URI
    (let ((resource-uri
           (format "%s#Parent%%20Task/First%%20Child%%2050%%25%%20Complete"
                   test-file)))
      (org-mcp-test--call-rename-headline-and-check
       resource-uri
       "First Child 50% Complete"
       "First Child 75% Complete"
       test-file
       org-mcp-test--regex-percent-after))))

(ert-deftest org-mcp-test-rename-headline-reject-empty-string ()
  "Test that renaming to an empty string is rejected."
  (org-mcp-test--assert-rename-headline-rejected
   "* Important Task
This task has content."
   "Important Task" ""))

(ert-deftest org-mcp-test-rename-headline-reject-whitespace-only ()
  "Test that renaming to whitespace-only is rejected."
  (org-mcp-test--assert-rename-headline-rejected
   "* Another Task
More content."
   "Another Task" "   "))

(ert-deftest org-mcp-test-rename-headline-reject-newline ()
  "Test that renaming to a title with embedded newline is rejected."
  (org-mcp-test--assert-rename-headline-rejected
   org-mcp-test--content-nested-siblings
   "Parent Task/First Child 50% Complete"
   "First Line\nSecond Line"))

(ert-deftest org-mcp-test-rename-headline-duplicate-requires-full-path ()
  "Test that a bare duplicate title cannot be addressed without a parent.
With `org-find-olp' navigation, a single-element path is resolved at the
top level.  When the title only exists at deeper levels under different
parents, the URI is rejected; the caller must include the parent to
disambiguate."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-duplicate-headlines-before))
    ;; Bare "Project Review" has no level-1 match -> rejected.
    (org-mcp-test--call-rename-headline-expecting-error
     test-file
     (format "%s#Project%%20Review" test-file)
     "Project Review"
     "Q1 Review")
    ;; Full path disambiguates and renames exactly that subtree's entry.
    (org-mcp-test--call-rename-headline-and-check
     (format "%s#Team%%20Updates/Project%%20Review" test-file)
     "Project Review"
     "Q1 Review"
     test-file
     org-mcp-test--regex-duplicate-first-renamed)))

(ert-deftest org-mcp-test-rename-headline-creates-id ()
  "Test that renaming a headline creates an Org ID and returns it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
      (let ((org-id-track-globally t)
            (org-id-locations-file (make-temp-file "test-org-id")))
        ;; Rename headline using path-based URI
        (let ((resource-uri
               (format
                "%s#Parent%%20Task/Third%%20Child%%20%%233"
                test-file)))
          (org-mcp-test--call-rename-headline-and-check
           resource-uri
           "Third Child #3"
           "Renamed Child"
           test-file
           org-mcp-test--pattern-renamed-headline-with-id)))))


(ert-deftest org-mcp-test-rename-headline-hierarchy ()
  "Test that headline hierarchy is correctly navigated.
Ensures that when searching for nested headlines, the function
correctly restricts search to the parent's subtree."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-hierarchy-before))
    (let ((resource-uri
           (format "%s#Second%%20Section/Target"
                   test-file)))
      (org-mcp-test--call-rename-headline-and-check
       resource-uri
       "Target"
       "Renamed Target"
       test-file
       org-mcp-test--regex-hierarchy-second-target-renamed))))

(ert-deftest org-mcp-test-rename-headline-with-todo-keyword ()
  "Test that headlines with TODO keywords can be renamed.
The navigation function should find headlines even when they have TODO keywords."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-keywords-before))
   ;; Try to rename using the headline title without TODO keyword
   (let ((resource-uri
          (format
           "%s#Project%%20Management/Review%%20Documents"
           test-file)))
     ;; This should work - finding "Review Documents" even though
     ;; the actual headline is "TODO Review Documents"
     (org-mcp-test--call-rename-headline-and-check
      resource-uri
      "Review Documents"
      "Q1 Planning Review"
      test-file
      org-mcp-test--regex-todo-keywords-after))))

;;; org-edit-body tests

(ert-deftest org-mcp-test-edit-body-single-line ()
  "Test org-edit-body tool for single-line replacement."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-nested-siblings
   `(,org-mcp-test--content-with-id-id)
   (org-mcp-test--call-edit-body-and-check
    test-file
    org-mcp-test--content-with-id-uri
    "Second child content."
    "Updated second child content."
    org-mcp-test--pattern-edit-body-single-line
    nil
    org-mcp-test--content-with-id-id)))

(ert-deftest org-mcp-test-edit-body-multiline ()
  "Test org-edit-body tool for multi-line replacement."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-todo
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-and-check
     test-file
     org-mcp-test--content-with-id-uri
     "Second line of content."
     "This has been replaced
with new multiline
content here."
     org-mcp-test--pattern-edit-body-multiline
     nil
     org-mcp-test--content-with-id-id)))

(ert-deftest org-mcp-test-edit-body-multiple-occurrences-error ()
  "Test error for multiple occurrences."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-repeated-text
      `("test-id")
    (org-mcp-test--call-edit-body-expecting-error
     test-file "org://test-id" "occurrence of pattern" "REPLACED")))


(ert-deftest org-mcp-test-edit-body-not-found ()
  "Test org-edit-body tool error when text is not found."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "nonexistent text"
     "replacement")))

(ert-deftest org-mcp-test-edit-body-empty ()
  "Test org-edit-body tool can add content to empty body."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((resource-uri
           (format "%s#Parent%%20Task/Third%%20Child%%20%%233"
                   test-file)))
      (org-mcp-test--call-edit-body-and-check
       test-file
       resource-uri
       ""
       "New content added."
       org-mcp-test--pattern-edit-body-empty))))

(ert-deftest org-mcp-test-edit-body-empty-old-non-empty-body ()
  "Test error when oldBody is empty but body has content."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "" ; Empty oldBody
     "replacement")))

(ert-deftest org-mcp-test-edit-body-empty-with-properties ()
  "Test adding content to empty body with properties drawer."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-no-body
      `(,org-mcp-test--timestamp-id)
    (org-mcp-test--call-edit-body-and-check
     test-file
     (format "%s" org-mcp-test--timestamp-id)
     ""
     "Content added after properties."
     org-mcp-test--pattern-edit-body-empty-with-props)))

(ert-deftest org-mcp-test-edit-body-nested-headlines ()
  "Test org-edit-body preserves nested headlines."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (org-mcp-test--call-edit-body-and-check
     test-file
     (format "%s#Parent%%20Task" test-file)
     "Some parent content."
     "Updated parent content"
     org-mcp-test--pattern-edit-body-nested-headlines)))

(ert-deftest org-mcp-test-edit-body-reject-headline-in-middle ()
  "Test org-edit-body rejects newBody with headline marker in middle."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "replacement text
* This would become a headline")))

(ert-deftest org-mcp-test-edit-body-accept-lower-level-headline ()
  "Test org-edit-body accepts newBody with lower-level headline."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-and-check
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "some text
*** Subheading content"
     org-mcp-test--pattern-edit-body-accept-lower-level)))

(ert-deftest org-mcp-test-edit-body-reject-higher-level-headline ()
  "Test org-edit-body rejects newBody with higher-level headline.
When editing a level 2 node, level 1 headlines should be rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     (format "%s#Parent%%20Task/Second%%20Child"
             test-file)
     "Second child content."
     "New text
* Top level heading")))

(ert-deftest org-mcp-test-edit-body-reject-headline-at-start ()
  "Test org-edit-body rejects newBody with headline at beginning."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "* Heading at start")))

(ert-deftest org-mcp-test-edit-body-reject-unbalanced-begin-block ()
  "Test org-edit-body rejects newBody with unbalanced BEGIN block."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "Some text
#+BEGIN_EXAMPLE
Code without END_EXAMPLE")))

(ert-deftest org-mcp-test-edit-body-reject-orphaned-end-block ()
  "Test org-edit-body rejects newBody with orphaned END block."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "Some text
#+END_SRC
Without BEGIN_SRC")))

(ert-deftest org-mcp-test-edit-body-reject-mismatched-blocks ()
  "Test org-edit-body rejects newBody with mismatched blocks."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "Text here
#+BEGIN_QUOTE
Some quote
#+END_EXAMPLE")))

(ert-deftest org-mcp-test-edit-body-reject-lowercase-unbalanced-begin ()
  "Test org-edit-body rejects newBody with lowercase unbalanced BEGIN."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "Some text
#+begin_example
Code without end_example")))

(ert-deftest org-mcp-test-edit-body-reject-lowercase-orphaned-end ()
  "Test org-edit-body rejects newBody with lowercase orphaned END."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "Some text
#+end_src
Without begin_src")))

(ert-deftest org-mcp-test-edit-body-reject-second-block-unclosed ()
  "Test org-edit-body rejects two blocks where the second is unclosed."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "First block:
#+BEGIN_EXAMPLE
done
#+END_EXAMPLE

Second block:
#+BEGIN_QUOTE
unfinished")))

;;; Resource template workaround tool tests

(defun org-mcp-test--call-read (uri)
  "Call org-read tool via JSON-RPC and return the result.
URI must be a bare {path}, {path}#{headline}, or {uuid} (no `org://' prefix)."
  (let ((params `((uri . ,uri))))
    (mcp-server-lib-ert-call-tool "org-read" params)))

(ert-deftest org-mcp-test-tool-read-file ()
  "Test org-read tool returns structured JSON for files."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let* ((result-text (org-mcp-test--call-read test-file))
           (result (json-parse-string result-text :object-type 'alist))
           (children (alist-get 'children result)))
      (should (equal (alist-get 'file result) test-file))
      (should (= (length children) 1))
      (should (equal (alist-get 'title (aref children 0)) "Parent Task")))))

(ert-deftest org-mcp-test-tool-read-headline ()
  "Test org-read-headline tool returns plain text for files."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((result-text (org-mcp-test--call-read-headline test-file)))
      (should (string= result-text org-mcp-test--content-nested-siblings)))))

(ert-deftest org-mcp-test-tool-read-outline ()
  "Test org-read-outline tool returns valid JSON outline structure."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let* ((result (org-mcp-test--call-read-outline test-file))
           (headings (alist-get 'headings result)))
      (should (= (length headings) 1))
      (should (string= (alist-get 'title (aref headings 0)) "Parent Task")))))

(ert-deftest org-mcp-test-tool-read-headline-single-level ()
  "Test org-read-headline with single-level path."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-slash-not-nested-before))
    (let ((result-text
           (org-mcp-test--call-read-headline
            (format "%s#Parent%%2FChild" test-file))))
      (should
       (string-match-p
        org-mcp-test--pattern-tool-read-headline-single
        result-text)))))

(ert-deftest org-mcp-test-tool-read-headline-nested ()
  "Test org-read-headline with nested path."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((result-text
           (org-mcp-test--call-read-headline
            (format "%s#Parent%%20Task/First%%20Child%%2050%%25%%20Complete" test-file))))
      (should
       (string-match-p
        org-mcp-test--pattern-tool-read-headline-nested
        result-text)))))

(ert-deftest org-mcp-test-tool-read-headline-by-id ()
  "Test org-read-headline tool returns headline content by ID."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (let ((result-text
           (org-mcp-test--call-read-headline org-mcp-test--content-with-id-id)))
      (should
       (string-match-p
        org-mcp-test--pattern-tool-read-by-id
        result-text)))))

(ert-deftest org-mcp-test-tool-read-rejects-org-prefix ()
  "Test that `org-read' rejects an `org://'-prefixed URI."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (should-error
     (org-mcp-test--call-read (concat "org://" test-file)))))

(ert-deftest org-mcp-test-tool-read-headline-rejects-org-prefix ()
  "Test that `org-read-headline' rejects an `org://'-prefixed URI."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (should-error
     (org-mcp-test--call-read-headline (concat "org://" test-file)))))

(ert-deftest org-mcp-test-tool-read-file-prefers-modified-buffer ()
  "Test org-read-headline file reads prefer modified visited buffers."
  (let ((test-content "* Task One\nOriginal body\n"))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((buffer (find-file-noselect test-file)))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (goto-char (point-max))
                (insert "Unsaved change\n")
                (should (buffer-modified-p)))
              (let ((result-text
                     (org-mcp-test--call-read-headline
                      (format "%s" test-file))))
                (should (string-match-p "Unsaved change" result-text))
                (should-not
                 (string-match-p
                  "Unsaved change"
                  (org-mcp-test--read-file test-file)))))
          (kill-buffer buffer))))))

;; Tests for body extraction across various metadata layouts.  These
;; verify `org-mcp--extract-structured-heading' (via the org-read tool)
;; correctly skips planning lines and PROPERTIES/LOGBOOK drawers in any
;; order, by delegating to `org-end-of-meta-data'.

(defun org-mcp-test--read-content (file headline)
  "Return parsed `content' field for HEADLINE in FILE via org-read."
  (let* ((uri (format "%s#%s" file headline))
         (result-text (org-mcp-test--call-read uri))
         (result (json-parse-string result-text :object-type 'alist)))
    (alist-get 'content result)))

(ert-deftest org-mcp-test-extract-body-no-metadata ()
  "Body is extracted when the heading has no metadata at all."
  (let ((test-content "* Plain Heading\nFirst body line.\nSecond body line."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (should
       (equal (org-mcp-test--read-content test-file "Plain%20Heading")
              "First body line.\nSecond body line.")))))

(ert-deftest org-mcp-test-extract-body-only-properties ()
  "Body is extracted past a lone PROPERTIES drawer."
  (let ((test-content
         "* Heading
:PROPERTIES:
:CUSTOM_ID: foo
:END:
Body after properties."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (should
       (equal (org-mcp-test--read-content test-file "Heading")
              "Body after properties.")))))

(ert-deftest org-mcp-test-extract-body-only-logbook ()
  "Body is extracted past a lone LOGBOOK drawer."
  (let ((test-content
         "* Heading
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2026-01-01 Thu 10:00]
:END:
Body after logbook."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (should
       (equal (org-mcp-test--read-content test-file "Heading")
              "Body after logbook.")))))

(ert-deftest org-mcp-test-extract-body-properties-then-logbook ()
  "Body is extracted past PROPERTIES followed by LOGBOOK."
  (let ((test-content
         "* Heading
:PROPERTIES:
:CUSTOM_ID: foo
:END:
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2026-01-01 Thu 10:00]
:END:
Body after both."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (should
       (equal (org-mcp-test--read-content test-file "Heading")
              "Body after both.")))))

(ert-deftest org-mcp-test-extract-body-logbook-then-properties ()
  "Body is extracted past LOGBOOK followed by PROPERTIES.
This was the bug in the previous manual drawer-skipping code: it only
handled PROPERTIES-then-LOGBOOK order, leaving the PROPERTIES drawer
inside the body when LOGBOOK came first."
  (let ((test-content
         "* Heading
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2026-01-01 Thu 10:00]
:END:
:PROPERTIES:
:CUSTOM_ID: foo
:END:
Body after both."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (should
       (equal (org-mcp-test--read-content test-file "Heading")
              "Body after both.")))))

(ert-deftest org-mcp-test-extract-body-with-planning-lines ()
  "Body is extracted past SCHEDULED/DEADLINE planning lines."
  (let ((test-content
         "* TODO Heading
SCHEDULED: <2026-01-01 Thu> DEADLINE: <2026-01-08 Thu>
:PROPERTIES:
:CUSTOM_ID: foo
:END:
Body after planning."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "|" "DONE"))))
        (should
         (equal (org-mcp-test--read-content test-file "Heading")
                "Body after planning."))))))

(ert-deftest org-mcp-test-extract-body-planning-properties-logbook ()
  "Body is extracted past planning + PROPERTIES + LOGBOOK in order."
  (let ((test-content
         "* TODO Heading
SCHEDULED: <2026-01-01 Thu>
:PROPERTIES:
:CUSTOM_ID: foo
:END:
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2026-01-01 Thu 10:00]
:END:
Body after everything."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "|" "DONE"))))
        (should
         (equal (org-mcp-test--read-content test-file "Heading")
                "Body after everything."))))))

(ert-deftest org-mcp-test-before-save-hook-runs ()
  "Test that before-save-hook runs when org-mcp saves a file."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--before-save-hook-initial-content))
    (let ((hook-called nil)
          (before-save-hook before-save-hook))
      (add-hook 'before-save-hook (lambda () (setq hook-called t)))
      (mcp-server-lib-ert-call-tool
       "org-edit-body"
       `((resource_uri . ,(format "%s#Headline" test-file))
         (old_body . "Original body")
         (new_body . "Updated body")))
      (should hook-called))))

(ert-deftest org-mcp-test-clock-add-saves-file-to-disk ()
  "Test org-clock-add saves the completed CLOCK entry to disk."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (result (org-mcp-test--call-clock-add
                    uri "2026-01-01T10:00:00" "2026-01-01T11:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'added result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-add-expected-regex))))

(ert-deftest org-mcp-test-clock-add-clean-buffer-saves ()
  "Test clock-add on a clean visiting buffer edits it and saves to disk."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (buffer (find-file-noselect test-file)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (should-not (buffer-modified-p)))
            (let ((result (org-mcp-test--call-clock-add
                           uri "2026-01-01T10:00:00" "2026-01-01T11:00:00")))
              (should (equal (alist-get 'success result) t))
              (should (eq (alist-get 'saved result) t))
              (should (equal (alist-get 'added result) t)))
            (org-mcp-test--verify-file-matches
             test-file org-mcp-test--clock-add-expected-regex)
            (org-mcp-test--verify-buffer-matches
             buffer org-mcp-test--clock-add-expected-regex)
            (with-current-buffer buffer
              (should-not (buffer-modified-p))))
        (kill-buffer buffer)))))

(defconst org-mcp-test--clock-add-modified-buffer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] => 1:00\n"
   ":END:\n"
   "\n"
   "\\* TODO Task Two\n"
   "\\'")
  "Regex matching the complete buffer after clock-add on a modified buffer.
The buffer also keeps the unsaved Task Two edit made before the call.")

(defconst org-mcp-test--clock-in-modified-buffer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\n"
   ":END:\n"
   "\n"
   "\\* TODO Task Two\n"
   "\\'")
  "Regex matching the complete buffer after clock-in on a modified buffer.
The buffer also keeps the unsaved Task Two edit made before the call.")

(defconst org-mcp-test--clock-out-modified-buffer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] => 1:00\n"
   ":END:\n"
   "\n"
   "\\* TODO Task Two\n"
   "\\'")
  "Regex matching the complete buffer after clock-out on a modified buffer.
The buffer also keeps the unsaved Task Two edit made before the call.")

(ert-deftest org-mcp-test-clock-add-modified-buffer-no-auto-save ()
  "Test clock-add on a pre-modified buffer edits in-buffer without auto-save."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (buffer (find-file-noselect test-file)))
      (unwind-protect
          (progn
            ;; Dirty the buffer with an unrelated edit
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "\n* TODO Task Two\n")
              (should (buffer-modified-p)))
            (let ((result (org-mcp-test--call-clock-add
                           uri "2026-01-01T10:00:00" "2026-01-01T11:00:00")))
              (should (equal (alist-get 'success result) t))
              (should (eq (alist-get 'saved result) :json-false))
              (should (equal (alist-get 'added result) t)))
            ;; Buffer must still be modified
            (with-current-buffer buffer
              (should (buffer-modified-p)))
            (org-mcp-test--verify-buffer-matches
             buffer org-mcp-test--clock-add-modified-buffer-expected-regex)
            ;; Disk must still equal the original content
            (should (string= (org-mcp-test--read-file test-file)
                             org-mcp-test--clock-task-content)))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-clock-in-saves-file-to-disk ()
  "Test org-clock-in saves the open CLOCK entry to disk."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (result (org-mcp-test--call-clock-in uri "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'clocked_in result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-in-expected-regex))))

(ert-deftest org-mcp-test-clock-in-modified-buffer-no-auto-save ()
  "Test clock-in on a pre-modified buffer edits in-buffer without auto-save.
When the visited buffer was already dirty, org-mcp must not save to disk."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (buffer (find-file-noselect test-file)))
      (unwind-protect
          (progn
            ;; Dirty the buffer with an unrelated edit
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "\n* TODO Task Two\n")
              (should (buffer-modified-p)))
            (let ((result (org-mcp-test--call-clock-in
                           uri "2026-01-01T10:00:00")))
              (should (equal (alist-get 'success result) t))
              (should (eq (alist-get 'saved result) :json-false))
              (should (equal (alist-get 'clocked_in result) t)))
            ;; Buffer must still be modified
            (with-current-buffer buffer
              (should (buffer-modified-p)))
            (org-mcp-test--verify-buffer-matches
             buffer org-mcp-test--clock-in-modified-buffer-expected-regex)
            ;; Disk must still equal the original content
            (should (string= (org-mcp-test--read-file test-file)
                             org-mcp-test--clock-task-content)))
        (kill-buffer buffer)))))

;;; Tests for org-clock-in with resolve=true (dangling clock cleanup)

(defconst org-mcp-test--clock-resolve-one-dangling-content
  "* TODO Task One\n:LOGBOOK:\nCLOCK: [2025-12-30 Tue 09:00]\n:END:\n"
  "Heading with exactly one dangling CLOCK entry.")

(defconst org-mcp-test--clock-resolve-multi-dangling-content
  (concat
   "* TODO Task One\n:LOGBOOK:\n"
   "CLOCK: [2025-12-29 Mon 09:00]\n"
   "CLOCK: [2025-12-30 Tue 09:00]\n"
   ":END:\n")
  "Heading with two dangling CLOCK entries.")

(defconst org-mcp-test--clock-resolve-mixed-content
  (concat
   "* TODO Task One\n:LOGBOOK:\n"
   "CLOCK: [2025-12-30 Tue 09:00]\n"
   "CLOCK: [2025-12-28 Sun 09:00]--[2025-12-28 Sun 10:00] =>  1:00\n"
   ":END:\n")
  "Heading mixing a dangling and a closed CLOCK entry.")

(defconst org-mcp-test--clock-resolve-other-heading-content
  (concat
   "* TODO Task One\n"
   "* TODO Task Two\n:LOGBOOK:\n"
   "CLOCK: [2025-12-30 Tue 09:00]\n"
   ":END:\n")
  "Dangling CLOCK under Task Two; resolve target is Task One.")

(defconst org-mcp-test--clock-in-resolve-mixed-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\n"
   "CLOCK: \\[2025-12-28 [A-Za-z]\\{2,3\\} 09:00\\]"
   "--\\[2025-12-28 [A-Za-z]\\{2,3\\} 10:00\\] =>  1:00\n"
   ":END:\n"
   "\\'")
  "File contents after resolve=true clears dangling and preserves closed.")

(defconst org-mcp-test--clock-in-resolve-other-heading-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\n"
   ":END:\n"
   "\\* TODO Task Two\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2025-12-30 [A-Za-z]\\{2,3\\} 09:00\\]\n"
   ":END:\n"
   "\\'")
  "After resolve=true on Task One, Task Two's dangling CLOCK survives.")

(ert-deftest org-mcp-test-clock-in-resolve-no-dangling ()
  "Test clock-in with resolve=true on a heading with no dangling clocks."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (result (org-mcp-test--call-clock-in
                    uri "2026-01-01T10:00:00" "true")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'clocked_in result) t))
      ;; The `resolved' key is only present when clocks were deleted.
      (should (null (assq 'resolved result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-in-expected-regex))))

(ert-deftest org-mcp-test-clock-in-resolve-one-dangling ()
  "Test clock-in with resolve=true deletes one dangling CLOCK and collapses drawer."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-resolve-one-dangling-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (result (org-mcp-test--call-clock-in
                    uri "2026-01-01T10:00:00" "true")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'resolved result) 1))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-in-expected-regex))))

(ert-deftest org-mcp-test-clock-in-resolve-multi-dangling ()
  "Test clock-in with resolve=true deletes multiple dangling CLOCK entries."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-resolve-multi-dangling-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (result (org-mcp-test--call-clock-in
                    uri "2026-01-01T10:00:00" "true")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'resolved result) 2))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-in-expected-regex))))

(ert-deftest org-mcp-test-clock-in-resolve-mixed ()
  "Test clock-in with resolve=true deletes dangling but preserves closed."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-resolve-mixed-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (result (org-mcp-test--call-clock-in
                    uri "2026-01-01T10:00:00" "true")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'resolved result) 1))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-in-resolve-mixed-expected-regex))))

(ert-deftest org-mcp-test-clock-in-resolve-scoped-to-subtree ()
  "Test resolve=true does not touch dangling clocks in sibling headings."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-resolve-other-heading-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (result (org-mcp-test--call-clock-in
                    uri "2026-01-01T10:00:00" "true")))
      (should (equal (alist-get 'success result) t))
      ;; No clocks were under Task One; Task Two's dangling CLOCK is untouched.
      (should (null (assq 'resolved result)))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--clock-in-resolve-other-heading-expected-regex))))

(ert-deftest org-mcp-test-clock-out-saves-file-to-disk ()
  "Test org-clock-out saves the closed CLOCK entry to disk.
This exercises the write path in org-mcp--complete-and-save."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (let ((result (org-mcp-test--call-clock-out "2026-01-01T11:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'clocked_out result) t)))
    (org-mcp-test--verify-file-matches
     test-file org-mcp-test--clock-out-expected-regex)))

(ert-deftest org-mcp-test-clock-out-modified-buffer-no-auto-save ()
  "Test clock-out on a pre-modified buffer edits in-buffer without auto-save."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (let* ((buffer (find-file-noselect test-file)))
      (unwind-protect
          (progn
            ;; Dirty the buffer with an unrelated edit
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "\n* TODO Task Two\n")
              (should (buffer-modified-p)))
            (let ((result (org-mcp-test--call-clock-out
                           "2026-01-01T11:00:00")))
              (should (equal (alist-get 'success result) t))
              (should (eq (alist-get 'saved result) :json-false))
              (should (equal (alist-get 'clocked_out result) t)))
            ;; Buffer must still be modified
            (with-current-buffer buffer
              (should (buffer-modified-p)))
            (org-mcp-test--verify-buffer-matches
             buffer org-mcp-test--clock-out-modified-buffer-expected-regex)
            ;; Disk must still equal the original content
            (should (string= (org-mcp-test--read-file test-file)
                             org-mcp-test--clock-task-with-open-clock)))
        (kill-buffer buffer)))))

;;; Tests for org-clock-into-drawer behavior (nil and custom drawer name)

(defconst org-mcp-test--clock-task-with-open-clock-no-drawer
  "* TODO Task One\nCLOCK: [2026-01-01 Thu 10:00]\n"
  "Org file content with an unclosed CLOCK bare under the heading.
Used for clock-out tests when `org-clock-into-drawer' is nil.")

(defconst org-mcp-test--clock-add-no-drawer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] => 1:00\n"
   "\\'")
  "File contents after clock-add with `org-clock-into-drawer' nil.
The CLOCK line appears bare under the heading -- no LOGBOOK drawer.")

(defconst org-mcp-test--clock-in-no-drawer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\n"
   "\\'")
  "File contents after clock-in with `org-clock-into-drawer' nil.")

(defconst org-mcp-test--clock-out-no-drawer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] => 1:00\n"
   "\\'")
  "File contents after clock-out with `org-clock-into-drawer' nil.")

(defconst org-mcp-test--clock-add-custom-drawer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":WORK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] => 1:00\n"
   ":END:\n"
   "\\'")
  "File contents after clock-add with custom `org-clock-into-drawer' name.")

(ert-deftest org-mcp-test-clock-add-no-drawer ()
  "Test clock-add inserts bare CLOCK when `org-clock-into-drawer' is nil."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let ((org-clock-into-drawer nil))
      (let* ((uri (format "%s#Task%%20One" test-file))
             (result (org-mcp-test--call-clock-add
                      uri "2026-01-01T10:00:00" "2026-01-01T11:00:00")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'added result) t))
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--clock-add-no-drawer-expected-regex)))))

(ert-deftest org-mcp-test-clock-in-no-drawer ()
  "Test clock-in inserts bare CLOCK when `org-clock-into-drawer' is nil."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let ((org-clock-into-drawer nil))
      (let* ((uri (format "%s#Task%%20One" test-file))
             (result (org-mcp-test--call-clock-in
                      uri "2026-01-01T10:00:00")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'clocked_in result) t))
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--clock-in-no-drawer-expected-regex)))))

(defconst org-mcp-test--clock-in-close-same-file-content
  (concat
   "* TODO Task One\n"
   "* TODO Task Two\n")
  "File with two tasks, neither clocked in.")

(defconst org-mcp-test--clock-in-close-same-file-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  *1:00\n"
   ":END:\n"
   "\\* TODO Task Two\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\]\n"
   ":END:\n"
   "\\'")
  "Regex matching file after clock-in to Task Two closes Task One.")

(defconst org-mcp-test--clock-in-close-different-file-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  *1:00\n"
   ":END:\n"
   "\\'")
  "Regex matching file after active clock closed by clock-in to different file.")

(ert-deftest org-mcp-test-clock-in-closes-active-same-file ()
  "Test clock-in closes an active clock in the same file."
  (org-mcp-test--with-temp-org-files
      ((file-1 org-mcp-test--clock-in-close-same-file-content))
    (let* ((uri-1 (format "%s#Task%%20One" file-1))
           (uri-2 (format "%s#Task%%20Two" file-1))
           (result-1 (org-mcp-test--call-clock-in
                      uri-1 "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result-1) t))
      (should (equal (alist-get 'clocked_in result-1) t))
      ;; Clock in to Task Two — should close Task One first
      (let ((result-2 (org-mcp-test--call-clock-in
                       uri-2 "2026-01-01T11:00:00")))
        (should (equal (alist-get 'success result-2) t))
        (should (eq (alist-get 'saved result-2) t))
        (should (equal (alist-get 'clocked_in result-2) t))
        (org-mcp-test--verify-file-matches
         file-1
         org-mcp-test--clock-in-close-same-file-expected-regex)))))

(defconst org-mcp-test--clock-in-close-same-file-open-clock-content
  (concat
   "* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: [2026-01-01 Thu 10:00]\n"
   ":END:\n"
   "* TODO Task Two\n")
  "File with an open clock on Task One and an unclocked Task Two.")

(defconst org-mcp-test--clock-in-close-same-modified-buffer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  *1:00\n"
   ":END:\n"
   "\\* TODO Task Two\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\]\n"
   ":END:\n"
   "\\* TODO Task Three\n"
   "\\'")
  "Regex matching the complete buffer after clock-in to Task Two at 11:00.
The buffer holds Task One's closed clock, Task Two's new clock, and the
unsaved Task Three edit made before the call.")

(ert-deftest org-mcp-test-clock-in-closes-active-same-modified-buffer ()
  "Test clock-in when the active clock and the target share a dirty buffer.
Both edits land in a buffer that already has unsaved edits, so the file
on disk stays unchanged and the response reports `saved' as false."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-in-close-same-file-open-clock-content))
    (let ((buffer (find-file-noselect test-file)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "* TODO Task Three\n")
              (should (buffer-modified-p)))
            (let ((result (org-mcp-test--call-clock-in
                           (format "%s#Task%%20Two" test-file)
                           "2026-01-01T11:00:00")))
              (should (equal (alist-get 'success result) t))
              (should (eq (alist-get 'saved result) :json-false))
              (should (equal (alist-get 'clocked_in result) t)))
            (with-current-buffer buffer
              (should (buffer-modified-p)))
            (org-mcp-test--verify-buffer-matches
             buffer
             org-mcp-test--clock-in-close-same-modified-buffer-expected-regex)
            (should
             (string=
              (org-mcp-test--read-file test-file)
              org-mcp-test--clock-in-close-same-file-open-clock-content)))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-clock-in-closes-active-different-file ()
  "Test clock-in closes an active clock in a different file."
  (org-mcp-test--with-temp-org-files
      ((file-1 org-mcp-test--clock-task-content)
       (file-2 org-mcp-test--clock-task-content))
    (let* ((uri-1 (format "%s#Task%%20One" file-1))
           (uri-2 (format "%s#Task%%20One" file-2))
           (result-1 (org-mcp-test--call-clock-in
                      uri-1 "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result-1) t))
      (should (equal (alist-get 'clocked_in result-1) t))
      ;; Clock in to file-2 — should close file-1 first
      (let ((result-2 (org-mcp-test--call-clock-in
                       uri-2 "2026-01-01T11:00:00")))
        (should (equal (alist-get 'success result-2) t))
        (should (eq (alist-get 'saved result-2) t))
        (should (equal (alist-get 'clocked_in result-2) t))
        (org-mcp-test--verify-file-matches
         file-1
         org-mcp-test--clock-in-close-different-file-expected-regex)))))

(defconst org-mcp-test--clock-in-at-eleven-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\]\n"
   ":END:\n"
   "\\'")
  "Regex matching the complete file after clock-in at 11:00.")

(defconst org-mcp-test--clock-closed-in-modified-buffer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  *1:00\n"
   ":END:\n"
   "\n"
   "\\* TODO Task Two\n"
   "\\'")
  "Regex matching the complete buffer whose open clock clock-in closed at 11:00.
The buffer also keeps the unsaved Task Two edit made before the call.")

(ert-deftest org-mcp-test-clock-in-closes-active-in-modified-buffer ()
  "Test clock-in reports `saved' false when the clock it closes stays unsaved.
The active clock sits in another allowed file whose buffer already has
unsaved edits.  The target file reaches disk, but the closed clock only
lands in that buffer, so the response covers both edits."
  (org-mcp-test--with-temp-org-files
      ((file-1 org-mcp-test--clock-task-with-open-clock)
       (file-2 org-mcp-test--clock-task-content))
    (let ((buffer (find-file-noselect file-1)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "\n* TODO Task Two\n")
              (should (buffer-modified-p)))
            (let ((result (org-mcp-test--call-clock-in
                           (format "%s#Task%%20One" file-2)
                           "2026-01-01T11:00:00")))
              (should (equal (alist-get 'success result) t))
              (should (eq (alist-get 'saved result) :json-false))
              (should (equal (alist-get 'clocked_in result) t)))
            (org-mcp-test--verify-file-matches
             file-2 org-mcp-test--clock-in-at-eleven-expected-regex)
            (with-current-buffer buffer
              (should (buffer-modified-p)))
            (org-mcp-test--verify-buffer-matches
             buffer
             org-mcp-test--clock-closed-in-modified-buffer-expected-regex)
            (should (string= (org-mcp-test--read-file file-1)
                             org-mcp-test--clock-task-with-open-clock)))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-clock-in-closes-active-explicit-start ()
  "Test clock-in with explicit start closes active clock at that start time."
  (org-mcp-test--with-temp-org-files
      ((file-1 org-mcp-test--clock-task-content)
       (file-2 org-mcp-test--clock-task-content))
    (let* ((uri-1 (format "%s#Task%%20One" file-1))
           (uri-2 (format "%s#Task%%20One" file-2))
           (result-1 (org-mcp-test--call-clock-in
                      uri-1 "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result-1) t))
      ;; Clock in to file-2 at 12:00 — should close file-1 at 12:00
      (let ((result-2 (org-mcp-test--call-clock-in
                       uri-2 "2026-01-01T12:00:00")))
        (should (equal (alist-get 'success result-2) t))
        ;; Verify file-1 has a 2-hour closed clock
         (should
          (string-match-p
           "=>  2:00"
           (org-mcp-test--read-file file-1)))))))

(ert-deftest org-mcp-test-clock-out-no-drawer ()
  "Test clock-out closes a bare CLOCK when `org-clock-into-drawer' is nil."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--clock-task-with-open-clock-no-drawer))
    (let ((org-clock-into-drawer nil))
      (let ((result (org-mcp-test--call-clock-out
                     "2026-01-01T11:00:00")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'clocked_out result) t)))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--clock-out-no-drawer-expected-regex))))

(ert-deftest org-mcp-test-clock-add-custom-drawer ()
  "Test clock-add uses custom drawer name from `org-clock-into-drawer'."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let ((org-clock-into-drawer "WORK"))
      (let* ((uri (format "%s#Task%%20One" test-file))
             (result (org-mcp-test--call-clock-add
                      uri "2026-01-01T10:00:00" "2026-01-01T11:00:00")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'added result) t))
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--clock-add-custom-drawer-expected-regex)))))

;;; Tests for org-clock-get-active

(defconst org-mcp-test--clock-mixed-open-closed-content
  (concat
   "* TODO Task One\n:LOGBOOK:\n"
   "CLOCK: [2026-01-02 Fri 09:00]\n"
   "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:00] =>  1:00\n"
   ":END:\n")
  "LOGBOOK with one open and one closed CLOCK entry.")

(defconst org-mcp-test--clock-only-closed-content
  (concat
   "* TODO Task One\n:LOGBOOK:\n"
   "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:00] =>  1:00\n"
   ":END:\n")
  "LOGBOOK with only a single closed CLOCK entry.")

(defconst org-mcp-test--clock-locale-variant-open-content
  "* TODO Task\n:LOGBOOK:\nCLOCK: [2026-01-01 Mon 10:00]\n:END:\n"
  "Open CLOCK with an incorrect day-of-week label (Mon for a Thursday).
Used to verify that date/time matching tolerates locale/label drift.")

(defmacro org-mcp-test--with-session-clock (file &rest body)
  "Run BODY with the Emacs clock running on the open CLOCK line in FILE.
Points `org-clock-marker' at the end of the first unclosed CLOCK line
in FILE, where `org-clock-in' leaves it, and unsets the marker again
afterwards so the clock state does not leak into other tests."
  (declare (indent 1) (debug t))
  `(let ((buffer (find-file-noselect ,file)))
     (unwind-protect
         (progn
           (with-current-buffer buffer
             (goto-char (point-min))
             (re-search-forward
              (concat "^[ \t]*" org-clock-string
                      "[ \t]*\\[[^]\n]+\\][ \t]*$"))
             (set-marker org-clock-marker (point) buffer))
           ,@body)
       (set-marker org-clock-marker nil)
       (kill-buffer buffer))))

(ert-deftest org-mcp-test-clock-get-active-none ()
  "Test org-clock-get-active reports no active clock."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let ((result (org-mcp-test--call-clock-get-active)))
      (should (eq (alist-get 'active result) :json-false)))))

(ert-deftest org-mcp-test-clock-get-active-open-clock ()
  "Test org-clock-get-active finds an open CLOCK in an allowed file."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (let ((result (org-mcp-test--call-clock-get-active)))
      (should (eq (alist-get 'active result) t))
      (should (equal (alist-get 'heading result) "Task One"))
      (should
       (equal (alist-get 'start result) "2026-01-01 Thu 10:00")))))

(ert-deftest org-mcp-test-clock-get-active-ignores-closed ()
  "Test org-clock-get-active ignores files with only closed clocks."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-only-closed-content))
    (let ((result (org-mcp-test--call-clock-get-active)))
      (should (eq (alist-get 'active result) :json-false)))))

(ert-deftest org-mcp-test-clock-get-active-mixed-open-closed ()
  "Test clock-get-active picks the open entry in a mixed LOGBOOK."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-mixed-open-closed-content))
    (let ((result (org-mcp-test--call-clock-get-active)))
      (should (eq (alist-get 'active result) t))
      (should
       (equal (alist-get 'start result) "2026-01-02 Fri 09:00")))))

(ert-deftest org-mcp-test-clock-get-active-multiple-files-first-wins ()
  "Test clock-get-active returns the first open clock by file order."
  (org-mcp-test--with-temp-org-files
      ((file-a
        "* TODO First Task\n:LOGBOOK:\nCLOCK: [2026-01-01 Thu 10:00]\n:END:\n")
       (file-b
        "* TODO Second Task\n:LOGBOOK:\nCLOCK: [2026-01-01 Thu 11:00]\n:END:\n"))
    (let ((result (org-mcp-test--call-clock-get-active)))
      (should (eq (alist-get 'active result) t))
      (should (equal (alist-get 'heading result) "First Task"))
      (should
       (equal (alist-get 'start result) "2026-01-01 Thu 10:00")))))

(ert-deftest org-mcp-test-clock-get-active-locale-variant-day ()
  "Test clock-get-active tolerates a non-canonical day-of-week label.
2026-01-01 is a Thursday; the stored CLOCK timestamp uses \"Mon\".
The Org element parser should still recognize the open clock."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-locale-variant-open-content))
    (let ((result (org-mcp-test--call-clock-get-active)))
      (should (eq (alist-get 'active result) t))
      (should
       (equal (alist-get 'start result) "2026-01-01 Mon 10:00")))))

(ert-deftest org-mcp-test-clock-get-active-session-clock-wins ()
  "Test clock-get-active returns the clock the session is running.
Both allowed files hold an open CLOCK, and the Emacs clock runs in the
one that does not come first in the allowed list."
  (org-mcp-test--with-temp-org-files
      ((file-a
        "* TODO First Task\n:LOGBOOK:\nCLOCK: [2026-01-01 Thu 10:00]\n:END:\n")
       (file-b
        "* TODO Second Task\n:LOGBOOK:\nCLOCK: [2026-01-01 Thu 11:00]\n:END:\n"))
    (org-mcp-test--with-session-clock file-b
      (let ((result (org-mcp-test--call-clock-get-active)))
        (should (eq (alist-get 'active result) t))
        (should (equal (alist-get 'heading result) "Second Task"))
        (should
         (equal (alist-get 'start result) "2026-01-01 Thu 11:00"))))))

(ert-deftest org-mcp-test-clock-find-active-session-clock-allowed ()
  "Test clock-find-active describes a session clock in an allowed file.
The entry carries the running clock's own marker, the heading text,
and `allowed' set to t."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (org-mcp-test--with-session-clock test-file
      (let ((active (org-mcp--clock-find-active)))
        (should (eq (alist-get 'marker active) org-clock-marker))
        (should (eq (alist-get 'allowed active) t))
        (should (equal (alist-get 'heading active) "Task One"))
        (should
         (equal (alist-get 'start active) "2026-01-01 Thu 10:00"))
        (should
         (org-mcp--paths-equal-p (alist-get 'file active) test-file))))))

(ert-deftest org-mcp-test-clock-find-active-session-clock-not-allowed ()
  "Test clock-find-active describes a session clock outside allowed files.
The entry is returned with `allowed' nil, and org-clock-get-active
reports it as active but not in an allowed file."
  (org-mcp-test--with-temp-org-files
      ((allowed-file org-mcp-test--clock-task-content)
       (outside-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-mcp-allowed-files (list allowed-file)))
      (org-mcp-test--with-session-clock outside-file
        (let ((active (org-mcp--clock-find-active)))
          (should active)
          (should (eq (alist-get 'allowed active) nil))
          (should (equal (alist-get 'heading active) "Task One"))
          (should
           (org-mcp--paths-equal-p
            (alist-get 'file active) outside-file)))
        (let ((result (org-mcp-test--call-clock-get-active)))
          (should (eq (alist-get 'active result) t))
          (should
           (eq (alist-get 'in_allowed_file result) :json-false)))))))

(ert-deftest org-mcp-test-clock-in-closes-session-clock-in-modified-buffer ()
  "Test clock-in reports `saved' false for a session clock left unsaved.
The Emacs clock runs in a non-allowed file whose buffer already has
unsaved edits.  Closing it only lands in that buffer, so the response
reports `saved' as false although the target file reaches disk."
  (org-mcp-test--with-temp-org-files
      ((allowed-file org-mcp-test--clock-task-content)
       (outside-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-mcp-allowed-files (list allowed-file)))
      (org-mcp-test--with-session-clock outside-file
        (let ((outside-buffer (find-buffer-visiting outside-file)))
          (with-current-buffer outside-buffer
            (goto-char (point-max))
            (insert "\n* TODO Task Two\n")
            (should (buffer-modified-p)))
          (let ((result (org-mcp-test--call-clock-in
                         (format "%s#Task%%20One" allowed-file)
                         "2026-01-01T11:00:00")))
            (should (equal (alist-get 'success result) t))
            (should (eq (alist-get 'saved result) :json-false))
            (should (equal (alist-get 'clocked_in result) t)))
          (org-mcp-test--verify-file-matches
           allowed-file org-mcp-test--clock-in-at-eleven-expected-regex)
          (with-current-buffer outside-buffer
            (should (buffer-modified-p)))
          (org-mcp-test--verify-buffer-matches
           outside-buffer
           org-mcp-test--clock-closed-in-modified-buffer-expected-regex)
          (should (string= (org-mcp-test--read-file outside-file)
                           org-mcp-test--clock-task-with-open-clock)))))))

(ert-deftest org-mcp-test-clock-get-active-dangling-without-session ()
  "Test clock-get-active scans allowed files when no clock is running.
With the Emacs clock idle, a CLOCK line left unclosed by an earlier
session is still found, even when an earlier allowed file holds only
closed clocks."
  (org-mcp-test--with-temp-org-files
      ((file-a org-mcp-test--clock-only-closed-content)
       (file-b
        "* TODO Second Task\n:LOGBOOK:\nCLOCK: [2026-01-01 Thu 11:00]\n:END:\n"))
    (should-not (org-clock-is-active))
    (let ((result (org-mcp-test--call-clock-get-active)))
      (should (eq (alist-get 'active result) t))
      (should (equal (alist-get 'heading result) "Second Task"))
      (should
       (equal (alist-get 'start result) "2026-01-01 Thu 11:00")))))

;;; Tests for org-clock-find-dangling

(ert-deftest org-mcp-test-clock-find-dangling-empty ()
  "Test clock-find-dangling returns no open clocks."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let ((result (org-mcp-test--call-clock-find-dangling)))
      (should (equal (alist-get 'total result) 0))
      (should (equal (length (alist-get 'open_clocks result)) 0)))))

(ert-deftest org-mcp-test-clock-find-dangling-mixed-open-closed ()
  "Test clock-find-dangling ignores closed entries in a mixed LOGBOOK."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-mixed-open-closed-content))
    (let* ((result (org-mcp-test--call-clock-find-dangling))
           (clocks (alist-get 'open_clocks result)))
      (should (equal (alist-get 'total result) 1))
      (should (equal (length clocks) 1))
      (should
       (equal (alist-get 'start (aref clocks 0))
              "2026-01-02 Fri 09:00")))))

(ert-deftest org-mcp-test-clock-find-dangling-multiple-files ()
  "Test clock-find-dangling aggregates open clocks across multiple files."
  (org-mcp-test--with-temp-org-files
      ((file-a
        "* TODO First Task\n:LOGBOOK:\nCLOCK: [2026-01-01 Thu 10:00]\n:END:\n")
       (file-b
        "* TODO Second Task\n:LOGBOOK:\nCLOCK: [2026-01-01 Thu 11:00]\n:END:\n"))
    (let* ((result (org-mcp-test--call-clock-find-dangling))
           (clocks (alist-get 'open_clocks result))
           (starts
            (mapcar (lambda (c) (alist-get 'start c))
                    (append clocks nil))))
      (should (equal (alist-get 'total result) 2))
      (should (member "2026-01-01 Thu 10:00" starts))
      (should (member "2026-01-01 Thu 11:00" starts)))))

;;; Tests for org-clock-delete

(defconst org-mcp-test--clock-delete-only-entry-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   "\\'")
  "After deleting the sole CLOCK entry, the LOGBOOK drawer is removed.")

(defconst org-mcp-test--clock-delete-multi-initial-content
  (concat
   "* TODO Task One\n:LOGBOOK:\n"
   "CLOCK: [2026-01-02 Fri 10:00]--[2026-01-02 Fri 11:00] =>  1:00\n"
   "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:00] =>  1:00\n"
   ":END:\n"))

(defconst org-mcp-test--clock-delete-one-of-several-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-02 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-02 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\'")
  "After deleting one of several CLOCK entries, the LOGBOOK drawer remains.")

(ert-deftest org-mcp-test-clock-delete-only-entry-removes-logbook ()
  "Test clock-delete removes the LOGBOOK drawer when it becomes empty."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-only-closed-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (result
            (org-mcp-test--call-clock-delete
             uri "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'deleted result) t))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--clock-delete-only-entry-expected-regex))))

(ert-deftest org-mcp-test-clock-delete-one-of-several-keeps-drawer ()
  "Test clock-delete keeps the LOGBOOK drawer when other entries remain."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-delete-multi-initial-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (result
            (org-mcp-test--call-clock-delete
             uri "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'deleted result) t))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--clock-delete-one-of-several-expected-regex))))

(defconst org-mcp-test--clock-delete-no-drawer-content
  (concat
   "* TODO Task One\n"
   "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:00] =>  1:00\n")
  "Heading with a bare closed CLOCK entry (no drawer).")

(defconst org-mcp-test--clock-delete-no-drawer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   "\\'")
  "After deleting a bare CLOCK with no drawer, only the heading remains.")

(ert-deftest org-mcp-test-clock-delete-no-drawer ()
  "Test clock-delete works when CLOCK lines are bare (no drawer).
Exercises `org-mcp--clock-remove-empty-logbook' when
`org-clock-drawer-name' returns nil -- it must be a no-op."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-delete-no-drawer-content))
    (let ((org-clock-into-drawer nil))
      (let* ((uri (format "%s#Task%%20One" test-file))
             (result
              (org-mcp-test--call-clock-delete
               uri "2026-01-01T10:00:00")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'deleted result) t))
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--clock-delete-no-drawer-expected-regex)))))

(ert-deftest org-mcp-test-clock-delete-locale-variant-day ()
  "Test clock-delete matches on date/time regardless of day-of-week label."
  (org-mcp-test--with-temp-org-files
      ((test-file
        (concat
         "* TODO Task\n:LOGBOOK:\n"
         "CLOCK: [2026-01-01 Mon 10:00]--[2026-01-01 Mon 11:00] =>  1:00\n"
         ":END:\n")))
    (let* ((uri (format "%s#Task" test-file))
           (result
            (org-mcp-test--call-clock-delete
             uri "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'deleted result) t)))))

(ert-deftest org-mcp-test-clock-delete-not-found-errors ()
  "Test clock-delete surfaces an MCP error when no entry matches."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-only-closed-content))
    (let ((uri (format "%s#Task%%20One" test-file)))
      (should-error
       (org-mcp-test--call-clock-delete
        uri "2026-01-03T09:00:00")))))

(defconst org-mcp-test--clock-delete-with-state-note-content
  (concat
   "* TODO Task One\n:LOGBOOK:\n"
   "- State \"DONE\"       from \"TODO\"       [2026-01-01 Thu 12:00]\n"
   "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:00] =>  1:00\n"
   ":END:\n")
  "LOGBOOK with a state-change note and one closed CLOCK entry.")

(defconst org-mcp-test--clock-delete-keeps-state-note-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "- State \"DONE\"       from \"TODO\"       "
   "\\[2026-01-01 [A-Za-z]\\{2,3\\} 12:00\\]\n"
   ":END:\n"
   "\\'")
  "After deleting the only CLOCK, the state note keeps the LOGBOOK alive.")

(ert-deftest org-mcp-test-clock-delete-keeps-drawer-with-state-note ()
  "Test clock-delete leaves LOGBOOK intact when state notes remain.
The drawer must NOT be removed just because the only CLOCK entry
is gone — any non-CLOCK content (e.g. state-change notes) must
keep the drawer alive."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-delete-with-state-note-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (result
            (org-mcp-test--call-clock-delete
             uri "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'deleted result) t))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--clock-delete-keeps-state-note-expected-regex))))

(defconst org-mcp-test--clock-delete-with-blank-line-content
  (concat
   "* TODO Task One\n:LOGBOOK:\n"
   "\n"
   "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:00] =>  1:00\n"
   ":END:\n")
  "LOGBOOK with a leading blank line and a single CLOCK entry.
The blank line is NOT part of the CLOCK element's region, so
deleting the CLOCK leaves whitespace-only content behind.")

(defconst org-mcp-test--clock-delete-keeps-blank-line-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\(?::PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:\n\\)?"
   ":LOGBOOK:\n"
   "\n"
   ":END:\n"
   "\\'")
  "After deleting the CLOCK, a lone blank line keeps LOGBOOK intact.")

(ert-deftest org-mcp-test-clock-delete-keeps-drawer-with-blank-line ()
  "Test clock-delete leaves LOGBOOK intact when only whitespace remains.
`org-remove-empty-drawer-at' treats a blank line as content, so
the drawer must remain.  This pins down behavior on the
whitespace-between-markers edge case."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-delete-with-blank-line-content))
    (let* ((uri (format "%s#Task%%20One" test-file))
           (result
            (org-mcp-test--call-clock-delete
             uri "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'deleted result) t))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--clock-delete-keeps-blank-line-expected-regex))))

;;; Tests for org-set-properties

(ert-deftest org-mcp-test-set-properties-new ()
  "Test setting a new property on a bare task."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((uri (format "%s#Simple%%20Task" test-file))
           (params `((uri . ,uri)
                     (properties . ((EFFORT . "2:00")))))
           (result-text
            (mcp-server-lib-ert-call-tool "org-set-properties" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (string-prefix-p "org://" (alist-get 'uri result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-set-properties-new))))

(ert-deftest org-mcp-test-set-properties-update ()
  "Test updating an existing property."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (let* ((uri (format "%s#Task%%20with%%20Properties"
                        test-file))
           (params `((uri . ,uri)
                     (properties . ((EFFORT . "2:30")))))
           (result-text
            (mcp-server-lib-ert-call-tool "org-set-properties" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-set-properties-update))))

(ert-deftest org-mcp-test-set-properties-delete ()
  "Test deleting a property via null value."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (let* ((uri (format "%s#Task%%20with%%20Properties"
                        test-file))
           (params `((uri . ,uri)
                     (properties . ((EFFORT)))))
           (result-text
            (mcp-server-lib-ert-call-tool "org-set-properties" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-set-properties-delete))))

(ert-deftest org-mcp-test-set-properties-forbid-special ()
  "Test that special properties are rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((uri (format "%s#Simple%%20Task" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-set-properties" 1
                `((uri . ,uri)
                  (properties . ((TODO . "DONE"))))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-set-properties-id-uri ()
  "Test setting properties via ID-based URI."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((uri (format "%s" org-mcp-test--crud-test-id))
          (params `((uri . ,uri)
                    (properties . ((EFFORT . "1:00")))))
          (result-text
           (mcp-server-lib-ert-call-tool "org-set-properties" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'uri result) (concat "org://" uri))))))

(ert-deftest org-mcp-test-set-properties-id-and-custom-id ()
  "Test a client sets ID and CUSTOM_ID on an existing heading.
The heading carries the client's ID and no other, the response
addresses it by that ID, and the ID is not added to
`org-id-locations'."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (org-mcp-test--with-id-tracking (list test-file) nil
      (let* ((params
              `((uri . ,(format "%s#Simple%%20Task" test-file))
                (properties
                 .
                 ((ID . ,org-mcp-test--client-id)
                  (CUSTOM_ID . "simple-task")))))
             (result
              (json-read-from-string
               (mcp-server-lib-ert-call-tool
                "org-set-properties" params))))
        (should
         (equal
          (alist-get 'uri result)
          (concat "org://" org-mcp-test--client-id)))
        (should
         (equal (alist-get 'properties_set result) ["ID" "CUSTOM_ID"]))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-set-properties-id-and-custom-id)
        (should-not
         (org-mcp-test--id-registered-p org-mcp-test--client-id))))))

(ert-deftest org-mcp-test-set-properties-invalid-name ()
  "Test an invalid property name refuses the whole call.
A valid property sent first is not written either, so the file and any
buffer visiting it stay unchanged.  The call comes from a buffer with
Emacs Lisp syntax, where a line break is not whitespace, and a name
with one is refused all the same."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (with-syntax-table emacs-lisp-mode-syntax-table
      (org-mcp-test--call-set-properties-expecting-error
       test-file (format "%s#Simple%%20Task" test-file)
       '((OK . "1") ("A\nB" . "v"))))))

(ert-deftest org-mcp-test-set-properties-multiline-value ()
  "Test a line break in a property value refuses the call."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (org-mcp-test--call-set-properties-expecting-error
     test-file (format "%s#Simple%%20Task" test-file)
     '((FOO . "x\r* Injected heading")))))

;;; Tests for org-update-scheduled

(ert-deftest org-mcp-test-update-scheduled-set ()
  "Test setting SCHEDULED on entry without one."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((uri (format "%s#Simple%%20Task" test-file))
           (params `((uri . ,uri)
                     (scheduled . "2026-03-27")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-update-scheduled" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'previous_scheduled result) ""))
      (should (string-match-p "<2026-03-27"
                              (alist-get 'new_scheduled result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-set))))

(ert-deftest org-mcp-test-update-scheduled-update ()
  "Test updating an existing SCHEDULED timestamp."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (let* ((uri (format "%s#Scheduled%%20Task" test-file))
           (params `((uri . ,uri)
                     (scheduled . "2026-04-15")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-update-scheduled" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (string-match-p "<2026-03-01"
                              (alist-get 'previous_scheduled result)))
      (should (string-match-p "<2026-04-15"
                              (alist-get 'new_scheduled result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-update))))

(ert-deftest org-mcp-test-update-scheduled-remove ()
  "Test removing SCHEDULED timestamp."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (let* ((uri (format "%s#Scheduled%%20Task" test-file))
           (params `((uri . ,uri)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-update-scheduled" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'new_scheduled result) ""))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-remove))))

(ert-deftest org-mcp-test-update-scheduled-invalid-date ()
  "Test that invalid date format triggers an error."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((uri (format "%s#Simple%%20Task" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-update-scheduled" 1
                `((uri . ,uri)
                  (scheduled . "not-a-date"))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-update-scheduled-id-uri ()
  "Test setting SCHEDULED via ID-based URI."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((uri (format "%s" org-mcp-test--crud-test-id))
          (params `((uri . ,uri)
                    (scheduled . "2026-03-27")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-update-scheduled" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'uri result) (concat "org://" uri))))))

;;; Tests for org-update-deadline

(ert-deftest org-mcp-test-update-deadline-set ()
  "Test setting DEADLINE on entry without one."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((uri (format "%s#Simple%%20Task" test-file))
           (params `((uri . ,uri)
                     (deadline . "2026-03-27")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-update-deadline" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'previous_deadline result) ""))
      (should (string-match-p "<2026-03-27"
                              (alist-get 'new_deadline result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-deadline-set))))

(ert-deftest org-mcp-test-update-deadline-update ()
  "Test updating an existing DEADLINE timestamp."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (let* ((uri (format "%s#Deadline%%20Task" test-file))
           (params `((uri . ,uri)
                     (deadline . "2026-04-15")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-update-deadline" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (string-match-p "<2026-03-15"
                              (alist-get 'previous_deadline result)))
      (should (string-match-p "<2026-04-15"
                              (alist-get 'new_deadline result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-deadline-update))))

(ert-deftest org-mcp-test-update-deadline-remove ()
  "Test removing DEADLINE timestamp."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (let* ((uri (format "%s#Deadline%%20Task" test-file))
           (params `((uri . ,uri)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-update-deadline" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'new_deadline result) ""))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-deadline-remove))))

(ert-deftest org-mcp-test-update-deadline-invalid-date ()
  "Test that invalid date format triggers an error."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((uri (format "%s#Simple%%20Task" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-update-deadline" 1
                `((uri . ,uri)
                  (deadline . "not-a-date"))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-update-deadline-id-uri ()
  "Test setting DEADLINE via ID-based URI."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((uri (format "%s" org-mcp-test--crud-test-id))
          (params `((uri . ,uri)
                    (deadline . "2026-03-27")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-update-deadline" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'uri result) (concat "org://" uri))))))

;;; Tests for org-set-tags

(ert-deftest org-mcp-test-set-tags-add ()
  "Test adding tags to a bare task."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((org-tag-alist '("work" "personal" "urgent"))
           (uri (format "%s#Simple%%20Task" test-file))
           (params `((uri . ,uri)
                     (tags . ["work" "urgent"])))
           (result-text
            (mcp-server-lib-ert-call-tool "org-set-tags" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'previous_tags result) []))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-set))))

(ert-deftest org-mcp-test-set-tags-replace ()
  "Test replacing existing tags."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let* ((org-tag-alist '("work" "personal" "urgent"))
           (uri (format "%s#Task%%20with%%20Tags"
                        test-file))
           (params `((uri . ,uri)
                     (tags . "personal")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-set-tags" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-replace))))

(ert-deftest org-mcp-test-set-tags-clear ()
  "Test clearing all tags."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let* ((uri (format "%s#Task%%20with%%20Tags"
                        test-file))
           (params `((uri . ,uri)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-set-tags" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'new_tags result) []))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-clear))))

(ert-deftest org-mcp-test-set-tags-invalid-name ()
  "Test that invalid tag names are rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((uri (format "%s#Simple%%20Task" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-set-tags" 1
                `((uri . ,uri)
                  (tags . "invalid tag!"))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-set-tags-free-form-with-alist ()
  "Free-form tags are accepted even when `org-tag-alist' is configured.
Org permits free-form tags, so we only enforce `org-tag-re' here,
not membership in the configured alist."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((org-tag-alist '("work" "personal"))
           (uri (format "%s#Simple%%20Task" test-file))
           (params `((uri . ,uri)
                     (tags . "nonexistent")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-set-tags" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'new_tags result) ["nonexistent"])))))

(ert-deftest org-mcp-test-set-tags-mutex-violation ()
  "Test that mutually exclusive tags are rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-tag-alist '(:startgroup "work" "personal" :endgroup "urgent"))
          (uri (format "%s#Simple%%20Task" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-set-tags" 1
                `((uri . ,uri)
                  (tags . ["work" "personal"]))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-set-tags-id-uri ()
  "Test setting tags via ID-based URI."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((uri (format "%s" org-mcp-test--crud-test-id))
          (params `((uri . ,uri)
                    (tags . "work")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-set-tags" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'uri result) (concat "org://" uri))))))

;;; Tests for org-set-priority

(ert-deftest org-mcp-test-set-priority-set ()
  "Test setting priority on a bare task."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((uri (format "%s#Simple%%20Task" test-file))
           (params `((uri . ,uri)
                     (priority . "A")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-set-priority" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'previous_priority result) ""))
      (should (equal (alist-get 'new_priority result) "A"))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-priority-set))))

(ert-deftest org-mcp-test-set-priority-change ()
  "Test changing existing priority."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (let* ((uri (format "%s#Priority%%20Task" test-file))
           (params `((uri . ,uri)
                     (priority . "C")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-set-priority" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'previous_priority result) "B"))
      (should (equal (alist-get 'new_priority result) "C"))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-priority-change))))

(ert-deftest org-mcp-test-set-priority-remove ()
  "Test removing priority."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (let* ((uri (format "%s#Priority%%20Task" test-file))
           (params `((uri . ,uri)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-set-priority" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'new_priority result) ""))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-priority-remove))))

(ert-deftest org-mcp-test-set-priority-out-of-range ()
  "Test that out-of-range priority is rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((uri (format "%s#Simple%%20Task" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-set-priority" 1
                `((uri . ,uri)
                  (priority . "Z"))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-set-priority-multi-char ()
  "Test that multi-character priority is rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((uri (format "%s#Simple%%20Task" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-set-priority" 1
                `((uri . ,uri)
                  (priority . "AB"))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-set-priority-id-uri ()
  "Test setting priority via ID-based URI."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((uri (format "%s" org-mcp-test--crud-test-id))
          (params `((uri . ,uri)
                    (priority . "A")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-set-priority" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'uri result) (concat "org://" uri))))))

;;; Tests for org-edit-body append mode

(ert-deftest org-mcp-test-edit-body-append ()
  "Test appending to existing body."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((uri (format "%s#Simple%%20Task" test-file))
           (params `((resource_uri . ,uri)
                     (old_body . "")
                     (new_body . "Appended line.")
                     (append . t)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-edit-body" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (string-prefix-p "org://" (alist-get 'uri result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-append-body))))

(ert-deftest org-mcp-test-edit-body-append-empty-entry ()
  "Test appending to entry with no body."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-empty-body))
    (let* ((uri (format "%s#Empty%%20Body%%20Task"
                        test-file))
           (params `((resource_uri . ,uri)
                     (old_body . "")
                     (new_body . "New body content.")
                     (append . t)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-edit-body" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-append-body-empty))))

(ert-deftest org-mcp-test-edit-body-append-before-children ()
  "Test that appended content goes before child headlines."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-children))
    (let* ((uri (format "%s#Parent%%20Task" test-file))
           (params `((resource_uri . ,uri)
                     (old_body . "")
                     (new_body . "Appended text.")
                     (append . t)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-edit-body" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-append-body-with-children))))

(ert-deftest org-mcp-test-edit-body-append-headline-error ()
  "Test that content with headlines is rejected in append mode."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((uri (format "%s#Simple%%20Task" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-edit-body" 1
                `((resource_uri . ,uri)
                  (old_body . "")
                  (new_body . "* A headline")
                  (append . t))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-edit-body-append-unbalanced-blocks-error ()
  "Test that unbalanced blocks are rejected in append mode."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((uri (format "%s#Simple%%20Task" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-edit-body" 1
                `((resource_uri . ,uri)
                  (old_body . "")
                  (new_body . "#+BEGIN_SRC\ncode\n")
                  (append . t))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-edit-body-append-id-uri ()
  "Test appending body via ID-based URI."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((uri (format "%s" org-mcp-test--crud-test-id))
          (params `((resource_uri . ,uri)
                    (old_body . "")
                    (new_body . "Appended.")
                    (append . t)))
          (result-text
           (mcp-server-lib-ert-call-tool "org-edit-body" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'uri result) (concat "org://" uri))))))

;;; Tests for org-add-logbook-note

(ert-deftest org-mcp-test-add-logbook-note-new ()
  "Test adding logbook note to task without LOGBOOK."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-log-into-drawer t))
      (let* ((uri (format "%s#Simple%%20Task" test-file))
             (params `((uri . ,uri)
                       (note . "This is my note.")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-add-logbook-note" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (string-prefix-p "org://" (alist-get 'uri result)))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-logbook-note-new)))))

(ert-deftest org-mcp-test-add-logbook-note-existing ()
  "Test adding logbook note to task with existing LOGBOOK."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-logbook))
    (let ((org-log-into-drawer t))
      (let* ((uri (format "%s#Task%%20with%%20Logbook"
                          test-file))
             (params `((uri . ,uri)
                       (note . "Another note.")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-add-logbook-note" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-logbook-note-existing)))))

(ert-deftest org-mcp-test-add-logbook-note-multiline ()
  "Test adding multiline logbook note with proper indentation."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-log-into-drawer t))
      (let* ((uri (format "%s#Simple%%20Task" test-file))
             (params `((uri . ,uri)
                       (note . "First line.\nSecond line.")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-add-logbook-note" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-logbook-note-multiline)))))

(ert-deftest org-mcp-test-add-logbook-note-whitespace-only-error ()
  "Test that whitespace-only note is rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((uri (format "%s#Simple%%20Task" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-add-logbook-note" 1
                `((uri . ,uri)
                  (note . "   "))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-add-logbook-note-id-uri ()
  "Test adding logbook note via ID-based URI."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((uri (format "%s" org-mcp-test--crud-test-id))
          (params `((uri . ,uri)
                    (note . "Test note.")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-add-logbook-note" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'uri result) (concat "org://" uri))))))

(ert-deftest org-mcp-test-add-logbook-note-special-chars ()
  "Test adding logbook note containing special characters."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-log-into-drawer t))
      (let* ((uri (format "%s#Simple%%20Task" test-file))
             (params
              `((uri . ,uri)
                (note
                 . "Quotes \"like this\", backslash \\, percent %, asterisk *.")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-add-logbook-note" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-logbook-note-special-chars)))))

(ert-deftest org-mcp-test-add-logbook-note-no-drawer ()
  "Test adding logbook note when `org-log-into-drawer' is nil.
The note is inserted under the heading rather than in a LOGBOOK
drawer, matching Org's own behavior."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-log-into-drawer nil))
      (let* ((uri (format "%s#Simple%%20Task" test-file))
             (params `((uri . ,uri)
                       (note . "Plain note.")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-add-logbook-note" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-logbook-note-no-drawer)))))

(ert-deftest org-mcp-test-add-logbook-note-custom-heading ()
  "Test that a non-default `org-log-note-headings' template is honored."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-log-into-drawer t)
          (org-log-note-headings
           '((note . "Custom note prefix %t"))))
      (let* ((uri (format "%s#Simple%%20Task" test-file))
             (params `((uri . ,uri)
                       (note . "My note.")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-add-logbook-note" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-logbook-note-custom-heading)))))

;; Helper functions for testing org-ql-query MCP tool

(defun org-mcp-test--call-ql-query (query)
  "Call org-ql-query tool via JSON-RPC and return the parsed result.
QUERY is the org-ql query sexp as a string."
  (let* ((params `((query . ,query)))
         (result-text
          (mcp-server-lib-ert-call-tool "org-ql-query" params)))
    (json-read-from-string result-text)))

(ert-deftest org-mcp-test-ql-query-uri-with-id ()
  "Test that org-ql-query returns org:// URI with UUID for headlines with IDs."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-with-id-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'matches result))
           (first-match (aref matches 0))
           (uri (alist-get 'uri first-match)))
      (should (equal (alist-get 'total result) 1))
      (should (equal uri org-mcp-test--content-with-id-resource-uri)))))

(ert-deftest org-mcp-test-ql-query-uri-without-id ()
  "Test that org-ql-query returns org:// path URI for headlines without IDs."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'matches result))
           (first-match (aref matches 0))
           (uri (alist-get 'uri first-match)))
      (should (equal (alist-get 'total result) 1))
      (should (string-match-p
               (concat "\\`org://" (regexp-quote test-file)
                       "#Simple%20Task\\'")
               uri)))))

;;; Extra-properties tests

(defconst org-mcp-test--content-parent-child
  "* [#A] Parent
** TODO Child Task
Child body."
  "Parent with priority A and a child TODO.")

(ert-deftest org-mcp-test-ql-extra-properties ()
  "Extra properties from `org-mcp-ql-extra-properties' appear in results."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-parent-child))
    (let ((org-mcp-ql-extra-properties
           `((parent-priority
              . ,(lambda ()
                   (let ((p (save-excursion
                              (when (org-up-heading-safe)
                                (org-element-property
                                 :priority (org-element-at-point))))))
                     (when p (char-to-string p)))))
             (rank . ,(lambda () 42)))))
      (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
             (matches (alist-get 'matches result))
             (match (aref matches 0)))
        (should (equal (alist-get 'parent-priority match) "A"))
        (should (equal (alist-get 'rank match) 42))))))

(ert-deftest org-mcp-test-ql-extra-properties-nil-omitted ()
  "Extra properties returning nil are omitted from results."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-mcp-ql-extra-properties
           `((nope . ,(lambda () nil)))))
      (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
             (matches (alist-get 'matches result))
             (match (aref matches 0)))
        (should-not (assq 'nope match))))))

(defconst org-mcp-test--content-ql-tags-scheduled-deadline
  "* TODO Tagged Task                                                 :work:home:
SCHEDULED: <2024-03-15 Fri> DEADLINE: <2024-03-20 Wed>"
  "TODO task with tags, scheduled, and deadline for org-ql query tests.")

(ert-deftest org-mcp-test-ql-query-exports-tags ()
  "Test that org-ql-query includes tags in match results."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ql-tags-scheduled-deadline))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'matches result))
           (match (aref matches 0)))
      (should (equal (alist-get 'tags match) ["work" "home"])))))

(ert-deftest org-mcp-test-ql-query-exports-scheduled ()
  "Test that org-ql-query includes scheduled date in match results."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ql-tags-scheduled-deadline))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'matches result))
           (match (aref matches 0)))
      (should (stringp (alist-get 'scheduled match)))
      (should (string-match-p "2024-03-15" (alist-get 'scheduled match))))))

(ert-deftest org-mcp-test-ql-query-exports-deadline ()
  "Test that org-ql-query includes deadline in match results."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ql-tags-scheduled-deadline))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'matches result))
           (match (aref matches 0)))
      (should (stringp (alist-get 'deadline match)))
      (should (string-match-p "2024-03-20" (alist-get 'deadline match))))))

(ert-deftest org-mcp-test-ql-query-no-tags-absent ()
  "Test that tags key is absent when headline has no tags."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'matches result))
           (match (aref matches 0)))
      (should-not (assq 'tags match)))))

(ert-deftest org-mcp-test-ql-query-no-scheduled-absent ()
  "Test that scheduled key is absent when headline has no scheduled date."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'matches result))
           (match (aref matches 0)))
      (should-not (assq 'scheduled match)))))

(ert-deftest org-mcp-test-ql-query-no-deadline-absent ()
  "Test that deadline key is absent when headline has no deadline."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'matches result))
           (match (aref matches 0)))
      (should-not (assq 'deadline match)))))

(defconst org-mcp-test--content-ql-priority-closed
  "* DONE [#A] Closed Task
CLOSED: [2024-04-01 Mon 15:30]"
  "DONE task with priority A and a CLOSED timestamp for ql tests.")

(ert-deftest org-mcp-test-ql-query-exports-priority ()
  "Test that org-ql-query returns priority as a one-character string."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ql-priority-closed))
    (let* ((result (org-mcp-test--call-ql-query "(done)"))
           (matches (alist-get 'matches result))
           (match (aref matches 0)))
      (should (equal (alist-get 'priority match) "A")))))

(ert-deftest org-mcp-test-ql-query-exports-closed ()
  "Test that org-ql-query includes CLOSED timestamp in match results."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ql-priority-closed))
    (let* ((result (org-mcp-test--call-ql-query "(done)"))
           (matches (alist-get 'matches result))
           (match (aref matches 0)))
      (should (stringp (alist-get 'closed match)))
      (should (string-match-p "2024-04-01" (alist-get 'closed match))))))

(ert-deftest org-mcp-test-ql-query-no-priority-absent ()
  "Test that priority key is absent when headline has no priority."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'matches result))
           (match (aref matches 0)))
      (should-not (assq 'priority match)))))

(ert-deftest org-mcp-test-ql-query-no-closed-absent ()
  "Test that closed key is absent when headline has no CLOSED timestamp."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'matches result))
           (match (aref matches 0)))
      (should-not (assq 'closed match)))))

(defconst org-mcp-test--content-ql-with-custom-prop
  "* TODO Task with Custom
:PROPERTIES:
:EFFORT: 2:00
:CONTEXT: laptop
:END:"
  "TODO task with custom properties for ql standard-properties test.")

(ert-deftest org-mcp-test-ql-query-exports-standard-properties ()
  "Test that org-ql-query includes non-filtered PROPERTIES drawer values."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ql-with-custom-prop))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'matches result))
           (match (aref matches 0))
           (props (alist-get 'properties match)))
      (should (equal (alist-get 'EFFORT props) "2:00"))
      (should (equal (alist-get 'CONTEXT props) "laptop")))))

;;; Tests for org-read structured metadata extraction
;;
;; These verify `org-mcp--extract-structured-heading' (via the org-read
;; tool) emits priority, planning timestamps, and ID consistently with
;; the canonical metadata extractor.

(defconst org-mcp-test--content-read-full-metadata
  "* TODO [#B] Full Metadata Task                                 :work:home:
SCHEDULED: <2024-05-01 Wed> DEADLINE: <2024-05-08 Wed>
:PROPERTIES:
:ID: full-meta-task-id
:END:
Body line."
  "TODO task with priority B, tags, scheduled, deadline, and ID.")

(defconst org-mcp-test--content-read-closed-task
  "* DONE [#A] Closed Read Task
CLOSED: [2024-05-15 Wed 09:00]"
  "DONE task with CLOSED timestamp for org-read tests.")

(defun org-mcp-test--read-structured (file headline)
  "Return parsed JSON alist for HEADLINE in FILE via org-read."
  (let* ((uri (format "%s#%s" file headline))
         (result-text (org-mcp-test--call-read uri)))
    (json-parse-string result-text :object-type 'alist)))

(ert-deftest org-mcp-test-read-exports-priority ()
  "Test that org-read returns priority as a one-character string."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-read-full-metadata))
    (let ((result (org-mcp-test--read-structured
                   test-file "Full%20Metadata%20Task")))
      (should (equal (alist-get 'priority result) "B")))))

(ert-deftest org-mcp-test-read-exports-scheduled-deadline ()
  "Test that org-read includes scheduled and deadline timestamps."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-read-full-metadata))
    (let ((result (org-mcp-test--read-structured
                   test-file "Full%20Metadata%20Task")))
      (should (equal (alist-get 'scheduled result) "<2024-05-01 Wed>"))
      (should (equal (alist-get 'deadline result) "<2024-05-08 Wed>")))))

(ert-deftest org-mcp-test-read-exports-id ()
  "Test that org-read includes ID from PROPERTIES drawer."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-read-full-metadata))
    (let ((result (org-mcp-test--read-structured
                   test-file "Full%20Metadata%20Task")))
      (should (equal (alist-get 'id result) "full-meta-task-id")))))

(ert-deftest org-mcp-test-read-exports-tags-with-inheritance ()
  "Test that org-read includes the heading's tag list."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-read-full-metadata))
    (let ((result (org-mcp-test--read-structured
                   test-file "Full%20Metadata%20Task")))
      (should (equal (alist-get 'tags result) ["work" "home"])))))

(ert-deftest org-mcp-test-read-exports-closed ()
  "Test that org-read includes CLOSED timestamp."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-read-closed-task))
    (let ((result (org-mcp-test--read-structured
                   test-file "Closed%20Read%20Task")))
      (should (equal (alist-get 'closed result)
                     "[2024-05-15 Wed 09:00]")))))

(ert-deftest org-mcp-test-read-bare-omits-optional-fields ()
  "Test that org-read omits optional fields when absent on bare heading."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((result (org-mcp-test--read-structured
                   test-file "Simple%20Task")))
      (should-not (assq 'priority result))
      (should-not (assq 'scheduled result))
      (should-not (assq 'deadline result))
      (should-not (assq 'closed result))
      (should-not (assq 'id result))
      (should-not (assq 'tags result)))))

;;; GTD query tool tests

(defconst org-mcp-test--content-gtd-items
  "* TODO Inbox item :#inbox:

* TODO [#B] Next action

* TODO [#A] High priority next"
  "Items for GTD query tool tests.")

(defmacro org-mcp-test--with-gtd-tools (file-specs bindings &rest body)
  "Create temp org files and enable org-mcp with GTD tool BINDINGS.
FILE-SPECS are (VAR CONTENT) pairs.  BINDINGS is a list of let-style
bindings for GTD customizations that must be set before `org-mcp-enable'."
  (declare (indent 2))
  (let* ((vars (mapcar #'car file-specs))
         (temp-vars (mapcar (lambda (v) (gensym (symbol-name v))) vars))
         (let-bindings (cl-mapcar (lambda (v tv) `(,v ,tv)) vars temp-vars))
         (inits (cl-mapcar
                 (lambda (tv spec)
                   `(setq ,tv (make-temp-file "org-mcp-test" nil ".org"
                                              ,(nth 1 spec))))
                 temp-vars file-specs))
         (cleanups (mapcar (lambda (tv)
                             `(when ,tv (delete-file ,tv)))
                           temp-vars)))
    `(let (,@temp-vars)
       (unwind-protect
           (progn
             ,@inits
             (let (,@let-bindings
                   (org-mcp-allowed-files (list ,@temp-vars))
                   ,@bindings)
               (org-mcp-enable)
               (unwind-protect
                   (mcp-server-lib-ert-with-server :tools t :resources t
                     ,@body)
                 (org-mcp-disable))))
         ,@cleanups))))

(ert-deftest org-mcp-test-query-inbox-tool ()
  "query-inbox tool returns inbox-tagged items."
  (org-mcp-test--with-gtd-tools
      ((test-file org-mcp-test--content-gtd-items))
      ((org-mcp-query-inbox-fn
        (lambda () '(and (not (done)) (tags "#inbox" "inbox"))))
       (org-mcp-query-sort-fn nil))
    (let* ((result-text
            (mcp-server-lib-ert-call-tool "query-inbox" nil))
           (result (json-read-from-string result-text))
           (matches (alist-get 'matches result)))
      (should (equal (alist-get 'total result) 1))
      (should (equal (alist-get 'title (aref matches 0))
                     "Inbox item")))))

(ert-deftest org-mcp-test-query-next-tool ()
  "query-next tool returns next action items."
  (org-mcp-test--with-gtd-tools
      ((test-file org-mcp-test--content-gtd-items))
      ((org-mcp-query-next-fn
        (lambda (&optional _tag-filter)
          '(and (todo "TODO") (not (tags "#inbox" "inbox")))))
       (org-mcp-query-sort-fn nil))
    (let* ((result-text
            (mcp-server-lib-ert-call-tool "query-next" nil))
           (result (json-read-from-string result-text))
           (matches (alist-get 'matches result)))
      (should (equal (alist-get 'total result) 2)))))

(ert-deftest org-mcp-test-query-backlog-tool ()
  "query-backlog tool returns backlog items."
  (org-mcp-test--with-gtd-tools
      ((test-file org-mcp-test--content-gtd-items))
      ((org-mcp-query-backlog-fn
        (lambda (&optional _tag-filter)
          '(todo "TODO")))
       (org-mcp-query-sort-fn nil))
    (let* ((result-text
            (mcp-server-lib-ert-call-tool "query-backlog" nil))
           (result (json-read-from-string result-text))
           (matches (alist-get 'matches result)))
      (should (equal (alist-get 'total result) 3)))))

(ert-deftest org-mcp-test-query-tools-not-registered-when-nil ()
  "GTD query tools are not registered when their fns are nil."
  (org-mcp-test--with-gtd-tools
      ((test-file org-mcp-test--content-bare-todo))
      ((org-mcp-query-inbox-fn nil)
       (org-mcp-query-next-fn nil)
       (org-mcp-query-backlog-fn nil))
    (should-error
     (mcp-server-lib-ert-call-tool "query-inbox" nil))
    (should-error
     (mcp-server-lib-ert-call-tool "query-next" nil))
    (should-error
     (mcp-server-lib-ert-call-tool "query-backlog" nil))))

;;; Native link tests

(defconst org-mcp-test--link-beta-id "8a3c5d2e-4b1f-4c6a-9e7d-0f2b3c4d5e6f"
  "ID of the Beta heading in `org-mcp-test--content-links'.")

(defconst org-mcp-test--content-links-preamble "#+TITLE: Links\n\n"
  "Preamble of `org-mcp-test--content-links'.")

(defconst org-mcp-test--content-links-alpha
  "* Alpha
:PROPERTIES:
:CUSTOM_ID: alpha-slug
:END:
Alpha body.
** Review
Alpha review.
"
  "Alpha subtree of `org-mcp-test--content-links'.
Alpha carries a custom ID and a child titled Review.")

(defconst org-mcp-test--content-links-beta
  (format
   "* Beta
:PROPERTIES:
:ID:       %s
:END:
Beta body.
** Review
Beta review.
"
   org-mcp-test--link-beta-id)
  "Beta subtree of `org-mcp-test--content-links'.
Beta carries an ID and a child titled Review, as Alpha does.")

(defconst org-mcp-test--content-links-gamma "* TODO Gamma\nGamma body.\n"
  "Gamma subtree of `org-mcp-test--content-links', with no identifier.")

(defconst org-mcp-test--content-links
  (concat
   org-mcp-test--content-links-preamble
   org-mcp-test--content-links-alpha
   org-mcp-test--content-links-beta
   org-mcp-test--content-links-gamma)
  "Org file whose headings are reached by ID, custom ID and title links.
Line 10 is the Beta heading.")

(defconst org-mcp-test--regex-links-alpha-tagged
  (concat
   "\\`"
   (regexp-quote org-mcp-test--content-links-preamble)
   "\\* Alpha[ \t]+:work:\n"
   " *:PROPERTIES:\n"
   " *:CUSTOM_ID: +alpha-slug\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Alpha body\\.\n"
   "\\*\\* Review\n"
   "Alpha review\\.\n"
   (regexp-quote org-mcp-test--content-links-beta)
   (regexp-quote org-mcp-test--content-links-gamma)
   "\\'")
  "Regex matching the links file after tagging Alpha.")

(defconst org-mcp-test--regex-links-beta-tagged
  (concat
   "\\`"
   (regexp-quote org-mcp-test--content-links-preamble)
   (regexp-quote org-mcp-test--content-links-alpha)
   "\\* Beta[ \t]+:work:\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--link-beta-id "\n"
   ":END:\n"
   "Beta body\\.\n"
   "\\*\\* Review\n"
   "Beta review\\.\n"
   (regexp-quote org-mcp-test--content-links-gamma)
   "\\'")
  "Regex matching the links file after tagging Beta.")

(defconst org-mcp-test--regex-links-gamma-tagged
  (concat
   "\\`"
   (regexp-quote
    (concat
     org-mcp-test--content-links-preamble
     org-mcp-test--content-links-alpha
     org-mcp-test--content-links-beta))
   "\\* TODO Gamma[ \t]+:work:\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Gamma body\\.\n"
   "\\'")
  "Regex matching the links file after tagging Gamma.")

(defconst org-mcp-test--regex-links-gamma-changed
  (concat
   "\\`"
   (regexp-quote
    (concat
     org-mcp-test--content-links-preamble
     org-mcp-test--content-links-alpha
     org-mcp-test--content-links-beta))
   "\\* TODO [^\n]*Gamma\n"
   "\\(?:.\\|\n\\)*"
   "Gamma body\\.\n"
   "\\'")
  "Regex matching the links file after any change inside Gamma only.")

(defconst org-mcp-test--regex-links-gamma-done
  (concat
   "\\`"
   (regexp-quote
    (concat
     org-mcp-test--content-links-preamble
     org-mcp-test--content-links-alpha
     org-mcp-test--content-links-beta))
   "\\* DONE Gamma\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Gamma body\\.\n"
   "\\'")
  "Regex matching the links file after marking Gamma DONE.")

(defconst org-mcp-test--regex-links-gamma-clocked
  (concat
   "\\`"
   (regexp-quote
    (concat
     org-mcp-test--content-links-preamble
     org-mcp-test--content-links-alpha
     org-mcp-test--content-links-beta))
   "\\* TODO Gamma\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-03-23 [A-Za-z]\\{2,3\\} 14:30\\]"
   "--\\[2026-03-23 [A-Za-z]\\{2,3\\} 16:45\\] => 2:15\n"
   ":END:\n"
   "Gamma body\\.\n"
   "\\'")
  "Regex matching the links file after clocking in and out of Gamma.")

(defconst org-mcp-test--regex-links-alpha-body-appended
  (concat
   "\\`"
   (regexp-quote org-mcp-test--content-links-preamble)
   "\\* Alpha\n"
   " *:PROPERTIES:\n"
   " *:CUSTOM_ID: +alpha-slug\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Alpha body\\.\n"
   "Alpha appended\\.\n"
   "\\*\\* Review\n"
   "Alpha review\\.\n"
   (regexp-quote org-mcp-test--content-links-beta)
   (regexp-quote org-mcp-test--content-links-gamma)
   "\\'")
  "Regex matching the links file after appending to Alpha's body.")

(defconst org-mcp-test--regex-links-alpha-review-renamed
  (concat
   "\\`"
   (regexp-quote org-mcp-test--content-links-preamble)
   "\\* Alpha\n"
   ":PROPERTIES:\n"
   ":CUSTOM_ID: alpha-slug\n"
   ":END:\n"
   "Alpha body\\.\n"
   "\\*\\* First Review\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Alpha review\\.\n"
   (regexp-quote org-mcp-test--content-links-beta)
   (regexp-quote org-mcp-test--content-links-gamma)
   "\\'")
  "Regex matching the links file after renaming Alpha's Review.")

(defconst org-mcp-test--regex-links-beta-clocked
  (concat
   "\\`"
   (regexp-quote org-mcp-test--content-links-preamble)
   (regexp-quote org-mcp-test--content-links-alpha)
   "\\* Beta\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--link-beta-id "\n"
   ":END:\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-03-23 [A-Za-z]\\{2,3\\} 14:30\\]"
   "--\\[2026-03-23 [A-Za-z]\\{2,3\\} 16:45\\] => 2:15\n"
   ":END:\n"
   "Beta body\\.\n"
   "\\*\\* Review\n"
   "Beta review\\.\n"
   (regexp-quote org-mcp-test--content-links-gamma)
   "\\'")
  "Regex matching the links file after adding a clock entry to Beta.")

(defconst org-mcp-test--regex-links-top-level-added
  (concat
   "\\`"
   (regexp-quote org-mcp-test--content-links-preamble)
   "\\* TODO New Task *\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "\n?"
   (regexp-quote org-mcp-test--content-links-alpha)
   (regexp-quote org-mcp-test--content-links-beta)
   (regexp-quote org-mcp-test--content-links-gamma)
   "\\'")
  "Regex matching the links file after adding a top-level TODO.")

(defconst org-mcp-test--regex-links-added-after-beta-review
  (concat
   "\\`"
   (regexp-quote
    (concat
     org-mcp-test--content-links-preamble
     org-mcp-test--content-links-alpha
     org-mcp-test--content-links-beta))
   "\n?"
   "\\*\\* TODO New Task *\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-Fa-f0-9-]+\n"
   " *:END:\n"
   (regexp-quote org-mcp-test--content-links-gamma)
   "\\'")
  "Regex matching the links file after adding a TODO after Beta's Review.")

(defvar org-mcp-test--link-canary nil
  "Set by the `elisp:' link in the refusal tests if it is ever run.")

(defun org-mcp-test--call-tool-expecting-error (test-file tool params)
  "Call TOOL with PARAMS expecting a tool error and return its message.
TEST-FILE is the test file path to verify remains unchanged."
  (let ((original-content (org-mcp-test--read-file test-file))
        (request
         (mcp-server-lib-create-tools-call-request tool 1 params)))
    (prog1 (cadr
            (should-error
             (mcp-server-lib-ert-process-tool-response
              (mcp-server-lib-process-jsonrpc-parsed
               request mcp-server-lib-ert-server-id))
             :type 'mcp-server-lib-tool-error))
      (should
       (string= (org-mcp-test--read-file test-file) original-content)))))

(ert-deftest org-mcp-test-link-read-accepted-forms ()
  "Reading tools accept the three link forms, bare and bracketed."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
      (list org-mcp-test--link-beta-id)
    (let ((alpha (string-trim-right org-mcp-test--content-links-alpha))
          (beta (string-trim-right org-mcp-test--content-links-beta))
          (gamma (string-trim-right org-mcp-test--content-links-gamma)))
      (dolist (case
               `((,(format "id:%s" org-mcp-test--link-beta-id) . ,beta)
                 (,(format "[[id:%s]]" org-mcp-test--link-beta-id)
                  . ,beta)
                 (,(format "[[id:%s][Beta]]" org-mcp-test--link-beta-id)
                  . ,beta)
                 (,(format "file:%s::#alpha-slug" test-file) . ,alpha)
                 (,(format "[[file:%s::#alpha-slug][Alpha]]" test-file)
                  . ,alpha)
                 (,(format "file:%s::*Gamma" test-file) . ,gamma)
                 (,(format "[[file:%s::*Gamma]]" test-file) . ,gamma)))
        (should
         (string=
          (org-mcp-test--call-read-headline (car case)) (cdr case)))))))

(ert-deftest org-mcp-test-link-read-structured ()
  "org-read resolves a link to a heading and a file link to the file."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
      (list org-mcp-test--link-beta-id)
    (let ((heading
           (json-read-from-string
            (org-mcp-test--call-read
             (format "[[id:%s][Beta]]" org-mcp-test--link-beta-id))))
          (file
           (json-read-from-string
            (org-mcp-test--call-read (format "file:%s" test-file)))))
      (should (equal (alist-get 'title heading) "Beta"))
      (should (equal (alist-get 'id heading) org-mcp-test--link-beta-id))
      (should (= (length (alist-get 'children heading)) 1))
      (should (equal (alist-get 'file file) test-file))
      (should (= (length (alist-get 'children file)) 3)))))

(ert-deftest org-mcp-test-link-file-without-search-addresses-file ()
  "A file link with no search part addresses the whole file.
Reading returns the file, adding a TODO under it adds a top-level
heading, and a tool that needs a heading refuses it."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-links))
      (should
       (string=
        (org-mcp-test--call-read-headline (format "file:%s" test-file))
        org-mcp-test--content-links))
      (should
       (string=
        (org-mcp-test--call-read-headline
         (format "[[file:%s][Links]]" test-file))
        org-mcp-test--content-links))
      (should
       (string-match-p
        "does not point to a heading"
        (org-mcp-test--call-tool-expecting-error
         test-file "org-set-tags"
         `((uri . ,(format "file:%s" test-file)) (tags . "work")))))
      (org-mcp-test--add-todo-and-check
       "New Task" "TODO" nil nil (format "file:%s" test-file) nil
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-links-top-level-added))))

(ert-deftest org-mcp-test-link-title-search-resolves-to-first-match ()
  "A title search that matches several headings resolves to the first."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (let ((link (format "file:%s::*Review" test-file)))
      (should
       (string=
        (org-mcp-test--call-read-headline link) "** Review\nAlpha review."))
      (mcp-server-lib-ert-call-tool
       "org-rename-headline"
       `((uri . ,link)
         (current_title . "Review")
         (new_title . "First Review")))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-links-alpha-review-renamed))))

(ert-deftest org-mcp-test-link-id-search-stays-in-subtree ()
  "The search part of an `id:' link runs within the ID's subtree."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
      (list org-mcp-test--link-beta-id)
    (should
     (string=
      (org-mcp-test--call-read-headline
       (format "[[id:%s::*Review]]" org-mcp-test--link-beta-id))
      "** Review\nBeta review."))
    (should
     (string-match-p
      "Cannot resolve link"
      (org-mcp-test--call-tool-expecting-error
       test-file "org-read-headline"
       `((uri
          .
          ,(format "id:%s::*Gamma" org-mcp-test--link-beta-id))))))))

(ert-deftest org-mcp-test-link-file-line-number ()
  "A file link's line number search goes to that line, as in Org."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (should
     (string=
      (org-mcp-test--call-read-headline (format "file:%s::10" test-file))
      (string-trim-right org-mcp-test--content-links-beta)))
    (should
     (string-match-p
      "does not point to a heading"
      (org-mcp-test--call-tool-expecting-error
       test-file "org-read-headline"
       `((uri . ,(format "file:%s::7" test-file))))))))

(ert-deftest org-mcp-test-link-write-id ()
  "Writing tools accept an `id:' link, bare and bracketed."
  (dolist (link
           (list
            (format "id:%s" org-mcp-test--link-beta-id)
            (format "[[id:%s][Beta]]" org-mcp-test--link-beta-id)))
    (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
        (list org-mcp-test--link-beta-id)
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-set-tags" `((uri . ,link) (tags . "work"))))))
        (should (equal (alist-get 'success result) t))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--regex-links-beta-tagged)))))

(ert-deftest org-mcp-test-link-write-custom-id ()
  "Writing tools accept a `file:' link to a custom ID, bare and bracketed."
  (dolist (form '("file:%s::#alpha-slug" "[[file:%s::#alpha-slug]]"))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-links))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-set-tags"
               `((uri . ,(format form test-file)) (tags . "work"))))))
        (should (equal (alist-get 'success result) t))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--regex-links-alpha-tagged)))))

(ert-deftest org-mcp-test-link-write-title ()
  "Writing tools accept a `file:' link to a title, bare and bracketed."
  (dolist (form '("file:%s::*Gamma" "[[file:%s::*Gamma][Gamma]]"))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-links))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-set-tags"
               `((uri . ,(format form test-file)) (tags . "work"))))))
        (should (equal (alist-get 'success result) t))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--regex-links-gamma-tagged)))))

(ert-deftest org-mcp-test-link-update-todo-state ()
  "org-update-todo-state accepts a title link."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (let ((result
           (org-mcp-test--call-update-todo-state
            (format "file:%s::*Gamma" test-file) "DONE" "TODO")))
      (should (equal (alist-get 'previous_state result) "TODO"))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-links-gamma-done))))

(ert-deftest org-mcp-test-link-edit-body ()
  "org-edit-body accepts a bracketed custom ID link."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (org-mcp-test--call-edit-body-and-check
     test-file
     (format "[[file:%s::#alpha-slug][Alpha]]" test-file)
     nil
     "Alpha appended."
     org-mcp-test--regex-links-alpha-body-appended
     t)))

(ert-deftest org-mcp-test-link-heading-tools ()
  "Every other tool that changes a heading accepts a link."
  (dolist (case
           '(("org-set-properties" (properties . ((FOO . "bar"))))
             ("org-update-scheduled" (scheduled . "2026-03-27"))
             ("org-update-deadline" (deadline . "2026-03-27"))
             ("org-set-priority" (priority . "A"))
             ("org-add-logbook-note" (note . "Checked"))))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-links))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               (car case)
               (cons
                `(uri . ,(format "[[file:%s::*Gamma][Gamma]]" test-file))
                (cdr case))))))
        (should (equal (alist-get 'success result) t))
        (should-not
         (string=
          (org-mcp-test--read-file test-file)
          org-mcp-test--content-links))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--regex-links-gamma-changed)))))

(ert-deftest org-mcp-test-link-clock-tools ()
  "Clock tools accept links: add and delete by ID, in and out by file."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
      (list org-mcp-test--link-beta-id)
    (org-mcp-test--call-clock-add
     (format "id:%s" org-mcp-test--link-beta-id)
     "2026-03-23T14:30:00"
     "2026-03-23T16:45:00")
    (org-mcp-test--verify-file-matches
     test-file org-mcp-test--regex-links-beta-clocked)
    (org-mcp-test--call-clock-delete
     (format "[[id:%s][Beta]]" org-mcp-test--link-beta-id)
     "2026-03-23T14:30:00")
    (org-mcp-test--verify-file-matches
     test-file
     (concat "\\`" (regexp-quote org-mcp-test--content-links) "\\'"))
    (org-mcp-test--call-clock-in
     (format "file:%s::*Gamma" test-file) "2026-03-23T14:30:00")
    (mcp-server-lib-ert-call-tool
     "org-clock-out"
     `((uri . ,(format "file:%s" test-file))
       (end_time . "2026-03-23T16:45:00")))
    (org-mcp-test--verify-file-matches
     test-file org-mcp-test--regex-links-gamma-clocked)))

(ert-deftest org-mcp-test-link-add-todo-after-sibling ()
  "org-add-todo takes its parent and the sibling to follow as links."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
        (list org-mcp-test--link-beta-id)
      (org-mcp-test--add-todo-and-check
       "New Task" "TODO" nil nil
       (format "[[id:%s][Beta]]" org-mcp-test--link-beta-id)
       (format "id:%s::*Review" org-mcp-test--link-beta-id)
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-links-added-after-beta-review))))

(ert-deftest org-mcp-test-link-add-todo-after-non-child-refused ()
  "org-add-todo refuses a sibling link that is not a child of the parent."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
        (list org-mcp-test--link-beta-id)
      (should
       (string-match-p
        "not found under parent"
        (org-mcp-test--call-tool-expecting-error
         test-file "org-add-todo"
         `((title . "New Task")
           (todo_state . "TODO")
           (tags . nil)
           (body . nil)
           (parent_uri . ,(format "id:%s" org-mcp-test--link-beta-id))
           (after_uri . ,(format "file:%s::*Review" test-file)))))))))

(ert-deftest org-mcp-test-link-refuses-types-that-open-or-run ()
  "A link type that opens or runs something is refused before any file opens."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (let ((canary
           (expand-file-name "org-mcp-test-link-canary"
                             (file-name-directory test-file)))
          (org-mcp-test--link-canary nil))
      (unwind-protect
          (dolist (link
                   (list
                    (format "shell:touch %s" canary)
                    (format "[[shell:touch %s][Gamma]]" canary)
                    "elisp:(setq org-mcp-test--link-canary t)"
                    "https://example.com/notes.org"
                    "help:org-link-open"
                    (format "file+sys:%s::*Gamma" test-file)))
            (dolist (call
                     `(("org-read-headline" (uri . ,link))
                       ("org-set-tags" (uri . ,link) (tags . "work"))))
              (should
               (string-match-p
                "not supported"
                (org-mcp-test--call-tool-expecting-error
                 test-file (car call) (cdr call)))))
            (should-not (find-buffer-visiting test-file))
            (should-not org-mcp-test--link-canary)
            (should-not (file-exists-p canary)))
        (when (file-exists-p canary)
          (delete-file canary))))))

(ert-deftest org-mcp-test-link-refuses-link-without-full-path ()
  "A relative, file-less or remote link is refused with a full-path hint."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (dolist (link
             (list
              (format "file:%s::*Gamma" (file-name-nondirectory test-file))
              (format "[[./%s::*Gamma]]" (file-name-nondirectory test-file))
              "[[#alpha-slug]]"
              "#alpha-slug"
              "[[*Gamma]]"
              "*Gamma"
              "[[Gamma]]"
              "file:/ssh:nonexistent.invalid:/tmp/notes.org::*Gamma"
              "[[/ssh:nonexistent.invalid:/tmp/notes.org::*Gamma]]"))
      (dolist (call
               `(("org-read-headline" (uri . ,link))
                 ("org-set-tags" (uri . ,link) (tags . "work"))))
        (should
         (string-match-p
          "Send a full path"
          (org-mcp-test--call-tool-expecting-error
           test-file (car call) (cdr call))))))
    (should-not (find-buffer-visiting test-file))))

(ert-deftest org-mcp-test-link-file-scope-override ()
  "A `file:' link names its file, so the scope override applies to it.
Under a root list, a `file:' link to a file below the root is readable
and writable, and one to a file outside every root is refused.  An
`id:' link names no file, so an ID in the file below the root stays
out of reach until a `file:' link names that file."
  (org-mcp-test--with-scope-dirs (list root)
    (let ((in (org-mcp-test--write-file
               root "in.org" org-mcp-test--scope-task-content))
          (out (org-mcp-test--write-file
                outside "out.org" org-mcp-test--scope-task-content))
          (with-id (org-mcp-test--write-file
                    root "with-id.org"
                    org-mcp-test--scope-task-with-id-content)))
      (should
       (string=
        (org-mcp-test--call-read-headline (format "file:%s::*Task" in))
        "* TODO Task\nBody"))
      (should
       (equal
        (alist-get
         'new_state
         (org-mcp-test--call-update-todo-state
          (format "[[file:%s::*Task][Task]]" in) "DONE" "TODO"))
        "DONE"))
      (org-mcp-test--verify-file-matches
       in org-mcp-test--scope-task-done-regex)
      (dolist (link
               (list
                (format "file:%s::*Task" out)
                (format "[[file:%s::*Task][Task]]" out)))
        (org-mcp-test--call-tool-refused
         "org-read-headline" `((uri . ,link)) "not in allowed list")
        (org-mcp-test--call-tool-refused
         "org-update-todo-state" `((uri . ,link) (new_state . "DONE"))
         "not in allowed list"
         out))
      (org-mcp-test--with-id-tracking
          (list allowed)
          `((,org-mcp-test--content-with-id-id . ,with-id))
        (let ((link (format "id:%s" org-mcp-test--content-with-id-id)))
          (org-mcp-test--call-tool-refused
           "org-read-headline" `((uri . ,link)) "not in allowed list")
          (org-mcp-test--call-tool-refused
           "org-update-todo-state" `((uri . ,link) (new_state . "DONE"))
           "not in allowed list"
           with-id))
        (should
         (string=
          (org-mcp-test--call-read-headline
           (format "file:%s::*Task" with-id))
          (string-trim-right
           org-mcp-test--scope-task-with-id-content)))))))

(ert-deftest org-mcp-test-link-remote-path-opens-no-connection ()
  "A link to a remote path is refused before any operation on the path.
The path is each spelling in `org-mcp-test--remote-spellings', which
are remote as written or only once expanded, and one more that `//'
makes remote.  Each is written as a `file:', a `file+emacs:' and a
bracketed link, with and without a description, and sent to a read,
a write, and as the parent and the sibling of `org-add-todo'.  Every
call asks for a full path, leaves the file unchanged, and the fake
remote method records no operation."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (paths
         (cons
          (concat
           "/tmp/..//" (substring org-mcp-test--remote-prefix 1) "/x.org")
          (org-mcp-test--remote-spellings))))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-links))
      (org-mcp-test--with-remote-probe ops
        (dolist (path paths)
          (dolist (link
                   (list
                    (format "file:%s::*Task" path)
                    (format "file+emacs:%s::*Task" path)
                    (format "[[%s::*Task]]" path)
                    (format "[[file:%s::*Task][Task]]" path)))
            (dolist (call
                     `(("org-read-headline" (uri . ,link))
                       ("org-set-tags" (uri . ,link) (tags . "work"))
                       ("org-add-todo"
                        (title . "New Task")
                        (todo_state . "TODO")
                        (tags . nil)
                        (body . nil)
                        (parent_uri . ,link))
                       ("org-add-todo"
                        (title . "New Task")
                        (todo_state . "TODO")
                        (tags . nil)
                        (body . nil)
                        (parent_uri . ,(format "file:%s::*Gamma" test-file))
                        (after_uri . ,link))))
              (org-mcp-test--call-tool-refused
               (car call) (cdr call) "Send a full path" test-file))))
        (should (null ops))))))

(ert-deftest org-mcp-test-link-refuses-regexp-search ()
  "A regexp search is refused, since Org answers it with a sparse tree."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (should
     (string-match-p
      "Regexp search is not supported"
      (org-mcp-test--call-tool-expecting-error
       test-file "org-read-headline"
       `((uri . ,(format "file:%s::/Gam.*/" test-file))))))
    (should-not (find-buffer-visiting test-file))))

(ert-deftest org-mcp-test-link-file-outside-allowed-files-refused ()
  "A file link naming a file outside the allowed files is refused."
  (org-mcp-test--with-temp-org-files
      ((allowed-file "* Allowed\n")
       (other-file org-mcp-test--content-links))
    (let ((org-mcp-allowed-files (list allowed-file))
          (link (format "file:%s::*Gamma" other-file)))
      (dolist (call
               `(("org-read-headline" (uri . ,link))
                 ("org-set-tags" (uri . ,link) (tags . "work"))))
        (should
         (string-match-p
          "not in allowed list"
          (org-mcp-test--call-tool-expecting-error
           other-file (car call) (cdr call)))))
      (should-not (find-buffer-visiting other-file)))))

(ert-deftest org-mcp-test-link-id-outside-allowed-files-refused ()
  "An ID in a file outside the allowed files is refused without its path."
  (org-mcp-test--with-temp-org-files
      ((allowed-file "* Allowed\n")
       (other-file org-mcp-test--content-links))
    (org-mcp-test--with-id-tracking
        (list allowed-file)
        `((,org-mcp-test--link-beta-id . ,other-file))
      (let ((link (format "id:%s" org-mcp-test--link-beta-id)))
        (dolist (call
                 `(("org-read-headline" (uri . ,link))
                   ("org-set-tags" (uri . ,link) (tags . "work"))))
          (let ((message
                 (org-mcp-test--call-tool-expecting-error
                  other-file (car call) (cdr call))))
            (should (string-match-p "not in allowed list" message))
            (should-not
             (string-match-p
              (regexp-quote (file-name-nondirectory other-file))
              message))))
        (should-not (find-buffer-visiting other-file))))))

(ert-deftest org-mcp-test-link-unknown-id-names-no-file ()
  "An unknown ID is refused with an error that names no file.
Each call runs from a buffer visiting an Org file, which Org's ID
lookup falls back to for an ID it does not know: once an allowed
file, once a file outside the allowed files."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links)
       (other-file "* Other\nOther body.\n"))
    (org-mcp-test--with-id-tracking
        (list test-file)
        `((,org-mcp-test--link-beta-id . ,test-file))
      (let ((org-id-locations-file
             (make-temp-file "org-mcp-test-id-locations")))
        (unwind-protect
            (dolist (current (list test-file other-file))
              (let ((buf (find-file-noselect current)))
                (unwind-protect
                    (with-current-buffer buf
                      (dolist (call
                               '(("org-read-headline"
                                  (uri . "id:no-such-id"))
                                 ("org-set-tags"
                                  (uri . "[[id:no-such-id][Gone]]")
                                  (tags . "work"))))
                        (should
                         (string=
                          (org-mcp-test--call-tool-expecting-error
                           current (car call) (cdr call))
                          "Cannot find ID 'no-such-id'"))))
                  (kill-buffer buf))))
          (delete-file org-id-locations-file))))))

(defun org-mcp-test--fold-state ()
  "Return, for each line of the current buffer, whether it is hidden.
The whole buffer is inspected, whatever its narrowing."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let (hidden)
        (while (not (eobp))
          (push (and (invisible-p (point)) t) hidden)
          (forward-line))
        (nreverse hidden)))))

(defun org-mcp-test--mark-state ()
  "Return the mark, the mark rings and Org's mark ring as plain data.
Each marker becomes a (BUFFER . POSITION) pair, since Org moves the
markers of its ring in place.  `org-mark-ring' is circular, so it is
read once around, starting at its head."
  (let ((pairs
         (lambda (markers)
           (mapcar
            (lambda (m) (cons (marker-buffer m) (marker-position m)))
            markers)))
        (ring org-mark-ring)
        (org-ring nil))
    (dotimes (_ org-mark-ring-length)
      (push (car ring) org-ring)
      (setq ring (cdr ring)))
    (list
     (mark t)
     (funcall pairs mark-ring)
     (funcall pairs global-mark-ring)
     (funcall pairs (nreverse org-ring)))))

(defun org-mcp-test--view-state ()
  "Return the user's view of Emacs, as far as a tool call could change it.
The value covers the current buffer, its point, narrowing and folding,
the mark and mark rings, the window configuration and the buffer
list."
  (list
   :view (list (current-buffer) (point) (point-min) (point-max))
   :folds (org-mcp-test--fold-state)
   :marks (org-mcp-test--mark-state)
   :windows (window-state-get nil t)
   :buffers (buffer-list)))

(ert-deftest org-mcp-test-link-leaves-user-view-unchanged ()
  "Resolving a link leaves the user's view of the buffer alone.
The buffer is shown in the selected window, folded so that only Parent
Task shows, narrowed to First Child and given a mark.  Each call
searches for a heading hidden in the fold, in a subtree other than
First Child: a write renames Second Child through an `id:' search, then
an `id:' read and a `file:' title read.  After each call the windows,
narrowing, point, folding, mark, mark rings and buffer list are as
they were.  The write lies after First Child, so it moves no position
the test compares."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      (list org-mcp-test--content-nested-siblings-parent-id)
    (let ((buf (find-file-noselect test-file))
          (parent org-mcp-test--content-nested-siblings-parent-id))
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer buf)
            (org-overview)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* First Child")
            (push-mark (point) t)
            (org-narrow-to-subtree)
            (forward-line 1)
            (let ((before (org-mcp-test--view-state)))
              (should (memq t (plist-get before :folds)))
              (should (mark t))
              (dolist (call
                       `(("org-rename-headline"
                          ,(format "[[id:%s::*Second Child]]" parent)
                          ((current_title . "Second Child")
                           (new_title . "Renamed Second Child")))
                         ("org-read-headline"
                          ,(format "id:%s::*Third Child #3" parent)
                          nil
                          "** Third Child #3")
                         ("org-read-headline"
                          ,(format "file:%s::*Renamed Second Child"
                                   test-file)
                          nil
                          ,(format
                            (concat
                             "** Renamed Second Child\n"
                             ":PROPERTIES:\n"
                             ":ID:       %s\n"
                             ":END:\n"
                             "Second child content.")
                            org-mcp-test--content-with-id-id))))
                (let ((uri (nth 1 call))
                      (expected (nth 3 call)))
                  (let ((result
                         (mcp-server-lib-ert-call-tool
                          (nth 0 call) (cons `(uri . ,uri) (nth 2 call)))))
                    (when expected
                      (should (string= result expected))))
                  (should
                   (equal
                    (cons uri (org-mcp-test--view-state))
                    (cons uri before)))))
              (org-mcp-test--verify-file-matches
               test-file
               org-mcp-test--expected-regex-renamed-second-child)))
        (kill-buffer buf)))))

;;; Resource tests

(defconst org-mcp-test--encoded-titles
  '("Budget? 50% of Q1 [draft] :: Ärger/Größe #3" "Literal %41 and %25")
  "Titles holding characters a URI must percent-encode.
The second holds `%41' and `%25', which a second decoding would turn
into `A' and `%'.")

(defconst org-mcp-test--content-encoded-titles
  (format "* %s\nBudget body.\n* %s\nLiteral body.\n"
          (nth 0 org-mcp-test--encoded-titles)
          (nth 1 org-mcp-test--encoded-titles))
  "Org file whose headings carry `org-mcp-test--encoded-titles'.")

(defun org-mcp-test--resource-uris (link)
  "Return org:// URIs for LINK, spelled as different clients spell them.
The first encodes only what a URI path may not hold, and `%'.  The
second encodes all but the unreserved characters, as JavaScript's
encodeURIComponent does.  The third is the first with its non-ASCII
characters left raw."
  (let ((path-chars (copy-sequence url-path-allowed-chars)))
    (aset path-chars ?% nil)
    (list
     (concat "org://" (url-hexify-string link path-chars))
     (concat "org://" (url-hexify-string link))
     (concat
      "org://"
      (mapconcat
       (lambda (char)
         (if (> char 127)
             (string char)
           (url-hexify-string (string char) path-chars)))
       link "")))))

(defun org-mcp-test--read-resource (uri)
  "Read the resource at URI and return its text, failing on an error."
  (let ((response
         (mcp-server-lib-process-jsonrpc-parsed
          (mcp-server-lib-create-resources-read-request uri)
          mcp-server-lib-ert-server-id)))
    (should-not (alist-get 'error response))
    (alist-get 'text (aref (alist-get 'contents (alist-get 'result response)) 0))))

(defun org-mcp-test--resource-error (uri)
  "Read the resource at URI expecting an invalid-params error.
Return the error message."
  (let ((error-object
         (alist-get
          'error
          (mcp-server-lib-process-jsonrpc-parsed
           (mcp-server-lib-create-resources-read-request uri)
           mcp-server-lib-ert-server-id))))
    (should error-object)
    (should
     (equal (alist-get 'code error-object) mcp-server-lib-jsonrpc-error-invalid-params))
    (alist-get 'message error-object)))

(ert-deftest org-mcp-test-resource-reads-every-link-form ()
  "The resource reads every address org-read takes and changes nothing.
Each native link is sent raw and in each spelling of
`org-mcp-test--resource-uris', each bare address raw, and every read
returns what org-read returns for the same address.  The reads run
once with no buffer on the file and once with one; the file stays
byte-for-byte unchanged and the buffer unmodified."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
      (list org-mcp-test--link-beta-id)
    (let ((before (org-mcp-test--read-file-raw test-file))
          (links
           (list
            (format "id:%s" org-mcp-test--link-beta-id)
            (format "[[id:%s]]" org-mcp-test--link-beta-id)
            (format "[[id:%s][Beta]]" org-mcp-test--link-beta-id)
            (format "id:%s::*Review" org-mcp-test--link-beta-id)
            (format "file:%s::#alpha-slug" test-file)
            (format "[[file:%s::#alpha-slug][Alpha]]" test-file)
            (format "file:%s::*Gamma" test-file)
            (format "[[file:%s::*Gamma]]" test-file)
            (format "file:%s::10" test-file)
            (format "file:%s" test-file)
            (format "[[file:%s][Links]]" test-file)))
          (bare
           (list
            org-mcp-test--link-beta-id
            test-file
            (format "%s#Alpha/Review" test-file))))
      (dolist (visiting '(nil t))
        (let ((buf (and visiting (find-file-noselect test-file))))
          (unwind-protect
              (progn
                (dolist (link links)
                  (let ((expected (org-mcp-test--call-read link)))
                    (dolist (uri
                             (cons
                              (concat "org://" link)
                              (org-mcp-test--resource-uris link)))
                      (should
                       (equal
                        (cons uri (org-mcp-test--read-resource uri))
                        (cons uri expected))))))
                (dolist (address bare)
                  (should
                   (equal
                    (org-mcp-test--read-resource (concat "org://" address))
                    (org-mcp-test--call-read address))))
                (should
                 (string= (org-mcp-test--read-file-raw test-file) before))
                (when buf
                  (should-not (buffer-modified-p buf))))
            (when buf
              (kill-buffer buf))))))))

(ert-deftest org-mcp-test-resource-decodes-link-once ()
  "The resource undoes the URI's percent-encoding exactly once.
The file name and the titles hold characters a URI must encode: a
space, `?', `#', `%', `[', `]', `::', `/' and non-ASCII letters.  The
file name and the second title also hold `%41' and `%25', which a
second decoding would turn into `A' and `%'.  Every spelling of
`org-mcp-test--resource-uris' reads the heading org-read reads for the
same link.  Sent without encoding, `%41' and `%25' are percent
escapes, so the URI names another file, and the resource refuses it as
org-read refuses that file.  `%0A' and `%0D' decode to a line feed and
a carriage return, so the resource refuses a title holding them as
org-read refuses the decoded link."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-encoded-titles "org-mcp-test ä#%41"))
    (dolist (title org-mcp-test--encoded-titles)
      (let* ((link (format "file:%s::*%s" test-file title))
             (expected (org-mcp-test--call-read link)))
        (should
         (equal (alist-get 'title (json-read-from-string expected)) title))
        (dolist (uri (org-mcp-test--resource-uris link))
          (should
           (equal
            (cons uri (org-mcp-test--read-resource uri))
            (cons uri expected))))))
    (dolist (case
             `((,(format "org://file:%s::*%s"
                         test-file (nth 1 org-mcp-test--encoded-titles))
                .
                ,(format "file:%s::*Literal A and %%"
                         (string-replace "%41" "A" test-file)))
               (,(format "org://file:%s::*Budget%%0Aline%%0Dfeed"
                         (url-hexify-string test-file))
                .
                ,(format "file:%s::*Budget\nline\rfeed" test-file))))
      (should
       (equal
        (org-mcp-test--resource-error (car case))
        (org-mcp-test--call-tool-expecting-error
         test-file "org-read" `((uri . ,(cdr case)))))))))

(defconst org-mcp-test--content-non-ascii-titles
  "* Ärger
Ärger body.
** TODO Größe #3
Größe body.
"
  "Org file whose headings have non-ASCII titles and no identifiers.")

(defconst org-mcp-test--regex-non-ascii-titles-done
  (concat
   "\\`\\* Ärger\n"
   "Ärger body\\.\n"
   "\\*\\* DONE Größe #3\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[A-Fa-f0-9-]+\n *:END:\n\\)?"
   "Größe body\\.\n"
   "\\'")
  "Regex matching `org-mcp-test--content-non-ascii-titles' after DONE.")

(defconst org-mcp-test--regex-non-ascii-titles-added
  (concat
   "\\`\\* Ärger\n"
   "Ärger body\\.\n"
   "\\*\\* DONE Größe #3\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[A-Fa-f0-9-]+\n *:END:\n\\)?"
   "Größe body\\.\n"
   "\\*\\* TODO New Task *\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[A-Fa-f0-9-]+\n *:END:\n\\)?"
   "\\'")
  "Regex matching the DONE file after adding a TODO under Ärger.")

(ert-deftest org-mcp-test-bare-outline-path-non-ascii-titles ()
  "A bare outline path decodes non-ASCII titles as UTF-8.
The titles are percent-encoded as org-mcp encodes them in the org://
URIs it returns.  org-read, the resource with the URI as returned,
org-read-headline, org-update-todo-state and org-add-todo's parent
all reach the heading, and the resource also reads the titles sent
raw."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-non-ascii-titles))
      (let* ((parent
              (format "%s#%s" test-file (url-hexify-string "Ärger")))
             (child
              (format "%s/%s" parent (url-hexify-string "Größe #3")))
             (expected (org-mcp-test--call-read child)))
        (should
         (equal
          (alist-get 'title (json-read-from-string expected)) "Größe #3"))
        (dolist (uri
                 (list
                  (concat "org://" child)
                  (format "org://%s#Ärger/Größe%%20%%233" test-file)))
          (should
           (equal
            (cons uri (org-mcp-test--read-resource uri))
            (cons uri expected))))
        (should
         (string=
          (org-mcp-test--call-read-headline child)
          "** TODO Größe #3\nGröße body."))
        (org-mcp-test--call-update-todo-state child "DONE" "TODO")
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--regex-non-ascii-titles-done)
        (org-mcp-test--add-todo-and-check
         "New Task" "TODO" nil nil parent nil
         (file-name-nondirectory test-file)
         test-file
         org-mcp-test--regex-non-ascii-titles-added)))))

(ert-deftest org-mcp-test-resource-bare-outline-path-decoded-once ()
  "A bare outline path on the resource decodes each title once, as org-read does.
`%2F' is a slash inside the title Parent/Child, not a separator
between Parent and Child."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-slash-not-nested-before))
    (let* ((address (format "%s#Parent%%2FChild" test-file))
           (expected (org-mcp-test--call-read address)))
      (should
       (equal (alist-get 'title (json-read-from-string expected)) "Parent/Child"))
      (should
       (equal (org-mcp-test--read-resource (concat "org://" address)) expected)))))

(ert-deftest org-mcp-test-resource-refuses-as-tools-do ()
  "The resource refuses a link with the message the read tools give.
The links run code or open something, name no local file by its full
path, search by regexp, or name what the call may not reach: a file
outside the allowed files, a missing file, an ID in a file outside
them and an unknown ID.  Each goes to org-read and org-read-headline,
which refuse it with the same message, and in each spelling of
`org-mcp-test--resource-uris' to the resource, which answers with an
invalid-params error carrying that message.  Nothing runs, no file is
opened or changed, and the remote path opens no connection."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links)
       (other-file org-mcp-test--content-links))
    (org-mcp-test--with-id-tracking
        (list test-file)
        `((,org-mcp-test--link-beta-id . ,other-file))
      (let ((canary
             (expand-file-name "org-mcp-test-link-canary"
                               (file-name-directory test-file)))
            (org-mcp-test--link-canary nil)
            (other-before (org-mcp-test--read-file-raw other-file)))
        (unwind-protect
            (org-mcp-test--with-remote-probe ops
              (dolist (link
                       (list
                        (format "shell:touch %s" canary)
                        (format "[[shell:touch %s][Gamma]]" canary)
                        "elisp:(setq org-mcp-test--link-canary t)"
                        "https://example.com/notes.org"
                        "help:org-link-open"
                        (format "file+sys:%s::*Gamma" test-file)
                        (format "file:%s::*Gamma" (file-name-nondirectory test-file))
                        "[[#alpha-slug]]"
                        "*Gamma"
                        (format "file:%s::*Task" (car (org-mcp-test--remote-spellings)))
                        (format "file:%s::/Gam.*/" test-file)
                        (format "file:%s::*Gamma" other-file)
                        (format "file:%s.missing.org::*Gamma" test-file)
                        (format "id:%s" org-mcp-test--link-beta-id)
                        "id:no-such-id"))
                (let ((message
                       (org-mcp-test--call-tool-expecting-error
                        test-file "org-read" `((uri . ,link)))))
                  (should
                   (equal
                    (org-mcp-test--call-tool-expecting-error
                     test-file "org-read-headline" `((uri . ,link)))
                    message))
                  (dolist (uri (org-mcp-test--resource-uris link))
                    (should
                     (equal
                      (cons uri (org-mcp-test--resource-error uri))
                      (cons uri message))))))
              (should (null ops)))
          (when (file-exists-p canary)
            (delete-file canary)))
        (should-not org-mcp-test--link-canary)
        (should-not (file-exists-p canary))
        (should-not (find-buffer-visiting other-file))
        (should
         (string= (org-mcp-test--read-file-raw other-file) other-before))))))

;;; Script installation tests

(ert-deftest org-mcp-test-install ()
  "Test org-mcp-stdio.sh installation to a temporary directory."
  (let* ((temp-dir (make-temp-file "org-mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (org-mcp-install))
          (should
           (file-exists-p (org-mcp--installed-script-path)))
          (should
           (file-executable-p (org-mcp--installed-script-path))))
      (delete-directory temp-dir t))))

(ert-deftest org-mcp-test-install-overwrite ()
  "Test org-mcp-stdio.sh installation when file already exists."
  (let* ((temp-dir (make-temp-file "org-mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir)
         (target (org-mcp--installed-script-path)))
    (unwind-protect
        (progn
          (write-region "existing content" nil target)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (org-mcp-install))
          (should (file-exists-p target))
          (should (file-executable-p target))
          (should
           (> (file-attribute-size (file-attributes target)) 20)))
      (delete-directory temp-dir t))))

(ert-deftest org-mcp-test-install-cancel ()
  "Test cancelling org-mcp-stdio.sh installation when file exists."
  (let* ((temp-dir (make-temp-file "org-mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir)
         (target (org-mcp--installed-script-path)))
    (unwind-protect
        (progn
          (write-region "existing content" nil target)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
            (should-error (org-mcp-install) :type 'user-error))
          (should
           (string=
            "existing content"
            (with-temp-buffer
              (insert-file-contents target)
              (buffer-string)))))
      (delete-directory temp-dir t))))

(ert-deftest org-mcp-test-uninstall ()
  "Test org-mcp-stdio.sh removal from a temporary directory."
  (let* ((temp-dir (make-temp-file "org-mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir)
         (target (org-mcp--installed-script-path)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (org-mcp-install)
            (should (file-exists-p target))
            (org-mcp-uninstall))
          (should-not (file-exists-p target)))
      (delete-directory temp-dir t))))

(ert-deftest org-mcp-test-uninstall-missing ()
  "Test uninstalling org-mcp-stdio.sh when script doesn't exist."
  (let* ((temp-dir (make-temp-file "org-mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir))
    (unwind-protect
        (should-error (org-mcp-uninstall) :type 'user-error)
      (delete-directory temp-dir t))))

(ert-deftest org-mcp-test-uninstall-cancel ()
  "Test cancelling org-mcp-stdio.sh uninstall."
  (let* ((temp-dir (make-temp-file "org-mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir)
         (target (org-mcp--installed-script-path)))
    (unwind-protect
        (progn
          (write-region "test content" nil target)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
            (org-mcp-uninstall))
          (should (file-exists-p target)))
      (delete-directory temp-dir t))))

(provide 'org-mcp-test)
;;; org-mcp-test.el ends here
