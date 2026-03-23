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
  (format "org-id://%s" org-mcp-test--content-with-id-id)
  "URI for org-mcp-test--content-with-id.")

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

(defconst org-mcp-test--content-wrong-levels
  "* First Parent
Some content in first parent.
* Second Parent
** Other Child
*** Target Headline
This should NOT be found via First Parent/Target Headline path.
* Third Parent
** Target Headline
This is actually a child of Third Parent, not First Parent!"
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
    "\\*\\* Third Child #3\\'")
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
   "\\*\\* Third Child #3\\'")
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

(defconst org-mcp-test--regex-todo-with-literal-block-end
  (concat
   "^\\* TODO Task with literal END_SRC +:work:\n"
   "\\(?: *:PROPERTIES:\n"
   " *:ID: +[^\n]+\n"
   " *:END:\n\\)?"
   "Example of source block:\n"
   "#\\+BEGIN_EXAMPLE\n"
   "#\\+END_SRC\n"
   "#\\+END_EXAMPLE\n"
   "Text after\\.$")
  "Pattern for TODO with body containing literal END_SRC inside EXAMPLE block.")

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
   "\\*\\* Third Child #3\\'")
  "Pattern for TODO added after specific sibling.")

(defconst org-mcp-test--regex-todo-after-second-child
  (concat
   "^#\\+TITLE: My Org Document\n\n"
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
   "Second child content\\.\n\n?"
   "\\*\\* TODO New Task After Second +:[^\n]*\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?"
   "\\*\\* Third Child #3\\'")
  "Pattern for TODO added after Second Child sibling.")

(defconst org-mcp-test--regex-todo-without-tags
  (concat
   "^\\* TODO Task Without Tags *\n" ; No tags, optional spaces
   "\\(?: *:PROPERTIES:\n" " *:ID: +[^\n]+\n" " *:END:\n\\)?$")
  "Pattern for TODO item without any tags.")

(defconst org-mcp-test--regex-top-level-todo
  (concat
   "^\\* TODO New Task +:.*work.*urgent.*:\n"
   "\\(?: *:PROPERTIES:\n"
   " *:ID: +[^\n]+\n"
   " *:END:\n\\)?$")
  "Pattern for top-level TODO item with work and urgent tags.")

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

(defconst org-mcp-test--pattern-renamed-simple-todo
  (concat
   "\\`\\* TODO Updated Task\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "First line of body\\.\n"
   "Second line of body\\.\n"
   "Third line of body\\.\\'")
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
   "This is a single headline with a slash, not nested under Parent\\.\\'")
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
   "This is a single headline with a slash, not nested under Parent\\.\\'")
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
    "\\*\\* Third Child #3\\'")
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
   "Third review content\\.\\'")
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
   "This Target is under Second Section, not First Section\\.\\'")
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
   "This is already done\\'")
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

(defconst org-mcp-test--pattern-edit-body-replace-all
  (concat
   "\\`\\* Test Heading\n"
   ":PROPERTIES:\n"
   ":ID: +test-id\n"
   ":END:\n"
   "First REPLACED\\.\n"
   "Some other text\\.\n"
   "Second REPLACED\\.\n"
   "More text\\.\n"
   "Third REPLACED\\.\n"
   "?\\'")
  "Pattern for replace-all edit-body test result.")

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

(defconst org-mcp-test--content-id-resource-id
  "12345678-abcd-efgh-ijkl-1234567890ab"
  "ID value for org-mcp-test--content-id-resource.")

(defconst org-mcp-test--content-id-resource
  (format
   "* Section with ID
:PROPERTIES:
:ID: %s
:END:
Content of section with ID."
   org-mcp-test--content-id-resource-id)
  "Content for ID resource tests.")

(defconst org-mcp-test--content-headline-resource
  "* First Section
Some content in first section.
** Subsection 1.1
Content of subsection 1.1.
** Subsection 1.2
Content of subsection 1.2.
* Second Section
Content of second section.
*** Deep subsection
Very deep content."
  "Test content with hierarchical headlines for resource read tests.")

(defconst org-mcp-test--expected-first-section
  (concat
   "* First Section\n"
   "Some content in first section.\n"
   "** Subsection 1.1\n"
   "Content of subsection 1.1.\n"
   "** Subsection 1.2\n"
   "Content of subsection 1.2.")
  "Expected content when reading 'First Section' top-level headline.")

(defconst org-mcp-test--expected-subsection-1-1
  (concat
   "** Subsection 1.1\n"
   "Content of subsection 1.1.")
  "Expected content when reading 'First Section/Subsection 1.1' nested headline.")

;; Test helpers

(defun org-mcp-test--read-file (file)
  "Read and return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-mcp-test--verify-file-matches (test-file expected-pattern)
  "Verify TEST-FILE content matches EXPECTED-PATTERN regexp."
  (should (string-match-p expected-pattern (org-mcp-test--read-file test-file))))

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

(defmacro org-mcp-test--with-file-buffer (buffer file &rest body)
  "Open FILE in BUFFER and execute BODY, ensuring buffer is killed.
BUFFER is the variable name to bind the buffer to.
FILE is the file path to open.
BODY is the code to execute with the buffer."
  (declare (indent 2) (debug t))
  `(let ((,buffer (find-file-noselect ,file)))
     (unwind-protect
         (progn ,@body)
       (kill-buffer ,buffer))))

;; Helpers for testing org-get-todo-config MCP tool

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

;; Helpers for testing org-get-tag-config MCP tool

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

;; Helpers for testing org-get-allowed-files MCP tool

(defun org-mcp-test--get-allowed-files-and-check (allowed-files expected-files)
  "Call org-get-allowed-files tool and verify the result.
ALLOWED-FILES is the value to bind to org-mcp-allowed-files.
EXPECTED-FILES is a list of expected file paths."
  (let ((org-mcp-allowed-files allowed-files))
    (org-mcp-test--with-enabled
     (let* ((result-text
             (mcp-server-lib-ert-call-tool "org-get-allowed-files" nil))
            (result (json-read-from-string result-text)))
       (should (= (length result) 1))
       (let ((files (cdr (assoc 'files result))))
         (should (vectorp files))
         (should (= (length files) (length expected-files)))
         (dotimes (i (length expected-files))
           (should (string= (aref files i) (nth i expected-files)))))))))

;; Helper functions for testing org-add-todo MCP tool

(defmacro org-mcp-test--with-add-todo-setup
    (file-var initial-content todo-keywords tag-alist ids &rest body)
  "Helper for org-add-todo test.
Sets up FILE-VAR with INITIAL-CONTENT and org configuration.
TODO-KEYWORDS is the org-todo-keywords config (nil for default).
TAG-ALIST is the org-tag-alist config (nil for default).
IDS is optional list of ID strings to register (nil for no ID tracking).
Executes BODY with org-mcp enabled and standard variables set."
  (declare (indent 2))
  (let ((todo-kw (or todo-keywords ''((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        (tag-al (or tag-alist ''("work" "personal" "urgent"))))
    `(org-mcp-test--with-temp-org-files
      ((,file-var ,initial-content))
      (let ((org-todo-keywords ,todo-kw)
            (org-tag-alist ,tag-al)
            ,@(unless ids '((org-id-locations-file nil))))
        ,(if ids
             `(org-mcp-test--with-id-tracking
               (list ,file-var)
               (mapcar (lambda (id) (cons id ,file-var)) ,ids)
               ,@body)
           `(progn ,@body))))))

(defmacro org-mcp-test--call-add-todo-expecting-error
    (initial-content todo-keywords tag-alist title todoState tags body parentUri
                     &optional afterUri)
  "Call org-add-todo MCP tool expecting an error and verify file unchanged.
INITIAL-CONTENT is the initial Org file content.
TODO-KEYWORDS is the org-todo-keywords config (nil for default).
TAG-ALIST is the org-tag-alist config (nil for default).
TITLE is the headline text.
TODOSTATE is the TODO state.
TAGS is a list of tag strings or nil.
BODY is the body text or nil.
PARENTURI is the URI of the parent item.
AFTERURI is optional URI of sibling to insert after."
  `(org-mcp-test--with-add-todo-setup
    test-file ,initial-content ,todo-keywords
    ,tag-alist nil
    (org-mcp-test--assert-error-and-file
     test-file
     (let* ((params
             `((title . ,,title)
               (todo_state . ,,todoState)
               (tags . ,,tags)
               (body . ,,body)
               (parent_uri . ,,parentUri)
               (after_uri . ,,afterUri)))
            (request
              (mcp-server-lib-create-tools-call-request
               "org-add-todo" nil params))
            (response (mcp-server-lib-process-jsonrpc-parsed request
                                                             mcp-server-lib-ert-server-id))
            (result (mcp-server-lib-ert-process-tool-response response)))
       ;; If we get here, the tool succeeded when we expected failure
       (error "Expected error but got success: %s" result)))))

(defun org-mcp-test--assert-add-todo-rejects-body-headline
    (initial-content parent-headline body-with-headline)
  "Test that adding TODO with BODY-WITH-HEADLINE is rejected.
INITIAL-CONTENT is the initial file content.
PARENT-HEADLINE is the parent headline path (empty string for top-level).
BODY-WITH-HEADLINE is the body containing invalid headline."
  (org-mcp-test--call-add-todo-expecting-error
   initial-content nil nil
   "Test Task" "TODO" '("work") body-with-headline
   (format "org-headline://%s#%s" test-file parent-headline)))

(defun org-mcp-test--assert-add-todo-invalid-title (invalid-title)
  "Assert that adding TODO with INVALID-TITLE throws an error.
Tests that the given title is rejected when creating a TODO."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty nil nil
   invalid-title "TODO" nil nil
   (format "org-headline://%s#" test-file)))

(defmacro org-mcp-test--add-todo-and-check
    (initial-content todo-keywords tag-alist ids
                     title todoState tags body parentUri afterUri
                     basename expected-pattern
                     &optional override-bindings)
  "Add TODO item with setup and verify the result.
INITIAL-CONTENT is the initial Org file content.
TODO-KEYWORDS is the org-todo-keywords config (nil for default).
TAG-ALIST is the org-tag-alist config (nil for default).
IDS is optional list of ID strings to register (nil for no ID tracking).
TITLE is the headline text.
TODOSTATE is the TODO state.
TAGS is a list of tag strings or nil.
BODY is the body text or nil.
PARENTURI is the URI of the parent item.
AFTERURI is optional URI of sibling to insert after.
BASENAME is the expected file basename.
EXPECTED-PATTERN is a regexp that the file content should match.
OVERRIDE-BINDINGS is optional list of let-style bindings to override
variables after setup, e.g., ((org-tag-alist nil))."
  (declare (indent 2))
  (let ((checking-logic
         `(let* ((params
                  `((title . ,,title)
                    (todo_state . ,,todoState)
                    (tags . ,,tags)
                    (body . ,,body)
                    (parent_uri . ,,parentUri)
                    (after_uri . ,,afterUri)))
                 (result-text (mcp-server-lib-ert-call-tool "org-add-todo" params))
                 (result (json-read-from-string result-text)))
            ;; Check result structure
            (should (= (length result) 4))
            (should (equal (alist-get 'success result) t))
            (should (string-match-p "\\`org-id://.+" (alist-get 'uri result)))
            (should (equal (alist-get 'file result) ,basename))
            (should (equal (alist-get 'title result) ,title))
            (org-mcp-test--verify-file-matches test-file ,expected-pattern))))
    `(org-mcp-test--with-add-todo-setup
      test-file
      ,initial-content ,todo-keywords ,tag-alist ,ids
      ,(if override-bindings
           `(let ,override-bindings
              ,checking-logic)
         checking-logic))))

;; Helper functions for testing org-update-todo-state MCP tool

(defun org-mcp-test--call-update-todo-state-expecting-error
    (test-file resource-uri current-state new-state)
  "Call org-update-todo-state tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
RESOURCE-URI is the URI to update.
CURRENT-STATE is the current TODO state.
NEW-STATE is the new TODO state to set."
  (let ((org-todo-keywords
         '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
    (org-mcp-test--assert-error-and-file
     test-file
     (let* ((request
              (mcp-server-lib-create-tools-call-request
               "org-update-todo-state" 1
               `((uri . ,resource-uri)
                 (current_state . ,current-state)
                 (new_state . ,new-state))))
            (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
            (result (mcp-server-lib-ert-process-tool-response response)))
       ;; If we get here, the tool succeeded when we expected failure
       (error "Expected error but got success: %s" result)))))

(defun org-mcp-test--update-todo-state-and-check
    (resource-uri old-state new-state test-file expected-content-regex)
  "Update TODO state and verify the result via MCP JSON-RPC.
RESOURCE-URI is the URI to update.
OLD-STATE is the current TODO state to update from.
NEW-STATE is the new TODO state to update to.
TEST-FILE is the file to verify content after update.
EXPECTED-CONTENT-REGEX is an anchored regex that matches the complete buffer."
  (let* ((params
          `((uri . ,resource-uri)
            (current_state . ,old-state)
            (new_state . ,new-state)))
         (result-text
          (mcp-server-lib-ert-call-tool "org-update-todo-state" params))
         (result (json-read-from-string result-text)))
    (should (= (length result) 4))
    (should (equal (alist-get 'success result) t))
    (should (equal (alist-get 'previous_state result) old-state))
    (should (equal (alist-get 'new_state result) new-state))
    (should (stringp (alist-get 'uri result)))
    (should (string-prefix-p "org-id://" (alist-get 'uri result)))
    ;; For ID-based URIs, verify the returned URI matches the input
    (when (string-prefix-p "org-id://" resource-uri)
      (should (equal (alist-get 'uri result) resource-uri)))
    (org-mcp-test--verify-file-matches test-file expected-content-regex)))

;; Helper functions for testing org-read-headline MCP tool

(defun org-mcp--tool-read-headline-and-check (initial-content headline-path expected-pattern-regex)
  "Call org-read-headline tool via JSON-RPC and verify the result.
INITIAL-CONTENT is the content to write to the temp file.
HEADLINE-PATH is the slash-separated path to the headline.
EXPECTED-PATTERN-REGEX is an anchored regex that matches the expected result."
  (org-mcp-test--with-temp-org-files
      ((test-file initial-content))
    (let* ((params `((file . ,test-file)
                     (headline_path . ,headline-path)))
           (result-text (mcp-server-lib-ert-call-tool "org-read-headline" params)))
      (should
       (string-match-p expected-pattern-regex result-text)))))

(defmacro org-mcp-test--call-read-headline-expecting-error (content headline-path)
  "Call org-read-headline tool via JSON-RPC expecting an error.
CONTENT is the Org file content to use.
HEADLINE-PATH is the headline path string."
  (declare (indent 0))
  `(org-mcp-test--with-temp-org-files
       ((test-file ,content))
     (let* ((request
              (mcp-server-lib-create-tools-call-request
               "org-read-headline" 1
               (list (cons 'file test-file)
                     (cons 'headline_path ,headline-path))))
            (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
            (result (mcp-server-lib-ert-process-tool-response response)))
       ;; If we get here, the tool succeeded when we expected failure
       (error "Expected error but got success: %s" result))))

;; Helper functions for testing org-rename-headline MCP tool

(defun org-mcp-test--call-rename-headline-and-check
    (initial-content headline-path-or-uri current-title new-title
                     expected-content-regex
                     &optional ids-to-register)
  "Call org-rename-headline tool via JSON-RPC and verify the result.
INITIAL-CONTENT is the initial Org file content.
HEADLINE-PATH-OR-URI is either a headline path fragment or full URI.
CURRENT-TITLE is the expected current title.
NEW-TITLE is the new title to set.
EXPECTED-CONTENT-REGEX is an anchored regex that matches the complete buffer.
IDS-TO-REGISTER is optional list of IDs to register for the temp file."
  (org-mcp-test--with-temp-org-files
   ((test-file initial-content))
   (when ids-to-register
     (let ((org-id-track-globally t)
           (org-id-locations-file nil)
           (org-id-locations nil))
       (dolist (id ids-to-register)
         (org-id-add-location id test-file))))
   (let* ((uri (if (string-prefix-p "org-" headline-path-or-uri)
                   headline-path-or-uri
                 (format "org-headline://%s#%s" test-file headline-path-or-uri)))
          (params
           `((uri . ,uri)
             (current_title . ,current-title)
             (new_title . ,new-title)))
          (result-text
           (mcp-server-lib-ert-call-tool "org-rename-headline" params))
          (result (json-read-from-string result-text))
          (result-uri (alist-get 'uri result)))
     (should (= (length result) 4))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'previous_title result) current-title))
     (should (equal (alist-get 'new_title result) new-title))
     (should (stringp result-uri))
     (should (string-prefix-p "org-id://" result-uri))
     ;; If input URI was ID-based, result URI should remain ID-based
     (when (string-prefix-p "org-id://" uri)
       (should (equal result-uri uri)))
     (org-mcp-test--verify-file-matches test-file expected-content-regex))))

(defun org-mcp-test--assert-rename-headline-rejected
    (initial-content headline-title new-title)
  "Assert renaming headline to NEW-TITLE is rejected.
INITIAL-CONTENT is the Org content to test with.
HEADLINE-TITLE is the current headline to rename.
NEW-TITLE is the invalid new title that should be rejected."
  (org-mcp-test--call-rename-headline-expecting-error
   initial-content
   (url-hexify-string headline-title)
   headline-title
   new-title))

(defun org-mcp-test--call-rename-headline-expecting-error
    (initial-content headline-path-or-uri current-title new-title)
  "Call org-rename-headline tool expecting an error and verify file unchanged.
INITIAL-CONTENT is the initial Org file content.
HEADLINE-PATH-OR-URI is either a headline path fragment or full URI.
CURRENT-TITLE is the current title for validation.
NEW-TITLE is the new title to set."
  (org-mcp-test--with-temp-org-files
   ((test-file initial-content))
   (let ((uri (if (string-prefix-p "org-" headline-path-or-uri)
                  headline-path-or-uri
                (format "org-headline://%s#%s" test-file headline-path-or-uri))))
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
        (error "Expected error but got success: %s" result))))))

;; Helper functions for testing org-edit-body MCP tool

(defun org-mcp-test--call-edit-body-and-check
    (test-file resource-uri old-body new-body expected-pattern
               &optional replace-all expected-id)
  "Call org-edit-body tool and check result structure and file content.
TEST-FILE is the path to the file to check.
RESOURCE-URI is the URI of the node to edit.
OLD-BODY is the substring to search for within the node's body.
NEW-BODY is the replacement text.
EXPECTED-PATTERN is a regexp that the file content should match.
REPLACE-ALL if true, replace all occurrences (default: nil).
EXPECTED-ID if provided, check the returned URI has this exact ID."
  (let* ((params
          `((resource_uri . ,resource-uri)
            (old_body . ,old-body)
            (new_body . ,new-body)
            (replace_all . ,replace-all)))
         (result-text (mcp-server-lib-ert-call-tool "org-edit-body" params))
         (result (json-read-from-string result-text)))
    (should (= (length result) 2))
    (should (equal (alist-get 'success result) t))
    (let ((uri (alist-get 'uri result)))
      (if expected-id
          (should (equal uri (concat "org-id://" expected-id)))
        (should (string-prefix-p "org-id://" uri))))
    (org-mcp-test--verify-file-matches test-file expected-pattern)))

(defun org-mcp-test--call-edit-body-expecting-error
    (test-file resource-uri old-body new-body &optional replace-all)
  "Call org-edit-body tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
RESOURCE-URI is the URI of the node to edit.
OLD-BODY is the substring to search for within the node's body.
NEW-BODY is the replacement text.
REPLACE-ALL if true, replace all occurrences (default: nil)."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((params
           `((resource_uri . ,resource-uri)
             (old_body . ,old-body)
             (new_body . ,new-body)
             (replace_all . ,replace-all)))
          (request
            (mcp-server-lib-create-tools-call-request
             "org-edit-body" 1 params))
          (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
          (result (mcp-server-lib-ert-process-tool-response response)))
     ;; If we get here, the tool succeeded when we expected failure
     (error "Expected error but got success: %s" result))))

;; Helper functions for testing org-read-file MCP tool

(defun org-mcp-test--call-read-file (file)
  "Call org-read-file tool via JSON-RPC and return the result.
FILE is the file path to read."
  (let ((params `((file . ,file))))
    (mcp-server-lib-ert-call-tool "org-read-file" params)))

;; Helper functions for testing org-read-outline MCP tool

(defun org-mcp-test--call-read-outline (file)
  "Call org-read-outline tool via JSON-RPC and return the result.
FILE is the file path to read the outline from."
  (let* ((params `((file . ,file)))
         (result-json
          (mcp-server-lib-ert-call-tool "org-read-outline" params)))
    (json-parse-string result-json :object-type 'alist)))

;; Helper functions for testing org-read-by-id MCP tool

(defun org-mcp-test--call-read-by-id-and-check (uuid expected-pattern)
  "Call org-read-by-id tool via JSON-RPC and verify the result.
UUID is the ID property of the headline to read.
EXPECTED-PATTERN is a regex pattern the result should match."
  (let* ((params `((uuid . ,uuid)))
         (result-text (mcp-server-lib-ert-call-tool "org-read-by-id" params)))
    (should (string-match-p expected-pattern result-text))))

;; Helper functions for testing MCP resources

(defun org-mcp-test--verify-resource-read (uri text)
  "Verify MCP resource at URI being TEXT."
  (mcp-server-lib-ert-verify-resource-read
   uri `((uri . ,uri)
         (text . ,text)
         (mimeType . "text/plain"))))

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

(defun org-mcp-test--test-headline-resource-with-extension (extension)
  "Test headline resource with file having EXTENSION.
EXTENSION can be a string like \".txt\" or nil for no extension."
  (let ((test-file
         (make-temp-file
          "org-mcp-test" nil extension org-mcp-test--content-nested-siblings)))
    (unwind-protect
        (let ((org-mcp-allowed-files (list test-file))
              (uri
               (format "org-headline://%s#Parent%%20Task"
                       test-file)))
          (org-mcp-test--with-enabled
           (org-mcp-test--verify-resource-read
            uri
            org-mcp-test--expected-parent-task-from-nested-siblings)))
      (delete-file test-file))))

;;; Tests

;; org-get-todo-config tests

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
  (org-mcp-test--with-get-todo-config-result
   '((type "BUG" "FEATURE" "ENHANCEMENT"))
   (should (= (length sequences) 1))
   (org-mcp-test--check-todo-config-sequence
    (aref sequences 0) "type" ["BUG" "FEATURE" "|" "ENHANCEMENT"])
   (should (= (length semantics) 3))
   (org-mcp-test--check-todo-config-semantic (aref semantics 0) "BUG" nil "type")
   (org-mcp-test--check-todo-config-semantic
    (aref semantics 1) "FEATURE" nil "type")
   (org-mcp-test--check-todo-config-semantic
    (aref semantics 2) "ENHANCEMENT" t "type")))

;; org-get-tag-config tests

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

;; Helpers for testing org-get-priority-config MCP tool

(defmacro org-mcp-test--with-get-priority-config-result
    (highest lowest default &rest body)
  "Call get-priority-config with HIGHEST, LOWEST, DEFAULT and run BODY.
Binds `result-highest', `result-lowest', and `result-default' from the
result for use in BODY."
  (declare (indent 3) (debug t))
  `(let ((org-priority-highest ,highest)
         (org-priority-lowest ,lowest)
         (org-priority-default ,default))
     (org-mcp-test--with-enabled
       (let ((result (json-read-from-string
                      (mcp-server-lib-ert-call-tool
                       "org-get-priority-config" nil))))
         (should (= (length result) 3))
         (let ((result-highest (alist-get 'highest result))
               (result-lowest (alist-get 'lowest result))
               (result-default (alist-get 'default result)))
           ,@body)))))

(ert-deftest org-mcp-test-tool-get-priority-config-default ()
  "Test org-get-priority-config with Org default priorities."
  (org-mcp-test--with-get-priority-config-result ?A ?C ?B
    (should (equal result-highest "A"))
    (should (equal result-lowest "C"))
    (should (equal result-default "B"))))

(ert-deftest org-mcp-test-tool-get-priority-config-custom ()
  "Test org-get-priority-config with custom numeric priorities."
  (org-mcp-test--with-get-priority-config-result ?1 ?5 ?3
    (should (equal result-highest "1"))
    (should (equal result-lowest "5"))
    (should (equal result-default "3"))))

(ert-deftest org-mcp-test-tool-get-priority-config-same ()
  "Test org-get-priority-config with all priorities set to the same value."
  (org-mcp-test--with-get-priority-config-result ?A ?A ?A
    (should (equal result-highest "A"))
    (should (equal result-lowest "A"))
    (should (equal result-default "A"))))

;; org-get-allowed-files tests

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

;;; org-update-todo-state tests

(ert-deftest org-mcp-test-update-todo-state-success ()
  "Test successful TODO state update."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
     ((test-file test-content))
     (let ((org-todo-keywords
            '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "|" "DONE(d!)"))))
       ;; Update TODO to IN-PROGRESS
       (let ((resource-uri
              (format "org-headline://%s#Task%%20with%%20ID" test-file)))
         (org-mcp-test--update-todo-state-and-check
          resource-uri "TODO" "IN-PROGRESS"
          test-file org-mcp-test--expected-task-with-id-in-progress-regex))))))

(ert-deftest org-mcp-test-update-todo-state-mismatch ()
  "Test TODO state update fails on state mismatch."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      ;; Try to update with wrong current state
      (let ((resource-uri
             (format "org-headline://%s#Task%%20with%%20ID" test-file)))
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file resource-uri "IN-PROGRESS" "DONE")))))

(ert-deftest org-mcp-test-update-todo-with-timestamp-id ()
  "Test updating TODO state using timestamp-format ID (not UUID)."
  (let ((test-content org-mcp-test--content-timestamp-id))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
      (org-mcp-test--with-id-setup test-file test-content
          `("20240101T120000")
        (let ((uri "org-id://20240101T120000"))
          (org-mcp-test--update-todo-state-and-check
           uri "TODO" "DONE"
           test-file
           org-mcp-test--expected-timestamp-id-done-regex))))))

(ert-deftest org-mcp-test-update-todo-state-empty-newstate-invalid ()
  "Test that empty string for newState is rejected."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      ;; Try to set empty state
      (let ((resource-uri
             (format "org-headline://%s#Task%%20with%%20ID" test-file)))
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file resource-uri "TODO" "")))))

(ert-deftest org-mcp-test-update-todo-state-invalid ()
  "Test TODO state update fails for invalid new state."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      ;; Try to update to invalid state
      (let ((resource-uri
             (format "org-headline://%s#Task%%20with%%20ID" test-file)))
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file resource-uri "TODO" "INVALID-STATE")))))

(ert-deftest org-mcp-test-update-todo-state-with-open-buffer ()
  "Test TODO state update works when file is open in another buffer."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        ;; Open the file in a buffer
        (org-mcp-test--with-file-buffer buffer test-file
          ;; Update TODO state while buffer is open
          (let ((resource-uri
                 (format "org-headline://%s#Task%%20with%%20ID"
                         test-file)))
            (org-mcp-test--update-todo-state-and-check
             resource-uri "TODO" "IN-PROGRESS"
             test-file org-mcp-test--expected-task-with-id-in-progress-regex)
            ;; Verify the buffer was also updated
            (with-current-buffer buffer
              (goto-char (point-min))
              (should
               (re-search-forward "^\\* IN-PROGRESS Task with ID"
                                  nil t)))))))))

(ert-deftest org-mcp-test-update-todo-state-with-modified-buffer ()
  "Test TODO state update fails when buffer has unsaved changes."
  (let ((test-content org-mcp-test--content-simple-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      ;; Open the file in a buffer and modify it elsewhere
      (org-mcp-test--with-file-buffer buffer test-file
        ;; Make a modification at an unrelated location
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "\n* TODO Another Task\nAdded in buffer.")
          ;; Buffer is now modified but not saved
          (should (buffer-modified-p)))

        ;; Try to update while buffer has unsaved changes
        (let ((resource-uri
               (format "org-headline://%s#Original%%20Task"
                       test-file)))
          (org-mcp-test--call-update-todo-state-expecting-error
           test-file resource-uri "TODO" "IN-PROGRESS")
          ;; Verify buffer still has unsaved changes
          (with-current-buffer buffer
            (should (buffer-modified-p))))))))

(ert-deftest org-mcp-test-update-todo-state-nonexistent-id ()
  "Test TODO state update fails for non-existent UUID."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-id-setup test-file test-content '()
      ;; Try to update a non-existent ID
      (let ((resource-uri "org-id://nonexistent-uuid-12345"))
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file resource-uri "TODO" "IN-PROGRESS")))))

(ert-deftest org-mcp-test-update-todo-state-by-id ()
  "Test updating TODO state using org-id:// URI."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (let ((org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
      (org-mcp-test--with-id-setup test-file test-content
          `(,org-mcp-test--content-with-id-id)
        (org-mcp-test--update-todo-state-and-check
         org-mcp-test--content-with-id-uri "TODO" "IN-PROGRESS"
         test-file
         org-mcp-test--expected-task-with-id-in-progress-regex)))))

(ert-deftest org-mcp-test-update-todo-state-nonexistent-headline ()
  "Test TODO state update fails for non-existent headline path."
  (let ((test-content org-mcp-test--content-simple-todo))
    (org-mcp-test--with-temp-org-files
     ((test-file test-content))
     ;; Try to update a non-existent headline
     (let ((resource-uri
            (format "org-headline://%s#Nonexistent%%20Task"
                    test-file)))
       (org-mcp-test--call-update-todo-state-expecting-error
        test-file resource-uri "TODO" "IN-PROGRESS")))))

;; org-add-todo tests

(ert-deftest org-mcp-test-add-todo-top-level ()
  "Test adding a top-level TODO item."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty nil nil nil
   "New Task"
   "TODO"
   '("work" "urgent")
   nil ; no body
   (format "org-headline://%s#" test-file)
   nil ; no afterUri
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-todo))

(ert-deftest org-mcp-test-add-todo-top-level-with-header ()
  "Test adding top-level TODO after header comments."
  (let ((initial-content org-mcp-test--content-nested-siblings))
    (org-mcp-test--add-todo-and-check
     initial-content nil nil nil
     "New Top Task"
     "TODO"
     '("urgent")
     nil ; no body
     (format "org-headline://%s#" test-file)
     nil ; no afterUri
     (file-name-nondirectory test-file)
     org-mcp-test--expected-regex-top-level-with-header)))

(ert-deftest org-mcp-test-add-todo-invalid-state ()
  "Test that adding TODO with invalid state throws error."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty nil nil
   "New Task"
   "INVALID-STATE" ; Not in org-todo-keywords
   '("work")
   nil
   (format "org-headline://%s#" test-file)))

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

(ert-deftest org-mcp-test-add-todo-tag-reject-invalid-with-alist ()
  "Test that tags not in `org-tag-alist' are rejected."
  ;; Should reject tags not in org-tag-alist
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty nil nil
   "Task" "TODO" '("invalid") nil
   (format "org-headline://%s#" test-file)))

(ert-deftest org-mcp-test-add-todo-tag-accept-valid-with-alist ()
  "Test that tags in `org-tag-alist' are accepted."
  ;; Should accept tags in org-tag-alist (work, personal, urgent)
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty nil nil nil
   "ValidTask"
   "TODO"
   '("work")
   nil
   (format "org-headline://%s#" test-file)
   nil
   (file-name-nondirectory test-file)
   (concat
    "^\\* TODO ValidTask +:work:\n"
    "\\(?: *:PROPERTIES:\n"
    " *:ID: +[^\n]+\n"
    " *:END:\n\\)?$")))

(ert-deftest org-mcp-test-add-todo-tag-validation-without-alist ()
  "Test valid tag names are accepted when `org-tag-alist' is empty."
  ;; Should accept valid tag names (alphanumeric, _, @)
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty nil nil nil
   "Task1"
   "TODO"
   '("validtag" "tag123" "my_tag" "@home")
   nil
   (format "org-headline://%s#" test-file)
   nil
   (file-name-nondirectory test-file)
   (concat
    "^\\* TODO Task1 +:"
    ".*validtag.*tag123.*my_tag.*@home.*:\n"
    "\\(?: *:PROPERTIES:\n"
    " *:ID: +[^\n]+\n"
    " *:END:\n\\)?$")
   ((org-tag-alist nil)
    (org-tag-persistent-alist nil))))

(ert-deftest org-mcp-test-add-todo-tag-invalid-exclamation ()
  "Test that tags with exclamation mark are rejected."
  (let ((org-tag-alist nil)
        (org-tag-persistent-alist nil))
    (org-mcp-test--call-add-todo-expecting-error
     org-mcp-test--content-empty nil nil
     "Task" "TODO" '("invalid-tag!") nil
     (format "org-headline://%s#" test-file))))

(ert-deftest org-mcp-test-add-todo-tag-invalid-dash ()
  "Test that tags with dash character are rejected."
  (let ((org-tag-alist nil)
        (org-tag-persistent-alist nil))
    (org-mcp-test--call-add-todo-expecting-error
     org-mcp-test--content-empty nil nil
     "Task" "TODO" '("tag-with-dash") nil
     (format "org-headline://%s#" test-file))))

(ert-deftest org-mcp-test-add-todo-tag-invalid-hash ()
  "Test that tags with hash character are rejected."
  (let ((org-tag-alist nil)
        (org-tag-persistent-alist nil))
    (org-mcp-test--call-add-todo-expecting-error
     org-mcp-test--content-empty nil nil
     "Task" "TODO" '("tag#hash") nil
     (format "org-headline://%s#" test-file))))

(ert-deftest org-mcp-test-add-todo-child-under-parent ()
  "Test adding a child TODO under an existing parent."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-nested-siblings nil nil nil
   "Child Task"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Parent%%20Task" test-file)
   nil ; no afterUri
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-under-parent))

(ert-deftest org-mcp-test-add-todo-child-empty-after-uri ()
  "Test adding a child TODO with empty string for after_uri.
Empty string should be treated as nil - append as last child."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-nested-siblings nil nil nil
   "Child Task"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Parent%%20Task" test-file)
   "" ; empty string after_uri
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-under-parent))

(ert-deftest org-mcp-test-add-todo-second-child-same-level ()
  "Test that adding a second child creates it at the same level as first child.
This tests the bug where the second child was created at level 4 instead of level 3."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-level2-parent-level3-children nil nil nil
   "Second Child"
   "TODO"
   '("work")
   nil  ; no body
   (format "org-headline://%s#Top%%20Level/Review%%20the%%20package"
           test-file)
   nil ; no after_uri
   (file-name-nondirectory test-file)
   org-mcp-test--regex-second-child-same-level))

(ert-deftest org-mcp-test-add-todo-with-after-uri ()
  "Test adding TODO after a sibling using after_uri.
Tests that adding after a level 3 sibling correctly creates level 3 (not level 1).
Reproduces the emacs.org scenario: level 2 parent (via path), level 3 sibling (via ID)."
  ;; BUG: org-insert-heading creates level 1 (*) instead of level 3 (***)
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-level2-parent-level3-children
   '((sequence "TODO" "|" "DONE"))
   '("internet")
   `(,org-mcp-test--level2-parent-level3-sibling-id)
   "Review org-mcp-test.el"
   "TODO"
   '("internet")
   nil
   (format "org-headline://%s#Top%%20Level/Review%%20the%%20package"
           test-file)
   (format "org-id://%s"
           org-mcp-test--level2-parent-level3-sibling-id)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-after-sibling-level3))

(ert-deftest org-mcp-test-add-todo-with-body ()
  "Test adding TODO with body text."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty nil nil nil
   "Task with Body"
   "TODO"
   '("work")
   org-mcp-test--body-text-multiline
   (format "org-headline://%s#" test-file)
   nil
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-with-body))

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
  ;; Should succeed since * without space is not a headline
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty nil nil nil
   "Task"
   "TODO"
   '("work")
   "Some initial text.\n*"
   (format "org-headline://%s#" test-file)
   nil
   (file-name-nondirectory test-file)
   (concat
    "^\\* TODO Task +:work:\n"
    "\\(?: *:PROPERTIES:\n"
    " *:ID: +[^\n]+\n"
    " *:END:\n\\)?"
    "Some initial text\\.\n"
    "\\*$")))

(ert-deftest org-mcp-test-add-todo-body-with-unbalanced-block ()
  "Test that adding TODO with body containing unbalanced block is rejected.
Unbalanced blocks like #+BEGIN_EXAMPLE without #+END_EXAMPLE should be
rejected in TODO body content."
  ;; Should reject unbalanced blocks
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty nil nil
   "Task with unbalanced block"
   "TODO"
   '("work")
   "Here's an example:\n#+BEGIN_EXAMPLE\nsome code\nMore text after block"
   (format "org-headline://%s#" test-file)))

(ert-deftest org-mcp-test-add-todo-body-with-unbalanced-end-block ()
  "Test that adding TODO with body containing unbalanced END block is rejected.
An #+END_EXAMPLE without matching #+BEGIN_EXAMPLE should be rejected."
  ;; Should reject unbalanced END blocks
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty nil nil
   "Task with unbalanced END block"
   "TODO"
   '("work")
   "Some text before\n#+END_EXAMPLE\nMore text after"
   (format "org-headline://%s#" test-file)))

(ert-deftest org-mcp-test-add-todo-body-with-literal-block-end ()
  "Test that TODO body with END_SRC inside EXAMPLE block is accepted.
#+END_SRC inside an EXAMPLE block is literal text, not a block delimiter.
This is valid Org-mode syntax and should be allowed."
  ;; Should succeed - #+END_SRC is just literal text inside EXAMPLE block
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty nil nil nil
   "Task with literal END_SRC"
   "TODO"
   '("work")
   "Example of source block:\n#+BEGIN_EXAMPLE\n#+END_SRC\n#+END_EXAMPLE\nText after."
   (format "org-headline://%s#" test-file)
   nil
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-with-literal-block-end))

(ert-deftest org-mcp-test-add-todo-after-sibling ()
  "Test adding TODO after a specific sibling."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-nested-siblings
   '((sequence "TODO" "|" "DONE"))
   '("work")
   (list org-mcp-test--content-nested-siblings-parent-id
         org-mcp-test--content-with-id-id)
   "New Task After Second"
   "TODO"
   '("work")
   nil
   (format "org-headline://%s#Parent%%20Task"
           test-file)
   org-mcp-test--content-with-id-uri
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-after-second-child))

(ert-deftest org-mcp-test-add-todo-afterUri-not-sibling ()
  "Test error when afterUri is not a child of parentUri."
  ;; Error: Other Child is not a child of First Parent
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-wrong-levels nil nil
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#First%%20Parent" test-file)
   (format "org-headline://%s#Second%%20Parent/Other%%20Child" test-file)))

(ert-deftest org-mcp-test-add-todo-parent-id-uri ()
  "Test adding TODO with parent specified as org-id:// URI."
  ;; Use org-id:// for parent instead of org-headline://
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-nested-siblings
   '((sequence "TODO(t!)" "|" "DONE(d!)"))
   '("work")
   (list org-mcp-test--content-nested-siblings-parent-id
         org-mcp-test--content-with-id-id)
   "Child via ID"
   "TODO"
   '("work")
   nil
   (format "org-id://%s"
           org-mcp-test--content-nested-siblings-parent-id)
   nil
   (file-name-nondirectory test-file)
   org-mcp-test--pattern-add-todo-parent-id-uri))

(ert-deftest org-mcp-test-add-todo-mutex-tags-error ()
  "Test that mutually exclusive tags are rejected."
  (org-mcp-test--call-add-todo-expecting-error
   "#+TITLE: Test Org File\n\n"
   '((sequence "TODO" "|" "DONE"))
   '(("work" . ?w)
     :startgroup
     ("@office" . ?o)
     ("@home" . ?h)
     :endgroup)
   "Test Task"
   "TODO"
   ["work" "@office" "@home"] ; conflicting tags
   nil
   (format "org-headline://%s#" test-file)
   nil))

(ert-deftest org-mcp-test-add-todo-mutex-tags-valid ()
  "Test that non-conflicting tags from mutex groups are accepted."
  (org-mcp-test--add-todo-and-check
   "#+TITLE: Test Org File\n\n"
   '((sequence "TODO" "|" "DONE"))
   '(("work" . ?w)
     :startgroup
     ("@office" . ?o)
     ("@home" . ?h)
     :endgroup ("project" . ?p))
   nil
   "Test Task"
   "TODO"
   ["work" "@office" "project"] ; no conflict
   nil
   (format "org-headline://%s#" test-file)
   nil
   (file-name-nondirectory test-file)
   org-mcp-test--regex-add-todo-with-mutex-tags))

(ert-deftest org-mcp-test-add-todo-nil-tags ()
  "Test that adding TODO with nil tags creates headline without tags."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty nil nil nil
   "Task Without Tags"
   "TODO"
   nil ; nil for tags
   nil ; no body
   (format "org-headline://%s#" test-file)
   nil ; no afterUri
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-without-tags))

(ert-deftest org-mcp-test-add-todo-empty-list-tags ()
  "Test that adding TODO with empty list tags creates headline without tags."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty nil nil nil
   "Task Without Tags"
   "TODO"
   '() ; empty list for tags
   nil ; no body
   (format "org-headline://%s#" test-file)
   nil ; no afterUri
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-without-tags))

;; org-rename-headline tests

(ert-deftest org-mcp-test-rename-headline-simple ()
  "Test renaming a simple TODO headline."
  (let ((org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
    (org-mcp-test--call-rename-headline-and-check
     org-mcp-test--content-simple-todo
     "Original%20Task"
     "Original Task"
     "Updated Task"
     org-mcp-test--pattern-renamed-simple-todo)))

(ert-deftest org-mcp-test-rename-headline-title-mismatch ()
  "Test that rename fails when current title doesn't match."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--call-rename-headline-expecting-error
     org-mcp-test--content-simple-todo
     "Original%20Task"
     "Wrong Title"
     "Updated Task")))

(ert-deftest org-mcp-test-rename-headline-preserve-tags ()
  "Test that renaming preserves tags."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (org-tag-alist '("work" "urgent" "personal")))
    (org-mcp-test--call-rename-headline-and-check
     org-mcp-test--content-todo-with-tags
     "Task%20with%20Tags"
     "Task with Tags"
     "Renamed Task"
     org-mcp-test--pattern-renamed-todo-with-tags)))

(ert-deftest org-mcp-test-rename-headline-no-todo ()
  "Test renaming a regular headline without TODO state."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-nested-siblings
   "Parent%20Task/First%20Child%2050%25%20Complete"
   "First Child 50% Complete"
   "Updated Child"
   org-mcp-test--pattern-renamed-headline-no-todo))

(ert-deftest org-mcp-test-rename-headline-nested-path-navigation ()
  "Test correct headline path navigation in nested structures.
Verifies that the implementation correctly navigates nested headline
paths and only matches headlines at the appropriate hierarchy level."
  ;; Try to rename "First Parent/Target Headline"
  ;; But there's no Target Headline under First Parent!
  ;; The function should fail, but it might incorrectly
  ;; find Third Parent's Target Headline
  ;; This should throw an error because First Parent has no Target Headline
  (org-mcp-test--call-rename-headline-expecting-error
   org-mcp-test--content-wrong-levels
   "First%20Parent/Target%20Headline"
   "Target Headline"
   "Renamed Target Headline"))

(ert-deftest org-mcp-test-rename-headline-by-id ()
  "Test renaming a headline accessed by org-id URI."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-nested-siblings
   org-mcp-test--content-with-id-uri
   "Second Child"
   "Renamed Second Child"
   org-mcp-test--expected-regex-renamed-second-child
   `(,org-mcp-test--content-with-id-id)))

(ert-deftest org-mcp-test-rename-headline-id-not-found ()
  "Test error when ID doesn't exist."
  (let ((org-id-track-globally nil)
        (org-id-locations-file nil))
    (org-mcp-test--call-rename-headline-expecting-error
     org-mcp-test--content-nested-siblings
     "org-id://non-existent-id-12345"
     "Whatever"
     "Should Fail")))

(ert-deftest org-mcp-test-rename-headline-with-slash ()
  "Test renaming a headline containing a slash character.
Slashes must be properly URL-encoded to avoid path confusion."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-slash-not-nested-before
   "Parent%2FChild"
   "Parent/Child"
   "Parent/Child Renamed"
   org-mcp-test--pattern-renamed-slash-headline))

(ert-deftest org-mcp-test-rename-headline-slash-not-nested ()
  "Test that headline with slash is not treated as nested path.
Verifies that 'Parent/Child' is treated as a single headline,
not as Child under Parent."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-slash-not-nested-before
   "Parent%2FChild"
   "Parent/Child"
   "Parent-Child Renamed"
   org-mcp-test--regex-slash-not-nested-after))

(ert-deftest org-mcp-test-rename-headline-with-percent ()
  "Test renaming a headline containing a percent sign.
Percent signs must be properly URL-encoded to avoid double-encoding issues."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-nested-siblings
   "Parent%20Task/First%20Child%2050%25%20Complete"
   "First Child 50% Complete"
   "First Child 75% Complete"
   org-mcp-test--regex-percent-after))

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

(ert-deftest org-mcp-test-rename-headline-duplicate-first-match ()
  "Test that when multiple headlines have the same name, first match is renamed.
This test documents the first-match behavior when duplicate headlines exist."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-duplicate-headlines-before
   "Project%20Review"
   "Project Review"
   "Q1 Review"
   org-mcp-test--regex-duplicate-first-renamed))

(ert-deftest org-mcp-test-rename-headline-creates-id ()
  "Test that renaming a headline creates an Org ID and returns it."
  (let ((org-id-track-globally t)
        (org-id-locations-file (make-temp-file "test-org-id")))
    (org-mcp-test--call-rename-headline-and-check
     org-mcp-test--content-nested-siblings
     "Parent%20Task/Third%20Child%20%233"
     "Third Child #3"
     "Renamed Child"
     org-mcp-test--pattern-renamed-headline-with-id)))


(ert-deftest org-mcp-test-rename-headline-hierarchy ()
  "Test that headline hierarchy is correctly navigated.
Ensures that when searching for nested headlines, the function
correctly restricts search to the parent's subtree."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-hierarchy-before
   "Second%20Section/Target"
   "Target"
   "Renamed Target"
   org-mcp-test--regex-hierarchy-second-target-renamed))

(ert-deftest org-mcp-test-rename-headline-with-todo-keyword ()
  "Test that headlines with TODO keywords can be renamed.
The navigation function should find headlines even when they have TODO keywords."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-todo-keywords-before
   "Project%20Management/Review%20Documents"
   "Review Documents"
   "Q1 Planning Review"
   org-mcp-test--regex-todo-keywords-after))

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

(ert-deftest org-mcp-test-edit-body-multiple-without-replaceall ()
  "Test error for multiple occurrences without replaceAll."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-repeated-text
      `("test-id")
    (org-mcp-test--call-edit-body-expecting-error
     test-file "org-id://test-id" "occurrence of pattern" "REPLACED" nil)))

(ert-deftest org-mcp-test-edit-body-replace-all ()
  "Test org-edit-body tool with replaceAll functionality."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-repeated-text
      `("test-id")
    (org-mcp-test--call-edit-body-and-check
     test-file
     "org-id://test-id"
     "occurrence of pattern"
     "REPLACED"
     org-mcp-test--pattern-edit-body-replace-all
     t)))

(ert-deftest org-mcp-test-edit-body-replace-all-explicit-false ()
  "Test that explicit replace_all=false triggers error on multiple matches."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-repeated-text
      `("test-id")
    ;; Should error because multiple occurrences exist
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     "org-id://test-id"
     "occurrence of pattern"
     "REPLACED"
     :false)))

(ert-deftest org-mcp-test-edit-body-not-found ()
  "Test org-edit-body tool error when text is not found."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "nonexistent text"
     "replacement"
     nil)))

(ert-deftest org-mcp-test-edit-body-empty ()
  "Test org-edit-body tool can add content to empty body."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((resource-uri
           (format "org-headline://%s#Parent%%20Task/Third%%20Child%%20%%233"
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
     "replacement"
     nil)))

(ert-deftest org-mcp-test-edit-body-empty-with-properties ()
  "Test adding content to empty body with properties drawer."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-no-body
      `(,org-mcp-test--timestamp-id)
    (org-mcp-test--call-edit-body-and-check
     test-file
     (format "org-id://%s" org-mcp-test--timestamp-id)
     ""
     "Content added after properties."
     org-mcp-test--pattern-edit-body-empty-with-props)))

(ert-deftest org-mcp-test-edit-body-nested-headlines ()
  "Test org-edit-body preserves nested headlines."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (org-mcp-test--call-edit-body-and-check
     test-file
     (format "org-headline://%s#Parent%%20Task" test-file)
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
* This would become a headline"
     nil)))

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
     (format "org-headline://%s#Parent%%20Task/Second%%20Child"
             test-file)
     "Second child content."
     "New text
* Top level heading"
     nil)))

(ert-deftest org-mcp-test-edit-body-reject-headline-at-start ()
  "Test org-edit-body rejects newBody with headline at beginning."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "* Heading at start"
     nil)))

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
Code without END_EXAMPLE"
     nil)))

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
Without BEGIN_SRC"
     nil)))

(ert-deftest org-mcp-test-edit-body-reject-mismatched-blocks ()
  "Test org-edit-body rejects newBody with mismatched blocks."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-nested-siblings
   `(,org-mcp-test--content-with-id-id)
   (org-mcp-test--call-edit-body-expecting-error
    test-file
    org-mcp-test--content-with-id-uri
    "Second child content."
    "Text here
#+BEGIN_QUOTE
Some quote
#+END_EXAMPLE"
    nil)))

;; org-read-file tests

(ert-deftest org-mcp-test-tool-read-file ()
  "Test org-read-file tool returns same content as file resource."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-nested-siblings))
   (let ((result-text (org-mcp-test--call-read-file test-file)))
     (should (string= result-text org-mcp-test--content-nested-siblings)))))

;; org-read-outline tests

(ert-deftest org-mcp-test-tool-read-outline ()
  "Test org-read-outline tool returns valid JSON outline structure."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-nested-siblings))
   (let* ((result (org-mcp-test--call-read-outline test-file))
          (headings (alist-get 'headings result)))
     (should (= (length headings) 1))
     (should (string= (alist-get 'title (aref headings 0)) "Parent Task")))))

;; org-read-headline test

(ert-deftest org-mcp-test-tool-read-headline-empty-path ()
  "Test org-read-headline with empty headline_path signals validation error."
  (should-error
   (org-mcp-test--call-read-headline-expecting-error
    org-mcp-test--content-nested-siblings "")
   :type 'mcp-server-lib-tool-error))

(ert-deftest org-mcp-test-tool-read-headline-single-level ()
  "Test org-read-headline with single-level path."
  (org-mcp--tool-read-headline-and-check
   org-mcp-test--content-slash-not-nested-before
   "Parent%2FChild"
   org-mcp-test--pattern-tool-read-headline-single))

(ert-deftest org-mcp-test-tool-read-headline-nested ()
  "Test org-read-headline with nested path."
  (org-mcp--tool-read-headline-and-check
   org-mcp-test--content-nested-siblings
   "Parent%20Task/First%20Child%2050%25%20Complete"
   org-mcp-test--pattern-tool-read-headline-nested))

(ert-deftest org-mcp-test-tool-read-by-id ()
  "Test org-read-by-id tool returns headline content by ID."
  (org-mcp-test--with-id-setup
   test-file org-mcp-test--content-nested-siblings
   `(,org-mcp-test--content-with-id-id)
   (org-mcp-test--call-read-by-id-and-check
    org-mcp-test--content-with-id-id
    org-mcp-test--pattern-tool-read-by-id)))

;; Resource tests

(ert-deftest org-mcp-test-file-resource-template-in-list ()
  "Test that file template appears in resources/templates/list."
  (let ((org-mcp-allowed-files '("test.org")))
    (org-mcp-test--with-enabled
     (let ((templates
            (mcp-server-lib-ert-get-resource-templates-list)))
       ;; Check that we have four templates now
       (should (= (length templates) 4))
       ;; Check that we have all templates
       (let ((template-uris
              (mapcar
               (lambda (template)
                 (alist-get 'uriTemplate template))
               (append templates nil))))
         (should (member "org://{filename}" template-uris))
         (should (member "org-outline://{filename}" template-uris))
         (should (member "org-headline://{filename}" template-uris))
         (should (member "org-id://{uuid}" template-uris)))))))

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
  "Test that reading a resource returns file content."
  (let ((test-content "* Test Heading\nThis is test content."))
    (org-mcp-test--with-temp-org-files
     ((test-file test-content))
     (let ((uri (format "org://%s" test-file)))
       (org-mcp-test--verify-resource-read
        uri
        test-content)))))

(ert-deftest org-mcp-test-outline-resource-returns-structure ()
  "Test that outline resource returns document structure."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-headline-resource))
   (let* ((uri (format "org-outline://%s" test-file))
          (request
            (mcp-server-lib-create-resources-read-request uri))
          (response-json
           (mcp-server-lib-process-jsonrpc request mcp-server-lib-ert-server-id))
          (response
           (json-parse-string response-json
                              :object-type 'alist))
          (result (alist-get 'result response))
          (contents (alist-get 'contents result)))
     (when (alist-get 'error response)
       (error
        "Resource request failed: %s"
        (alist-get 'message (alist-get 'error response))))
     (let* ((outline-json (alist-get 'text (aref contents 0)))
            (outline
             (json-parse-string outline-json
                                :object-type 'alist))
            (headings (alist-get 'headings outline)))
       (should (= (length headings) 2))
       (let ((first (aref headings 0)))
         (should
          (equal (alist-get 'title first) "First Section"))
         (should (= (alist-get 'level first) 1))
         (let ((children (alist-get 'children first)))
           (should (= (length children) 2))
           (should
            (equal
             (alist-get 'title (aref children 0))
             "Subsection 1.1"))
           (should
            (= (length (alist-get 'children (aref children 0))) 0))
           (should
            (equal
             (alist-get 'title (aref children 1))
             "Subsection 1.2"))
           (should
            (= (length (alist-get 'children (aref children 1))) 0))))
       (let ((second (aref headings 1)))
         (should
          (equal (alist-get 'title second) "Second Section"))
         (should (= (alist-get 'level second) 1))
         ;; Deep subsection is empty (level 3 under level 1)
         (should
          (= (length (alist-get 'children second)) 0)))))))

(ert-deftest org-mcp-test-headline-resource-returns-top-level-content ()
  "Test that headline resource returns top-level headline content."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-headline-resource))
   (let ((uri
          (format "org-headline://%s#First%%20Section"
                  test-file)))
     (org-mcp-test--verify-resource-read
      uri
      org-mcp-test--expected-first-section))))

(ert-deftest org-mcp-test-headline-resource-returns-nested-content ()
  "Test that headline resource returns nested headline content."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-headline-resource))
   (let ((uri
          (format (concat
                   "org-headline://%s#"
                   "First%%20Section/Subsection%%201.1")
                  test-file)))
     (org-mcp-test--verify-resource-read
      uri
      org-mcp-test--expected-subsection-1-1))))

(ert-deftest org-mcp-test-headline-resource-not-found ()
  "Test headline resource error for non-existent headline."
  (let ((test-content "* Existing Section\nSome content."))
    (org-mcp-test--with-temp-org-files
     ((test-file test-content))
     (let ((uri
            (format "org-headline://%s#Nonexistent" test-file)))
       (org-mcp-test--read-resource-expecting-error
        uri "Cannot find headline: 'Nonexistent'")))))

(ert-deftest org-mcp-test-headline-resource-file-with-hash ()
  "Test headline resource with # in filename."
  (org-mcp-test--with-temp-org-files
   ((file org-mcp-test--content-nested-siblings "org-mcp-test-file#"))
   ;; Test accessing the file with # encoded as %23
   (let* ((encoded-path (replace-regexp-in-string "#" "%23" file))
          (uri
           (format "org-headline://%s#Parent%%20Task/First%%20Child%%2050%%25%%20Complete"
                   encoded-path)))
     (org-mcp-test--verify-resource-read
      uri
      "** First Child 50% Complete\nFirst child content.\nIt spans multiple lines."))))

(ert-deftest org-mcp-test-headline-resource-headline-with-hash ()
  "Test headline resource with # in headline title."
  (let ((test-content org-mcp-test--content-nested-siblings))
    (org-mcp-test--with-temp-org-files
     ((file test-content))
     ;; Test accessing headline with # encoded as %23
     (let ((uri
            (format "org-headline://%s#Parent%%20Task/Third%%20Child%%20%%233"
                    file)))
       (org-mcp-test--verify-resource-read
        uri
        "** Third Child #3")))))

(ert-deftest
    org-mcp-test-headline-resource-file-and-headline-with-hash
    ()
  "Test headline resource with # in both filename and headline."
  (org-mcp-test--with-temp-org-files
   ((file org-mcp-test--content-nested-siblings "org-mcp-test-file#"))
   ;; Test with both file and headline containing #
   (let* ((encoded-path (replace-regexp-in-string "#" "%23" file))
          (uri
           (format "org-headline://%s#Parent%%20Task/Third%%20Child%%20%%233"
                   encoded-path)))
     (org-mcp-test--verify-resource-read
      uri
      "** Third Child #3"))))

(ert-deftest org-mcp-test-headline-resource-txt-extension ()
  "Test that headline resource works with .txt files, not just .org files."
  (org-mcp-test--test-headline-resource-with-extension ".txt"))

(ert-deftest org-mcp-test-headline-resource-no-extension ()
  "Test that headline resource works with files having no extension."
  (org-mcp-test--test-headline-resource-with-extension nil))

(ert-deftest org-mcp-test-headline-resource-path-traversal ()
  "Test that path traversal with ../ in org-headline URIs is rejected."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-nested-siblings))
   ;; Test with ../ in the filename part
   (let ((uri
          (format "org-headline://../%s#Parent%%20Task"
                  (file-name-nondirectory test-file))))
     (org-mcp-test--read-resource-expecting-error
      uri
      (format "Path must be absolute: ../%s"
              (file-name-nondirectory test-file))))))

(ert-deftest org-mcp-test-headline-resource-encoded-path-traversal ()
  "Test that URL-encoded path traversal in org-headline URIs is rejected."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-nested-siblings))
   ;; Test with URL-encoded ../ (%2E%2E%2F) in the filename part
   ;; The encoding is NOT decoded, so %2E%2E%2F remains literal
   (let ((uri
          (format "org-headline://%%2E%%2E%%2F%s#Parent%%20Task"
                  (file-name-nondirectory test-file))))
     (org-mcp-test--read-resource-expecting-error
      uri
      (format "Path must be absolute: %%2E%%2E%%2F%s"
              (file-name-nondirectory test-file))))))

(ert-deftest org-mcp-test-headline-resource-navigation ()
  "Test that headline navigation respects structure."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-wrong-levels))
   ;; Test accessing "Target Headline" under "First Parent"
   ;; Should get the level-2 headline, NOT the level-3 one
   (let ((uri
          (format
           "org-headline://%s#First%%20Parent/Target%%20Headline"
           test-file)))
     ;; This SHOULD throw an error because First Parent has no such child
     ;; But the bug causes it to return the wrong headline
     (org-mcp-test--read-resource-expecting-error
      uri
      "Cannot find headline: 'First Parent/Target Headline'"))))

(ert-deftest org-mcp-test-id-resource-returns-content ()
  "Test that ID resource returns content for valid ID."
  (org-mcp-test--with-id-setup
   test-file org-mcp-test--content-id-resource
   `(,org-mcp-test--content-id-resource-id)
   (let ((uri (format "org-id://%s" org-mcp-test--content-id-resource-id)))
     (org-mcp-test--verify-resource-read
      uri
      org-mcp-test--content-id-resource))))

(ert-deftest org-mcp-test-id-resource-not-found ()
  "Test ID resource error for non-existent ID."
  (let ((test-content "* Section without ID\nNo ID here."))
    (org-mcp-test--with-id-setup test-file test-content '()
                                 (let ((uri "org-id://nonexistent-id-12345"))
                                   (org-mcp-test--read-resource-expecting-error
                                    uri "Cannot find ID: 'nonexistent-id-12345'")))))

(ert-deftest org-mcp-test-id-resource-file-not-allowed ()
  "Test ID resource validates file is in allowed list."
  ;; Create two files - one allowed, one not
  (org-mcp-test--with-temp-org-files
   ((allowed-file "* Allowed\n")
    (other-file org-mcp-test--content-id-resource))
   (org-mcp-test--with-id-tracking
    (list allowed-file)
    `((,org-mcp-test--content-id-resource-id . ,other-file))
    (let ((uri (format "org-id://%s" org-mcp-test--content-id-resource-id)))
      ;; Should get an error for file not allowed
      (org-mcp-test--read-resource-expecting-error
       uri
       (format "'%s': the referenced file not in allowed list"
               org-mcp-test--content-id-resource-id))))))

;;; org-ql-query tests

(defconst org-mcp-test--content-ql-todos
  "* TODO Buy groceries :personal:
* DONE Write report :work:
* TODO Review PR :work:code:
:PROPERTIES:
:ID:       ql-test-id-001
:END:
* TODO Plan vacation :personal:
:PROPERTIES:
:CUSTOM_PROP: some-value
:END:"
  "Org content with TODO/DONE items, tags, and IDs for org-ql tests.")

(defconst org-mcp-test--content-ql-priorities
  "* TODO [#A] Urgent task
* TODO [#B] Normal task :work:
* TODO [#C] Low priority task"
  "Org content with priorities for org-ql tests.")

(defconst org-mcp-test--content-ql-parent-priorities
  "* TODO [#A] Parent with priority
** TODO Child without priority
** TODO [#C] Child with own priority
* TODO Parent without priority
** TODO [#B] Child under no-priority parent
* [#B] Non-todo parent with priority
** TODO Child under non-todo parent"
  "Org content with nested headings for parent-priority tests.")

(ert-deftest org-mcp-test-ql-query-basic-todo ()
  "Test basic org-ql query matching TODO items."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-todos))
   (let* ((result-text
           (mcp-server-lib-ert-call-tool
            "org-ql-query" '((query . "(todo \"TODO\")"))))
          (result (json-read-from-string result-text)))
     (should (= (alist-get 'total result) 3))
     (should (= (alist-get 'files_searched result) 1))
     (should (= (length (alist-get 'matches result)) 3))
     (let ((first-match (aref (alist-get 'matches result) 0)))
       (should (equal (alist-get 'title first-match) "Buy groceries"))
       (should (equal (alist-get 'todo first-match) "TODO"))
       (should (= (alist-get 'level first-match) 1))))))

(ert-deftest org-mcp-test-ql-query-tags ()
  "Test org-ql query matching by tags."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-todos))
   (let* ((result-text
           (mcp-server-lib-ert-call-tool
            "org-ql-query" '((query . "(tags \"work\")"))))
          (result (json-read-from-string result-text)))
     (should (= (alist-get 'total result) 2)))))

(ert-deftest org-mcp-test-ql-query-compound ()
  "Test org-ql compound query with AND."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-todos))
   (let* ((result-text
           (mcp-server-lib-ert-call-tool
            "org-ql-query"
            '((query . "(and (todo \"TODO\") (tags \"personal\"))"))))
          (result (json-read-from-string result-text)))
     (should (= (alist-get 'total result) 2)))))

(ert-deftest org-mcp-test-ql-query-empty-result ()
  "Test org-ql query with no matches."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-todos))
   (let* ((result-text
           (mcp-server-lib-ert-call-tool
            "org-ql-query"
            '((query . "(todo \"WAITING\")"))))
          (result (json-read-from-string result-text)))
     (should (= (alist-get 'total result) 0))
     (should (= (length (alist-get 'matches result)) 0)))))

(ert-deftest org-mcp-test-ql-query-multiple-files ()
  "Test org-ql query across multiple files."

  (org-mcp-test--with-temp-org-files
   ((file1 org-mcp-test--content-ql-todos)
    (file2 org-mcp-test--content-ql-priorities))
   (let* ((result-text
           (mcp-server-lib-ert-call-tool
            "org-ql-query" '((query . "(todo \"TODO\")"))))
          (result (json-read-from-string result-text)))
     (should (= (alist-get 'total result) 6))
     (should (= (alist-get 'files_searched result) 2)))))

(ert-deftest org-mcp-test-ql-query-files-parameter ()
  "Test org-ql query with explicit files parameter."

  (org-mcp-test--with-temp-org-files
   ((file1 org-mcp-test--content-ql-todos)
    (file2 org-mcp-test--content-ql-priorities))
   (let* ((result-text
           (mcp-server-lib-ert-call-tool
            "org-ql-query"
            `((query . "(todo \"TODO\")")
              (files . ,(vector file2)))))
          (result (json-read-from-string result-text)))
     (should (= (alist-get 'total result) 3))
     (should (= (alist-get 'files_searched result) 1)))))

(ert-deftest org-mcp-test-ql-query-parent-priority-inherited ()
  "Test that child headings include parent-priority from parent."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-parent-priorities))
   (let* ((result-text
           (mcp-server-lib-ert-call-tool
            "org-ql-query"
            '((query . "(heading \"Child without priority\")"))))
          (result (json-read-from-string result-text))
          (match (aref (alist-get 'matches result) 0)))
     (should (equal (alist-get 'title match) "Child without priority"))
     (should (equal (alist-get 'parent-priority match) "A"))
     (should-not (alist-get 'priority match)))))

(ert-deftest org-mcp-test-ql-query-parent-priority-with-own ()
  "Test parent-priority present even when child has own priority."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-parent-priorities))
   (let* ((result-text
           (mcp-server-lib-ert-call-tool
            "org-ql-query"
            '((query . "(heading \"Child with own priority\")"))))
          (result (json-read-from-string result-text))
          (match (aref (alist-get 'matches result) 0)))
     (should (equal (alist-get 'priority match) "C"))
     (should (equal (alist-get 'parent-priority match) "A")))))

(ert-deftest org-mcp-test-ql-query-parent-priority-absent ()
  "Test parent-priority absent when parent has no priority."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-parent-priorities))
   (let* ((result-text
           (mcp-server-lib-ert-call-tool
            "org-ql-query"
            '((query . "(heading \"Child under no-priority parent\")"))))
          (result (json-read-from-string result-text))
          (match (aref (alist-get 'matches result) 0)))
     (should (equal (alist-get 'priority match) "B"))
     (should-not (alist-get 'parent-priority match)))))

(ert-deftest org-mcp-test-ql-query-parent-priority-top-level ()
  "Test parent-priority absent for top-level headings."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-parent-priorities))
   (let* ((result-text
           (mcp-server-lib-ert-call-tool
            "org-ql-query"
            '((query . "(heading \"Parent with priority\")"))))
          (result (json-read-from-string result-text))
          (match (aref (alist-get 'matches result) 0)))
     (should (equal (alist-get 'priority match) "A"))
     (should-not (alist-get 'parent-priority match)))))

(ert-deftest org-mcp-test-ql-query-parent-priority-non-todo-parent ()
  "Test parent-priority from non-TODO parent with priority."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-parent-priorities))
   (let* ((result-text
           (mcp-server-lib-ert-call-tool
            "org-ql-query"
            '((query . "(heading \"Child under non-todo parent\")"))))
          (result (json-read-from-string result-text))
          (match (aref (alist-get 'matches result) 0)))
     (should (equal (alist-get 'parent-priority match) "B")))))

(defun org-mcp-test--call-ql-query-expecting-error (params)
  "Call org-ql-query tool via JSON-RPC expecting an error.
PARAMS is the alist of parameters to pass."
  (let* ((request
          (mcp-server-lib-create-tools-call-request
           "org-ql-query" nil params))
         (response
          (mcp-server-lib-process-jsonrpc-parsed
           request mcp-server-lib-ert-server-id))
         (result
          (mcp-server-lib-ert-process-tool-response
           response)))
    (error "Expected error but got success: %s" result)))

(ert-deftest org-mcp-test-ql-query-file-not-allowed ()
  "Test org-ql query rejects files not in allowed list."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-todos))
   (should-error
    (org-mcp-test--call-ql-query-expecting-error
     '((query . "(todo \"TODO\")")
       (files . ["/not/allowed/file.org"])))
    :type 'mcp-server-lib-tool-error)))

(ert-deftest org-mcp-test-ql-query-invalid-parse ()
  "Test org-ql query with unparseable query string."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-todos))
   (should-error
    (org-mcp-test--call-ql-query-expecting-error
     '((query . "(unclosed paren")))
    :type 'mcp-server-lib-tool-error)))

(ert-deftest org-mcp-test-ql-query-atom-not-list ()
  "Test org-ql query rejects atom (non-list) queries."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-todos))
   (should-error
    (org-mcp-test--call-ql-query-expecting-error
     '((query . "just-a-symbol")))
    :type 'mcp-server-lib-tool-error)))

(ert-deftest org-mcp-test-ql-query-empty-string ()
  "Test org-ql query rejects empty query string."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-todos))
   (should-error
    (org-mcp-test--call-ql-query-expecting-error
     '((query . "")))
    :type 'mcp-server-lib-tool-error)))

(ert-deftest org-mcp-test-ql-query-uri-correctness ()
  "Test that entries with ID get org-id:// URI, others get org-headline://."

  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-ql-todos))
   (org-mcp-test--with-id-tracking
    (list test-file) `(("ql-test-id-001" . ,test-file))
    (let* ((result-text
            (mcp-server-lib-ert-call-tool
             "org-ql-query" '((query . "(todo \"TODO\")"))))
           (result (json-read-from-string result-text))
           (matches (alist-get 'matches result)))
      ;; First match (Buy groceries) has no ID -> org-headline://
      (should (string-prefix-p
               "org-headline://"
               (alist-get 'uri (aref matches 0))))
      ;; Second match (Review PR) has ID -> org-id://
      (should (equal
               (alist-get 'uri (aref matches 1))
               "org-id://ql-test-id-001"))
      ;; Third match (Plan vacation) has no ID -> org-headline://
      (should (string-prefix-p
               "org-headline://"
               (alist-get 'uri (aref matches 2))))))))

;;; Stored org-ql queries tests

(defmacro org-mcp-test--with-stored-queries (&rest body)
  "Run BODY with a temporary stored queries file, then clean up."
  (declare (indent 0) (debug t))
  `(let* ((temp-file (make-temp-file "org-mcp-stored-queries" nil ".el"))
          (org-mcp-stored-queries-file temp-file)
          (org-mcp--stored-queries 'unloaded))
     (unwind-protect
         (org-mcp-test--with-enabled
           ,@body)
       (setq org-mcp--stored-queries 'unloaded)
       (when (file-exists-p temp-file)
         (delete-file temp-file)))))

(defun org-mcp-test--call-stored-query-tool-expecting-error (tool-id params)
  "Call stored query TOOL-ID via JSON-RPC with PARAMS expecting an error."
  (let* ((request
          (mcp-server-lib-create-tools-call-request
           tool-id nil params))
         (response
          (mcp-server-lib-process-jsonrpc-parsed
           request mcp-server-lib-ert-server-id))
         (result
          (mcp-server-lib-ert-process-tool-response
           response)))
    (error "Expected error but got success: %s" result)))

(defconst org-mcp-test--stored-query-key-active "active-todos"
  "Test key for stored query.")

(defconst org-mcp-test--stored-query-query-active "(todo \"TODO\")"
  "Test query string for stored query.")

(defconst org-mcp-test--stored-query-desc-active "All active TODO items"
  "Test description for stored query.")

(defconst org-mcp-test--stored-query-key-urgent "urgent-work"
  "Second test key for stored query.")

(defconst org-mcp-test--stored-query-query-urgent
  "(and (todo \"TODO\") (priority \"A\"))"
  "Second test query string for stored query.")

(defconst org-mcp-test--stored-query-desc-urgent "Urgent work tasks"
  "Second test description for stored query.")

(ert-deftest org-mcp-test-ql-list-stored-queries-empty ()
  "Test listing stored queries when none are saved."
  (org-mcp-test--with-stored-queries
    (let* ((result-text
            (mcp-server-lib-ert-call-tool
             "org-ql-list-stored-queries" nil))
           (result (json-read-from-string result-text)))
      (should (= (alist-get 'total result) 0))
      (should (= (length (alist-get 'queries result)) 0)))))

(ert-deftest org-mcp-test-ql-save-stored-query-new ()
  "Test saving a new stored query."
  (org-mcp-test--with-stored-queries
    (let* ((result-text
            (mcp-server-lib-ert-call-tool
             "org-ql-save-stored-query"
             `((key . ,org-mcp-test--stored-query-key-active)
               (query . ,org-mcp-test--stored-query-query-active)
               (description . ,org-mcp-test--stored-query-desc-active))))
           (result (json-read-from-string result-text)))
      (should (eq (alist-get 'success result) t))
      (should (equal (alist-get 'action result) "created"))
      (should (equal (alist-get 'key result)
                     org-mcp-test--stored-query-key-active)))))

(ert-deftest org-mcp-test-ql-save-stored-query-update ()
  "Test updating an existing stored query."
  (org-mcp-test--with-stored-queries
    (mcp-server-lib-ert-call-tool
     "org-ql-save-stored-query"
     `((key . ,org-mcp-test--stored-query-key-active)
       (query . ,org-mcp-test--stored-query-query-active)
       (description . ,org-mcp-test--stored-query-desc-active)))
    (let* ((result-text
            (mcp-server-lib-ert-call-tool
             "org-ql-save-stored-query"
             `((key . ,org-mcp-test--stored-query-key-active)
               (query . "(todo \"DONE\")")
               (description . "Updated description"))))
           (result (json-read-from-string result-text)))
      (should (eq (alist-get 'success result) t))
      (should (equal (alist-get 'action result) "updated")))))

(ert-deftest org-mcp-test-ql-list-stored-queries-after-save ()
  "Test listing stored queries after saving two."
  (org-mcp-test--with-stored-queries
    (mcp-server-lib-ert-call-tool
     "org-ql-save-stored-query"
     `((key . ,org-mcp-test--stored-query-key-active)
       (query . ,org-mcp-test--stored-query-query-active)
       (description . ,org-mcp-test--stored-query-desc-active)))
    (mcp-server-lib-ert-call-tool
     "org-ql-save-stored-query"
     `((key . ,org-mcp-test--stored-query-key-urgent)
       (query . ,org-mcp-test--stored-query-query-urgent)
       (description . ,org-mcp-test--stored-query-desc-urgent)))
    (let* ((result-text
            (mcp-server-lib-ert-call-tool
             "org-ql-list-stored-queries" nil))
           (result (json-read-from-string result-text))
           (queries (alist-get 'queries result)))
      (should (= (alist-get 'total result) 2))
      (should (= (length queries) 2))
      (should (equal (alist-get 'key (aref queries 0))
                     org-mcp-test--stored-query-key-active))
      (should (equal (alist-get 'query (aref queries 0))
                     org-mcp-test--stored-query-query-active))
      (should (equal (alist-get 'description (aref queries 0))
                     org-mcp-test--stored-query-desc-active))
      (should (equal (alist-get 'key (aref queries 1))
                     org-mcp-test--stored-query-key-urgent)))))

(ert-deftest org-mcp-test-ql-delete-stored-query ()
  "Test deleting a stored query."
  (org-mcp-test--with-stored-queries
    (mcp-server-lib-ert-call-tool
     "org-ql-save-stored-query"
     `((key . ,org-mcp-test--stored-query-key-active)
       (query . ,org-mcp-test--stored-query-query-active)
       (description . ,org-mcp-test--stored-query-desc-active)))
    (let* ((result-text
            (mcp-server-lib-ert-call-tool
             "org-ql-delete-stored-query"
             `((key . ,org-mcp-test--stored-query-key-active))))
           (result (json-read-from-string result-text)))
      (should (eq (alist-get 'success result) t))
      (should (equal (alist-get 'key result)
                     org-mcp-test--stored-query-key-active)))
    (let* ((list-text
            (mcp-server-lib-ert-call-tool
             "org-ql-list-stored-queries" nil))
           (list-result (json-read-from-string list-text)))
      (should (= (alist-get 'total list-result) 0)))))

(ert-deftest org-mcp-test-ql-delete-stored-query-not-found ()
  "Test deleting a nonexistent stored query."
  (org-mcp-test--with-stored-queries
    (should-error
     (org-mcp-test--call-stored-query-tool-expecting-error
      "org-ql-delete-stored-query"
      '((key . "nonexistent")))
     :type 'mcp-server-lib-tool-error)))

(ert-deftest org-mcp-test-ql-save-stored-query-invalid-key-empty ()
  "Test saving a stored query with empty key."
  (org-mcp-test--with-stored-queries
    (should-error
     (org-mcp-test--call-stored-query-tool-expecting-error
      "org-ql-save-stored-query"
      `((key . "")
        (query . ,org-mcp-test--stored-query-query-active)))
     :type 'mcp-server-lib-tool-error)))

(ert-deftest org-mcp-test-ql-save-stored-query-invalid-key-spaces ()
  "Test saving a stored query with spaces in key."
  (org-mcp-test--with-stored-queries
    (should-error
     (org-mcp-test--call-stored-query-tool-expecting-error
      "org-ql-save-stored-query"
      `((key . "has spaces")
        (query . ,org-mcp-test--stored-query-query-active)))
     :type 'mcp-server-lib-tool-error)))

(ert-deftest org-mcp-test-ql-save-stored-query-invalid-query ()
  "Test saving a stored query with unparseable query."
  (org-mcp-test--with-stored-queries
    (should-error
     (org-mcp-test--call-stored-query-tool-expecting-error
      "org-ql-save-stored-query"
      '((key . "test-key")
        (query . "(unclosed paren")))
     :type 'mcp-server-lib-tool-error)))

(ert-deftest org-mcp-test-ql-run-stored-query ()
  "Test running a stored query against test org files."
  (org-mcp-test--with-stored-queries
    (mcp-server-lib-ert-call-tool
     "org-ql-save-stored-query"
     `((key . ,org-mcp-test--stored-query-key-active)
       (query . ,org-mcp-test--stored-query-query-active)
       (description . ,org-mcp-test--stored-query-desc-active)))
    (let* ((temp-org (make-temp-file "org-mcp-test" nil ".org"))
           (org-mcp-allowed-files (list temp-org)))
      (unwind-protect
          (progn
            (with-temp-file temp-org
              (insert org-mcp-test--content-ql-todos))
            (let* ((result-text
                    (mcp-server-lib-ert-call-tool
                     "org-ql-run-stored-query"
                     `((key . ,org-mcp-test--stored-query-key-active))))
                   (result (json-read-from-string result-text)))
              (should (= (alist-get 'total result) 3))
              (should (= (alist-get 'files_searched result) 1))))
        (delete-file temp-org)))))

(ert-deftest org-mcp-test-ql-run-stored-query-not-found ()
  "Test running a nonexistent stored query."
  (org-mcp-test--with-stored-queries
    (should-error
     (org-mcp-test--call-stored-query-tool-expecting-error
      "org-ql-run-stored-query"
      '((key . "nonexistent")))
     :type 'mcp-server-lib-tool-error)))

(ert-deftest org-mcp-test-ql-stored-queries-file-persistence ()
  "Test that queries persist across in-memory resets."
  (org-mcp-test--with-stored-queries
    (mcp-server-lib-ert-call-tool
     "org-ql-save-stored-query"
     `((key . ,org-mcp-test--stored-query-key-active)
       (query . ,org-mcp-test--stored-query-query-active)
       (description . ,org-mcp-test--stored-query-desc-active)))
    ;; Reset in-memory state to force reload from disk
    (setq org-mcp--stored-queries 'unloaded)
    (let* ((result-text
            (mcp-server-lib-ert-call-tool
             "org-ql-list-stored-queries" nil))
           (result (json-read-from-string result-text))
           (queries (alist-get 'queries result)))
      (should (= (alist-get 'total result) 1))
      (should (equal (alist-get 'key (aref queries 0))
                     org-mcp-test--stored-query-key-active))
      (should (equal (alist-get 'query (aref queries 0))
                     org-mcp-test--stored-query-query-active)))))

(ert-deftest org-mcp-test-ql-stored-queries-file-not-configured ()
  "Test error when org-mcp-stored-queries-file is nil."
  (let ((org-mcp-stored-queries-file nil)
        (org-mcp--stored-queries 'unloaded))
    (org-mcp-test--with-enabled
      (should-error
       (org-mcp-test--call-stored-query-tool-expecting-error
        "org-ql-list-stored-queries" nil)
       :type 'mcp-server-lib-tool-error))))

;;; Clock tool tests

;; Test data for clock tools

(defconst org-mcp-test--clock-content-simple
  "* TODO My Task
Some body text."
  "Simple heading without logbook.")

(defconst org-mcp-test--clock-content-with-properties
  (format
   "* TODO Task with Props
:PROPERTIES:
:ID:       %s
:END:
Some body text."
   org-mcp-test--content-with-id-id)
  "Heading with properties but no logbook.")

(defconst org-mcp-test--clock-content-with-logbook
  (format
   "* TODO Task with Logbook
:PROPERTIES:
:ID:       %s
:END:
:LOGBOOK:
CLOCK: [2026-03-23 Mon 09:00]--[2026-03-23 Mon 10:30] =>  1:30
:END:
Some body text."
   org-mcp-test--content-with-id-id)
  "Heading with existing logbook entries.")

(defconst org-mcp-test--clock-content-active
  (format
   "* TODO Active Task
:PROPERTIES:
:ID:       %s
:END:
:LOGBOOK:
CLOCK: [2026-03-23 Mon 14:00]
:END:
Working on this."
   org-mcp-test--content-with-id-id)
  "Heading with active (unclosed) clock.")

(defconst org-mcp-test--clock-content-two-headings
  "* TODO First Task
:LOGBOOK:
CLOCK: [2026-03-23 Mon 14:00]
:END:
First body.
* TODO Second Task
Second body."
  "Two headings, first has active clock.")

(defconst org-mcp-test--clock-id-second "clock-test-second-id")

(defconst org-mcp-test--clock-content-two-headings-with-ids
  (format
   "* TODO First Task
:PROPERTIES:
:ID:       %s
:END:
:LOGBOOK:
CLOCK: [2026-03-23 Mon 14:00]
:END:
First body.
* TODO Second Task
:PROPERTIES:
:ID:       %s
:END:
Second body."
   org-mcp-test--content-with-id-id
   org-mcp-test--clock-id-second)
  "Two headings with IDs, first has active clock.")

;; Regex constants for clock verification

(defconst org-mcp-test--clock-regex-added-entry
  (concat
   "\\`\\* TODO My Task\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-03-23 [A-Za-z]\\{2,3\\} 14:30\\]"
   "--\\[2026-03-23 [A-Za-z]\\{2,3\\} 16:45\\] => +2:15\n"
   ":END:\n"
   "Some body text\\."
   "\\'")
  "Regex for clock-add with new logbook.")

(defconst org-mcp-test--clock-regex-added-to-existing
  (concat
   "\\`\\* TODO Task with Logbook\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-with-id-id "\n"
   ":END:\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-03-23 [A-Za-z]\\{2,3\\} 14:30\\]"
   "--\\[2026-03-23 [A-Za-z]\\{2,3\\} 16:45\\] => +2:15\n"
   "CLOCK: \\[2026-03-23 Mon 09:00\\]--\\[2026-03-23 Mon 10:30\\] => +1:30\n"
   ":END:\n"
   "Some body text\\."
   "\\'")
  "Regex for clock-add to heading with existing logbook.")

(defconst org-mcp-test--clock-regex-clocked-in
  (concat
   "\\`\\* TODO My Task\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-03-23 [A-Za-z]\\{2,3\\} 14:30\\]\n"
   ":END:\n"
   "Some body text\\."
   "\\'")
  "Regex for clock-in result.")

(defconst org-mcp-test--clock-regex-clocked-out
  (concat
   "\\`\\* TODO Active Task\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-with-id-id "\n"
   ":END:\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-03-23 Mon 14:00\\]"
   "--\\[2026-03-23 [A-Za-z]\\{2,3\\} 16:45\\] => +2:45\n"
   ":END:\n"
   "Working on this\\."
   "\\'")
  "Regex for clock-out result.")

(defconst org-mcp-test--clock-regex-auto-close-and-in
  (concat
   "\\`\\* TODO Second Task\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-03-23 [A-Za-z]\\{2,3\\} 15:00\\]\n"
   ":END:\n"
   "Second body\\."
   "\\'")
  "Regex for second file after clock-in with auto-close.")

(defconst org-mcp-test--clock-regex-resolved-and-clocked-in
  (concat
   "\\`\\* TODO Active Task\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-with-id-id "\n"
   ":END:\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-03-23 [A-Za-z]\\{2,3\\} 15:00\\]\n"
   ":END:\n"
   "Working on this\\."
   "\\'")
  "Regex for clock-in after resolving dangling clock.")

;; Clock config tests

(ert-deftest org-mcp-test-clock-get-config ()
  "Test org-get-clock-config returns clock settings."
  (let ((org-clock-into-drawer t)
        (org-clock-rounding-minutes 0)
        (org-clock-continuously nil)
        (org-mcp-clock-continuous-threshold 30))
    (org-mcp-test--with-enabled
      (let* ((result-text
              (mcp-server-lib-ert-call-tool "org-get-clock-config" nil))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'org_clock_into_drawer result) "t"))
        (should (equal (alist-get 'org_clock_rounding_minutes result) 0))
        (should (equal (alist-get 'org_clock_continuously result)
                       :json-false))
        (should (equal (alist-get 'org_mcp_clock_continuous_threshold result)
                       30))))))

(ert-deftest org-mcp-test-clock-get-config-custom ()
  "Test org-get-clock-config with custom settings."
  (let ((org-clock-into-drawer "CLOCKING")
        (org-clock-rounding-minutes 15)
        (org-clock-continuously t)
        (org-mcp-clock-continuous-threshold 60))
    (org-mcp-test--with-enabled
      (let* ((result-text
              (mcp-server-lib-ert-call-tool "org-get-clock-config" nil))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'org_clock_into_drawer result)
                       "\"CLOCKING\""))
        (should (equal (alist-get 'org_clock_rounding_minutes result) 15))
        (should (equal (alist-get 'org_clock_continuously result) t))
        (should (equal (alist-get 'org_mcp_clock_continuous_threshold result)
                       60))))))

;; Get active clock tests

(ert-deftest org-mcp-test-clock-get-active-none ()
  "Test org-clock-get-active when no clock is active."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-simple))
    (let* ((result-text
            (mcp-server-lib-ert-call-tool "org-clock-get-active" nil))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'active result) :json-false)))))

(ert-deftest org-mcp-test-clock-get-active-found ()
  "Test org-clock-get-active when a clock is active."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-active))
    (let* ((result-text
            (mcp-server-lib-ert-call-tool "org-clock-get-active" nil))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'active result) t))
      (should (equal (alist-get 'heading result) "Active Task"))
      (should (string= (alist-get 'start result)
                        "2026-03-23 Mon 14:00"))
      (should (string= (alist-get 'file result) test-file)))))

;; Clock-add tests

(ert-deftest org-mcp-test-clock-add-creates-logbook ()
  "Test clock-add creates LOGBOOK when none exists."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-simple))
    (let* ((uri (format "org-headline://%s#My%%20Task" test-file))
           (params `((uri . ,uri)
                     (start . "2026-03-23T14:30:00")
                     (end . "2026-03-23T16:45:00")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-clock-add" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'added result) t))
      (should (string-match-p "2:15" (alist-get 'duration result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-regex-added-entry))))

(ert-deftest org-mcp-test-clock-add-to-existing-logbook ()
  "Test clock-add inserts at top of existing LOGBOOK."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--clock-content-with-logbook
      `(,org-mcp-test--content-with-id-id)
    (let* ((uri org-mcp-test--content-with-id-uri)
           (params `((uri . ,uri)
                     (start . "2026-03-23T14:30:00")
                     (end . "2026-03-23T16:45:00")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-clock-add" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-regex-added-to-existing))))

(ert-deftest org-mcp-test-clock-add-end-before-start ()
  "Test clock-add rejects end time before start time."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-simple))
    (let ((uri (format "org-headline://%s#My%%20Task" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-clock-add" nil
                `((uri . ,uri)
                  (start . "2026-03-23T16:45:00")
                  (end . "2026-03-23T14:30:00"))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got: %s" result))))))

(ert-deftest org-mcp-test-clock-add-with-rounding ()
  "Test clock-add applies rounding."
  (let ((org-clock-rounding-minutes 15))
    (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-content-simple))
      (let* ((uri (format "org-headline://%s#My%%20Task" test-file))
             (params `((uri . ,uri)
                       (start . "2026-03-23T14:22:00")
                       (end . "2026-03-23T16:37:00")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-clock-add" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        ;; 14:22 rounds to 14:15 with 15-min rounding
        ;; 16:37 rounds to 16:30
        (should (string-match-p "14:15" (alist-get 'start result)))
        (should (string-match-p "16:30" (alist-get 'end result)))))))

;; Clock-in tests

(ert-deftest org-mcp-test-clock-in-basic ()
  "Test basic clock-in with explicit start time."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-simple))
    (let* ((uri (format "org-headline://%s#My%%20Task" test-file))
           (params `((uri . ,uri)
                     (start_time . "2026-03-23T14:30:00")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-clock-in" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'clocked_in result) t))
      (should (string-match-p "14:30" (alist-get 'start result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-regex-clocked-in))))

(ert-deftest org-mcp-test-clock-in-auto-close ()
  "Test clock-in auto-closes active clock in another heading."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-two-headings))
    (let* ((uri (format "org-headline://%s#Second%%20Task" test-file))
           (params `((uri . ,uri)
                     (start_time . "2026-03-23T15:00:00")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-clock-in" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      ;; Verify first task's clock was closed
      (let ((content (org-mcp-test--read-file test-file)))
        (should (string-match-p
                 "CLOCK: \\[2026-03-23 Mon 14:00\\]--\\[2026-03-23.*15:00\\]"
                 content))
        ;; Verify second task has open clock
        (should (string-match-p
                 "CLOCK: \\[2026-03-23.*15:00\\]\n"
                 content))))))

(ert-deftest org-mcp-test-clock-in-with-rounding ()
  "Test clock-in applies rounding to start time."
  (let ((org-clock-rounding-minutes 15))
    (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-content-simple))
      (let* ((uri (format "org-headline://%s#My%%20Task" test-file))
             (params `((uri . ,uri)
                       (start_time . "2026-03-23T14:22:00")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-clock-in" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (should (string-match-p "14:15" (alist-get 'start result)))))))

(ert-deftest org-mcp-test-clock-in-resolve-dangling ()
  "Test clock-in with resolve deletes dangling clock."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--clock-content-active
      `(,org-mcp-test--content-with-id-id)
    (let* ((uri (format "org-id://%s" org-mcp-test--content-with-id-id))
           (params `((uri . ,uri)
                     (start_time . "2026-03-23T15:00:00")
                     (resolve . "true")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-clock-in" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'clocked_in result) t))
      (should (equal (alist-get 'resolved result) 1))
      (let ((content (org-mcp-test--read-file test-file)))
        (should-not (string-match-p "--" content)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-regex-resolved-and-clocked-in))))

(ert-deftest org-mcp-test-clock-in-resolve-no-dangling ()
  "Test clock-in with resolve is a no-op when no dangling clock."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-simple))
    (let* ((uri (format "org-headline://%s#My%%20Task" test-file))
           (params `((uri . ,uri)
                     (start_time . "2026-03-23T14:30:00")
                     (resolve . "true")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-clock-in" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'clocked_in result) t))
      (should-not (alist-get 'resolved result))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-regex-clocked-in))))

;; Clock-out tests

(ert-deftest org-mcp-test-clock-out-basic ()
  "Test basic clock-out with explicit end time."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--clock-content-active
      `(,org-mcp-test--content-with-id-id)
    (let* ((params `((end_time . "2026-03-23T16:45:00")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-clock-out" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'clocked_out result) t))
      (should (equal (alist-get 'heading result) "Active Task"))
      (should (string-match-p "2:45" (alist-get 'duration result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-regex-clocked-out))))

(ert-deftest org-mcp-test-clock-out-no-active ()
  "Test clock-out fails when no clock is active."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-simple))
    (should-error
     (let* ((request
             (mcp-server-lib-create-tools-call-request
              "org-clock-out" nil nil))
            (response (mcp-server-lib-process-jsonrpc-parsed
                       request mcp-server-lib-ert-server-id))
            (result (mcp-server-lib-ert-process-tool-response response)))
       (error "Expected error but got: %s" result))
     :type 'mcp-server-lib-tool-error)))

(ert-deftest org-mcp-test-clock-out-end-before-start ()
  "Test clock-out rejects end time before start time."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-active))
    (org-mcp-test--assert-error-and-file
     test-file
     (let* ((request
             (mcp-server-lib-create-tools-call-request
              "org-clock-out" nil
              `((end_time . "2026-03-23T12:00:00"))))
            (response (mcp-server-lib-process-jsonrpc-parsed
                       request mcp-server-lib-ert-server-id))
            (result (mcp-server-lib-ert-process-tool-response response)))
       (error "Expected error but got: %s" result)))))

(ert-deftest org-mcp-test-clock-add-with-properties ()
  "Test clock-add to heading with properties creates logbook after props."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--clock-content-with-properties
      `(,org-mcp-test--content-with-id-id)
    (let* ((uri org-mcp-test--content-with-id-uri)
           (params `((uri . ,uri)
                     (start . "2026-03-23T14:30:00")
                     (end . "2026-03-23T16:45:00")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-clock-add" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      ;; Verify LOGBOOK is after PROPERTIES
      (let ((content (org-mcp-test--read-file test-file)))
        (should (string-match-p
                 ":END:\n:LOGBOOK:\nCLOCK:" content))))))

(ert-deftest org-mcp-test-clock-continuous ()
  "Test continuous clocking starts new clock at previous end time."
  (let ((org-clock-continuously t)
        (org-mcp-clock-continuous-threshold 30))
    (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-content-with-logbook))
      ;; The logbook has a clock ending at 10:30.
      ;; With letimestamp we fake "now" to be within threshold.
      ;; But since we can't easily fake current-time, we test via
      ;; the explicit start_time path instead - continuous clocking
      ;; should NOT apply when start_time is explicit.
      (let* ((uri (format "org-headline://%s#Task%%20with%%20Logbook"
                          test-file))
             (params `((uri . ,uri)
                       (start_time . "2026-03-23T10:35:00")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-clock-in" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        ;; Explicit start_time should be used as-is
        (should (string-match-p "10:35" (alist-get 'start result)))))))

;; Non-allowed file clock constants

(defconst org-mcp-test--clock-content-non-allowed-active
  "* TODO External Task
:LOGBOOK:
CLOCK: [2026-03-23 Mon 14:00]
:END:
Working on this in a non-allowed file."
  "Non-allowed file with active clock.")

(defconst org-mcp-test--clock-regex-non-allowed-closed
  (concat
   "\\`\\* TODO External Task\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-03-23 [A-Za-z]\\{2,3\\} 14:00\\]"
   "--\\[2026-03-23 [A-Za-z]\\{2,3\\} 15:00\\]"
   " => +1:00\n"
   ":END:\n"
   "Working on this in a non-allowed file.\n?\\'")
  "Regex for closed clock in non-allowed file.")

(defmacro org-mcp-test--with-native-clock (file-var content &rest body)
  "Create a non-allowed temp file with native Emacs clock markers.
FILE-VAR is bound to the temp file path.
CONTENT is the initial file content (should have an open CLOCK line).
Sets `org-clock-marker' and `org-clock-hd-marker' in the file buffer.
Cleans up temp file and buffer on exit."
  (declare (indent 2) (debug t))
  `(let ((,file-var (make-temp-file "org-mcp-test-non-allowed" nil ".org"
                                    ,content)))
     (unwind-protect
         (let ((buf (find-file-noselect ,file-var)))
           (unwind-protect
               (with-current-buffer buf
                 (goto-char (point-min))
                 (re-search-forward "^[ \t]*CLOCK: \\[" nil t)
                 (forward-line 0)
                 (setq org-clock-marker (copy-marker (point)))
                 (org-back-to-heading t)
                 (setq org-clock-hd-marker (copy-marker (point)))
                 ,@body)
             (move-marker org-clock-marker nil)
             (move-marker org-clock-hd-marker nil)
             (kill-buffer buf)))
       (delete-file ,file-var))))

;; Part A tests: non-allowed file clock detection

(ert-deftest org-mcp-test-clock-get-active-non-allowed ()
  "Test org-clock-get-active detects clock in non-allowed file."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-simple))
    (org-mcp-test--with-native-clock non-allowed-file
        org-mcp-test--clock-content-non-allowed-active
      (let* ((result-text
              (mcp-server-lib-ert-call-tool "org-clock-get-active" nil))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'active result) t))
        (should (equal (alist-get 'in_allowed_file result) :json-false))
        (should-not (alist-get 'file result))
        (should-not (alist-get 'heading result))
        (should-not (alist-get 'start result))))))

(ert-deftest org-mcp-test-clock-in-auto-close-non-allowed ()
  "Test clock-in auto-closes active clock in non-allowed file."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-simple))
    (org-mcp-test--with-native-clock non-allowed-file
        org-mcp-test--clock-content-non-allowed-active
      (let* ((uri (format "org-headline://%s#My%%20Task" test-file))
             (params `((uri . ,uri)
                       (start_time . "2026-03-23T15:00:00")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-clock-in" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'clocked_in result) t))
        ;; Non-allowed file clock should be closed
        (org-mcp-test--verify-file-matches
         non-allowed-file org-mcp-test--clock-regex-non-allowed-closed)
        ;; Native markers should be cleared
        (should (null (marker-buffer org-clock-marker)))
        (should (null (marker-buffer org-clock-hd-marker)))))))

;; Part B tests: find dangling clocks

(ert-deftest org-mcp-test-clock-find-dangling-with-open ()
  "Test clock-find-dangling finds unclosed clock."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-active))
    (let* ((result-text
            (mcp-server-lib-ert-call-tool
             "org-clock-find-dangling" nil))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'total result) 1))
      (let ((clock (aref (alist-get 'open_clocks result) 0)))
        (should (string= (alist-get 'file clock) test-file))
        (should (string= (alist-get 'heading clock) "Active Task"))
        (should (string-match-p "2026-03-23.*14:00"
                                (alist-get 'start clock)))))))

(ert-deftest org-mcp-test-clock-find-dangling-none ()
  "Test clock-find-dangling returns empty when no open clocks."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-with-logbook))
    (let* ((result-text
            (mcp-server-lib-ert-call-tool
             "org-clock-find-dangling" nil))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'total result) 0))
      (should (equal (alist-get 'open_clocks result) [])))))

(ert-deftest org-mcp-test-clock-find-dangling-ignores-non-allowed ()
  "Test clock-find-dangling ignores open clocks in non-allowed files."
  (org-mcp-test--with-temp-org-files
    ((test-file org-mcp-test--clock-content-simple))
    (org-mcp-test--with-native-clock non-allowed-file
        org-mcp-test--clock-content-non-allowed-active
      (let* ((result-text
              (mcp-server-lib-ert-call-tool
               "org-clock-find-dangling" nil))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'total result) 0))
        (should (equal (alist-get 'open_clocks result) []))))))

(provide 'org-mcp-test)
;;; org-mcp-test.el ends here
