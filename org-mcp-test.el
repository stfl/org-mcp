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

(defconst org-mcp-test--content-with-id-link
  (concat "id:" org-mcp-test--content-with-id-id)
  "Link to the heading carrying org-mcp-test--content-with-id-id.")

(defconst org-mcp-test--content-with-id-resource-uri
  (format "org://%s" org-mcp-test--content-with-id-id)
  "The ID behind the org:// scheme, which tools refuse as no link.")

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
  "ID value for Other Child in the add-todo sibling tests.")

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

(defconst org-mcp-test--hierarchy-second-section-id
  "hierarchy-second-section-id"
  "ID of Second Section in org-mcp-test--content-hierarchy-before.")

(defconst org-mcp-test--content-hierarchy-before
  (format
   "* First Section
** Target
Some content.
* Second Section
:PROPERTIES:
:ID:       %s
:END:
** Other Item
More content.
** Target
This Target is under Second Section, not First Section."
   org-mcp-test--hierarchy-second-section-id)
  "Content with duplicate 'Target' headlines under different parents.
The second parent carries an ID, so a link can search its subtree.")

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
          "\\*\\*\\* TODO Review org-mcp-test\\.el +.*:internet:.*\n\\'")
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
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching complete buffer after updating timestamp ID task to DONE.")

(defconst org-mcp-test--expected-task-one-in-progress-regex
  (concat
   "\\`\\* IN-PROGRESS Task One"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching complete buffer with Task One in IN-PROGRESS state.")

(defconst org-mcp-test--expected-task-with-id-in-progress-regex
  (concat
   "\\`\\* IN-PROGRESS Task with ID"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching complete buffer with Task with ID in IN-PROGRESS state.")

(defconst org-mcp-test--expected-regex-top-level-with-header
  (concat
   "\\`#\\+TITLE: My Org Document\n"
   "\n"
   "\\* TODO New Top Task +.*:urgent:\n"
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
    "\\*\\* TODO Child Task +.*:work:.*\n")
   org-mcp-test--content-with-id-id)
  "Pattern for child TODO (level 2) added under parent (level 1) with existing child (level 2).")

(defconst org-mcp-test--regex-child-into-childless-parent
  (concat
   "\\`\\* Parent Task\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--childless-parent-id "\n"
   ":END:\n"
   "Some parent content\\.\n"
   "\\*\\* TODO Only Child +.*:work:.*\n\\'")
  "Pattern for first child (level 2) added under a previously-childless parent (level 1).")

(defconst org-mcp-test--regex-second-child-same-level
  (concat
   "\\`\\* Top Level\n"
   "\\*\\* Review the package\n"
   "\\*\\*\\* Review org-mcp\\.el\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?"  ; Review org-mcp.el has ID
   "Main package file\n"
   "\\*\\*\\* TODO Second Child +.*:work:.*\n\\'")
  "Pattern for second child (level 3) added at same level as first child (level 3) under parent (level 2).")

(defconst org-mcp-test--regex-todo-with-body
  (concat
   "^\\* TODO Task with Body +:[^\n]*\n"
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
   "\\*\\* Second Child\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-with-id-id "\n"
   ":END:\n"
   "Second child content\\.\n"
   "\\*\\* Third Child #3\n?\\'")
  "Pattern for TODO added after specific sibling.")

(defconst org-mcp-test--regex-todo-without-tags
  (concat
   "\\`\\* TODO Task Without Tags *\n\\'") ; No tags, optional spaces
  "Pattern for TODO item without any tags.")

(defconst org-mcp-test--pattern-add-todo-parent-id-link
  (concat
   "^\\* Parent Task\n"
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
   "\\*\\* TODO Child via ID +:work:\n\\'")
  "Pattern for TODO added via the parent's `id:' link.")

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
   " *:END:\n\\'")
  "Pattern for a TODO created with a client-set CUSTOM_ID.
The heading carries that CUSTOM_ID and no ID.")

(defconst org-mcp-test--pattern-add-todo-with-properties
  (concat
   "\\`\\* TODO Task with Properties +:work:\n"
   " *:PROPERTIES:\n"
   " *:EFFORT: +1:00\n"
   " *:OWNER: +alice\n"
   " *:ESTIMATE: +3\n"
   " *:END:\n"
   (regexp-quote org-mcp-test--body-text-multiline)
   "\n\\'")
  "Pattern for a TODO created with tags, a body and properties.
The drawer sits between the heading and the body, a number is written
as its text, and a property sent as null is not written.")

(defconst org-mcp-test--pattern-add-todo-with-boolean-properties
  (concat
   "\\`\\* TODO Flagged Task\n"
   " *:PROPERTIES:\n"
   " *:ENABLED: +t\n"
   " *:DISABLED: +nil\n"
   " *:LITERAL_T: +t\n"
   " *:LITERAL_NIL: +nil\n"
   " *:END:\n\\'")
  "Pattern for a TODO created with boolean and t/nil string properties.
JSON true is written as t and false as nil, the strings \"t\" and
\"nil\" as given, and a property sent as null is not written.")

(defconst org-mcp-test--pattern-renamed-simple-todo
  (concat
   "\\`\\* TODO Updated Task\n"
   "First line of body\\.\n"
   "Second line of body\\.\n"
   "Third line of body\\.\n?\\'")
  "Pattern for renamed simple TODO, which gains no ID.")

(defconst org-mcp-test--pattern-renamed-todo-with-tags
  (concat
   "\\`\\* TODO Renamed Task[ \t]+:work:urgent:\n"
   "Task description\\.\n?\\'")
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
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    "\\(?: *:PROPERTIES:\n *:ID: +%s\n *:END:\n\\)?"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\n?"
    "\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for renamed headline without TODO state.")

(defconst org-mcp-test--pattern-renamed-headline-without-id
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
    "\\*\\* Second Child\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "Second child content\\.\n"
    "\\*\\* Renamed Child\n?\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for a headline without an ID renamed, which gains none.")

(defconst org-mcp-test--pattern-renamed-slash-headline
  (concat
   "\\`\\* Parent\n"
   "\\*\\* Real Child\n"
   "Content here\\.\n"
   "\\* Parent/Child Renamed\n"
   "This is a single headline with a slash, not nested under Parent\\.\n?\\'")
  "Pattern for renamed headline containing slash character.")

(defconst org-mcp-test--regex-slash-not-nested-after
  (concat
   "\\`\\* Parent\n"
   "\\*\\* Real Child\n"
   "Content here\\.\n"
   "\\* Parent-Child Renamed\n"
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
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--hierarchy-second-section-id "\n"
   ":END:\n"
   "\\*\\* Other Item\n"
   "More content\\.\n"
   "\\*\\* Renamed Target\n"
   "This Target is under Second Section, not First Section\\.\n?\\'")
  "Regex for hierarchy test after renaming second Target.")

(defconst org-mcp-test--regex-add-todo-with-mutex-tags
  (concat
   "\\`#\\+TITLE: Test Org File\n"
   "\n"
   "\\* TODO Test Task[ \t]+\\(:[^:\n]+\\)+:\n\\'")
  "Regex for add-todo test accepting any tag order.")

(defconst org-mcp-test--regex-todo-keywords-after
  (concat
   "\\`\\* Project Management\n"
   "\\*\\* TODO Q1 Planning Review\n"
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
    ":PROPERTIES:\n"
    ":ID: +nested-siblings-parent-id-002\n"
    ":END:\n"
    "Updated parent content\n"
    "\\*\\* First Child 50%% Complete\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\n?"
    "\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for nested headlines edit-body test result.
No heading gains an ID, the first child included.")

(defconst org-mcp-test--pattern-edit-body-empty
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
    "\\*\\* Second Child\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\n"
    "New content added\\.\n\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for edit-body test with empty body adding content.
The fixture's last heading ends the file without a newline; the
content goes on a line of its own below it.")

(defconst org-mcp-test--pattern-edit-body-empty-with-props
  (format (concat
           "\\`\\* TODO Task with ID but no body\n"
           ":PROPERTIES:\n"
           ":ID: +%s\n"
           ":END:\n"
           "Content added after properties\\.\n\\'")
          org-mcp-test--timestamp-id)
  "Pattern for edit-body with existing properties adding content.
The fixture ends in `:END:' without a newline; the content goes on a
line of its own below it.")

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
   "\\*\\* Third Child #3")
  "Pattern for edit-body accepting lower-level headlines.")

(defconst org-mcp-test--pattern-tool-read-headline-single
  (concat
   "\\`\\* Parent/Child\n"
   "This is a single headline with a slash, not nested under Parent\\.\n"
   "?\\'")
  "Pattern for org-node-text tool single-level path result.")

(defconst org-mcp-test--pattern-tool-read-headline-nested
  (concat
   "\\`\\*\\* First Child 50% Complete\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "?\\'")
  "Pattern for org-node-text tool nested path result.")

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
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\'")
  "Regex matching the complete file after org-clock-add adds a closed CLOCK entry.")

(defconst org-mcp-test--clock-in-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\n"
   ":END:\n"
   "\\'")
  "Regex matching the complete file after org-clock-in inserts an open CLOCK entry.")

(defconst org-mcp-test--clock-out-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
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
   "Body\n"
   "\\'")
  "Regex matching the complete scope-test file after Task becomes DONE.")

(defconst org-mcp-test--scope-task-with-id-content
  (format "* TODO Task\n:PROPERTIES:\n:ID:       %s\n:END:\nBody\n"
          org-mcp-test--content-with-id-id)
  "Scope-test file whose heading carries an ID.")

(defconst org-mcp-test--scope-id-link
  (format "id:%s" org-mcp-test--content-with-id-id)
  "Link to Task in `org-mcp-test--scope-task-with-id-content'.")

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

(defun org-mcp-test--file-link (file search)
  "Return the `file:' link to FILE with SEARCH after `::'.
The file name is written the way `org-store-link' writes it.  SEARCH
is `*Title', `#custom-id' or a line of text."
  (concat "file:" (abbreviate-file-name file) "::" search))

(defun org-mcp-test--should-resolve-to (link title)
  "Assert that a later tool call reaches the heading TITLE through LINK.
LINK must be an `id:' or `file:' link.  org-node-read must read a heading
titled TITLE there and return LINK itself as that heading's link."
  (should (string-match-p "\\`\\(?:id\\|file\\):" link))
  (let ((heading (json-read-from-string (org-mcp-test--call-read link))))
    (should (equal (alist-get 'title heading) title))
    (should (equal (alist-get 'link heading) link))))

(defmacro org-mcp-test--assert-error-and-file (test-file error-form)
  "Assert that ERROR-FORM throws an error and TEST-FILE remains unchanged."
  (declare (indent 1) (debug t))
  `(let ((original-content (org-mcp-test--read-file ,test-file)))
     (should-error ,error-form :type 'mcp-server-lib-tool-error)
     (should (string= (org-mcp-test--read-file ,test-file) original-content))))

(defvar org-mcp-test--in-request nil
  "Non-nil while the MCP server handles a request.")

(defvar org-mcp-test--created-ids nil
  "Buffers in which Org's ID generator ran during an MCP request.")

(defun org-mcp-test--mark-request (orig &rest args)
  "Call ORIG with ARGS with `org-mcp-test--in-request' bound to t."
  (let ((org-mcp-test--in-request t))
    (apply orig args)))

(defun org-mcp-test--record-created-id (&rest _)
  "Record that Org's ID generator ran while org-mcp handled a request."
  (when org-mcp-test--in-request
    (push (buffer-name) org-mcp-test--created-ids)))

(defmacro org-mcp-test--with-enabled (&rest body)
  "Run BODY with org-mcp enabled, ensuring cleanup.
Fails the test when Org's ID generator runs while org-mcp handles
any request in BODY, since org-mcp creates no identifiers.  Every
path that makes an ID, `org-id-get-create' and `org-id-get' with
CREATE included, goes through `org-id-new', which is watched.  Test
setup outside requests may still create IDs."
  (declare (indent defun) (debug t))
  `(let ((org-mcp-test--created-ids nil)
         ;; An enclosing use of this macro already watches, and removes
         ;; the advice when it is done.
         (watching
          (advice-member-p #'org-mcp-test--record-created-id 'org-id-new)))
     (unless watching
       (advice-add 'mcp-server-lib-process-jsonrpc
                   :around #'org-mcp-test--mark-request)
       (advice-add 'org-id-new :before #'org-mcp-test--record-created-id))
     (org-mcp-enable)
     (unwind-protect
         ;; `org-mcp-enable' registers the server record itself, so the
         ;; helper leaves it alone and `initialize' reports org-mcp's own
         ;; serverInfo rather than the helper's test values.  The version
         ;; is spelled out rather than taken from `org-mcp-version', so
         ;; that this pins what a client receives instead of re-deriving
         ;; it from the same header the server reads.  A release bumps it
         ;; here too.
         (prog1 (mcp-server-lib-ert-with-server
                  :tools t
                  :resources t
                  :version "0.9.0"
                  ,@body)
           (should-not org-mcp-test--created-ids))
       (org-mcp-disable)
       (unless watching
         (advice-remove 'org-id-new #'org-mcp-test--record-created-id)
         (advice-remove 'mcp-server-lib-process-jsonrpc
                        #'org-mcp-test--mark-request)))))

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
then registers each ID location.  `org-id-locations' and
`org-id-files' are bound, so no registration outlives BODY."
  (declare (indent 2) (debug t))
  `(let ((org-id-track-globally t)
         (org-id-locations-file nil) ; Prevent saving to disk
         (org-id-locations nil)
         (org-id-files nil)
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

;; Helper functions for testing org-config-todo MCP tool

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
                     (mcp-server-lib-ert-call-tool "org-config-todo" nil))))
        (should (= (length result) 2))
        (let ((sequences (cdr (assoc 'sequences result)))
              (semantics (cdr (assoc 'semantics result))))
          ,@body)))))

;; Helper functions for testing org-config-tags MCP tool

(defmacro org-mcp-test--get-tag-config-and-check
    (expected-alist expected-persistent expected-inheritance expected-exclude)
  "Call org-config-tags tool and check result against expected values.
EXPECTED-ALIST is the expected value for org-tag-alist (string).
EXPECTED-PERSISTENT is the expected value for org-tag-persistent-alist (string).
EXPECTED-INHERITANCE is the expected value for org-use-tag-inheritance (string).
EXPECTED-EXCLUDE is the expected value for
org-tags-exclude-from-inheritance (string)."
  (declare (indent defun) (debug t))
  `(org-mcp-test--with-enabled
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool "org-config-tags" nil))))
      (should (= (length result) 4))
      (should (equal (alist-get 'org-tag-alist result) ,expected-alist))
      (should (equal (alist-get 'org-tag-persistent-alist result)
                     ,expected-persistent))
      (should (equal (alist-get 'org-use-tag-inheritance result)
                     ,expected-inheritance))
      (should (equal (alist-get 'org-tags-exclude-from-inheritance result)
                     ,expected-exclude)))))

;; Helper functions for testing org-config-allowed-files MCP tool

(defun org-mcp-test--call-get-allowed-files ()
  "Call org-config-allowed-files tool and return the parsed result."
  (json-read-from-string
   (mcp-server-lib-ert-call-tool "org-config-allowed-files" nil)))

(defun org-mcp-test--get-allowed-files-and-check (allowed-files expected-files)
  "Call org-config-allowed-files tool and verify the result.
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

;; Helper functions for testing org-node-create MCP tool

(defun org-mcp-test--call-add-todo-expecting-error
    (test-file title todo tags content parent &optional previous-sibling
               properties)
  "Call org-node-create MCP tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
TITLE is the headline text.
TODO is the TODO state.
TAGS is a list of tag strings or nil.
CONTENT is the body text or nil.
PARENT is the link to the parent item.
PREVIOUS-SIBLING is the optional link to the sibling to insert after.
PROPERTIES is an optional alist sent as the properties parameter."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((params
           `((title . ,title)
             (todo . ,todo)
             (tags . ,tags)
             (content . ,content)
             (parent . ,parent)
             (previous_sibling . ,previous-sibling)
             ,@(when properties `((properties . ,properties)))))
          (request
            (mcp-server-lib-create-tools-call-request
             "org-node-create" nil params))
          (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
          (result (mcp-server-lib-ert-process-tool-response response)))
     ;; If we get here, the tool succeeded when we expected failure
     (error "Expected error but got success: %s" result))))

(defun org-mcp-test--add-todo-and-check
    (title todo tags content parent previous-sibling
           basename test-file expected-pattern &optional properties
           expected-link)
  "Add TODO item, verify the result and return the parsed response.
TITLE is the headline text.
TODO is the TODO state.
TAGS is a list of tag strings or nil.
CONTENT is the body text or nil.
PARENT is the link to the parent item.
PREVIOUS-SIBLING is the optional link to the sibling to insert after.
BASENAME is the expected file basename.
TEST-FILE is the path to the file to check.
EXPECTED-PATTERN is a regexp that the file content should match.
PROPERTIES is an optional alist sent as the properties parameter.
EXPECTED-LINK is the link the response must carry; it defaults to the
title link, since the new heading has no identifier."
  (let* ((params
          `((title . ,title)
            (todo . ,todo)
            (tags . ,tags)
            (content . ,content)
            (parent . ,parent)
            (previous_sibling . ,previous-sibling)
            ,@(when properties `((properties . ,properties)))))
         (result-text (mcp-server-lib-ert-call-tool "org-node-create" params))
         (result (json-read-from-string result-text)))
    ;; Check result structure
    (should (= (length result) 5))
    (should (equal (alist-get 'success result) t))
    (should (eq (alist-get 'saved result) t))
    (should
     (equal (alist-get 'link result)
            (or expected-link
                (org-mcp-test--file-link test-file (concat "*" title)))))
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
    (test-file link properties)
  "Call org-node-set-properties expecting an error, verify nothing changed.
TEST-FILE is the file that must stay unchanged on disk and in any
buffer visiting it.  LINK is the link to the headline.  PROPERTIES is the
alist sent as the properties parameter."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((request
           (mcp-server-lib-create-tools-call-request
            "org-node-set-properties" nil
            `((link . ,link) (after . ,properties))))
          (response
           (mcp-server-lib-process-jsonrpc-parsed
            request mcp-server-lib-ert-server-id))
          (result (mcp-server-lib-ert-process-tool-response response)))
     (error "Expected error but got success: %s" result)))
  (org-mcp-test--verify-no-modified-buffer test-file))

;; Helper functions for testing org-node-set-todo MCP tool

(defun org-mcp-test--call-update-todo-state
    (link new-state &optional current-state note files)
  "Call org-node-set-todo tool via JSON-RPC and return the result.
LINK is the link to the headline, NEW-STATE is the new TODO state to set.
CURRENT-STATE, when provided, is the expected current TODO state.
NOTE, when provided, is a note to attach to the state transition.
FILES, when provided, is sent as the `files' parameter."
  (let* ((params
          `((link . ,link)
            (after . ,new-state)
            ,@(when current-state `((before . ,current-state)))
            ,@(when note `((note . ,note)))
            ,@(when files `((files . ,files)))))
         (result-text
          (mcp-server-lib-ert-call-tool "org-node-set-todo" params)))
    (json-read-from-string result-text)))

(defun org-mcp-test--call-update-todo-state-expecting-error
    (test-file link current-state new-state)
  "Call org-node-set-todo tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
LINK is the link to the headline to update.
CURRENT-STATE is the expected current TODO state (nil to omit).
NEW-STATE is the new TODO state to set."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((request
            (mcp-server-lib-create-tools-call-request
             "org-node-set-todo" 1
             `((link . ,link)
               ,@(when current-state `((before . ,current-state)))
               (after . ,new-state))))
          (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
          (result (mcp-server-lib-ert-process-tool-response response)))
     ;; If we get here, the tool succeeded when we expected failure
     (error "Expected error but got success: %s" result))))

(defun org-mcp-test--update-todo-state-and-check
    (link old-state new-state test-file expected-content-regex
                  &optional expected-link)
  "Update TODO state and verify the result via MCP JSON-RPC.
LINK is the link to the headline to update.
OLD-STATE is the current TODO state to update from.
NEW-STATE is the new TODO state to update to.
TEST-FILE is the file to verify content after update.
EXPECTED-CONTENT-REGEX is an anchored regex that matches the complete buffer.
EXPECTED-LINK is the link the response must carry.  It may be omitted
when LINK is an `id:' link with no search part, which the
response returns as it is."
  (let ((result
         (org-mcp-test--call-update-todo-state
          link new-state old-state)))
    (should (= (length result) 5))
    (should (equal (alist-get 'success result) t))
    (should (eq (alist-get 'saved result) t))
    (should (equal (alist-get 'before result) old-state))
    (should (equal (alist-get 'after result) new-state))
    (should
     (equal (alist-get 'link result)
            (or expected-link
                (and (string-match-p "\\`id:[^:]*\\'" link)
                     link))))
    (org-mcp-test--verify-file-matches test-file expected-content-regex)))

;; Helper functions for testing org-node-set-title MCP tool

(defun org-mcp-test--call-rename-headline-and-check
    (link current-title new-title test-file expected-content-regex)
  "Call org-node-set-title tool via JSON-RPC and verify the result.
LINK is the link to the headline.
CURRENT-TITLE is the expected current title.
NEW-TITLE is the new title to set.
TEST-FILE is the file to verify content after rename.
EXPECTED-CONTENT-REGEX is an anchored regex that matches the complete buffer.
The response must link to the renamed heading: by LINK itself when it
is an `id:' link with no search part, else by its new title."
  (let* ((params
          `((link . ,link)
            (before . ,current-title)
            (after . ,new-title)))
         (result-text
          (mcp-server-lib-ert-call-tool "org-node-set-title" params))
         (result (json-read-from-string result-text))
         (result-link (alist-get 'link result)))
    (should (= (length result) 5))
    (should (equal (alist-get 'success result) t))
    (should (eq (alist-get 'saved result) t))
    (should (equal (alist-get 'before result) current-title))
    (should (equal (alist-get 'after result) new-title))
    (should
     (equal result-link
            (if (string-match-p "\\`id:[^:]*\\'" link)
                link
              (org-mcp-test--file-link test-file (concat "*" new-title)))))
    (org-mcp-test--verify-file-matches test-file expected-content-regex)))

(defun org-mcp-test--call-rename-headline-expecting-error
    (test-file link current-title new-title)
  "Call org-node-set-title tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
LINK is the link to the headline to rename.
CURRENT-TITLE is the current title for validation.
NEW-TITLE is the new title to set."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((params
           `((link . ,link)
             (before . ,current-title)
             (after . ,new-title)))
          (request
            (mcp-server-lib-create-tools-call-request
             "org-node-set-title" 1 params))
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

;; Helper functions for testing org-node-set-content MCP tool

(defun org-mcp-test--call-edit-body-and-check
    (test-file link old-body new-body expected-pattern
               append expected-link)
  "Call org-node-set-content tool and check result structure and file content.
TEST-FILE is the path to the file to check.
LINK is the link to the node to edit.
OLD-BODY is the substring to search for within the node's body.
NEW-BODY is the replacement text.
EXPECTED-PATTERN is a regexp that the file content should match.
APPEND if true, append new-body to end of body.
EXPECTED-LINK is the link to the edited heading the response carries."
  (let* ((params
          `((link . ,link)
            (before . ,old-body)
            (after . ,new-body)
            (append . ,append)))
         (result-text (mcp-server-lib-ert-call-tool "org-node-set-content" params))
         (result (json-read-from-string result-text)))
    (should (= (length result) 3))
    (should (equal (alist-get 'success result) t))
    (should (eq (alist-get 'saved result) t))
    (should (equal (alist-get 'link result) expected-link))
    (org-mcp-test--verify-file-matches test-file expected-pattern)))

(defun org-mcp-test--call-edit-body-expecting-error
    (test-file link old-body new-body)
  "Call org-node-set-content tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
LINK is the link to the node to edit.
OLD-BODY is the substring to search for within the node's body.
NEW-BODY is the replacement text."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((params
           `((link . ,link)
             (before . ,old-body)
             (after . ,new-body)))
          (request
            (mcp-server-lib-create-tools-call-request
             "org-node-set-content" 1 params))
          (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
          (result (mcp-server-lib-ert-process-tool-response response)))
     ;; If we get here, the tool succeeded when we expected failure
     (error "Expected error but got success: %s" result))))

;; Helper functions for testing org-node-read MCP tool

(defun org-mcp-test--call-read (link)
  "Call org-node-read tool via JSON-RPC and return the result.
LINK is the native Org link sent as the `link' parameter."
  (let ((params `((link . ,link))))
    (mcp-server-lib-ert-call-tool "org-node-read" params)))

;; Helper functions for testing org-read-outline MCP tool

(defun org-mcp-test--call-read-outline (file)
  "Call org-read-outline tool via JSON-RPC and return the result.
FILE is the file path to read the outline from."
  (let* ((params `((file . ,file)))
         (result-json
          (mcp-server-lib-ert-call-tool "org-read-outline" params)))
    (json-parse-string result-json :object-type 'alist)))

;; Helper functions for testing org-node-text MCP tool

(defun org-mcp-test--call-read-headline (link &optional files)
  "Call org-node-text tool via JSON-RPC and return the result.
LINK is the native Org link sent as the `link' parameter.
FILES, when provided, is sent as the `files' parameter."
  (let ((params `((link . ,link) ,@(when files `((files . ,files))))))
    (mcp-server-lib-ert-call-tool "org-node-text" params)))

;; Helper functions for testing clock MCP tools

(defun org-mcp-test--call-clock-add (link start end)
  "Call org-clock-add tool via JSON-RPC and return the parsed result.
LINK is the link to the headline, START and END are ISO 8601 timestamps."
  (let* ((params `((link . ,link) (start . ,start) (end . ,end)))
         (result-text
          (mcp-server-lib-ert-call-tool "org-clock-add" params)))
    (json-read-from-string result-text)))

(defun org-mcp-test--call-clock-in
    (link &optional start-time resolve clock-out)
  "Call org-clock-in tool via JSON-RPC and return the parsed result.
LINK is the link to the headline.  START-TIME is an optional ISO 8601 timestamp.
RESOLVE when non-nil is passed as the `resolve' parameter (e.g. \"true\").
CLOCK-OUT when non-nil is passed as the `clock_out' parameter, the link
to the heading of the running clock."
  (let* ((params
          (append
           `((link . ,link))
           (when start-time `((start_time . ,start-time)))
           (when resolve `((resolve . ,resolve)))
           (when clock-out `((clock_out . ,clock-out)))))
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
  "Call org-clock-active tool via JSON-RPC and return the parsed result."
  (let ((result-text
         (mcp-server-lib-ert-call-tool "org-clock-active" nil)))
    (json-read-from-string result-text)))

(defun org-mcp-test--call-clock-delete (link start)
  "Call org-clock-delete tool via JSON-RPC and return the parsed result.
LINK is the link to the headline.  START is the ISO 8601 start timestamp of the
clock entry to delete."
  (let* ((params `((link . ,link) (start . ,start)))
         (result-text
          (mcp-server-lib-ert-call-tool "org-clock-delete" params)))
    (json-read-from-string result-text)))

(defun org-mcp-test--call-clock-find-dangling ()
  "Call org-clock-dangling tool via JSON-RPC and return the parsed result."
  (let ((result-text
         (mcp-server-lib-ert-call-tool
          "org-clock-dangling" nil)))
    (json-read-from-string result-text)))

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


;;; Tests

(ert-deftest org-mcp-test-tool-get-todo-config-empty ()
  "Test org-config-todo with empty `org-todo-keywords'."
  (org-mcp-test--with-get-todo-config-result
   nil
   (should (assoc 'sequences result))
   (should (assoc 'semantics result))
   (should (equal sequences []))
   (should (equal semantics []))))

(ert-deftest org-mcp-test-tool-get-todo-config-default ()
  "Test org-config-todo with default `org-todo-keywords'."
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
  "Test org-config-todo with single keyword."
  (org-mcp-test--with-get-todo-config-result '((sequence "DONE"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "sequence" ["|" "DONE"])
    (should (= (length semantics) 1))
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "DONE" t "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-explicit-bar ()
  "Test org-config-todo with explicit | and multiple states."
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
  "Test org-config-todo with type keywords."
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
  "Test org-config-todo with multiple sequences."
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
  "Test org-config-todo with no done states."
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
  "Test org-config-todo with type keywords and no separator."
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
  "Test org-config-tags with empty `org-tag-alist'."
  (let ((org-tag-alist nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t))
    (org-mcp-test--get-tag-config-and-check "nil" "nil" "t" "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-simple ()
  "Test org-config-tags with simple tags."
  (let ((org-tag-alist '("work" "personal" "urgent"))
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t)
        (org-tags-exclude-from-inheritance nil))
    (org-mcp-test--get-tag-config-and-check
     "(\"work\" \"personal\" \"urgent\")" "nil" "t" "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-with-keys ()
  "Test org-config-tags with fast selection keys."
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
  "Test org-config-tags with tag groups."
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
  "Test org-config-tags with persistent tags."
  (let ((org-tag-alist '(("work" . ?w)))
        (org-tag-persistent-alist '(("important" . ?i) "recurring"))
        (org-tags-exclude-from-inheritance nil))
    (org-mcp-test--get-tag-config-and-check
     "((\"work\" . 119))" "((\"important\" . 105) \"recurring\")"
     "t"
     "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance-enabled ()
  "Test org-config-tags with inheritance enabled."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t))
    (org-mcp-test--get-tag-config-and-check
     "(\"work\" \"personal\")" "nil" "t" "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance-disabled ()
  "Test org-config-tags with inheritance disabled."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance nil))
    (org-mcp-test--get-tag-config-and-check
     "(\"work\" \"personal\")" "nil" "nil" "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance-selective ()
  "Test org-config-tags with selective inheritance (list)."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance '("work")))
    (org-mcp-test--get-tag-config-and-check
     "(\"work\" \"personal\")" "nil" "(\"work\")"
     "nil")))

(defun org-mcp-test--call-get-tag-candidates ()
  "Call org-config-tag-candidates and return the parsed `tags' vector."
  (let* ((result-text
          (mcp-server-lib-ert-call-tool "org-config-tag-candidates" nil))
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
  "Test org-config-allowed-files with empty configuration."
  (org-mcp-test--get-allowed-files-and-check nil nil))

(ert-deftest org-mcp-test-tool-get-allowed-files-single ()
  "Test org-config-allowed-files with single file."
  (org-mcp-test--get-allowed-files-and-check
   '("/home/user/tasks.org")
   '("/home/user/tasks.org")))

(ert-deftest org-mcp-test-tool-get-allowed-files-multiple ()
  "Test org-config-allowed-files with multiple files."
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

(ert-deftest org-mcp-test-file-link-relative-allowed-entry ()
  "A file: link with an absolute path resolves when allowed list uses relative path."
  (org-mcp-test--with-temp-org-files
      ((real-file "* Heading\nBody"))
    (let* ((dir (file-name-directory real-file))
           (name (file-name-nondirectory real-file))
           (org-directory dir)
           (org-mcp-allowed-files (list name))
           (link (org-mcp-test--file-link real-file "*Heading"))
           (result (org-mcp-test--call-read-headline link)))
      (should (string= result "* Heading\nBody")))))

(ert-deftest org-mcp-test-file-link-absolute-allowed-entry ()
  "A file: link with an absolute path resolves when allowed list uses absolute path."
  (org-mcp-test--with-temp-org-files
      ((real-file "* Heading\nBody"))
    ;; with-temp-org-files already binds allowed-files to (list real-file).
    (let* ((link (org-mcp-test--file-link real-file "*Heading"))
           (result (org-mcp-test--call-read-headline link)))
      (should (string= result "* Heading\nBody")))))

(ert-deftest org-mcp-test-file-link-interchangeable-allowed-entries ()
  "Same file is reachable whether the allowed entry is absolute or relative."
  (org-mcp-test--with-temp-org-files
      ((real-file "* Heading\nBody"))
    (let* ((dir (file-name-directory real-file))
           (name (file-name-nondirectory real-file))
           (link (org-mcp-test--file-link real-file "*Heading")))
      ;; Configured as absolute path
      (let ((org-mcp-allowed-files (list real-file)))
        (should (string= (org-mcp-test--call-read-headline link)
                         "* Heading\nBody")))
      ;; Configured as relative path (same physical file)
      (let ((org-directory dir)
            (org-mcp-allowed-files (list name)))
        (should (string= (org-mcp-test--call-read-headline link)
                         "* Heading\nBody"))))))

(ert-deftest org-mcp-test-file-link-relative-allowed-rejects-other-file ()
  "Relative allowed entry does not accidentally allow a sibling file."
  (org-mcp-test--with-temp-org-files
      ((allowed-file "* Allowed\n")
       (forbidden-file "* Forbidden\n"))
    (let* ((dir (file-name-directory allowed-file))
           (allowed-name (file-name-nondirectory allowed-file))
           (org-directory dir)
           ;; Only the relative allowed-name is in the list.
           (org-mcp-allowed-files (list allowed-name))
           (link (org-mcp-test--file-link forbidden-file "*Forbidden")))
      (org-mcp-test--call-tool-refused
       "org-node-text" `((link . ,link))
       (org-mcp-test--refused-path-regexp link)
       forbidden-file))))

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

(ert-deftest org-mcp-test-fallback-link-tool-uses-agenda-files ()
  "Tool calls with a link work when allowed list is nil and `org-agenda-files' is set."
  (org-mcp-test--with-temp-org-files
      ((agenda-file "* Heading\nBody"))
    (let ((org-mcp-allowed-files nil)
          (org-agenda-files (list agenda-file)))
      (let* ((link (org-mcp-test--file-link agenda-file "*Heading"))
             (result (org-mcp-test--call-read-headline link)))
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
      (let* ((link (org-mcp-test--file-link agenda-file "*Heading"))
             (result (org-mcp-test--call-read-headline link)))
        (should (string= result "* Heading\nBody"))))))

(ert-deftest org-mcp-test-fallback-explicit-overrides-agenda ()
  "When `org-mcp-allowed-files' is set, `org-agenda-files' is ignored."
  (org-mcp-test--with-temp-org-files
      ((allowed "* Allowed\n")
       (agenda-only "* AgendaOnly\n"))
    (let ((org-mcp-allowed-files (list allowed))
          (org-agenda-files (list allowed agenda-only)))
      ;; Allowed file is reachable
      (let ((link (org-mcp-test--file-link allowed "*Allowed")))
        (should (string= (org-mcp-test--call-read-headline link)
                         "* Allowed")))
      ;; A file only in `org-agenda-files' must NOT be reachable.
      (let ((link (org-mcp-test--file-link agenda-only "*AgendaOnly")))
        (org-mcp-test--call-tool-refused
         "org-node-text" `((link . ,link))
         (org-mcp-test--refused-path-regexp link)
         agenda-only)))))

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
  (let ((link (org-mcp-test--file-link file "*Task")))
    (org-mcp-test--call-tool-refused
     "org-node-text" `((link . ,link)) "not in allowed list")
    (org-mcp-test--call-tool-refused
     "org-node-set-todo"
     `((link . ,link) (before . "TODO") (after . "DONE"))
     "not in allowed list"
     file)))

(defun org-mcp-test--assert-scope-permitted (file)
  "Assert that the Task heading in FILE can be read and then written.
FILE holds `org-mcp-test--scope-task-content'; afterwards its Task
is DONE."
  (let ((link (org-mcp-test--file-link file "*Task")))
    (should
     (string= (org-mcp-test--call-read-headline link) "* TODO Task\nBody"))
    (let ((result
           (org-mcp-test--call-update-todo-state link "DONE" "TODO")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'after result) "DONE")))
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
       "org-query" `((query . "(todo)") (files . ,(vector out)))
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
               "org-query"
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
       "org-query" `((query . "(todo)") (files . ,(vector out)))
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
       (string= (org-mcp-test--call-read-headline (concat "file:" archive))
                org-mcp-test--scope-task-content))
      (org-mcp-test--assert-scope-refused txt)
      (org-mcp-test--assert-scope-refused gpg)
      (org-mcp-test--call-tool-refused
       "org-node-text" `((link . ,(concat "file:" dir)))
       "not in allowed list"))))

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
through a read and a write of a `file:' link, with and without a
heading search, and a query.  The path on its own, and with an
outline path appended, is no link and refused as such."
  (dolist (override '(nil t))
    (org-mcp-test--with-scope-dirs override
      (org-mcp-test--with-remote-probe ops
        (dolist (remote (org-mcp-test--remote-spellings))
          (let ((file-link (concat "file:" remote))
                (heading-link (concat "file:" remote "::*Task")))
            (dolist (old-form (list remote (concat remote "#Task")))
              (org-mcp-test--call-tool-refused
               "org-node-text" `((link . ,old-form))
               "\\`Not an Org link: "))
            (org-mcp-test--call-tool-refused
             "org-node-text" `((link . ,file-link))
             "names no local file by its full path")
            (org-mcp-test--call-tool-refused
             "org-node-text" `((link . ,heading-link))
             "names no local file by its full path")
            (org-mcp-test--call-tool-refused
             "org-node-set-todo"
             `((link . ,heading-link) (after . "DONE"))
             "names no local file by its full path")
            (org-mcp-test--call-tool-refused
             "org-read-outline" `((file . ,remote)) "not in allowed list")
            (org-mcp-test--assert-files-refused
             (vector remote) "not in allowed list")))
        ;; A remote directory is never walked.
        (org-mcp-test--assert-files-refused
         (vector (concat org-mcp-test--remote-prefix "/dir/"))
         "not in allowed list")
        ;; A relative name, which would inherit a remote
        ;; `default-directory', is refused as no full path.
        (let ((default-directory
               (concat org-mcp-test--remote-prefix "/dir/")))
          (org-mcp-test--assert-files-refused
           ["x.org"] "\\`files entry names no file by its full path: x\\.org")
          (org-mcp-test--assert-files-refused
           ["."] "\\`files entry names no file by its full path: \\."))
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
           "org-node-text" `((link . ,(concat "file:" link)))
           "not in allowed list")
          (org-mcp-test--call-tool-refused
           "org-node-set-todo"
           `((link . ,(concat "file:" link "::*Task")) (after . "DONE"))
           "not in allowed list")
          (org-mcp-test--call-tool-refused
           "org-query" `((query . "(todo)") (files . ,(vector link)))
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
                          . "names no local file by its full path")
                         (,via . "not in allowed list")))
          (org-mcp-test--call-tool-refused
           "org-node-text" `((link . ,(concat "file:" path)))
           message target)
          (org-mcp-test--call-tool-refused
           "org-node-set-todo"
           `((link . ,(concat "file:" path "::*Task"))
             (before . "TODO")
             (after . "DONE"))
           message target)
          (org-mcp-test--call-tool-refused
           "org-node-create"
           `((title . "New")
             (todo . "TODO")
             (content . nil)
             (parent . ,(concat "file:" path)))
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
           "org-node-text"
           `((link . ,(org-mcp-test--file-link out "*Task")))
           "not in allowed list"))
        (should (null ops))))))

(ert-deftest org-mcp-test-scope-override-bare-id-stays-in-allowed-files ()
  "An `id:' link alone never takes the override; naming its file does."
  (org-mcp-test--with-scope-dirs t
    (let ((out (org-mcp-test--write-file
                outside "out.org"
                org-mcp-test--scope-task-with-id-content)))
      (org-mcp-test--with-id-tracking
          (list allowed)
          `((,org-mcp-test--content-with-id-id . ,out))
        (org-mcp-test--call-tool-refused
         "org-node-text" `((link . ,org-mcp-test--scope-id-link))
         "not in allowed list")
        (should
         (string-prefix-p
          "* TODO Task"
          (org-mcp-test--call-read-headline (org-mcp-test--file-link out "*Task"))))))))

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
       "org-node-text" `((link . ,(concat "file:" inner)))
       "not in allowed list")
      (org-mcp-test--call-tool-refused
       "org-node-text" `((link . ,(concat "file:" root)))
       "not in allowed list"))))

(ert-deftest org-mcp-test-tool-get-allowed-files-reports-override-policy ()
  "org-config-allowed-files reports whether overriding is permitted and where."
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
FILES, when non-nil, is the `files' parameter of org-query,
org-config-tag-candidates and org-clock-dangling.  Every file
searched holds one `org-mcp-test--file-set-template' heading, so the
three tools must agree: the titles the query matches, one per file
searched, the tags beyond the configured ones, and the headings
holding an open clock."
  (let* ((org-tag-alist nil)
         (org-tag-persistent-alist nil)
         (query
          (org-mcp-test--call-with-files
           "org-query" '((query . "(todo)")) files))
         (titles
          (sort (mapcar (lambda (match) (alist-get 'title match))
                        (alist-get 'children query))
                #'string<))
         (tags
          (org-mcp-test--call-with-files
           "org-config-tag-candidates" nil files))
         (clocks
          (org-mcp-test--call-with-files
           "org-clock-dangling" nil files)))
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
The refusal of org-query, org-config-tag-candidates and
org-clock-dangling must match the regexp MESSAGE."
  (pcase-dolist (`(,tool . ,params)
                 '(("org-query" (query . "(todo)"))
                   ("org-config-tag-candidates")
                   ("org-clock-dangling")))
    (org-mcp-test--call-tool-refused
     tool (append params `((files . ,files))) message)))

(defun org-mcp-test--refused-path-regexp (path)
  "Return a regexp matching the whole refusal message naming PATH."
  (concat "\\`'" (regexp-quote path)
          "': the referenced file not in allowed list\\'"))

(defmacro org-mcp-test--with-scope-dirs-and-view (override &rest body)
  "Run BODY as `org-mcp-test--with-scope-dirs' does, OVERRIDE included.
A view matching every TODO heading is configured as well, so that
org-view is registered and reaches whatever the allowed files are."
  (declare (indent 1) (debug t))
  `(let ((org-mcp-views '((todo :name "Todo" :query (todo))))
         (org-mcp-filters nil)
         (org-mcp-query-sort-fn nil))
     (org-mcp-test--with-scope-dirs ,override
       ,@body)))

(defun org-mcp-test--view-scope-titles ()
  "Return the sorted titles the scope-test view matches."
  (sort
   (mapcar
    (lambda (match) (alist-get 'title match))
    (append
     (alist-get
      'children
      (json-read-from-string
       (mcp-server-lib-ert-call-tool "org-view" '((view . "todo")))))
     nil))
   #'string<))

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
              "org-query" '((query . "(todo)"))
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
      ;; the call gave it, and so is one that can be listed but not
      ;; searched, whose files the call could not open.
      (make-directory locked)
      (unwind-protect
          (dolist (mode '(#o000 #o400))
            (set-file-modes locked mode)
            (org-mcp-test--assert-files-refused
             (vector locked)
             (concat
              "\\`Cannot read directory: " (regexp-quote locked) "\\'")))
        (set-file-modes locked #o700)))))

(ert-deftest org-mcp-test-file-set-walk-error-names-directory ()
  "A directory that fails while it is listed fails the call and is named.
The refusal names it as the call reaches it: the entry, here a
symlink, followed by the path below it.  A subdirectory that passed
the readability check and then fails to open, as when it is removed
meanwhile, is such a directory.  So is a named directory that can be
searched but not listed; the refusal names it by the entry alone."
  (org-mcp-test--with-scope-dirs t
    (let ((tree (expand-file-name "tree" outside))
          (link (expand-file-name "link" outside))
          (list-directory (symbol-function 'file-name-all-completions)))
      (org-mcp-test--write-set-file tree "a/b/deep.org" "deep")
      (make-symbolic-link tree link)
      (let ((failing (file-truename (expand-file-name "a/b" tree))))
        (cl-letf (((symbol-function 'file-name-all-completions)
                   (lambda (file directory)
                     (if (string= directory failing)
                         (signal 'file-missing
                                 (list
                                  "Opening directory"
                                  "No such file or directory"
                                  directory))
                       (funcall list-directory file directory)))))
          (org-mcp-test--assert-files-refused
           (vector link)
           (concat
            "\\`Cannot read directory: "
            (regexp-quote (concat link "/a/b"))
            "\\'"))))
      (set-file-modes tree #o300)
      (unwind-protect
          (org-mcp-test--assert-files-refused
           (vector link)
           (concat "\\`Cannot read directory: " (regexp-quote link) "\\'"))
        (set-file-modes tree #o700)))))

(ert-deftest org-mcp-test-file-set-ignores-agenda-restriction ()
  "An agenda restriction never widens what the set-scanning tools reach.
While the agenda is restricted, as by `C-c a <', the function
`org-agenda-files' returns the file of the restriction.  No tool
working on a set of files reaches it, whether the call names files,
names an empty set or names none, and no view does."
  (org-mcp-test--with-scope-dirs-and-view t
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
            (should (equal (org-mcp-test--view-scope-titles) '("alpha"))))
        (put 'org-agenda-files 'org-restrict restriction)))))

(ert-deftest org-mcp-test-file-set-does-not-carry-over ()
  "A named set lasts for its call only, also when the call fails.
Later calls naming no files, a view included, run over the allowed
files, and org-config-allowed-files reports them unchanged."
  (org-mcp-test--with-scope-dirs-and-view t
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
              (should (equal (org-mcp-test--view-scope-titles) '("alpha")))
              (should (equal (org-mcp-test--scan-files) '("alpha"))))))
      (should (equal (org-mcp-test--scan-files (vector beta)) '("beta")))
      (funcall check)
      ;; A call failing while its set is in force.
      (org-mcp-test--call-tool-refused
       "org-query"
       `((query . "(no-such-predicate)") (files . ,(vector beta)))
       "Org-ql query error")
      (funcall check)
      ;; A call failing while its set is built, after taking beta.
      (let ((missing (expand-file-name "missing.org" outside)))
        (org-mcp-test--assert-files-refused
         (vector beta missing) (org-mcp-test--refused-path-regexp missing)))
      (funcall check))))

(ert-deftest org-mcp-test-view-refuses-files-with-the-clock-state ()
  "A view and the clock state tool refuse a `files' parameter.
They declare none, and mcp-server-lib refuses a parameter a tool does
not declare before the tool runs."
  (org-mcp-test--with-scope-dirs-and-view t
    (let* ((alpha (org-mcp-test--write-set-file root "alpha.org" "alpha"))
           (beta (org-mcp-test--write-set-file outside "beta.org" "beta"))
           (org-mcp-allowed-files (list alpha)))
      (dolist (call '(("org-view" (view . "todo")) ("org-clock-active")))
        (org-mcp-test--call-tool-refused
         (car call)
         (append (cdr call) `((files . ,(vector beta))))
         "Unexpected parameter: files"))
      (should (equal (org-mcp-test--view-scope-titles) '("alpha"))))))

(ert-deftest org-mcp-test-query-tools-never-search-current-buffer ()
  "An empty set of files searches nothing, not the current buffer.
Given no files, `org-ql-select' searches the current buffer.  This
covers a named directory holding no Org file, and allowed files of
which none exists, for a view too."
  (org-mcp-test--with-scope-dirs-and-view t
    (let ((buffer (get-buffer-create "org-mcp-test-current")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (insert (format org-mcp-test--file-set-template "here" "here"))
            (should (equal (org-mcp-test--scan-files (vector outside)) nil))
            (let ((org-mcp-allowed-files
                   (list (expand-file-name "missing.org" outside))))
              (should (equal (org-mcp-test--scan-files) nil))
              (should (equal (org-mcp-test--view-scope-titles) nil))))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-file-set-closes-buffers-it-opens ()
  "A call naming files closes the buffers it opened for them.
After each tool taking `files' searches a directory whose files no
buffer visits but one, the buffer list is as before: the buffer the
user had open stays, and no other file of the set is left visited.
An `id:' link looked up in `files' visits no file it searches, when
no file holds the ID and when one does, and the tool leaves only the
buffer of the file holding it, which it reads, as for a heading in an
allowed file.  Org reads the files into a hidden work buffer of its
own, which is no file's buffer."
  (org-mcp-test--with-scope-dirs t
    (let* ((files
            (mapcar
             (lambda (title)
               (org-mcp-test--write-set-file
                outside (concat title ".org") title))
             '("alpha" "beta" "gamma")))
           (searched (org-mcp-test--write-set-file root "a.org" "a"))
           (holder
            (org-mcp-test--write-file
             root "holder.org" org-mcp-test--scope-task-with-id-content))
           (open (find-file-noselect (car files)))
           (before (buffer-list))
           (file-buffers
            (lambda () (seq-filter #'buffer-file-name (buffer-list))))
           (file-buffers-before (funcall file-buffers)))
      (unwind-protect
          (progn
            (should
             (equal (org-mcp-test--scan-files (vector outside))
                    '("alpha" "beta" "gamma")))
            (should (equal (buffer-list) before))
            (should (eq (find-buffer-visiting (car files)) open))
            (org-mcp-test--call-tool-refused
             "org-node-text"
             `((link . "id:no-such-id") (files . ,(vector outside)))
             "\\`Cannot find ID 'no-such-id' in files: ")
            (should (equal (funcall file-buffers) file-buffers-before))
            (should
             (string=
              (org-mcp-test--call-read-headline
               org-mcp-test--scope-id-link (vector root))
              (string-trim-right org-mcp-test--scope-task-with-id-content)))
            (should-not (find-buffer-visiting searched))
            (should
             (equal (cl-set-difference
                     (funcall file-buffers) file-buffers-before)
                    (list (find-buffer-visiting holder)))))
        (dolist (file (cons holder files))
          (when-let* ((buffer (find-buffer-visiting file)))
            (kill-buffer buffer)))))))

(ert-deftest org-mcp-test-file-set-validates-parameter ()
  "`files' is an array of absolute paths, or a single path.
A blank value, which some clients send for every optional parameter
they do not use, means no files: with null, false, \"\" or [] each
tool runs over the allowed files."
  (org-mcp-test--with-scope-dirs t
    (let* ((alpha (org-mcp-test--write-set-file root "alpha.org" "alpha"))
           (beta (org-mcp-test--write-set-file outside "beta.org" "beta"))
           (org-mcp-allowed-files (list alpha)))
      (should (equal (org-mcp-test--scan-files beta) '("beta")))
      (dolist (blank '("" [] :json-false))
        (should (equal (org-mcp-test--scan-files blank) '("alpha"))))
      ;; JSON null, sent as a present parameter.
      (pcase-dolist (`(,tool . ,params)
                     '(("org-query" (query . "(todo)"))
                       ("org-config-tag-candidates")
                       ("org-clock-dangling")))
        (should
         (equal
          (org-mcp-test--call-with-files tool params nil)
          (org-mcp-test--call-with-files
           tool (append params '((files))) nil))))
      (should
       (equal
        (alist-get
         'files_searched
         (org-mcp-test--call-with-files
          "org-query" '((query . "(todo)") (files)) nil))
        1))
      (org-mcp-test--assert-files-refused [1] "non-empty array of paths")
      ;; A relative entry is refused, even where it would name a file
      ;; the setting permits, and so is a set holding one.
      (let ((default-directory outside))
        (dolist (relative '("beta.org" "." "./beta.org" "sub/../beta.org"))
          (dolist (files (list (vector relative) (vector alpha relative)
                               relative))
            (org-mcp-test--assert-files-refused
             files
             (concat "\\`files entry names no file by its full path: "
                     (regexp-quote relative)
                     "\\.  Send a full path, such as /home/user/notes\\.org\\'")))
          (org-mcp-test--call-tool-refused
           "org-node-text"
           `((link . ,org-mcp-test--scope-id-link) (files . ,(vector relative)))
           "\\`files entry names no file by its full path: "))
        ;; `~/' is a full path.
        (should
         (equal (org-mcp-test--scan-files
                 (vector (concat "~/" (file-relative-name beta "~"))))
                '("beta")))))))

(defmacro org-mcp-test--with-add-todo-setup
    (file-var initial-content &rest body)
  "Helper for org-node-create test.
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
PARENT-HEADLINE is the parent's title (empty string for top-level).
BODY-WITH-HEADLINE is the body containing invalid headline."
  (org-mcp-test--with-add-todo-setup test-file initial-content
    (let ((parent-link
           (if (string-empty-p parent-headline)
               (concat "file:" test-file)
             (org-mcp-test--file-link
              test-file (concat "*" parent-headline)))))
      (org-mcp-test--call-add-todo-expecting-error
       test-file "Test Task" "TODO" '("work") body-with-headline parent-link))))

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
    (let ((parent-link (concat "file:" test-file)))
      (org-mcp-test--call-add-todo-expecting-error
       test-file invalid-title "TODO" nil nil parent-link))))

(defun org-mcp-test--assert-rename-headline-rejected
    (initial-content headline-title new-title)
  "Assert renaming headline to NEW-TITLE is rejected.
INITIAL-CONTENT is the Org content to test with.
HEADLINE-TITLE is the current headline to rename.
NEW-TITLE is the invalid new title that should be rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file initial-content))
    (let ((link
           (org-mcp-test--file-link test-file (concat "*" headline-title))))
      (org-mcp-test--call-rename-headline-expecting-error
       test-file link headline-title new-title))))

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

(ert-deftest org-mcp-test-tools-not-in-list-after-disable ()
  "Test that tools are unregistered after `org-mcp-disable'."
  (let ((org-mcp-allowed-files '("test.org")))
    ;; Enable then disable
    (org-mcp-enable)
    (org-mcp-disable)
    ;; Start server and check tools
    (mcp-server-lib-ert-with-server
     :tools nil
     :resources nil
     (let ((tools
            (alist-get
             'tools
             (mcp-server-lib-ert-get-success-result
              "tools/list"
              (mcp-server-lib-create-tools-list-request)))))
       ;; Check that the tool list is empty
       (should (= (length tools) 0))))))

(defconst org-mcp-test--unconditional-tool-ids
  '("org-clock-active"
    "org-clock-add"
    "org-clock-dangling"
    "org-clock-delete"
    "org-clock-in"
    "org-clock-out"
    "org-config-allowed-files"
    "org-config-clock"
    "org-config-priority"
    "org-config-tag-candidates"
    "org-config-tags"
    "org-config-todo"
    "org-node-add-note"
    "org-node-create"
    "org-node-read"
    "org-node-set-content"
    "org-node-set-deadline"
    "org-node-set-priority"
    "org-node-set-properties"
    "org-node-set-scheduled"
    "org-node-set-tags"
    "org-node-set-title"
    "org-node-set-todo"
    "org-node-text"
    "org-query"
    "org-read-outline")
  "Every tool id org-mcp registers regardless of configuration, sorted.")

(defun org-mcp-test--registered-tools ()
  "Return the tools of the tools/list response."
  (append
   (alist-get
    'tools
    (mcp-server-lib-ert-get-success-result
     "tools/list" (mcp-server-lib-create-tools-list-request)))
   nil))

(defun org-mcp-test--registered-tool-description (id)
  "Return the description tools/list gives for the tool ID."
  (alist-get
   'description
   (cl-find
    id (org-mcp-test--registered-tools)
    :key (lambda (tool) (alist-get 'name tool))
    :test #'string=)))

(defun org-mcp-test--registered-tool-ids ()
  "Return the ids in the tools/list response, sorted.
Sorted because `mcp-server-lib' leaves the response order
unspecified, so pinning it here would pin something org-mcp does not
control."
  (sort
   (mapcar
    (lambda (tool) (alist-get 'name tool))
    (org-mcp-test--registered-tools))
   #'string<))

(ert-deftest org-mcp-test-registered-tool-ids-without-views ()
  "The registered tools are exactly the unconditional ones."
  (let ((org-mcp-views nil))
    (org-mcp-test--with-enabled
      (should
       (equal
        (org-mcp-test--registered-tool-ids)
        org-mcp-test--unconditional-tool-ids)))))

(ert-deftest org-mcp-test-file-resource-read ()
  "Test that reading org:// resource returns structured JSON."
  (let ((test-content "* Test Heading\nThis is test content."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let* ((uri (format "org://file:%s" test-file))
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
            'children (org-mcp-test--call-read-outline test-file))))
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

(ert-deftest org-mcp-test-tool-read-outline-ceiling ()
  "An outline past the node ceiling is refused, as a read of it is.
The outline is the file read one generation deep, so the same
setting bounds it and the refusal names the heading it stopped at."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-outline-depth))
    (let ((org-mcp-read-max-nodes 5))
      (should (org-mcp-test--call-read-outline test-file)))
    (let ((org-mcp-read-max-nodes 4))
      (org-mcp-test--call-tool-refused
       "org-read-outline" `((file . ,test-file))
       (concat
        "\\`Too many nodes: more than 4\\.  The walk stops at "
        (regexp-quote
         (org-mcp-test--file-link test-file "*Second Section")))))))

(ert-deftest org-mcp-test-file-not-in-allowed-list-returns-error ()
  "Test that reading a file not in allowed list returns an error."
  (org-mcp-test--with-temp-org-files
      ((allowed-file "Allowed content")
       (forbidden-file "Forbidden content"))
    (let ((org-mcp-allowed-files (list allowed-file)))
      ;; Try to read the forbidden file
      (let ((uri (format "org://file:%s" forbidden-file)))
        (org-mcp-test--read-resource-expecting-error
         uri
         (format "'file:%s': the referenced file not in allowed list"
                 forbidden-file))))))

(ert-deftest org-mcp-test-read-headline-not-found ()
  "Test org-node-text tool error for non-existent headline.
The error names the link and says Org found no match."
  (let ((test-content "* Existing Section\nSome content."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((link (org-mcp-test--file-link test-file "*Nonexistent")))
        (org-mcp-test--call-tool-refused
         "org-node-text" `((link . ,link))
         (concat "\\`Cannot resolve link " (regexp-quote link) ": No match")
         test-file)))))

(ert-deftest org-mcp-test-read-headline-file-with-hash ()
  "Test org-node-text tool with # in filename.
A `file:' link takes the file name as it is, with no encoding."
  (org-mcp-test--with-temp-org-files
      ((file org-mcp-test--content-nested-siblings "org-mcp-test-file#"))
    (let* ((link
            (org-mcp-test--file-link file "*First Child 50% Complete"))
           (result (org-mcp-test--call-read-headline link)))
      (should
       (string=
        result
        "** First Child 50% Complete\nFirst child content.\nIt spans multiple lines.")))))

(ert-deftest org-mcp-test-read-headline-title-with-hash ()
  "Test org-node-text tool with # in headline title."
  (org-mcp-test--with-temp-org-files
      ((file org-mcp-test--content-nested-siblings))
    (let* ((link (org-mcp-test--file-link file "*Third Child #3"))
           (result (org-mcp-test--call-read-headline link)))
      (should (string= result "** Third Child #3")))))

(ert-deftest
    org-mcp-test-read-headline-file-and-title-with-hash
    ()
  "Test org-node-text tool with # in both filename and headline."
  (org-mcp-test--with-temp-org-files
      ((file org-mcp-test--content-nested-siblings "org-mcp-test-file#"))
    (let* ((link (org-mcp-test--file-link file "*Third Child #3"))
           (result (org-mcp-test--call-read-headline link)))
      (should (string= result "** Third Child #3")))))

(ert-deftest org-mcp-test-read-headline-path-traversal ()
  "Test that path traversal with ../ in a `file:' link is rejected.
The path is relative, so it names no file."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (org-mcp-test--call-tool-refused
     "org-node-text"
     `((link
        .
        ,(format "file:../%s::*Parent Task"
                 (file-name-nondirectory test-file))))
     "names no local file by its full path"
     test-file)))

(ert-deftest org-mcp-test-read-headline-encoded-path-traversal ()
  "Test that URL-encoded path traversal in a `file:' link is rejected.
A tool takes a link as it is and decodes nothing, so %2E%2E%2F stays
literal and the path is relative."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (org-mcp-test--call-tool-refused
     "org-node-text"
     `((link
        .
        ,(format "file:%%2E%%2E%%2F%s::*Parent Task"
                 (file-name-nondirectory test-file))))
     "names no local file by its full path"
     test-file)))

(ert-deftest org-mcp-test-read-headline-outline-path-refused ()
  "An outline path of titles is no link, so it is refused, not resolved.
Nothing is read, neither the level-3 Target Headline, nor the one
under Third Parent, although First Parent has no such child."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-wrong-levels))
   (org-mcp-test--call-tool-refused
    "org-node-text"
    `((link . ,(format "%s#First%%20Parent/Target%%20Headline" test-file)))
    "\\`Not an Org link: "
    test-file)))

(ert-deftest org-mcp-test-read-headline-id-search-reaches-deep-heading ()
  "An `id:' link with a title search reaches a heading two levels below it.
Other Child's ID scopes the search to its subtree, so the level-3
Target Headline under Second Parent is read, never the one under Third
Parent."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-wrong-levels))
    (org-mcp-test--with-id-tracking
        (list test-file)
        `((,org-mcp-test--other-child-id . ,test-file))
      (let ((result
             (org-mcp-test--call-read-headline
              (format "id:%s::*Target Headline"
                      org-mcp-test--other-child-id))))
        (should
         (string=
          result
          (concat
           "*** Target Headline\n"
           "This should NOT be found via First Parent/Target Headline path.")))))))

(ert-deftest org-mcp-test-read-headline-one-title-outline-path-refused ()
  "A one-title outline path is no link, so it is refused, not resolved.
The title only exists below level 1.  The path is refused as it is,
before any lookup, and the file is left alone."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-wrong-levels))
    (org-mcp-test--call-tool-refused
     "org-node-text"
     `((link . ,(format "%s#Target%%20Headline" test-file)))
     "\\`Not an Org link: "
     test-file)))

(ert-deftest org-mcp-test-id-resource-not-found ()
  "Test org-node-text tool error for non-existent ID."
  (let ((test-content "* Section without ID\nNo ID here."))
    (org-mcp-test--with-id-setup test-file test-content '()
      (org-mcp-test--call-tool-refused
       "org-node-text" '((link . "id:nonexistent-id-12345"))
       "\\`Cannot find ID 'nonexistent-id-12345'\\'"
       test-file))))

(ert-deftest org-mcp-test-id-resource-file-not-allowed ()
  "Test org-node-text tool validates file is in allowed list."
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
      (org-mcp-test--call-tool-refused
       "org-node-text" '((link . "id:test-id-789"))
       (org-mcp-test--refused-path-regexp "id:test-id-789")
       other-file))))

(ert-deftest org-mcp-test-update-todo-state-success ()
  "Test successful TODO state update."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "|" "DONE(d!)"))))
        ;; Update TODO to IN-PROGRESS
        (let ((link
               (org-mcp-test--file-link test-file "*Task One")))
          (org-mcp-test--update-todo-state-and-check
           link "TODO" "IN-PROGRESS"
           test-file org-mcp-test--expected-task-one-in-progress-regex
           (org-mcp-test--file-link test-file "*Task One")))))))

(ert-deftest org-mcp-test-update-todo-state-mismatch ()
  "Test TODO state update fails on state mismatch."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        ;; Try to update with wrong current state
        (let ((link
               (org-mcp-test--file-link test-file "*Task One")))
          (org-mcp-test--call-update-todo-state-expecting-error
           test-file link "IN-PROGRESS" "DONE"))))))

(ert-deftest org-mcp-test-update-todo-with-timestamp-id ()
  "Test updating TODO state using timestamp-format ID (not UUID)."
  (let ((test-content org-mcp-test--content-timestamp-id))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
      (org-mcp-test--with-id-setup test-file test-content
          `("20240101T120000")
        (let ((link "id:20240101T120000"))
          (org-mcp-test--update-todo-state-and-check
           link "TODO" "DONE"
           test-file
           org-mcp-test--expected-timestamp-id-done-regex))))))

(ert-deftest org-mcp-test-update-todo-state-empty-newstate-invalid ()
  "Test that empty string for after is rejected."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        ;; Try to set empty state
        (let ((link
               (org-mcp-test--file-link test-file "*Task One")))
          (org-mcp-test--call-update-todo-state-expecting-error
           test-file link "TODO" ""))))))

(ert-deftest org-mcp-test-update-todo-state-invalid ()
  "Test TODO state update fails for invalid new state."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        ;; Try to update to invalid state
        (let ((link
               (org-mcp-test--file-link test-file "*Task One")))
          (org-mcp-test--call-update-todo-state-expecting-error
           test-file link "TODO" "INVALID-STATE"))))))

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
                (let ((link
                       (org-mcp-test--file-link test-file "*Task One")))
                  (org-mcp-test--update-todo-state-and-check
                   link "TODO" "IN-PROGRESS"
                   test-file org-mcp-test--expected-task-one-in-progress-regex
                   (org-mcp-test--file-link test-file "*Task One"))
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
              (let ((link
                     (org-mcp-test--file-link test-file "*Task One")))
                (let ((result
                       (org-mcp-test--call-update-todo-state
                        link "IN-PROGRESS" "TODO")))
                  (should (equal (alist-get 'success result) t))
                  (should (eq (alist-get 'saved result) :json-false))
                  (should (equal (alist-get 'after result) "IN-PROGRESS")))
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
  "Test TODO state update fails for non-existent ID."
  (let ((test-content "* TODO Task One\nTask description."))
    (let ((org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
      (org-mcp-test--with-id-setup test-file test-content '()
        ;; Try to update a non-existent ID
        (org-mcp-test--call-tool-refused
         "org-node-set-todo"
         '((link . "id:nonexistent-uuid-12345")
           (before . "TODO")
           (after . "IN-PROGRESS"))
         "\\`Cannot find ID 'nonexistent-uuid-12345'\\'"
         test-file)))))

(ert-deftest org-mcp-test-update-todo-state-by-id ()
  "Test updating TODO state using an `id:' link."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (let ((org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
      (org-mcp-test--with-id-setup test-file test-content
          `(,org-mcp-test--content-with-id-id)
        (org-mcp-test--update-todo-state-and-check
         org-mcp-test--content-with-id-link "TODO" "IN-PROGRESS"
         test-file
         org-mcp-test--expected-task-with-id-in-progress-regex)))))

(ert-deftest org-mcp-test-update-todo-state-rejects-org-prefix ()
  "Test that `org-node-set-todo' rejects an `org://'-prefixed URI.
The refusal says to drop the prefix and names the link forms, both for
an ID and for an `id:' link behind the prefix."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (let ((org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
      (org-mcp-test--with-id-setup test-file test-content
          `(,org-mcp-test--content-with-id-id)
        (dolist (uri (list org-mcp-test--content-with-id-resource-uri
                           (concat "org://"
                                   org-mcp-test--content-with-id-link)))
          (org-mcp-test--call-tool-refused
           "org-node-set-todo"
           `((link . ,uri)
             (before . "TODO")
             (after . "IN-PROGRESS"))
           (concat "\\`Not an Org link: " (regexp-quote uri)
                   "\\.  Drop org://.*Send id:<uuid>, "
                   "file:<path>::#<custom-id>, file:<path>::\\*<title> "
                   "or file:<path>")
           test-file))))))

(ert-deftest org-mcp-test-update-todo-state-nonexistent-headline ()
  "Test TODO state update fails for a title no heading has."
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
        (let ((link
               (org-mcp-test--file-link test-file "*Nonexistent Task")))
          (org-mcp-test--call-update-todo-state-expecting-error
           test-file link "TODO" "IN-PROGRESS"))))))

(ert-deftest org-mcp-test-update-todo-state-without-current-state ()
  "Test TODO state update without providing before."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "|" "DONE(d!)"))))
        (let ((link
               (org-mcp-test--file-link test-file "*Task One")))
          (let ((result
                 (org-mcp-test--call-update-todo-state
                  link "IN-PROGRESS")))
            (should (= (length result) 5))
            (should (equal (alist-get 'success result) t))
            (should (equal (alist-get 'before result) "TODO"))
            (should (equal (alist-get 'after result) "IN-PROGRESS"))
            (should
             (equal (alist-get 'link result)
                    (org-mcp-test--file-link test-file "*Task One")))
            (org-mcp-test--verify-file-matches
             test-file org-mcp-test--expected-task-one-in-progress-regex)))))))

(ert-deftest org-mcp-test-update-todo-state-without-current-state-no-state ()
  "Test TODO state update without before on headline with no TODO state."
  (let ((test-content "* Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO(t!)" "|" "DONE(d!)"))))
        (let ((link
               (org-mcp-test--file-link test-file "*Task One")))
          (let ((result
                 (org-mcp-test--call-update-todo-state
                  link "TODO")))
            (should (= (length result) 5))
            (should (equal (alist-get 'success result) t))
            (should (equal (alist-get 'before result) ""))
            (should (equal (alist-get 'after result) "TODO"))
            (should
             (equal (alist-get 'link result)
                    (org-mcp-test--file-link test-file "*Task One")))))))))

(defconst org-mcp-test--expected-task-one-done-with-note-regex
  (concat
   "\\`\\* DONE Task One\n"
   ":LOGBOOK:\n"
   "- State \"DONE\"[ \t]+from \"TODO\"[ \t]+\\[.*\\] \\\\\\\\\n"
   "  Test note\n"
   ":END:\n"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching buffer after updating Task One to DONE with a note.")

(defconst org-mcp-test--expected-task-one-done-with-note-no-drawer-regex
  (concat
   "\\`\\* DONE Task One\n"
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
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after setting EFFORT property on bare task.")

(defconst org-mcp-test--pattern-set-properties-update
  (concat
   "\\`\\* TODO Task with Properties\n"
   " *:PROPERTIES:\n"
   " *:EFFORT: +2:30\n"
   " *:CATEGORY: +work\n"
   " *:END:\n"
   "Some body\\.\n?\\'")
  "Pattern after updating EFFORT property.")

(defconst org-mcp-test--pattern-set-properties-delete
  (concat
   "\\`\\* TODO Task with Properties\n"
   " *:PROPERTIES:\n"
   " *:CATEGORY: +work\n"
   " *:END:\n"
   "Some body\\.\n?\\'")
  "Pattern after deleting EFFORT property.")

(defconst org-mcp-test--pattern-set-properties-booleans
  (concat
   "\\`\\* TODO Task with Properties\n"
   " *:PROPERTIES:\n"
   " *:EFFORT: +nil\n"
   " *:CATEGORY: +work\n"
   " *:ENABLED: +t\n"
   " *:LITERAL_T: +t\n"
   " *:LITERAL_NIL: +nil\n"
   " *:END:\n"
   "Some body\\.\n?\\'")
  "Pattern after setting boolean and t/nil string properties.
EFFORT, set to JSON false, holds nil rather than being deleted; JSON
true is written as t, and the strings \"t\" and \"nil\" as given.")

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
   "Task body text\\.\n?\\'")
  "Pattern after setting SCHEDULED on bare task.")

(defconst org-mcp-test--pattern-scheduled-update
  (concat
   "\\`\\* TODO Scheduled Task\n"
   "SCHEDULED: <2026-04-15 .*>\n"
   "Task body\\.\n?\\'")
  "Pattern after updating existing SCHEDULED.")

(defconst org-mcp-test--pattern-scheduled-remove
  (concat
   "\\`\\* TODO Scheduled Task\n"
   "Task body\\.\n?\\'")
  "Pattern after removing SCHEDULED.")

(defconst org-mcp-test--pattern-deadline-set
  (concat
   "\\`\\* TODO Simple Task\n"
   "DEADLINE: <2026-03-27 .*>\n"
   "Task body text\\.\n?\\'")
  "Pattern after setting DEADLINE on bare task.")

(defconst org-mcp-test--pattern-deadline-update
  (concat
   "\\`\\* TODO Deadline Task\n"
   "DEADLINE: <2026-04-15 .*>\n"
   "Task body\\.\n?\\'")
  "Pattern after updating existing DEADLINE.")

(defconst org-mcp-test--pattern-deadline-remove
  (concat
   "\\`\\* TODO Deadline Task\n"
   "Task body\\.\n?\\'")
  "Pattern after removing DEADLINE.")

(defconst org-mcp-test--pattern-tags-set
  (concat
   "\\`\\* TODO Simple Task[ \t]+:work:urgent:\n"
   "Task body text\\.\n?\\'")
  "Pattern after setting tags on bare task.")

(defconst org-mcp-test--pattern-tags-replace
  (concat
   "\\`\\* TODO Task with Tags[ \t]+:personal:\n"
   "Task description\\.\n?\\'")
  "Pattern after replacing tags.")

(defconst org-mcp-test--pattern-tags-clear
  (concat
   "\\`\\* TODO Task with Tags\n"
   "Task description\\.\n?\\'")
  "Pattern after clearing all tags.")

(defconst org-mcp-test--pattern-priority-set
  (concat
   "\\`\\* TODO \\[#A\\] Simple Task\n"
   "Task body text\\.\n?\\'")
  "Pattern after setting priority A on bare task.")

(defconst org-mcp-test--pattern-priority-change
  (concat
   "\\`\\* TODO \\[#C\\] Priority Task\n"
   "Task body\\.\n?\\'")
  "Pattern after changing priority from B to C.")

(defconst org-mcp-test--pattern-priority-remove
  (concat
   "\\`\\* TODO Priority Task\n"
   "Task body\\.\n?\\'")
  "Pattern after removing priority.")

(defconst org-mcp-test--pattern-append-body
  (concat
   "\\`\\* TODO Simple Task\n"
   "Task body text\\.\n"
   "Appended line\\.\n?\\'")
  "Pattern after appending to body.")

(defconst org-mcp-test--pattern-append-body-empty
  (concat
   "\\`\\* TODO Empty Body Task\n"
   "New body content\\.\n?\\'")
  "Pattern after appending to empty body.")

(defconst org-mcp-test--pattern-append-body-with-children
  (concat
   "\\`\\* TODO Parent Task\n"
   "Parent body\\.\n"
   "Appended text\\.\n"
   "\\*\\* Child One\n"
   "Child body\\.\n?\\'")
  "Pattern after appending body before children.")

(defconst org-mcp-test--pattern-logbook-note-new
  (concat
   "\\`\\* TODO Simple Task\n"
   ":LOGBOOK:\n"
   "- Note taken on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  This is my note\\.\n"
   ":END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after adding logbook note to task without LOGBOOK.")

(defconst org-mcp-test--pattern-logbook-note-existing
  (concat
   "\\`\\* TODO Task with Logbook\n"
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
   ":LOGBOOK:\n"
   "- Note taken on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  Quotes \"like this\", backslash \\\\, percent %, asterisk \\*\\.\n"
   ":END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after adding logbook note with special characters.")

(defconst org-mcp-test--pattern-logbook-note-no-drawer
  (concat
   "\\`\\* TODO Simple Task\n"
   "- Note taken on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  Plain note\\.\n"
   "Task body text\\.\n?\\'")
  "Pattern after adding logbook note with `org-log-into-drawer' nil.")

(defconst org-mcp-test--pattern-logbook-note-custom-heading
  (concat
   "\\`\\* TODO Simple Task\n"
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
        (let ((link
               (org-mcp-test--file-link test-file "*Task One")))
          (let* ((params `((link . ,link)
                           (after . "DONE")
                           (note . "Test note")))
                 (result-text (mcp-server-lib-ert-call-tool
                               "org-node-set-todo" params))
                 (result (json-read-from-string result-text)))
            (should (equal (alist-get 'success result) t))
            (should (equal (alist-get 'before result) "TODO"))
            (should (equal (alist-get 'after result) "DONE"))
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
        (let ((link
               (org-mcp-test--file-link test-file "*Task One")))
          (let* ((params `((link . ,link)
                           (after . "DONE")
                           (note . "Test note")))
                 (result-text (mcp-server-lib-ert-call-tool
                               "org-node-set-todo" params))
                 (result (json-read-from-string result-text)))
            (should (equal (alist-get 'success result) t))
            (should (equal (alist-get 'before result) "TODO"))
            (should (equal (alist-get 'after result) "DONE"))
            (org-mcp-test--verify-file-matches
             test-file
             org-mcp-test--expected-task-one-done-with-note-no-drawer-regex)))))))

(ert-deftest org-mcp-test-update-todo-state-triggers-repeat ()
  "Test that marking DONE on a task with a repeater triggers the repeat.
The response reports the state Org left the entry in, the not-done
keyword the repeat reset it to, not the done keyword asked for."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-task-scheduled-repeat))
    (let ((org-log-repeat nil)
          (org-todo-keywords '((sequence "TODO" "|" "DONE"))))
      (let* ((link (org-mcp-test--file-link test-file "*Weekly Task"))
             (result (org-mcp-test--call-update-todo-state link "DONE")))
        ;; Response fields
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'before result) "TODO"))
        (should (equal (alist-get 'after result) "TODO"))
        ;; File: repeat fired — state reverted to TODO, SCHEDULED advanced
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--expected-weekly-task-repeat-triggered-regex)))))

(ert-deftest org-mcp-test-update-todo-state-repeat-to-state ()
  "Test that REPEAT_TO_STATE is respected when repeat triggers.
The response reports the keyword `REPEAT_TO_STATE' named, which is
the state Org left the entry in."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-task-scheduled-repeat-to-state))
    (let ((org-log-repeat nil)
          (org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE"))))
      (let* ((link (org-mcp-test--file-link test-file "*Weekly Task"))
             (result (org-mcp-test--call-update-todo-state link "DONE")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'before result) "TODO"))
        (should (equal (alist-get 'after result) "NEXT"))
        ;; File: state reverted to NEXT (from REPEAT_TO_STATE)
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--expected-weekly-task-repeat-to-state-regex)))))

(ert-deftest org-mcp-test-add-todo-top-level ()
  "Test adding a top-level TODO item."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-link (concat "file:" test-file)))
      (org-mcp-test--add-todo-and-check
       "New Task"
       "TODO"
       '("work" "urgent")
       nil ; no body
       parent-link
       nil ; no previous_sibling
       (file-name-nondirectory test-file)
       test-file
       (concat
        "^\\* TODO New Task +:.*work.*urgent.*:\n\\'")))))

(ert-deftest org-mcp-test-add-todo-top-level-with-header ()
  "Test adding top-level TODO after header comments."
  (let ((initial-content org-mcp-test--content-nested-siblings))
    (org-mcp-test--with-add-todo-setup test-file initial-content
      (let ((parent-link (concat "file:" test-file)))
        (org-mcp-test--add-todo-and-check
         "New Top Task"
         "TODO"
         '("urgent")
         nil ; no body
         parent-link
         nil ; no previous_sibling
         (file-name-nondirectory test-file)
         test-file
         org-mcp-test--expected-regex-top-level-with-header)))))

(defconst org-mcp-test--content-file-drawer-preamble
  ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: T\n\n"
  "Preamble opening with a file-level property drawer, as org-roam writes.")

(defconst org-mcp-test--content-file-drawer
  (concat org-mcp-test--content-file-drawer-preamble "* Existing\n")
  "File with a file-level property drawer, a title and one heading.")

(defconst org-mcp-test--regex-file-drawer-top-level-added
  (concat
   "\\`" (regexp-quote org-mcp-test--content-file-drawer-preamble)
   "\\* TODO New\n"
   "\n?"
   "\\* Existing\n"
   "\\'")
  "Regex matching the whole drawer file with New before Existing.")

(defconst org-mcp-test--regex-file-drawer-added-after-existing
  (concat
   "\\`" (regexp-quote org-mcp-test--content-file-drawer-preamble)
   "\\* Existing\n"
   "\n?"
   "\\* TODO New\n"
   "\\'")
  "Regex matching the whole drawer file with New after Existing.")

(defconst org-mcp-test--content-file-drawer-only
  ":PROPERTIES:\n:ID: file-id\n:END:\n"
  "File holding a file-level property drawer and no heading.")

(defconst org-mcp-test--regex-file-drawer-only-added
  (concat
   "\\`" (regexp-quote org-mcp-test--content-file-drawer-only)
   "\\* TODO New\n"
   "\\'")
  "Regex matching the whole drawer-only file with New below the drawer.")

(defconst org-mcp-test--content-preamble-text-preamble
  "#+TITLE: T\n\nIntro text.\n\n"
  "Preamble holding a paragraph after its keyword line.")

(defconst org-mcp-test--content-preamble-text
  (concat org-mcp-test--content-preamble-text-preamble "* Existing\n")
  "File whose preamble holds a paragraph, followed by one heading.")

(defconst org-mcp-test--regex-preamble-text-top-level-added
  (concat
   "\\`" (regexp-quote org-mcp-test--content-preamble-text-preamble)
   "\\* TODO New\n"
   "\n?"
   "\\* Existing\n"
   "\\'")
  "Regex matching the whole file with New after the preamble's paragraph.")

(ert-deftest org-mcp-test-add-todo-top-level-after-preamble ()
  "A top-level TODO goes after the file's preamble, before every heading.
The preamble is everything before the first heading.  A file-level
property drawer with its ID, as org-roam writes one, stays intact
above the new heading, and so does a paragraph after the keyword
lines; in a file with only a drawer, the heading goes below it.  With
previous_sibling, the TODO goes after that heading, as in any file."
  (pcase-dolist (`(,content ,after ,expected)
                 `((,org-mcp-test--content-file-drawer
                    nil ,org-mcp-test--regex-file-drawer-top-level-added)
                   (,org-mcp-test--content-file-drawer
                    "*Existing"
                    ,org-mcp-test--regex-file-drawer-added-after-existing)
                   (,org-mcp-test--content-file-drawer-only
                    nil ,org-mcp-test--regex-file-drawer-only-added)
                   (,org-mcp-test--content-preamble-text
                    nil
                    ,org-mcp-test--regex-preamble-text-top-level-added)))
    (org-mcp-test--with-add-todo-setup test-file content
      (org-mcp-test--add-todo-and-check
       "New" "TODO" nil nil
       (concat "file:" test-file)
       (and after (org-mcp-test--file-link test-file after))
       (file-name-nondirectory test-file)
       test-file
       expected))))

(defconst org-mcp-test--content-parent-last-line "* Parent"
  "File whose only line is its heading, with no newline after it.")

(defconst org-mcp-test--regex-parent-last-line-child-added
  "\\`\\* Parent\n\\*\\* TODO New\n\\'"
  "Regex matching the whole last-line file with New as Parent's child.")

(ert-deftest org-mcp-test-add-todo-child-of-heading-ending-file ()
  "A child goes below a parent heading that ends the file without a newline.
The parent's heading line stays intact."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-parent-last-line
    (org-mcp-test--add-todo-and-check
     "New" "TODO" nil nil
     (org-mcp-test--file-link test-file "*Parent")
     nil
     (file-name-nondirectory test-file)
     test-file
     org-mcp-test--regex-parent-last-line-child-added)))

(defconst org-mcp-test--content-heading-first-line "* Existing\n"
  "File whose first line is its only heading.")

(defconst org-mcp-test--regex-heading-first-line-top-level-added
  "\\`\\* TODO New\n\\* Existing\n\\'"
  "Regex matching the whole first-line file with New before Existing.")

(ert-deftest org-mcp-test-add-todo-top-level-before-heading-on-first-line ()
  "A top-level TODO goes before a heading on the file's first line.
That heading counts as one the file has, so the new heading gets a
line of its own above it."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-heading-first-line
    (org-mcp-test--add-todo-and-check
     "New" "TODO" nil nil
     (concat "file:" test-file)
     nil
     (file-name-nondirectory test-file)
     test-file
     org-mcp-test--regex-heading-first-line-top-level-added)))

(ert-deftest org-mcp-test-add-todo-invalid-state ()
  "Test that adding TODO with invalid state throws error."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-link (concat "file:" test-file)))
      (org-mcp-test--call-add-todo-expecting-error
       test-file
       "New Task"
       "INVALID-STATE" ; Not in org-todo-keywords
       '("work")
       nil
       parent-link))))

(ert-deftest org-mcp-test-add-todo-invalid-state-error-lists-valid-states ()
  "Invalid TODO state error lists every valid keyword from `org-todo-keywords-1'."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-empty))
    (let ((org-todo-keywords
           '((sequence "TODO" "NEXT" "|" "DONE" "CANCELED")))
          (org-tag-alist '("work")))
      (let* ((parent-link (concat "file:" test-file))
             (params
              `((title . "New Task")
                (todo . "BOGUS")
                (tags . ("work"))
                (content . nil)
                (parent . ,parent-link)))
             (request
              (mcp-server-lib-create-tools-call-request
               "org-node-create" nil params))
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
      (let ((parent-link (concat "file:" test-file)))
        (org-mcp-test--add-todo-and-check
         "Cancel Me"
         "CANCELED"
         '("work")
         nil
         parent-link
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
      (let ((parent-link (concat "file:" test-file)))
        (org-mcp-test--call-add-todo-expecting-error
         test-file
         "New Task"
         "TODO(t!)" ; selection-key form is not the state name
         '("work")
         nil
         parent-link)))))

(ert-deftest org-mcp-test-add-todo-valid-state-multiple-sequences ()
  "Adding TODO accepts a keyword drawn from a non-first sequence."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-empty))
    (let ((org-todo-keywords
           '((sequence "TODO" "|" "DONE")
             (type "BUG" "FEATURE" "|" "FIXED")))
          (org-tag-alist '("work"))
          (org-id-locations-file nil))
      (let ((parent-link (concat "file:" test-file)))
        (org-mcp-test--add-todo-and-check
         "File Bug"
         "BUG"
         '("work")
         nil
         parent-link
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
    (let ((parent-link (concat "file:" test-file)))
      (org-mcp-test--add-todo-and-check
       "Task1"
       "TODO"
       '("freeform")
       nil
       parent-link
       nil
       (file-name-nondirectory test-file)
       test-file
       (concat
        "^\\* TODO Task1 +:freeform:\n\\'")))))

(ert-deftest org-mcp-test-add-todo-tag-accept-valid-with-alist ()
  "Test that tags in `org-tag-alist' are accepted."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-link (concat "file:" test-file)))
      ;; Should accept tags in org-tag-alist (work, personal, urgent)
      (org-mcp-test--add-todo-and-check
       "ValidTask"
       "TODO"
       '("work")
       nil
       parent-link
       nil
       (file-name-nondirectory test-file)
       test-file
       (concat
        "^\\* TODO ValidTask +:work:\n\\'")))))

(ert-deftest org-mcp-test-add-todo-tag-validation-without-alist ()
  "Test valid tag names are accepted when `org-tag-alist' is empty."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((org-tag-alist nil)
          (org-tag-persistent-alist nil))
      (let ((parent-link (concat "file:" test-file)))
        ;; Should accept valid tag names (alphanumeric, _, @)
        (org-mcp-test--add-todo-and-check
         "Task1"
         "TODO"
         '("validtag" "tag123" "my_tag" "@home")
         nil
         parent-link
         nil
         (file-name-nondirectory test-file)
         test-file
         (concat
          "^\\* TODO Task1 +:"
          ".*validtag.*tag123.*my_tag.*@home.*:\n\\'"))))))

(ert-deftest org-mcp-test-add-todo-tag-invalid-characters ()
  "Test that tags with characters outside `org-tag-re' are rejected."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((org-tag-alist nil)
          (org-tag-persistent-alist nil))
      (let ((parent-link (concat "file:" test-file)))
        ;; Reject tags containing characters outside `org-tag-re'
        ;; (which permits [:alnum:], `_', `@', `#', `%').  Note that
        ;; Emacs's [:alnum:] is Unicode-aware, so e.g. \"café\" is a
        ;; legal tag and is therefore not tested here.
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" '("invalid-tag!") nil parent-link)
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" '("tag-with-dash") nil parent-link)
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" '("tag with space") nil parent-link)
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" '("tag:colon") nil parent-link)
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" '("tag.dot") nil parent-link)
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO" '("tag~tilde") nil parent-link)))))

(ert-deftest org-mcp-test-add-todo-tag-org-tag-re-extras ()
  "Test that `#' and `%' are accepted (per `org-tag-re')."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((org-tag-alist nil)
          (org-tag-persistent-alist nil))
      (let ((parent-link (concat "file:" test-file)))
        (org-mcp-test--add-todo-and-check
         "Task1"
         "TODO"
         '("tag#hash" "pct%tag")
         nil
         parent-link
         nil
         (file-name-nondirectory test-file)
         test-file
         (concat
          "^\\* TODO Task1 +:"
          ".*tag#hash.*pct%tag.*:\n\\'"))))))

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
      (let ((parent-link (concat "file:" test-file)))
        ;; Both the umbrella tag and a child tag are valid.
        (org-mcp-test--add-todo-and-check
         "Task1"
         "TODO"
         '("project" "proj_a")
         nil
         parent-link
         nil
         (file-name-nondirectory test-file)
         test-file
         (concat
          "^\\* TODO Task1 +:"
          "\\(?:project:proj_a\\|proj_a:project\\):\n\\'"))))))

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
      (let ((parent-link (concat "file:" test-file)))
        (org-mcp-test--call-add-todo-expecting-error
         test-file "Task" "TODO"
         ["@office" "@home"]
         nil parent-link)))))

(ert-deftest org-mcp-test-add-todo-child-under-parent ()
  "Test adding a child TODO under an existing parent."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-nested-siblings
    (let ((parent-link
           (org-mcp-test--file-link test-file "*Parent Task")))
      (org-mcp-test--add-todo-and-check
       "Child Task"
       "TODO"
       '("work")
       nil ; no body
       parent-link
       nil ; no previous_sibling
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-child-under-parent))))

(ert-deftest org-mcp-test-add-todo-child-empty-after-link ()
  "Test adding a child TODO with empty string for previous_sibling.
Empty string should be treated as nil - append as last child."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-nested-siblings
    (let ((parent-link
           (org-mcp-test--file-link test-file "*Parent Task")))
      (org-mcp-test--add-todo-and-check
       "Child Task"
       "TODO"
       '("work")
       nil ; no body
       parent-link
       "" ; empty string previous_sibling
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
    (org-mcp-test--with-id-tracking
        (list test-file)
        `((,org-mcp-test--childless-parent-id . ,test-file))
      (let ((parent-link
             (concat "id:" org-mcp-test--childless-parent-id)))
        (org-mcp-test--add-todo-and-check
         "Only Child"
         "TODO"
         '("work")
         nil ; no body
         parent-link
         nil ; no previous_sibling
         (file-name-nondirectory test-file)
         test-file
         org-mcp-test--regex-child-into-childless-parent)))))

(ert-deftest org-mcp-test-add-todo-second-child-same-level ()
  "Test that adding a second child creates it at the same level as first child.
This tests the bug where the second child was created at level 4 instead of level 3."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-level2-parent-level3-children
    (let ((parent-link
           (org-mcp-test--file-link test-file "*Review the package")))
      (org-mcp-test--add-todo-and-check
       "Second Child"
       "TODO"
       '("work")
       nil  ; no body
       parent-link
       nil ; no previous_sibling
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-second-child-same-level))))

(ert-deftest org-mcp-test-add-todo-with-after-link ()
  "Test adding TODO after a sibling using previous_sibling.
Tests that adding after a level 3 sibling correctly creates level 3.
Reproduces the emacs.org scenario: level 2 parent (via its title),
level 3 sibling (via its ID)."
  (let ((initial-content org-mcp-test--content-level2-parent-level3-children))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-tag-alist '("internet")))
      (org-mcp-test--with-id-setup test-file initial-content
          `(,org-mcp-test--level2-parent-level3-sibling-id)
        (let ((parent-link
               (org-mcp-test--file-link test-file "*Review the package"))
              (after-link
               (concat "id:" org-mcp-test--level2-parent-level3-sibling-id)))
          ;; BUG: org-insert-heading creates level 1 (*) instead of level 3 (***)
          (org-mcp-test--add-todo-and-check
           "Review org-mcp-test.el"
           "TODO"
           '("internet")
           nil
           parent-link
           after-link
           (file-name-nondirectory test-file)
           test-file
           org-mcp-test--regex-after-sibling-level3))))))

(ert-deftest org-mcp-test-add-todo-with-body ()
  "Test adding TODO with body text."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-link (concat "file:" test-file))
          (body-text org-mcp-test--body-text-multiline))
      (org-mcp-test--add-todo-and-check
       "Task with Body"
       "TODO"
       '("work")
       body-text
       parent-link
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
    (let ((parent-link (concat "file:" test-file))
          (body-with-asterisk "Some initial text.\n*"))
      ;; Should succeed since * without space is not a headline
      (org-mcp-test--add-todo-and-check
       "Task"
       "TODO"
       '("work")
       body-with-asterisk
       parent-link
       nil
       (file-name-nondirectory test-file)
       test-file
       (concat
        "^\\* TODO Task +:work:\n"
        "Some initial text\\.\n"
        "\\*$")))))

(ert-deftest org-mcp-test-add-todo-body-with-unbalanced-block ()
  "Test that adding TODO with body containing unbalanced block is rejected.
Unbalanced blocks like #+BEGIN_EXAMPLE without #+END_EXAMPLE should be
rejected in TODO body content."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let
        ((parent-link (concat "file:" test-file))
         (body-with-unbalanced-block
          "Here's an example:\n#+BEGIN_EXAMPLE\nsome code\nMore text after block"))
      ;; Should reject unbalanced blocks
      (org-mcp-test--call-add-todo-expecting-error
       test-file
       "Task with unbalanced block"
       "TODO"
       '("work")
       body-with-unbalanced-block
       parent-link))))

(ert-deftest org-mcp-test-add-todo-body-with-unbalanced-end-block ()
  "Test that adding TODO with body containing unbalanced END block is rejected.
An #+END_EXAMPLE without matching #+BEGIN_EXAMPLE should be rejected."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-link (concat "file:" test-file))
          (body-with-unbalanced-end
           "Some text before\n#+END_EXAMPLE\nMore text after"))
      ;; Should reject unbalanced END blocks
      (org-mcp-test--call-add-todo-expecting-error
       test-file
       "Task with unbalanced END block"
       "TODO"
       '("work")
       body-with-unbalanced-end
       parent-link))))

(ert-deftest org-mcp-test-add-todo-body-with-literal-block-end ()
  "Test that TODO body with END_SRC inside EXAMPLE block is accepted.
#+END_SRC inside an EXAMPLE block is literal text, not a block delimiter.
This is valid Org-mode syntax and should be allowed."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-link (concat "file:" test-file))
          (body-with-literal-end
           "Example of source block:\n#+BEGIN_EXAMPLE\n#+END_SRC\n#+END_EXAMPLE\nText after."))
      ;; Should succeed - #+END_SRC is just literal text inside EXAMPLE block
      (org-mcp-test--add-todo-and-check
       "Task with literal END_SRC"
       "TODO"
       '("work")
       body-with-literal-end
       parent-link
       nil
       (file-name-nondirectory test-file)
       test-file
       (concat
        "^\\* TODO Task with literal END_SRC +:work:\n"
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
          (let ((parent-link
                 (org-mcp-test--file-link test-file "*Parent Task"))
                (after-link (concat "id:" first-id)))
            (org-mcp-test--add-todo-and-check
             "New Task After First"
             "TODO"
             '("work")
             nil
             parent-link
             after-link
             (file-name-nondirectory test-file)
             test-file
             org-mcp-test--regex-todo-after-sibling))))))))

(ert-deftest org-mcp-test-add-todo-after-link-not-sibling ()
  "Test error when previous_sibling is not a child of parent."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (org-tag-alist '("work")))
    (org-mcp-test--with-id-setup
     test-file
     org-mcp-test--content-wrong-levels
     `(,org-mcp-test--other-child-id)
     (let* ((parent-link
             (org-mcp-test--file-link test-file "*First Parent"))
            (after-link
             (concat "id:" org-mcp-test--other-child-id)))
       ;; Error: Other Child is not a child of First Parent
       (org-mcp-test--call-tool-refused
        "org-node-create"
        `((title . "New Task")
          (todo . "TODO")
          (tags . ["work"])
          (content . nil)
          (parent . ,parent-link)
          (previous_sibling . ,after-link))
        (concat "\\`Sibling " (regexp-quote after-link)
                " not found under parent\\'")
        test-file)))))

(ert-deftest org-mcp-test-add-todo-parent-id-link ()
  "Test adding TODO with parent specified as an `id:' link."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((org-todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)")))
          (org-tag-alist '("work"))
          (org-id-locations-file nil))
      (org-mcp-test--with-id-tracking
          (list test-file)
          `((,org-mcp-test--content-nested-siblings-parent-id . ,test-file))
        (let ((parent-link
               (concat "id:"
                       org-mcp-test--content-nested-siblings-parent-id)))
          (org-mcp-test--add-todo-and-check
           "Child via ID"
           "TODO"
           '("work")
           nil
           parent-link
           nil
           (file-name-nondirectory test-file)
           test-file
           org-mcp-test--pattern-add-todo-parent-id-link))))))

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
        (let ((parent-link (concat "file:" test-file)))
          (org-mcp-test--call-add-todo-expecting-error
           test-file
           "Test Task"
           "TODO"
           ["work" "@office" "@home"] ; conflicting tags
           nil
           parent-link
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
        (let ((parent-link (concat "file:" test-file)))
          (org-mcp-test--add-todo-and-check
           "Test Task"
           "TODO"
           ["work" "@office" "project"] ; no conflict
           nil
           parent-link
           nil
           (file-name-nondirectory test-file)
           test-file
           org-mcp-test--regex-add-todo-with-mutex-tags))))))

(ert-deftest org-mcp-test-add-todo-nil-tags ()
  "Test that adding TODO with nil tags creates headline without tags."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-link (concat "file:" test-file)))
      (org-mcp-test--add-todo-and-check
       "Task Without Tags"
       "TODO"
       nil ; nil for tags
       nil ; no body
       parent-link
       nil ; no previous_sibling
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-todo-without-tags))))

(ert-deftest org-mcp-test-add-todo-empty-list-tags ()
  "Test that adding TODO with empty list tags creates headline without tags."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-link (concat "file:" test-file)))
      (org-mcp-test--add-todo-and-check
       "Task Without Tags"
       "TODO"
       '() ; empty list for tags
       nil ; no body
       parent-link
       nil ; no previous_sibling
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
            (concat "id:" org-mcp-test--content-nested-siblings-parent-id)
            nil
            (file-name-nondirectory test-file)
            test-file
            org-mcp-test--pattern-add-todo-with-id-property
            `((ID . ,org-mcp-test--client-id))
            (concat "id:" org-mcp-test--client-id))))
     (should (equal (alist-get 'link result)
                    (concat "id:" org-mcp-test--client-id)))
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
     (concat "file:" test-file)
     nil
     (file-name-nondirectory test-file)
     test-file
     org-mcp-test--pattern-add-todo-with-custom-id-property
     '((CUSTOM_ID . "custom-id-task"))
     (org-mcp-test--file-link test-file "#custom-id-task"))))

(ert-deftest org-mcp-test-add-todo-with-properties ()
  "Test a new TODO gets arbitrary properties next to tags and a body."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (org-mcp-test--add-todo-and-check
     "Task with Properties"
     "TODO"
     '("work")
     org-mcp-test--body-text-multiline
     (concat "file:" test-file)
     nil
     (file-name-nondirectory test-file)
     test-file
     org-mcp-test--pattern-add-todo-with-properties
     '((EFFORT . "1:00") (OWNER . "alice") (ESTIMATE . 3) (SKIPPED)))))

(ert-deftest org-mcp-test-add-todo-properties-booleans ()
  "Test a new TODO gets boolean properties as t and nil.
JSON true writes t and false writes nil, while null in the same call
is skipped.  The strings \"t\" and \"nil\" are written as given."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (org-mcp-test--add-todo-and-check
     "Flagged Task"
     "TODO"
     nil
     nil
     (concat "file:" test-file)
     nil
     (file-name-nondirectory test-file)
     test-file
     org-mcp-test--pattern-add-todo-with-boolean-properties
     '((ENABLED . t)
       (DISABLED . :json-false)
       (LITERAL_T . "t")
       (LITERAL_NIL . "nil")
       (SKIPPED)))))

(ert-deftest org-mcp-test-add-todo-properties-forbid-special ()
  "Test a create call with a special property fails, file unchanged."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (org-mcp-test--call-add-todo-expecting-error
     test-file "Task" "TODO" nil nil (concat "file:" test-file) nil
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
         test-file "Task" "TODO" nil nil (concat "file:" test-file) nil
         `((,name . "value")))
        (org-mcp-test--verify-no-modified-buffer test-file)))))

(ert-deftest org-mcp-test-add-todo-properties-multiline-value ()
  "Test a create call with a line break in a property value fails.
The break would inject a heading, so the call edits nothing."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (org-mcp-test--call-add-todo-expecting-error
     test-file "Task" "TODO" nil nil (concat "file:" test-file) nil
     '((FOO . "x\n* Injected heading")))
    (org-mcp-test--verify-no-modified-buffer test-file)))

(defconst org-mcp-test--regex-todo-without-properties
  "\\`\\* TODO Plain Task *\n\\'"
  "Pattern for an empty file after adding a TODO with no properties.")

(ert-deftest org-mcp-test-add-todo-blank-properties-set-none ()
  "A blank properties parameter sets no properties.
Some clients send null, false, \"\", [] or {} for every optional
parameter they do not use; each adds the TODO with no drawer, as
when the parameter is left out."
  (dolist (blank '(null :json-false "" [] empty-object))
    (org-mcp-test--with-add-todo-setup test-file
        org-mcp-test--content-empty
      (mcp-server-lib-ert-call-tool
       "org-node-create"
       `((title . "Plain Task")
         (todo . "TODO")
         (content . nil)
         (parent . ,(concat "file:" test-file))
         (properties
          .
          ,(pcase blank
             ('null nil)
             ;; `json-encode' writes an empty hash table as {}.
             ('empty-object (make-hash-table))
             (_ blank)))))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-todo-without-properties))))

(defconst org-mcp-test--pattern-flagged-task-and-bare-todo
  (concat
   "\\`\\* TODO Task\n"
   " *:PROPERTIES:\n"
   " *:FLAG: +nil\n"
   " *:END:\n"
   "\\* TODO Simple Task\n"
   "Task body text\\.\n?\\'")
  "Pattern after adding a TODO whose FLAG property is JSON false.
The new top-level heading goes before the file's first heading.")

(ert-deftest org-mcp-test-properties-accept-booleans-refuse-arrays ()
  "Test booleans are accepted and arrays refused as property values.
A create call with a boolean writes it; a set call with an array
fails and leaves the file unchanged."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-bare-todo
    (mcp-server-lib-ert-call-tool
     "org-node-create"
     `((title . "Task")
       (todo . "TODO")
       (content . nil)
       (parent . ,(concat "file:" test-file))
       (properties . ((FLAG . :json-false)))))
    (org-mcp-test--verify-file-matches
     test-file org-mcp-test--pattern-flagged-task-and-bare-todo)
    (org-mcp-test--call-set-properties-expecting-error
     test-file (org-mcp-test--file-link test-file "*Simple Task")
     '((ITEMS . ["a" "b"])))))

(ert-deftest org-mcp-test-rename-headline-simple ()
  "Test renaming a simple TODO headline."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-simple-todo))
    (let ((org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
          (link
           (org-mcp-test--file-link test-file "*Original Task")))
      ;; Rename the headline
      (org-mcp-test--call-rename-headline-and-check
       link
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
      (let* ((link
              (org-mcp-test--file-link test-file "*Original Task")))
        (org-mcp-test--call-rename-headline-expecting-error
         test-file link "Wrong Title" "Updated Task")))))

(ert-deftest org-mcp-test-rename-headline-preserve-tags ()
  "Test that renaming preserves tags."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-tag-alist '("work" "urgent" "personal")))
      ;; Rename the headline
      (let ((link
             (org-mcp-test--file-link test-file "*Task with Tags")))
        (org-mcp-test--call-rename-headline-and-check
         link
         "Task with Tags"
         "Renamed Task"
         test-file
         org-mcp-test--pattern-renamed-todo-with-tags)))))

(ert-deftest org-mcp-test-rename-headline-no-todo ()
  "Test renaming a regular headline without TODO state."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    ;; Rename the headline
    (let ((link
           (org-mcp-test--file-link test-file "*First Child 50% Complete")))
      (org-mcp-test--call-rename-headline-and-check
       link
       "First Child 50% Complete"
       "Updated Child"
       test-file
       org-mcp-test--pattern-renamed-headline-no-todo))))

(ert-deftest org-mcp-test-rename-headline-nested-path-navigation ()
  "An outline path is no link, so a rename through one is refused.
First Parent has no Target Headline; the path is refused as it is,
never resolved to another parent's Target Headline, and the file is
left unchanged."
  (let ((initial-content org-mcp-test--content-wrong-levels))
    (org-mcp-test--with-temp-org-files
        ((test-file initial-content))
      (let* ((link
              (format "%s#First%%20Parent/Target%%20Headline"
                      test-file)))
        (org-mcp-test--call-tool-refused
         "org-node-set-title"
         `((link . ,link)
           (before . "Target Headline")
           (after . "Renamed Target Headline"))
         (concat "\\`Not an Org link: " (regexp-quote link))
         test-file)))))

(ert-deftest org-mcp-test-rename-headline-by-id ()
  "Test renaming a headline accessed by an `id:' link."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((org-id-track-globally t)
          (org-id-locations-file nil)
          (org-id-locations nil))
      ;; Register the ID without file scanning
      (org-id-add-location org-mcp-test--content-with-id-id test-file)
      ;; Rename using the `id:' link
      (org-mcp-test--call-rename-headline-and-check
       org-mcp-test--content-with-id-link
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
      (org-mcp-test--call-tool-refused
       "org-node-set-title"
       '((link . "id:non-existent-id-12345")
         (before . "Whatever")
         (after . "Should Fail"))
       "\\`Cannot find ID 'non-existent-id-12345'\\'"
       test-file))))

(ert-deftest org-mcp-test-rename-headline-with-slash ()
  "Test renaming a headline containing a slash character.
A title link takes the slash as it is, with no encoding."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-slash-not-nested-before))
    (let ((link
           (org-mcp-test--file-link test-file "*Parent/Child")))
      (org-mcp-test--call-rename-headline-and-check
       link
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
    (let ((link
           (org-mcp-test--file-link test-file "*Parent/Child")))
      (org-mcp-test--call-rename-headline-and-check
       link
       "Parent/Child"
       "Parent-Child Renamed"
       test-file
       org-mcp-test--regex-slash-not-nested-after))))

(ert-deftest org-mcp-test-rename-headline-with-percent ()
  "Test renaming a headline containing a percent sign.
A title link takes the percent sign as it is, with no encoding."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((link
           (org-mcp-test--file-link test-file "*First Child 50% Complete")))
      (org-mcp-test--call-rename-headline-and-check
       link
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
   "First Child 50% Complete"
   "First Line\nSecond Line"))

(ert-deftest org-mcp-test-rename-headline-duplicate-title-resolves-first ()
  "Test renaming one of several headings sharing a title.
An outline path, bare or through a parent, is no link and is refused,
leaving the file unchanged.  A title link resolves, as in Org, to the
first heading with that title, so it renames the one under Team
Updates and no other."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-duplicate-headlines-before))
    (dolist (path (list (format "%s#Project%%20Review" test-file)
                        (format "%s#Team%%20Updates/Project%%20Review"
                                test-file)))
      (org-mcp-test--call-tool-refused
       "org-node-set-title"
       `((link . ,path)
         (before . "Project Review")
         (after . "Q1 Review"))
       (concat "\\`Not an Org link: " (regexp-quote path))
       test-file))
    (org-mcp-test--call-rename-headline-and-check
     (org-mcp-test--file-link test-file "*Project Review")
     "Project Review"
     "Q1 Review"
     test-file
     org-mcp-test--regex-duplicate-first-renamed)))

(ert-deftest org-mcp-test-rename-headline-creates-no-id ()
  "Test that renaming a headline without an ID creates none.
The response links to the heading by its new title, and Org's ID
locations gain no entry."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
      (let ((org-id-track-globally t)
            (org-id-locations-file nil)
            (org-id-locations nil))
        ;; Rename headline using its title link
        (let ((link
               (org-mcp-test--file-link test-file "*Third Child #3")))
          (org-mcp-test--call-rename-headline-and-check
           link
           "Third Child #3"
           "Renamed Child"
           test-file
           org-mcp-test--pattern-renamed-headline-without-id)
          (should-not org-id-locations)))))


(ert-deftest org-mcp-test-rename-headline-hierarchy ()
  "Test that headline hierarchy is correctly navigated.
Ensures that when searching for nested headlines, the function
correctly restricts search to the parent's subtree: an `id:' link's
title search runs within the heading with that ID, so it renames the
Target under Second Section, not the first Target in the file."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-hierarchy-before))
    (org-mcp-test--with-id-tracking
        (list test-file)
        `((,org-mcp-test--hierarchy-second-section-id . ,test-file))
      (let ((link
             (format "id:%s::*Target"
                     org-mcp-test--hierarchy-second-section-id)))
        (org-mcp-test--call-rename-headline-and-check
         link
         "Target"
         "Renamed Target"
         test-file
         org-mcp-test--regex-hierarchy-second-target-renamed)))))

(ert-deftest org-mcp-test-rename-headline-with-todo-keyword ()
  "Test that headlines with TODO keywords can be renamed.
The navigation function should find headlines even when they have TODO keywords."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-keywords-before))
   ;; Try to rename using the headline title without TODO keyword
   (let ((link
          (org-mcp-test--file-link test-file "*Review Documents")))
     ;; This should work - finding "Review Documents" even though
     ;; the actual headline is "TODO Review Documents"
     (org-mcp-test--call-rename-headline-and-check
      link
      "Review Documents"
      "Q1 Planning Review"
      test-file
      org-mcp-test--regex-todo-keywords-after))))

;;; org-node-set-content tests

(ert-deftest org-mcp-test-edit-body-single-line ()
  "Test org-node-set-content tool for single-line replacement."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-nested-siblings
   `(,org-mcp-test--content-with-id-id)
   (org-mcp-test--call-edit-body-and-check
    test-file
    org-mcp-test--content-with-id-link
    "Second child content."
    "Updated second child content."
    org-mcp-test--pattern-edit-body-single-line
    nil
    (concat "id:" org-mcp-test--content-with-id-id))))

(ert-deftest org-mcp-test-edit-body-multiline ()
  "Test org-node-set-content tool for multi-line replacement."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-todo
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-and-check
     test-file
     org-mcp-test--content-with-id-link
     "Second line of content."
     "This has been replaced
with new multiline
content here."
     org-mcp-test--pattern-edit-body-multiline
     nil
     (concat "id:" org-mcp-test--content-with-id-id))))

(ert-deftest org-mcp-test-edit-body-multiple-occurrences-error ()
  "Test error for multiple occurrences."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-repeated-text
      `("test-id")
    (org-mcp-test--call-tool-refused
     "org-node-set-content"
     '((link . "id:test-id")
       (before . "occurrence of pattern")
       (after . "REPLACED"))
     "\\`Text appears 3 times (must be unique)\\'"
     test-file)))


(ert-deftest org-mcp-test-edit-body-not-found ()
  "Test org-node-set-content tool error when text is not found."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-link
     "nonexistent text"
     "replacement")))

(ert-deftest org-mcp-test-edit-body-empty ()
  "Test org-node-set-content tool can add content to empty body."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((link
           (org-mcp-test--file-link test-file "*Third Child #3")))
      (org-mcp-test--call-edit-body-and-check
       test-file
       link
       ""
       "New content added."
       org-mcp-test--pattern-edit-body-empty
       nil
       (org-mcp-test--file-link test-file "*Third Child #3")))))

(ert-deftest org-mcp-test-edit-body-empty-old-non-empty-body ()
  "An empty before is refused when the node already has content.
The refusal names the assertion the call made, what the tool found
instead and what to send in its place, because a client has only the
message to act on."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (should
     (string=
      "An empty before asserts the node has no content, \
and this node has some; send the part of the content to replace"
      (org-mcp-test--call-tool-expecting-error
       test-file "org-node-set-content"
       `((link . ,org-mcp-test--content-with-id-link)
         (before . "")
         (after . "replacement")))))))

(ert-deftest org-mcp-test-edit-body-empty-with-properties ()
  "Test adding content to empty body with properties drawer.
The content goes below the drawer, which stays intact, so the
response links to the heading by its ID."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-no-body
      `(,org-mcp-test--timestamp-id)
    (org-mcp-test--call-edit-body-and-check
     test-file
     (concat "id:" org-mcp-test--timestamp-id)
     ""
     "Content added after properties."
     org-mcp-test--pattern-edit-body-empty-with-props
     nil
     (concat "id:" org-mcp-test--timestamp-id))))

(ert-deftest org-mcp-test-edit-body-nested-headlines ()
  "Test org-node-set-content preserves nested headlines."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (org-mcp-test--call-edit-body-and-check
     test-file
     (org-mcp-test--file-link test-file "*Parent Task")
     "Some parent content."
     "Updated parent content"
     org-mcp-test--pattern-edit-body-nested-headlines
     nil
     (concat "id:" org-mcp-test--content-nested-siblings-parent-id))))

(ert-deftest org-mcp-test-edit-body-reject-headline-in-middle ()
  "Test org-node-set-content rejects after with headline marker in middle."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-link
     "Second child content."
     "replacement text
* This would become a headline")))

(ert-deftest org-mcp-test-edit-body-accept-lower-level-headline ()
  "Test org-node-set-content accepts after with lower-level headline."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-and-check
     test-file
     org-mcp-test--content-with-id-link
     "Second child content."
     "some text
*** Subheading content"
     org-mcp-test--pattern-edit-body-accept-lower-level
     nil
     (concat "id:" org-mcp-test--content-with-id-id))))

(ert-deftest org-mcp-test-edit-body-reject-higher-level-headline ()
  "Test org-node-set-content rejects after with higher-level headline.
When editing a level 2 node, level 1 headlines should be rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     (org-mcp-test--file-link test-file "*Second Child")
     "Second child content."
     "New text
* Top level heading")))

(ert-deftest org-mcp-test-edit-body-reject-headline-at-start ()
  "Test org-node-set-content rejects after with headline at beginning."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-link
     "Second child content."
     "* Heading at start")))

(ert-deftest org-mcp-test-edit-body-reject-unbalanced-begin-block ()
  "Test org-node-set-content rejects after with unbalanced BEGIN block."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-link
     "Second child content."
     "Some text
#+BEGIN_EXAMPLE
Code without END_EXAMPLE")))

(ert-deftest org-mcp-test-edit-body-reject-orphaned-end-block ()
  "Test org-node-set-content rejects after with orphaned END block."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-link
     "Second child content."
     "Some text
#+END_SRC
Without BEGIN_SRC")))

(ert-deftest org-mcp-test-edit-body-reject-mismatched-blocks ()
  "Test org-node-set-content rejects after with mismatched blocks."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-link
     "Second child content."
     "Text here
#+BEGIN_QUOTE
Some quote
#+END_EXAMPLE")))

(ert-deftest org-mcp-test-edit-body-reject-lowercase-unbalanced-begin ()
  "Test org-node-set-content rejects after with lowercase unbalanced BEGIN."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-link
     "Second child content."
     "Some text
#+begin_example
Code without end_example")))

(ert-deftest org-mcp-test-edit-body-reject-lowercase-orphaned-end ()
  "Test org-node-set-content rejects after with lowercase orphaned END."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-link
     "Second child content."
     "Some text
#+end_src
Without begin_src")))

(ert-deftest org-mcp-test-edit-body-reject-second-block-unclosed ()
  "Test org-node-set-content rejects two blocks where the second is unclosed."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-link
     "Second child content."
     "First block:
#+BEGIN_EXAMPLE
done
#+END_EXAMPLE

Second block:
#+BEGIN_QUOTE
unfinished")))

(defconst org-mcp-test--content-block-body-target "* Task\nTask body.\n"
  "File whose one heading receives bodies holding indented blocks.")

(defconst org-mcp-test--regex-block-body-target-unchanged
  (concat "\\`" (regexp-quote org-mcp-test--content-block-body-target) "\\'")
  "Regex matching `org-mcp-test--content-block-body-target' unchanged.")

(defconst org-mcp-test--regex-block-body-escaped-appended
  (concat
   "\\`\\* Task\nTask body\\.\n- item\n  #\\+begin_example\n"
   "  ,\\*\\* x\n  #\\+end_example\n\\'")
  "Regex matching the target after appending an indented escaped block.")

(ert-deftest org-mcp-test-body-refuses-star-line-in-indented-block ()
  "A body whose indented block holds a star line is refused.
Org parses the star line as a heading, which breaks the block, so the
body would add a heading.  The refusal holds for a line deeper than
the heading the body goes under, in org-node-create and in both modes of
org-node-set-content, and leaves the file unchanged."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-block-body-target))
    (let ((link (org-mcp-test--file-link test-file "*Task")))
      (dolist (call
               `(("org-node-create"
                  (title . "New")
                  (todo . "TODO")
                  (content . "- item\n  #+begin_example\n** x\n  #+end_example")
                  (parent . ,(concat "file:" test-file)))
                 ("org-node-set-content"
                  (link . ,link)
                  (before . "")
                  (after . "  #+begin_example\n** z\n  #+end_example")
                  (append . t))
                 ("org-node-set-content"
                  (link . ,link)
                  (before . "Task body.")
                  (after . "  #+begin_example\n*** z\n  #+end_example"))))
        (org-mcp-test--call-tool-refused
         (car call) (cdr call)
         "\\`Body contains unclosed EXAMPLE block\\'"
         test-file)
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--regex-block-body-target-unchanged)))))

(ert-deftest org-mcp-test-body-accepts-escaped-star-line-in-indented-block ()
  "A body whose indented block escapes its star line is appended as is."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-block-body-target))
    (let ((link (org-mcp-test--file-link test-file "*Task")))
      (org-mcp-test--call-edit-body-and-check
       test-file link ""
       "- item\n  #+begin_example\n  ,** x\n  #+end_example"
       org-mcp-test--regex-block-body-escaped-appended t link))))

;;; Read tool tests

(ert-deftest org-mcp-test-tool-read-file ()
  "Test org-node-read tool returns structured JSON for files."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let* ((result-text (org-mcp-test--call-read (concat "file:" test-file)))
           (result (json-parse-string result-text :object-type 'alist))
           (children (alist-get 'children result)))
      (should (equal (alist-get 'file result) test-file))
      (should (= (length children) 1))
      (should (equal (alist-get 'title (aref children 0)) "Parent Task")))))

(ert-deftest org-mcp-test-tool-read-headline ()
  "Test org-node-text tool returns plain text for files."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((result-text
           (org-mcp-test--call-read-headline (concat "file:" test-file))))
      (should (string= result-text org-mcp-test--content-nested-siblings)))))

(ert-deftest org-mcp-test-tool-read-outline ()
  "Test org-read-outline tool returns valid JSON outline structure."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let* ((result (org-mcp-test--call-read-outline test-file))
           (headings (alist-get 'children result)))
      (should (= (length headings) 1))
      (should (string= (alist-get 'title (aref headings 0)) "Parent Task")))))

(ert-deftest org-mcp-test-tool-read-outline-file-link ()
  "org-read-outline takes a `file:' link to the file as well as its path.
A bare and a bracketed link with no search part read the same outline
as the path.  A link with a search part, a relative path and a file outside
the allowed files, as a path or a link, are refused with tool errors
that say why, and the files stay unchanged."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings)
       (other-file org-mcp-test--content-links))
    (let ((org-mcp-allowed-files (list test-file))
          (expected (org-mcp-test--call-read-outline test-file)))
      (dolist (link (list (concat "file:" test-file)
                          (format "[[file:%s][Siblings]]" test-file)))
        (should (equal (org-mcp-test--call-read-outline link) expected)))
      (pcase-dolist (`(,file ,refusal)
                     `((,(org-mcp-test--file-link test-file "*Parent Task")
                        "\\`org-read-outline takes a file's path or file: \
link, not an id: link or a search: ")
                       (,(file-name-nondirectory test-file)
                        "\\`Path must be absolute: ")
                       (,other-file "not in allowed list\\'")
                       (,(concat "file:" other-file)
                        "not in allowed list\\'")))
        (should
         (string-match-p
          refusal
          (org-mcp-test--call-tool-expecting-error
           test-file "org-read-outline" `((file . ,file))))))
      (should
       (string=
        (org-mcp-test--read-file other-file) org-mcp-test--content-links))
      (should-not (find-buffer-visiting other-file)))))

(ert-deftest org-mcp-test-tool-read-outline-refuses-heading-links-unresolved ()
  "org-read-outline refuses a heading link and an org:// string unresolved.
An `id:' link, unknown or known, bare or with a search part, and a
`file:' link with a search part are refused as parsed, and Emacs's ID
index is never consulted or rescanned.  A string starting
with org:// is refused as no link, with the hint to drop the prefix,
as the link tools refuse it.  The file stays unchanged."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((parent-id org-mcp-test--content-nested-siblings-parent-id))
      (org-mcp-test--with-id-tracking
          (list test-file)
          `((,parent-id . ,test-file))
        (org-mcp-test--without-id-index
          (dolist (link
                   (list
                    "id:0f1e2d3c-4b5a-4968-8776-a5b4c3d2e1f0"
                    (concat "id:" parent-id)
                    (format "[[id:%s::*Second Child][Second]]" parent-id)
                    (org-mcp-test--file-link test-file "*Parent Task")))
            (org-mcp-test--call-tool-refused
             "org-read-outline" `((file . ,link))
             (concat
              "\\`org-read-outline takes a file's path or file: link, \
not an id: link or a search: "
              (regexp-quote link) "\\'")
             test-file))
          (dolist (uri
                   (list
                    (concat "org://" test-file)
                    (concat "org://file:" test-file)
                    (concat "org://id:" parent-id)))
            (org-mcp-test--call-tool-refused
             "org-read-outline" `((file . ,uri))
             (concat
              "\\`Not an Org link: " (regexp-quote uri)
              "\\.  Drop org://, which only a resource URI starts with\\.  ")
             test-file)))))))

(ert-deftest org-mcp-test-tool-read-headline-single-level ()
  "Test org-node-text with a title holding a slash."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-slash-not-nested-before))
    (let ((result-text
           (org-mcp-test--call-read-headline
            (org-mcp-test--file-link test-file "*Parent/Child"))))
      (should
       (string-match-p
        org-mcp-test--pattern-tool-read-headline-single
        result-text)))))

(ert-deftest org-mcp-test-tool-read-headline-nested ()
  "Test org-node-text with a nested heading's title link."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((result-text
           (org-mcp-test--call-read-headline
            (org-mcp-test--file-link test-file "*First Child 50% Complete"))))
      (should
       (string-match-p
        org-mcp-test--pattern-tool-read-headline-nested
        result-text)))))

(ert-deftest org-mcp-test-tool-read-headline-by-id ()
  "Test org-node-text tool returns headline content by ID."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (let ((result-text
           (org-mcp-test--call-read-headline org-mcp-test--content-with-id-link)))
      (should
       (string-match-p
        org-mcp-test--pattern-tool-read-by-id
        result-text)))))

(ert-deftest org-mcp-test-tool-read-rejects-org-prefix ()
  "Test that `org-node-read' rejects an `org://'-prefixed URI.
The refusal says to drop the prefix, for a path and for a `file:' link
behind it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (dolist (uri (list (concat "org://" test-file)
                       (concat "org://file:" test-file)))
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,uri))
       (concat "\\`Not an Org link: " (regexp-quote uri)
               "\\.  Drop org://")
       test-file))))

(ert-deftest org-mcp-test-tool-read-headline-rejects-org-prefix ()
  "Test that `org-node-text' rejects an `org://'-prefixed URI.
The refusal says to drop the prefix, for a path and for a `file:' link
behind it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (dolist (uri (list (concat "org://" test-file)
                       (concat "org://file:" test-file)))
      (org-mcp-test--call-tool-refused
       "org-node-text" `((link . ,uri))
       (concat "\\`Not an Org link: " (regexp-quote uri)
               "\\.  Drop org://")
       test-file))))

(ert-deftest org-mcp-test-tool-read-file-prefers-modified-buffer ()
  "Test org-node-text file reads prefer modified visited buffers."
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
                      (concat "file:" test-file))))
                (should (string-match-p "Unsaved change" result-text))
                (should-not
                 (string-match-p
                  "Unsaved change"
                  (org-mcp-test--read-file test-file)))))
          (kill-buffer buffer))))))

;; Tests for body extraction across various metadata layouts.  These
;; verify `org-mcp--extract-structured-heading' (via the org-node-read tool)
;; correctly skips planning lines and PROPERTIES/LOGBOOK drawers in any
;; order, by delegating to `org-end-of-meta-data'.

(defun org-mcp-test--read-content (file headline)
  "Return parsed `content' field for HEADLINE in FILE via org-node-read.
HEADLINE is the heading's title, reached through its title link."
  (let* ((link (org-mcp-test--file-link file (concat "*" headline)))
         (result-text (org-mcp-test--call-read link))
         (result (json-parse-string result-text :object-type 'alist)))
    (alist-get 'content result)))

(ert-deftest org-mcp-test-extract-body-no-metadata ()
  "Body is extracted when the heading has no metadata at all."
  (let ((test-content "* Plain Heading\nFirst body line.\nSecond body line."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (should
       (equal (org-mcp-test--read-content test-file "Plain Heading")
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
       "org-node-set-content"
       `((link . ,(org-mcp-test--file-link test-file "*Headline"))
         (before . "Original body")
         (after . "Updated body")))
      (should hook-called))))

(ert-deftest org-mcp-test-clock-add-saves-file-to-disk ()
  "Test org-clock-add saves the completed CLOCK entry to disk."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let* ((link (org-mcp-test--file-link test-file "*Task One"))
           (result (org-mcp-test--call-clock-add
                    link "2026-01-01T10:00:00" "2026-01-01T11:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'added result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-add-expected-regex))))

(ert-deftest org-mcp-test-clock-add-clean-buffer-saves ()
  "Test clock-add on a clean visiting buffer edits it and saves to disk."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let* ((link (org-mcp-test--file-link test-file "*Task One"))
           (buffer (find-file-noselect test-file)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (should-not (buffer-modified-p)))
            (let ((result (org-mcp-test--call-clock-add
                           link "2026-01-01T10:00:00" "2026-01-01T11:00:00")))
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
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\n"
   "\\* TODO Task Two\n"
   "\\'")
  "Regex matching the complete buffer after clock-add on a modified buffer.
The buffer also keeps the unsaved Task Two edit made before the call.")

(defconst org-mcp-test--clock-in-modified-buffer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
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
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
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
    (let* ((link (org-mcp-test--file-link test-file "*Task One"))
           (buffer (find-file-noselect test-file)))
      (unwind-protect
          (progn
            ;; Dirty the buffer with an unrelated edit
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "\n* TODO Task Two\n")
              (should (buffer-modified-p)))
            (let ((result (org-mcp-test--call-clock-add
                           link "2026-01-01T10:00:00" "2026-01-01T11:00:00")))
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
    (let* ((link (org-mcp-test--file-link test-file "*Task One"))
           (result (org-mcp-test--call-clock-in link "2026-01-01T10:00:00")))
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
    (let* ((link (org-mcp-test--file-link test-file "*Task One"))
           (buffer (find-file-noselect test-file)))
      (unwind-protect
          (progn
            ;; Dirty the buffer with an unrelated edit
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "\n* TODO Task Two\n")
              (should (buffer-modified-p)))
            (let ((result (org-mcp-test--call-clock-in
                           link "2026-01-01T10:00:00")))
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
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\n"
   ":END:\n"
   "\\* TODO Task Two\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2025-12-30 [A-Za-z]\\{2,3\\} 09:00\\]\n"
   ":END:\n"
   "\\'")
  "After resolve=true on Task One, Task Two's dangling CLOCK survives.")

(defconst org-mcp-test--clock-in-keeps-dangling-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\n"
   "CLOCK: \\[2025-12-30 [A-Za-z]\\{2,3\\} 09:00\\]\n"
   ":END:\n"
   "\\'")
  "File contents after clock-in without resolve over a dangling CLOCK.")

(defconst org-mcp-test--clock-in-over-dangling-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\n"
   "CLOCK: \\[2025-12-30 [A-Za-z]\\{2,3\\} 09:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\] => 49:00\n"
   ":END:\n"
   "\\'")
  "File contents after clock-in closes the running clock under the heading.
The only open CLOCK is the running clock; clock_out names it, and it is
closed at the new clock's start.")

(defconst org-mcp-test--clock-running-elsewhere-content
  "* TODO Running Task\n:LOGBOOK:\nCLOCK: [2026-01-01 Thu 09:00]\n:END:\n"
  "File whose Running Task holds the Emacs clock in the resolve tests.")

(defconst org-mcp-test--clock-running-elsewhere-closed-regex
  (concat
   "\\`\\* TODO Running Task\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 09:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\] =>  1:00\n"
   ":END:\n"
   "\\'")
  "Regex matching the Running Task file once clock-in at 10:00 closed it.")

(defun org-mcp-test--check-clock-in-resolve
    (content resolve expected-regex resolved)
  "Clock in to Task One of a file holding CONTENT with RESOLVE, and check it.
Meanwhile the Emacs clock runs on Running Task in another allowed file,
so every open CLOCK line in CONTENT is dangling as Org defines it.  The
call names that clock in clock_out and clocks in at 10:00, which closes
it.  The file must then match EXPECTED-REGEX, and the response report
RESOLVED deleted clocks, or nil for none."
  (org-mcp-test--with-temp-org-files
      ((test-file content)
       (running-file org-mcp-test--clock-running-elsewhere-content))
    (org-mcp-test--with-session-clock running-file
      (let ((result
             (org-mcp-test--call-clock-in
              (org-mcp-test--file-link test-file "*Task One")
              "2026-01-01T10:00:00" resolve
              (org-mcp-test--file-link running-file "*Running Task"))))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'resolved result) resolved))
        (org-mcp-test--verify-file-matches test-file expected-regex)
        (org-mcp-test--verify-file-matches
         running-file org-mcp-test--clock-running-elsewhere-closed-regex)))))

(ert-deftest org-mcp-test-clock-in-resolve-no-dangling ()
  "Test clock-in with resolve=true on a heading with no dangling clocks."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let* ((link (org-mcp-test--file-link test-file "*Task One"))
           (result (org-mcp-test--call-clock-in
                    link "2026-01-01T10:00:00" "true")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'clocked_in result) t))
      ;; The `resolved' key is only present when clocks were deleted.
      (should (null (assq 'resolved result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-in-expected-regex))))

(ert-deftest org-mcp-test-clock-in-resolve-one-dangling ()
  "Test clock-in with resolve=true deletes one dangling CLOCK and collapses drawer."
  (org-mcp-test--check-clock-in-resolve
   org-mcp-test--clock-resolve-one-dangling-content "true"
   org-mcp-test--clock-in-expected-regex 1))

(ert-deftest org-mcp-test-clock-in-resolve-multi-dangling ()
  "Test clock-in with resolve=true deletes multiple dangling CLOCK entries."
  (org-mcp-test--check-clock-in-resolve
   org-mcp-test--clock-resolve-multi-dangling-content "true"
   org-mcp-test--clock-in-expected-regex 2))

(ert-deftest org-mcp-test-clock-in-resolve-mixed ()
  "Test clock-in with resolve=true deletes dangling but preserves closed."
  (org-mcp-test--check-clock-in-resolve
   org-mcp-test--clock-resolve-mixed-content "true"
   org-mcp-test--clock-in-resolve-mixed-expected-regex 1))

(ert-deftest org-mcp-test-clock-in-resolve-scoped-to-subtree ()
  "Test resolve=true deletes no dangling clock in a sibling heading.
Task Two's dangling CLOCK lies outside Task One, the heading clocked in
to, so it survives."
  (org-mcp-test--check-clock-in-resolve
   org-mcp-test--clock-resolve-other-heading-content "true"
   org-mcp-test--clock-in-resolve-other-heading-expected-regex nil))

(ert-deftest org-mcp-test-clock-in-resolve-true-forms ()
  "Test clock-in reads resolve given as JSON true and as \"true\" alike.
Each deletes the dangling CLOCK under the heading."
  (dolist (resolve '(t "true"))
    (org-mcp-test--check-clock-in-resolve
     org-mcp-test--clock-resolve-one-dangling-content resolve
     org-mcp-test--clock-in-expected-regex 1)))

(ert-deftest org-mcp-test-clock-in-resolve-false-forms ()
  "Test clock-in reads resolve given as false, \"false\" or \"\" as false.
The dangling CLOCK under the heading is then kept."
  (dolist (resolve '(:json-false "false" ""))
    (org-mcp-test--check-clock-in-resolve
     org-mcp-test--clock-resolve-one-dangling-content resolve
     org-mcp-test--clock-in-keeps-dangling-expected-regex nil)))

(ert-deftest org-mcp-test-clock-in-resolve-closes-running-clock ()
  "Test resolve=true closes the running clock under the heading.
The running clock is never deleted, whether the Emacs clock runs on it
or it is the open CLOCK line found with no Emacs clock running:
clock_out names it, and it is closed at the new clock's start.
resolve deletes only the dangling clocks left, here none."
  (dolist (emacs-clock '(nil t))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--clock-resolve-one-dangling-content))
      (let ((link (org-mcp-test--file-link test-file "*Task One")))
        (cl-flet
         ((clock-in
           ()
           (let ((result
                  (org-mcp-test--call-clock-in
                   link "2026-01-01T10:00:00" "true" link)))
             (should (equal (alist-get 'clocked_in result) t))
             (should (null (assq 'resolved result))))))
         (if emacs-clock
             (org-mcp-test--with-session-clock test-file
               (clock-in)
               (should-not (org-clock-is-active)))
           (clock-in)))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--clock-in-over-dangling-expected-regex)))))

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

(ert-deftest org-mcp-test-clock-out-failed-after-save-hook ()
  "Test a save failing after the file holds the close says the close was made.
A buffer-local `after-save-hook' fails once the file holds the closed
CLOCK line.  The close stays in buffer and file, the buffer reads
unmodified, and the error says the change was made, so a client does
not close the clock a second time."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (let ((buffer (find-file-noselect test-file)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (add-hook 'after-save-hook
                        (lambda () (error "Save hook failed"))
                        nil t))
            (org-mcp-test--call-tool-refused
             "org-clock-out" '((end_time . "2026-01-01T11:00:00"))
             "\\`The change was made and saved, but .*Save hook failed")
            (org-mcp-test--verify-file-matches
             test-file org-mcp-test--clock-out-expected-regex)
            (org-mcp-test--verify-buffer-matches
             buffer org-mcp-test--clock-out-expected-regex)
            (org-mcp-test--verify-no-modified-buffer test-file)
            (with-current-buffer buffer
              (kill-local-variable 'after-save-hook)))
        (kill-buffer buffer)))))

;;; Tests for org-clock-into-drawer behavior (nil and custom drawer name)

(defconst org-mcp-test--clock-task-with-open-clock-no-drawer
  "* TODO Task One\nCLOCK: [2026-01-01 Thu 10:00]\n"
  "Org file content with an unclosed CLOCK bare under the heading.
Used for clock-out tests when `org-clock-into-drawer' is nil.")

(defconst org-mcp-test--clock-add-no-drawer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   "\\'")
  "File contents after clock-add with `org-clock-into-drawer' nil.
The CLOCK line appears bare under the heading -- no LOGBOOK drawer.")

(defconst org-mcp-test--clock-in-no-drawer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\n"
   "\\'")
  "File contents after clock-in with `org-clock-into-drawer' nil.")

(defconst org-mcp-test--clock-out-no-drawer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   "\\'")
  "File contents after clock-out with `org-clock-into-drawer' nil.")

(defconst org-mcp-test--clock-add-custom-drawer-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":WORK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\'")
  "File contents after clock-add with custom `org-clock-into-drawer' name.")

(ert-deftest org-mcp-test-clock-add-no-drawer ()
  "Test clock-add inserts bare CLOCK when `org-clock-into-drawer' is nil."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let ((org-clock-into-drawer nil))
      (let* ((link (org-mcp-test--file-link test-file "*Task One"))
             (result (org-mcp-test--call-clock-add
                      link "2026-01-01T10:00:00" "2026-01-01T11:00:00")))
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
      (let* ((link (org-mcp-test--file-link test-file "*Task One"))
             (result (org-mcp-test--call-clock-in
                      link "2026-01-01T10:00:00")))
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
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\* TODO Task Two\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\]\n"
   ":END:\n"
   "\\'")
  "Regex matching file after clock-in to Task Two closes Task One.")

(defconst org-mcp-test--clock-in-close-different-file-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\'")
  "Regex matching file after active clock closed by clock-in to different file.")

(ert-deftest org-mcp-test-clock-in-closes-active-same-file ()
  "Test clock-in closes an active clock in the same file."
  (org-mcp-test--with-temp-org-files
      ((file-1 org-mcp-test--clock-in-close-same-file-content))
    (let* ((link-1 (org-mcp-test--file-link file-1 "*Task One"))
           (link-2 (org-mcp-test--file-link file-1 "*Task Two"))
           (result-1 (org-mcp-test--call-clock-in
                      link-1 "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result-1) t))
      (should (equal (alist-get 'clocked_in result-1) t))
      ;; Clock in to Task Two, naming Task One's clock to close first
      (let ((result-2 (org-mcp-test--call-clock-in
                       link-2 "2026-01-01T11:00:00" nil link-1)))
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
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\* TODO Task Two\n"
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
                           (org-mcp-test--file-link test-file "*Task Two")
                           "2026-01-01T11:00:00" nil
                           (org-mcp-test--file-link test-file "*Task One"))))
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
    (let* ((link-1 (org-mcp-test--file-link file-1 "*Task One"))
           (link-2 (org-mcp-test--file-link file-2 "*Task One"))
           (result-1 (org-mcp-test--call-clock-in
                      link-1 "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result-1) t))
      (should (equal (alist-get 'clocked_in result-1) t))
      ;; Clock in to file-2, naming file-1's clock to close first
      (let ((result-2 (org-mcp-test--call-clock-in
                       link-2 "2026-01-01T11:00:00" nil link-1)))
        (should (equal (alist-get 'success result-2) t))
        (should (eq (alist-get 'saved result-2) t))
        (should (equal (alist-get 'clocked_in result-2) t))
        (org-mcp-test--verify-file-matches
         file-1
         org-mcp-test--clock-in-close-different-file-expected-regex)
        (org-mcp-test--verify-file-matches
         file-2 org-mcp-test--clock-in-at-eleven-expected-regex)))))

(defconst org-mcp-test--clock-in-at-eleven-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
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
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
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
                           (org-mcp-test--file-link file-2 "*Task One")
                           "2026-01-01T11:00:00" nil
                           (org-mcp-test--file-link file-1 "*Task One"))))
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
    (let* ((link-1 (org-mcp-test--file-link file-1 "*Task One"))
           (link-2 (org-mcp-test--file-link file-2 "*Task One"))
           (result-1 (org-mcp-test--call-clock-in
                      link-1 "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result-1) t))
      ;; Clock in to file-2 at 12:00 — should close file-1 at 12:00
      (let ((result-2 (org-mcp-test--call-clock-in
                       link-2 "2026-01-01T12:00:00" nil link-1)))
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

(defconst org-mcp-test--clock-task-with-spaced-open-clock
  "* TODO Task One\n:LOGBOOK:\nCLOCK:  [2026-01-01 Thu 10:00]\n:END:\n"
  "Org file whose open CLOCK line has two spaces after `CLOCK:'.")

(defconst org-mcp-test--clock-out-spaced-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK:  \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\'")
  "File contents after clock-out closes a CLOCK line spaced that way.")

(ert-deftest org-mcp-test-clock-out-spaced-clock-line ()
  "Test clock-out closes every CLOCK line Org itself reads as a clock.
The session clock runs on a line whose timestamp is two spaces after
`CLOCK:'.  Org's clock-out reads it, so the line keeps its spacing and
the close is appended to it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-spaced-open-clock))
    (org-mcp-test--with-session-clock test-file
      (let ((result (org-mcp-test--call-clock-out "2026-01-01T11:00:00")))
        (should (equal (alist-get 'clocked_out result) t)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-out-spaced-expected-regex))))

(defconst org-mcp-test--clock-out-zero-time-content
  (concat
   "* TODO Task One\n"
   "* TODO Task Two\n"
   ":LOGBOOK:\n"
   "CLOCK: [2026-01-01 Thu 10:00]\n"
   ":END:\n"
   "* TODO Task Three\n")
  "Three headings, the middle one holding an open CLOCK.
A heading on either side of it, so a response that reads the file from
its start, or past the drawer, names the wrong one.")

(defconst org-mcp-test--clock-out-zero-time-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   "\\* TODO Task Two\n"
   "\\* TODO Task Three\n"
   "\\'")
  "File contents once Org took away a closed clock of no length.
The CLOCK line goes, the drawer it empties goes with it, and the
headings around them stand.")

(ert-deftest org-mcp-test-clock-out-removes-zero-time-clock ()
  "Test Org's clock-out settings decide what the file holds after a close.
With `org-clock-out-remove-zero-time-clocks', closing a clock at its
own start takes its CLOCK line away, and the drawer it empties with
it, leaving the headings around it alone.  The response reports the
close all the same, with a duration of 0:00, and links to the heading
the clock ran on."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-out-zero-time-content))
    (let ((org-clock-out-remove-zero-time-clocks t))
      (org-mcp-test--with-session-clock test-file
        (let ((result
               (org-mcp-test--call-clock-out "2026-01-01T10:00:00")))
          (should (equal (alist-get 'clocked_out result) t))
          (should (equal (alist-get 'duration result) "0:00"))
          (should (equal (alist-get 'heading result) "Task Two"))
          (should
           (equal (alist-get 'link result)
                  (org-mcp-test--file-link test-file "*Task Two"))))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--clock-out-zero-time-expected-regex)))))

(defconst org-mcp-test--clock-out-switch-state-expected-regex
  (concat
   "\\`\\* DONE Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\'")
  "File contents once org-clock-out-switch-to-state moved Task One to DONE.")

(ert-deftest org-mcp-test-clock-out-switches-todo-state ()
  "Test `org-clock-out-switch-to-state' rewrites the heading closed out of.
The keyword Org sets reaches the file, while the response reports the
close alone and names the heading by its title."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-clock-out-switch-to-state "DONE"))
      (org-mcp-test--with-session-clock test-file
        (let ((result
               (org-mcp-test--call-clock-out "2026-01-01T11:00:00")))
          (should (equal (alist-get 'clocked_out result) t))
          (should (equal (alist-get 'heading result) "Task One")))
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--clock-out-switch-state-expected-regex)))))

(ert-deftest org-mcp-test-clock-add-custom-drawer ()
  "Test clock-add uses custom drawer name from `org-clock-into-drawer'."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let ((org-clock-into-drawer "WORK"))
      (let* ((link (org-mcp-test--file-link test-file "*Task One"))
             (result (org-mcp-test--call-clock-add
                      link "2026-01-01T10:00:00" "2026-01-01T11:00:00")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'added result) t))
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--clock-add-custom-drawer-expected-regex)))))

;;; Tests for org-clock-active

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

(ert-deftest org-mcp-test-clock-get-active-none ()
  "Test org-clock-active reports no active clock."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let ((result (org-mcp-test--call-clock-get-active)))
      (should (eq (alist-get 'active result) :json-false)))))

(ert-deftest org-mcp-test-clock-get-active-open-clock ()
  "Test org-clock-active finds an open CLOCK in an allowed file."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (let ((result (org-mcp-test--call-clock-get-active)))
      (should (eq (alist-get 'active result) t))
      (should (equal (alist-get 'heading result) "Task One"))
      (should
       (equal (alist-get 'start result) "2026-01-01 Thu 10:00")))))

(ert-deftest org-mcp-test-clock-get-active-ignores-closed ()
  "Test org-clock-active ignores files with only closed clocks."
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
The entry is returned with `allowed' nil, and org-clock-active
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

(ert-deftest org-mcp-test-clock-in-refuses-session-clock-outside-allowed-files ()
  "Test clock-in refuses while the session clock runs outside the allowed files.
org-mcp tells a client nothing about that clock, so no clock_out can
name it: the call is refused with or without one.  Neither file, the
buffer of the running clock, nor the running clock changes."
  (org-mcp-test--with-temp-org-files
      ((allowed-file org-mcp-test--clock-task-content)
       (outside-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-mcp-allowed-files (list allowed-file)))
      (org-mcp-test--with-session-clock outside-file
        (let ((position (marker-position org-clock-marker)))
          (dolist (clock-out
                   (list nil (org-mcp-test--file-link outside-file "*Task One")))
            (should
             (string-match-p
              "\\`A clock is running in a file outside the allowed files\\.  \
Ask the user to clock out of it before clocking in\\'"
              (org-mcp-test--call-tool-expecting-error
               allowed-file "org-clock-in"
               `((link . ,(org-mcp-test--file-link allowed-file "*Task One"))
                 (start_time . "2026-01-01T11:00:00")
                 ,@(when clock-out `((clock_out . ,clock-out))))))))
          (should (string= (org-mcp-test--read-file outside-file)
                           org-mcp-test--clock-task-with-open-clock))
          (org-mcp-test--verify-no-modified-buffer outside-file)
          (should (eq (org-clock-is-active) (find-buffer-visiting outside-file)))
          (should (= (marker-position org-clock-marker) position)))))))

(ert-deftest org-mcp-test-clock-out-refuses-session-clock-outside-allowed-files ()
  "Test clock-out refuses while the session clock runs outside the allowed files.
The refusal is the whole message, so neither the file, the heading nor
the start of that clock reaches the client, and org-mcp writes no file
outside the allowed files.  Neither the file, the buffer of the
running clock, nor the running clock changes."
  (org-mcp-test--with-temp-org-files
      ((allowed-file org-mcp-test--clock-task-content)
       (outside-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-mcp-allowed-files (list allowed-file)))
      (org-mcp-test--with-session-clock outside-file
        (let ((position (marker-position org-clock-marker)))
          (should
           (string-match-p
            "\\`A clock is running in a file outside the allowed files\\.  \
Ask the user to clock out of it in Emacs\\'"
            (org-mcp-test--call-tool-expecting-error
             outside-file "org-clock-out"
             '((end_time . "2026-01-01T11:00:00")))))
          (should (string= (org-mcp-test--read-file outside-file)
                           org-mcp-test--clock-task-with-open-clock))
          (org-mcp-test--verify-no-modified-buffer outside-file)
          (should (eq (org-clock-is-active) (find-buffer-visiting outside-file)))
          (should (= (marker-position org-clock-marker) position)))))))

(defconst org-mcp-test--clock-in-after-clock-out-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 10:30\\] =>  0:30\n"
   ":END:\n"
   "\\* TODO Task Two\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\]\n"
   ":END:\n"
   "\\'")
  "Regex matching the file after clock-out of Task One and clock-in to Two.
Task One's clock keeps the end org-clock-out gave it.")

(ert-deftest org-mcp-test-clock-in-after-clock-out-of-session-clock ()
  "Test clock-out stops the Emacs clock, so the next clock-in needs no clock_out.
Closing Task One's CLOCK line through Org stops the Emacs clock the
line belongs to, leaving `org-clock-marker' unset rather than pointing
at a closed clock.  No clock runs then, so clocking in to Task Two
needs no clock_out, and Task One keeps its 10:30 end."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-session-clock test-file
      (org-mcp-test--call-clock-out "2026-01-01T10:30:00")
      (should-not (org-clock-is-active))
      (let ((result (org-mcp-test--call-clock-in
                     (org-mcp-test--file-link test-file "*Task Two")
                     "2026-01-01T11:00:00")))
        (should (equal (alist-get 'clocked_in result) t)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-in-after-clock-out-expected-regex))))

(ert-deftest org-mcp-test-clock-in-after-session-clock-closed-outside ()
  "Test clock-in proceeds once the Emacs clock outside the allowed files ends.
The Emacs clock points at a CLOCK line outside the allowed files that
has since been closed, so no clock runs and clock-in needs no
clock_out."
  (org-mcp-test--with-temp-org-files
      ((allowed-file org-mcp-test--clock-task-content)
       (outside-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-mcp-allowed-files (list allowed-file)))
      (org-mcp-test--with-session-clock outside-file
        (with-current-buffer (marker-buffer org-clock-marker)
          (save-excursion
            (goto-char org-clock-marker)
            (insert "--[2026-01-01 Thu 10:30] =>  0:30"))
          (save-buffer))
        (let ((result (org-mcp-test--call-clock-in
                       (org-mcp-test--file-link allowed-file "*Task One")
                       "2026-01-01T11:00:00")))
          (should (equal (alist-get 'clocked_in result) t)))
        (org-mcp-test--verify-file-matches
         allowed-file org-mcp-test--clock-in-at-eleven-expected-regex)))))

(ert-deftest org-mcp-test-clock-in-close-failed-after-save-hook ()
  "Test a save failing after the close reached the file says the close was made.
A buffer-local `after-save-hook' fails once the running clock's file
holds the close.  The clock stays closed there, in buffer and on disk,
and the Emacs clock stays stopped; no new clock opens, and the error
says the clock was closed and saved, so a client does not close it a
second time."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content)
       (running-file org-mcp-test--clock-running-elsewhere-content))
    (org-mcp-test--with-session-clock running-file
      (let ((buffer (find-buffer-visiting running-file)))
        (with-current-buffer buffer
          (add-hook 'after-save-hook
                    (lambda () (error "Save hook failed"))
                    nil t))
        (org-mcp-test--call-tool-refused
         "org-clock-in"
         `((link . ,(org-mcp-test--file-link test-file "*Task One"))
           (start_time . "2026-01-01T10:00:00")
           (clock_out
            . ,(org-mcp-test--file-link running-file "*Running Task")))
         "\\`The running clock was closed and saved, but .*Save hook failed"
         test-file)
        (with-current-buffer buffer
          (kill-local-variable 'after-save-hook))
        (org-mcp-test--verify-file-matches
         running-file org-mcp-test--clock-running-elsewhere-closed-regex)
        (org-mcp-test--verify-buffer-matches
         buffer org-mcp-test--clock-running-elsewhere-closed-regex)
        (org-mcp-test--verify-no-modified-buffer running-file)
        (should-not (org-clock-is-active))))))

(ert-deftest org-mcp-test-clock-in-close-failed-save ()
  "Test a save failing before it wrote the file says the close is unsaved.
A buffer-local `write-contents-functions' fails, so the close of the
running clock reaches its buffer alone.  The error says the clock was
closed but not saved, the buffer keeps the close and stays modified,
the file keeps its open CLOCK line, and no new clock opens."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content)
       (running-file org-mcp-test--clock-running-elsewhere-content))
    (org-mcp-test--with-session-clock running-file
      (let ((buffer (find-buffer-visiting running-file)))
        (with-current-buffer buffer
          (add-hook 'write-contents-functions
                    (lambda () (error "Save hook failed"))
                    nil t))
        (org-mcp-test--call-tool-refused
         "org-clock-in"
         `((link . ,(org-mcp-test--file-link test-file "*Task One"))
           (start_time . "2026-01-01T10:00:00")
           (clock_out
            . ,(org-mcp-test--file-link running-file "*Running Task")))
         "\\`The running clock was closed but not saved: .*Save hook failed"
         test-file)
        (with-current-buffer buffer
          (kill-local-variable 'write-contents-functions)
          (should (buffer-modified-p)))
        (org-mcp-test--verify-buffer-matches
         buffer org-mcp-test--clock-running-elsewhere-closed-regex)
        (should (string= (org-mcp-test--read-file running-file)
                         org-mcp-test--clock-running-elsewhere-content))
        (should-not (org-clock-is-active))))))

(ert-deftest org-mcp-test-clock-in-refuses-without-clock-out ()
  "Test clock-in refuses while a clock runs and clock_out is not sent.
The refusal names the running clock's heading by title and link, so
the client can ask the user, and neither the file nor the running
clock changes.  A blank clock_out, false, \"\" or spaces, counts as
not sent."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-session-clock test-file
      (let ((position (marker-position org-clock-marker)))
        (dolist (clock-out '(nil :json-false "" "  "))
          (should
           (string-match-p
            (format
             "\\`conflict: A clock is running on 'Task One' (%s) since \
2026-01-01 Thu 10:00\\.  Ask the user whether to clock out of it, then \
send its link as clock_out\\'"
             (regexp-quote (org-mcp-test--file-link test-file "*Task One")))
            (org-mcp-test--call-tool-expecting-error
             test-file "org-clock-in"
             `((link . ,(org-mcp-test--file-link test-file "*Task Two"))
               (start_time . "2026-01-01T11:00:00")
               ,@(when clock-out `((clock_out . ,clock-out))))))))
        (org-mcp-test--verify-no-modified-buffer test-file)
        (should (eq (org-clock-is-active) (find-buffer-visiting test-file)))
        (should (= (marker-position org-clock-marker) position))))))

(ert-deftest org-mcp-test-clock-in-refuses-mismatched-clock-out ()
  "Test clock-in refuses a clock_out that does not name the running clock.
Another heading of the file, a heading that does not exist, the whole
file, and a heading of another file are each refused with the running
clock's heading named, and nothing changes."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-in-close-same-file-open-clock-content)
       (other-file org-mcp-test--clock-task-content))
    (org-mcp-test--with-session-clock test-file
      (let ((position (marker-position org-clock-marker)))
        (dolist (clock-out
                 (list (org-mcp-test--file-link test-file "*Task Two")
                       (org-mcp-test--file-link test-file "*Nope")
                       (concat "file:" test-file)
                       (org-mcp-test--file-link other-file "*Task One")))
          (should
           (string-match-p
            (format
             "\\`conflict: clock_out does not name the running clock: \
%s\\.  The clock runs on 'Task One' (%s) since 2026-01-01 Thu \
10:00\\'"
             (regexp-quote clock-out)
             (regexp-quote (org-mcp-test--file-link test-file "*Task One")))
            (org-mcp-test--call-tool-expecting-error
             test-file "org-clock-in"
             `((link . ,(org-mcp-test--file-link test-file "*Task Two"))
               (start_time . "2026-01-01T11:00:00")
               (clock_out . ,clock-out))))))
        (should (string= (org-mcp-test--read-file other-file)
                         org-mcp-test--clock-task-content))
        (org-mcp-test--verify-no-modified-buffer test-file)
        (should (eq (org-clock-is-active) (find-buffer-visiting test-file)))
        (should (= (marker-position org-clock-marker) position))))))

(ert-deftest org-mcp-test-clock-in-clock-out-closes-session-clock ()
  "Test clock-in closes the session clock clock_out names, then clocks in.
Task One's clock is closed at the new clock's start, Task Two's clock
is opened, and the Emacs clock no longer runs."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-session-clock test-file
      (let ((result (org-mcp-test--call-clock-in
                     (org-mcp-test--file-link test-file "*Task Two")
                     "2026-01-01T11:00:00" nil
                     (org-mcp-test--file-link test-file "*Task One"))))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'clocked_in result) t)))
      (should-not (org-clock-is-active))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-in-close-same-file-expected-regex))))

(defconst org-mcp-test--clock-out-id "3f6c2a8e-5d41-4b7a-9e20-6c1d8f0b4a57"
  "ID of the clocked heading in `org-mcp-test--clock-open-with-id-content'.")

(defconst org-mcp-test--clock-open-with-id-content
  (concat
   "* TODO Task One\n"
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--clock-out-id "\n"
   ":END:\n"
   ":LOGBOOK:\n"
   "CLOCK: [2026-01-01 Thu 10:00]\n"
   ":END:\n"
   "* TODO Task Two\n")
  "File with an open clock on Task One, which has an ID, and a Task Two.")

(defconst org-mcp-test--clock-in-over-id-clock-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":PROPERTIES:\n"
   ":ID: +" (regexp-quote org-mcp-test--clock-out-id) "\n"
   ":END:\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\* TODO Task Two\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\]\n"
   ":END:\n"
   "\\'")
  "Regex matching the ID file after clock-in to Task Two closes Task One.")

(ert-deftest org-mcp-test-clock-in-clock-out-id-in-running-file ()
  "Test an id: clock_out is looked up in the running clock's file.
The ID is found there without Org's ID index being consulted, and the
clock is closed."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-open-with-id-content))
    (org-mcp-test--with-session-clock test-file
      (org-mcp-test--without-id-index
        (let ((result (org-mcp-test--call-clock-in
                       (org-mcp-test--file-link test-file "*Task Two")
                       "2026-01-01T11:00:00" nil
                       (concat "id:" org-mcp-test--clock-out-id))))
          (should (equal (alist-get 'clocked_in result) t))))
      (should-not (org-clock-is-active))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-in-over-id-clock-expected-regex))))

(ert-deftest org-mcp-test-clock-in-bad-target-keeps-running-clock ()
  "Test clock-in to a link that names no heading changes nothing.
clock_out names the running clock, but the heading to clock in to is
resolved before any clock is closed, so the refusal leaves the file,
its buffer and the running clock as they were."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-session-clock test-file
      (let ((position (marker-position org-clock-marker)))
        (should
         (string-match-p
          "\\`Cannot resolve link "
          (org-mcp-test--call-tool-expecting-error
           test-file "org-clock-in"
           `((link . ,(org-mcp-test--file-link test-file "*Nope"))
             (start_time . "2026-01-01T11:00:00")
             (clock_out
              . ,(org-mcp-test--file-link test-file "*Task One"))))))
        (org-mcp-test--verify-no-modified-buffer test-file)
        (should (eq (org-clock-is-active) (find-buffer-visiting test-file)))
        (should (= (marker-position org-clock-marker) position))))))

(ert-deftest org-mcp-test-clock-in-refuses-start-before-running-clock ()
  "Test clock-in refuses a start before the running clock's start.
Closing Task One's clock, started at 10:00, at 09:00 would give it a
negative duration, so the call is refused and nothing changes."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-session-clock test-file
      (let ((position (marker-position org-clock-marker)))
        (should
         (string-match-p
          "\\`Start time \\[2026-01-01 [A-Za-z]\\{2,3\\} 09:00\\] is \
before the running clock's start \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\\'"
          (org-mcp-test--call-tool-expecting-error
           test-file "org-clock-in"
           `((link . ,(org-mcp-test--file-link test-file "*Task Two"))
             (start_time . "2026-01-01T09:00:00")
             (clock_out
              . ,(org-mcp-test--file-link test-file "*Task One"))))))
        (org-mcp-test--verify-no-modified-buffer test-file)
        (should (eq (org-clock-is-active) (find-buffer-visiting test-file)))
        (should (= (marker-position org-clock-marker) position))))))

(defconst org-mcp-test--clock-duplicate-titles-content
  (concat
   "* Project A\n"
   "** Meeting\n"
   "* Project B\n"
   "** Meeting\n"
   ":LOGBOOK:\n"
   "CLOCK: [2026-01-01 Thu 10:00]\n"
   ":END:\n"
   "* Review\n")
  "File whose second Meeting, titled like the first, holds an open clock.")

(defconst org-mcp-test--clock-duplicate-titles-expected-regex
  (concat
   "\\`\\* Project A\n"
   "\\*\\* Meeting\n"
   "\\* Project B\n"
   "\\*\\* Meeting\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\* Review\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\]\n"
   ":END:\n"
   "\\'")
  "Regex matching the file once the second Meeting's clock is closed.
Review holds the new clock, and the first Meeting is untouched.")

(ert-deftest org-mcp-test-clock-in-clock-out-duplicate-title ()
  "Test clock_out accepts the link the refusal names for a duplicate title.
The clock runs on the second of two headings titled Meeting.  Its title
link finds the first one, yet it is the link the refusal names, so
sending it back as clock_out, bracketed here, closes the running clock."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-duplicate-titles-content))
    (org-mcp-test--with-session-clock test-file
      (let ((meeting (org-mcp-test--file-link test-file "*Meeting"))
            (review (org-mcp-test--file-link test-file "*Review")))
        (should
         (string-match-p
          (format "\\`conflict: A clock is running on 'Meeting' (%s) since "
                  (regexp-quote meeting))
          (org-mcp-test--call-tool-expecting-error
           test-file "org-clock-in"
           `((link . ,review) (start_time . "2026-01-01T11:00:00")))))
        (let ((result
               (org-mcp-test--call-clock-in
                review "2026-01-01T11:00:00" nil
                (format "[[%s][Meeting]]" meeting))))
          (should (equal (alist-get 'clocked_in result) t))))
      (should-not (org-clock-is-active)))
    (org-mcp-test--verify-file-matches
     test-file org-mcp-test--clock-duplicate-titles-expected-regex)))

(ert-deftest org-mcp-test-clock-in-refuses-clock-out-without-running-clock ()
  "Test clock-in refuses a clock_out while no clock runs.
There is no clock for clock_out to name, so the call changes nothing."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let ((link (org-mcp-test--file-link test-file "*Task One")))
      (should
       (string-match-p
        (format
         "\\`conflict: clock_out names a clock to close, but no clock \
is running: %s\\'"
         (regexp-quote link))
        (org-mcp-test--call-tool-expecting-error
         test-file "org-clock-in"
         `((link . ,link)
           (start_time . "2026-01-01T10:00:00")
           (clock_out . ,link)))))
      (org-mcp-test--verify-no-modified-buffer test-file))))

(ert-deftest org-mcp-test-clock-in-refuses-unknown-resolve ()
  "Test clock-in refuses a resolve that is neither true nor false."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (should
     (string-match-p
      "\\`resolve must be true or false: \"yes\"\\'"
      (org-mcp-test--call-tool-expecting-error
       test-file "org-clock-in"
       `((link . ,(org-mcp-test--file-link test-file "*Task One"))
         (resolve . "yes")))))))

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

(defmacro org-mcp-test--with-narrowing (file title &rest body)
  "Run BODY with the buffer visiting FILE narrowed to the heading TITLE.
The buffer is narrowed with `org-narrow-to-subtree', as a user would.
Afterwards the buffer must still be narrowed to the same region.  A
buffer this macro opened is killed; one that already visited FILE is
left to whoever opened it."
  (declare (indent 2) (debug t))
  `(let* ((opened (not (find-buffer-visiting ,file)))
          (buffer (find-file-noselect ,file))
          (start nil)
          (end nil))
     (unwind-protect
         (progn
           (with-current-buffer buffer
             (goto-char (point-min))
             (re-search-forward
              (concat "^\\*+ .*" (regexp-quote ,title)))
             (org-narrow-to-subtree)
             (setq start (point-min))
             (setq end (point-max)))
           ,@body
           (with-current-buffer buffer
             (should (buffer-narrowed-p))
             (should (= (point-min) start))
             (should (= (point-max) end))))
       (when opened
         (kill-buffer buffer)))))

(ert-deftest org-mcp-test-clock-get-active-dangling-ignores-narrowing ()
  "Test clock-get-active finds a dangling clock outside the user's narrowing.
With no clock running, the buffer is narrowed to Task Two, and Task
One's open clock is still found; the buffer stays narrowed to Task Two."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-in-close-same-file-open-clock-content))
    (should-not (org-clock-is-active))
    (org-mcp-test--with-narrowing test-file "Task Two"
      (let ((result (org-mcp-test--call-clock-get-active)))
        (should (eq (alist-get 'active result) t))
        (should (equal (alist-get 'heading result) "Task One"))
        (should
         (equal (alist-get 'start result) "2026-01-01 Thu 10:00"))))))

(ert-deftest org-mcp-test-clock-get-active-session-clock-ignores-narrowing ()
  "Test clock-get-active finds the session clock outside the user's narrowing.
The Emacs clock runs on Task One while its buffer is narrowed to Task
Two; the clock is still reported, and the buffer stays narrowed."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-session-clock test-file
      (org-mcp-test--with-narrowing test-file "Task Two"
        (let ((result (org-mcp-test--call-clock-get-active)))
          (should (eq (alist-get 'active result) t))
          (should (equal (alist-get 'heading result) "Task One"))
          (should
           (equal (alist-get 'start result) "2026-01-01 Thu 10:00")))))))

;;; Tests for org-clock-dangling

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

(ert-deftest org-mcp-test-clock-find-dangling-ignores-narrowing ()
  "Test clock-find-dangling finds a clock outside the user's narrowing.
The buffer is narrowed to Task Two, and Task One's open clock is still
found; the buffer stays narrowed to Task Two."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-narrowing test-file "Task Two"
      (let* ((result (org-mcp-test--call-clock-find-dangling))
             (clocks (alist-get 'open_clocks result)))
        (should (equal (alist-get 'total result) 1))
        (should (equal (alist-get 'heading (aref clocks 0)) "Task One"))
        (should
         (equal (alist-get 'start (aref clocks 0))
                "2026-01-01 Thu 10:00"))))))

;;; Tests for org-clock-delete

(defconst org-mcp-test--clock-delete-only-entry-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
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
    (let* ((link (org-mcp-test--file-link test-file "*Task One"))
           (result
            (org-mcp-test--call-clock-delete
             link "2026-01-01T10:00:00")))
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
    (let* ((link (org-mcp-test--file-link test-file "*Task One"))
           (result
            (org-mcp-test--call-clock-delete
             link "2026-01-01T10:00:00")))
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
   "\\'")
  "After deleting a bare CLOCK with no drawer, only the heading remains.")

(ert-deftest org-mcp-test-clock-delete-no-drawer ()
  "Test clock-delete works when CLOCK lines are bare (no drawer).
Exercises `org-mcp--clock-remove-empty-logbook' when
`org-clock-drawer-name' returns nil -- it must be a no-op."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-delete-no-drawer-content))
    (let ((org-clock-into-drawer nil))
      (let* ((link (org-mcp-test--file-link test-file "*Task One"))
             (result
              (org-mcp-test--call-clock-delete
               link "2026-01-01T10:00:00")))
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
    (let* ((link (org-mcp-test--file-link test-file "*Task"))
           (result
            (org-mcp-test--call-clock-delete
             link "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'deleted result) t)))))

(ert-deftest org-mcp-test-clock-delete-not-found-errors ()
  "Test clock-delete surfaces an MCP error when no entry matches."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-only-closed-content))
    (let ((link (org-mcp-test--file-link test-file "*Task One")))
      (org-mcp-test--call-tool-refused
       "org-clock-delete"
       `((link . ,link) (start . "2026-01-03T09:00:00"))
       "\\`No clock entry starting at \\[2026-01-03 [^]]+ 09:00\\] found\\'"
       test-file))))

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
    (let* ((link (org-mcp-test--file-link test-file "*Task One"))
           (result
            (org-mcp-test--call-clock-delete
             link "2026-01-01T10:00:00")))
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
   ":LOGBOOK:\n"
   "\n"
   ":END:\n"
   "\\'")
  "After deleting the CLOCK, a lone blank line keeps LOGBOOK intact.")

(defconst org-mcp-test--clock-delete-drops-blank-line-expected-regex
  "\\`\\* TODO Task One\n\\'"
  "After deleting the CLOCK, the blank-line-only LOGBOOK is gone.")

(ert-deftest org-mcp-test-clock-delete-drawer-with-blank-line ()
  "Test clock-delete leaves a blank-line-only LOGBOOK to Org's judgment.
`org-remove-empty-drawer-at' removes a drawer without contents.
Org 9.8's drawer parser records leading blank lines as
`:pre-blank', so a drawer holding only a blank line has no
contents and goes (ORG-NEWS 9.8, \"`org-element-drawer-parser'
assigns `:pre-blank' property\").  Earlier Org parses the blank
line as contents, so the drawer stays."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-delete-with-blank-line-content))
    (let* ((link (org-mcp-test--file-link test-file "*Task One"))
           (result
            (org-mcp-test--call-clock-delete
             link "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'deleted result) t))
      (org-mcp-test--verify-file-matches
       test-file
       (if (version< (org-version) "9.8")
           org-mcp-test--clock-delete-keeps-blank-line-expected-regex
         org-mcp-test--clock-delete-drops-blank-line-expected-regex)))))

;;; Tests for org-node-set-properties

(ert-deftest org-mcp-test-set-properties-new ()
  "Test setting a new property on a bare task."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (params `((link . ,link)
                     (after . ((EFFORT . "2:00")))))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-properties" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should
       (equal (alist-get 'link result)
              (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-set-properties-new))))

(ert-deftest org-mcp-test-set-properties-update ()
  "Test updating an existing property."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (let* ((link (org-mcp-test--file-link test-file "*Task with Properties"))
           (params `((link . ,link)
                     (after . ((EFFORT . "2:30")))))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-properties" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-set-properties-update))))

(ert-deftest org-mcp-test-set-properties-delete ()
  "Test deleting a property via null value."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (let* ((link (org-mcp-test--file-link test-file "*Task with Properties"))
           (params `((link . ,link)
                     (after . ((EFFORT)))))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-properties" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-set-properties-delete))))

(ert-deftest org-mcp-test-set-properties-booleans ()
  "Test booleans set properties to t and nil.
JSON false overwrites EFFORT with nil and does not delete it, as null
would.  JSON true writes t, and the strings \"t\" and \"nil\" are
written as given."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (let* ((params
            `((link
               .
               ,(org-mcp-test--file-link test-file "*Task with Properties"))
              (after
               .
               ((EFFORT . :json-false)
                (ENABLED . t)
                (LITERAL_T . "t")
                (LITERAL_NIL . "nil")))))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool "org-node-set-properties" params))))
      (should
       (equal
        (alist-get 'properties_set result)
        ["EFFORT" "ENABLED" "LITERAL_T" "LITERAL_NIL"]))
      (should-not (alist-get 'properties_deleted result))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-set-properties-booleans))))

(ert-deftest org-mcp-test-set-properties-forbid-special ()
  "Test that special properties are rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-node-set-properties" 1
                `((link . ,link)
                  (after . ((TODO . "DONE"))))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-set-properties-id-link ()
  "Test setting properties via an `id:' link."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((link (concat "id:" org-mcp-test--crud-test-id))
          (params `((link . ,link)
                    (after . ((EFFORT . "1:00")))))
          (result-text
           (mcp-server-lib-ert-call-tool "org-node-set-properties" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

(ert-deftest org-mcp-test-set-properties-id-and-custom-id ()
  "Test a client sets ID and CUSTOM_ID on an existing heading.
The heading carries the client's ID and no other, the response
addresses it by that ID, and the ID is not added to
`org-id-locations'."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (org-mcp-test--with-id-tracking (list test-file) nil
      (let* ((params
              `((link . ,(org-mcp-test--file-link test-file "*Simple Task"))
                (after
                 .
                 ((ID . ,org-mcp-test--client-id)
                  (CUSTOM_ID . "simple-task")))))
             (result
              (json-read-from-string
               (mcp-server-lib-ert-call-tool
                "org-node-set-properties" params))))
        (should
         (equal
          (alist-get 'link result)
          (concat "id:" org-mcp-test--client-id)))
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
       test-file (org-mcp-test--file-link test-file "*Simple Task")
       '((OK . "1") ("A\nB" . "v"))))))

(ert-deftest org-mcp-test-set-properties-multiline-value ()
  "Test a line break in a property value refuses the call."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (org-mcp-test--call-set-properties-expecting-error
     test-file (org-mcp-test--file-link test-file "*Simple Task")
     '((FOO . "x\r* Injected heading")))))

;;; Tests for failed writes and the saved flag

(defconst org-mcp-test--pattern-set-first-property
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:FIRST: +1\n"
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after setting FIRST on the bare task.")

(defconst org-mcp-test--pattern-set-first-property-with-user-edit
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:FIRST: +1\n"
   " *:END:\n"
   "Task body text\\.\n"
   "\\* TODO Other Task\n"
   "\\'")
  "Pattern after setting FIRST on the bare task beside the user's edit.
The user added Other Task before the call and left it unsaved.")

(defconst org-mcp-test--content-bare-todo-final-newline
  "* TODO Simple Task\nTask body text.\n"
  "The bare task ending in a line break, so saving it changes no byte.")

(defconst org-mcp-test--pattern-set-first-and-second-property
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:FIRST: +1\n"
   " *:SECOND: +2\n"
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after setting FIRST and then SECOND on the bare task.")

(defconst org-mcp-test--pattern-bare-todo-with-user-edit
  (concat
   "\\`\\* TODO Simple Task\n"
   "Task body text\\.\n"
   "\\* TODO Other Task\n"
   "\\'")
  "Pattern of the bare task with the user's unsaved Other Task edit.")

(defun org-mcp-test--fail-on-second (property _value)
  "Signal an error when PROPERTY is SECOND.
On `org-property-changed-functions', it makes org-node-set-properties fail
after it has written FIRST and SECOND."
  (when (equal property "SECOND")
    (error "Property hook failed")))

(defun org-mcp-test--save-on-first-fail-on-second (property value)
  "Save the buffer when PROPERTY is FIRST, and fail when it is SECOND.
On `org-property-changed-functions', it writes the file partway through
org-node-set-properties, which then fails.  VALUE is passed on."
  (if (equal property "FIRST")
      (save-buffer)
    (org-mcp-test--fail-on-second property value)))

(ert-deftest org-mcp-test-failed-write-leaves-buffer-and-file-unchanged ()
  "Test a write that fails partway leaves its buffer and file as they were.
org-node-set-properties writes FIRST and SECOND, then a hook fails.  The
edit is undone in a buffer with undo on and in one with undo off, and
either buffer reads unmodified afterwards with its undo setting kept."
  (dolist (undo-disabled '(nil t))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-bare-todo))
      (let ((buffer (find-file-noselect test-file))
            (org-property-changed-functions
             '(org-mcp-test--fail-on-second)))
        (unwind-protect
            (progn
              (when undo-disabled
                (with-current-buffer buffer
                  (buffer-disable-undo)))
              (org-mcp-test--call-tool-refused
               "org-node-set-properties"
               `((link
                  . ,(org-mcp-test--file-link test-file "*Simple Task"))
                 (after . ((FIRST . "1") (SECOND . "2"))))
               "Property hook failed" test-file)
              (with-current-buffer buffer
                (should
                 (string= (buffer-string) org-mcp-test--content-bare-todo))
                (should-not (buffer-modified-p))
                (should (eq (eq buffer-undo-list t) undo-disabled))))
          (kill-buffer buffer))))))

(ert-deftest org-mcp-test-failed-write-keeps-user-edits ()
  "Test a failed write undoes only its own edit in a buffer the user changed.
The buffer holds the user's unsaved edit before the call.  After the
call fails, the buffer holds that edit alone and still reads modified,
and the file is unchanged."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((buffer (find-file-noselect test-file))
          (org-property-changed-functions
           '(org-mcp-test--fail-on-second)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "\n* TODO Other Task\n"))
            (org-mcp-test--call-tool-refused
             "org-node-set-properties"
             `((link
                . ,(org-mcp-test--file-link test-file "*Simple Task"))
               (after . ((FIRST . "1") (SECOND . "2"))))
             "Property hook failed" test-file)
            (org-mcp-test--verify-buffer-matches
             buffer org-mcp-test--pattern-bare-todo-with-user-edit)
            (with-current-buffer buffer
              (should (buffer-modified-p))))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-write-after-failed-write-saves ()
  "Test a write after a failed one reaches disk and reports `saved' true.
The failed call leaves no edit behind, so the buffer holds no unsaved
edits of its own when the next call arrives, and that call saves it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((buffer (find-file-noselect test-file))
          (link (org-mcp-test--file-link test-file "*Simple Task")))
      (unwind-protect
          (progn
            (let ((org-property-changed-functions
                   '(org-mcp-test--fail-on-second)))
              (org-mcp-test--call-tool-refused
               "org-node-set-properties"
               `((link . ,link)
                 (after . ((FIRST . "1") (SECOND . "2"))))
               "Property hook failed" test-file))
            (let ((result
                   (json-read-from-string
                    (mcp-server-lib-ert-call-tool
                     "org-node-set-properties"
                     `((link . ,link) (after . ((FIRST . "1"))))))))
              (should (eq (alist-get 'saved result) t)))
            (org-mcp-test--verify-file-matches
             test-file org-mcp-test--pattern-set-first-property)
            (org-mcp-test--verify-no-modified-buffer test-file))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-failed-save-leaves-buffer-unchanged ()
  "Test a write whose save fails leaves its buffer and file as they were.
The edit is undone along with the failed save, so the buffer reads
unmodified and holds no edit a later call would take for the user's."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((buffer (find-file-noselect test-file)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local write-contents-functions
                          (list (lambda () (error "Save failed")))))
            (org-mcp-test--call-tool-refused
             "org-node-set-properties"
             `((link
                . ,(org-mcp-test--file-link test-file "*Simple Task"))
               (after . ((FIRST . "1"))))
             "Save failed" test-file)
            (with-current-buffer buffer
              (should
               (string= (buffer-string) org-mcp-test--content-bare-todo))
              (should-not (buffer-modified-p))))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-failed-write-after-hook-save-restores-file ()
  "Test a write that fails after a hook saved partway puts the file back.
A hook saves the buffer once FIRST is written, and the call fails on
SECOND.  The buffer held no edits of the user's, so org-mcp saves the
restored buffer again: buffer and file both hold the original text, and
the buffer reads unmodified."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo-final-newline))
    (let ((buffer (find-file-noselect test-file))
          (org-property-changed-functions
           '(org-mcp-test--save-on-first-fail-on-second)))
      (unwind-protect
          (progn
            (org-mcp-test--call-tool-refused
             "org-node-set-properties"
             `((link
                . ,(org-mcp-test--file-link test-file "*Simple Task"))
               (after . ((FIRST . "1") (SECOND . "2"))))
             "Property hook failed" test-file)
            (with-current-buffer buffer
              (should
               (string=
                (buffer-string)
                org-mcp-test--content-bare-todo-final-newline))
              (should-not (buffer-modified-p))))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-failed-write-after-hook-save-keeps-user-edits ()
  "Test a failed write does not save a buffer the user changed to undo a hook.
The buffer holds the user's unsaved edit.  A hook saves it once FIRST
is written, and the call fails on SECOND.  The buffer is put back to
the user's edit alone and still reads modified, and org-mcp does not
save it again, so the file keeps what the hook wrote."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((buffer (find-file-noselect test-file))
          (org-property-changed-functions
           '(org-mcp-test--save-on-first-fail-on-second)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "\n* TODO Other Task\n"))
            (org-mcp-test--call-tool-refused
             "org-node-set-properties"
             `((link
                . ,(org-mcp-test--file-link test-file "*Simple Task"))
               (after . ((FIRST . "1") (SECOND . "2"))))
             "Property hook failed")
            (org-mcp-test--verify-buffer-matches
             buffer org-mcp-test--pattern-bare-todo-with-user-edit)
            (with-current-buffer buffer
              (should (buffer-modified-p)))
            (org-mcp-test--verify-file-matches
             test-file
             org-mcp-test--pattern-set-first-property-with-user-edit))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-failed-after-save-hook-keeps-saved-change ()
  "Test a failure after the file was written keeps the change and says so.
A buffer-local `after-save-hook' fails once the file holds the change.
The change stays in buffer and file, the buffer reads unmodified, and
the error says that the change was made, so a client does not repeat
it.  Once the hook is gone, the next write reports `saved' true."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((buffer (find-file-noselect test-file))
          (link (org-mcp-test--file-link test-file "*Simple Task")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (add-hook 'after-save-hook
                        (lambda () (error "Save hook failed"))
                        nil t))
            (org-mcp-test--call-tool-refused
             "org-node-set-properties"
             `((link . ,link) (after . ((FIRST . "1"))))
             "\\`The change was made and saved, but .*Save hook failed")
            (org-mcp-test--verify-file-matches
             test-file org-mcp-test--pattern-set-first-property)
            (org-mcp-test--verify-buffer-matches
             buffer org-mcp-test--pattern-set-first-property)
            (org-mcp-test--verify-no-modified-buffer test-file)
            (with-current-buffer buffer
              (kill-local-variable 'after-save-hook))
            (let ((result
                   (json-read-from-string
                    (mcp-server-lib-ert-call-tool
                     "org-node-set-properties"
                     `((link . ,link) (after . ((SECOND . "2"))))))))
              (should (eq (alist-get 'saved result) t)))
            (org-mcp-test--verify-file-matches
             test-file org-mcp-test--pattern-set-first-and-second-property))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-hook-save-reports-saved ()
  "Test `saved' is true when a hook saves the buffer during the call.
The buffer holds the user's unsaved edit, so org-mcp does not save it,
but a function on `org-property-changed-functions' does.  The change is
on disk, and the response says so."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((buffer (find-file-noselect test-file)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "\n* TODO Other Task\n"))
            (let* ((org-property-changed-functions
                    (list
                     (lambda (&rest _)
                       (with-current-buffer buffer
                         (save-buffer)))))
                   (result
                    (json-read-from-string
                     (mcp-server-lib-ert-call-tool
                      "org-node-set-properties"
                      `((link
                         .
                         ,(org-mcp-test--file-link
                           test-file "*Simple Task"))
                        (after . ((FIRST . "1"))))))))
              (should (eq (alist-get 'saved result) t)))
            (org-mcp-test--verify-file-matches
             test-file
             org-mcp-test--pattern-set-first-property-with-user-edit)
            (org-mcp-test--verify-no-modified-buffer test-file))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-clock-in-clock-out-hook-save-reports-saved ()
  "Test clock-in reports `saved' true when `org-clock-out-hook' saves.
The active clock sits in another allowed file whose buffer holds the
user's unsaved edit.  org-mcp does not save that buffer, but the hook
does when the clock is closed, so both files hold their change."
  (org-mcp-test--with-temp-org-files
      ((file-1 org-mcp-test--clock-task-with-open-clock)
       (file-2 org-mcp-test--clock-task-content))
    (let ((buffer (find-file-noselect file-1))
          (org-clock-out-hook (list #'save-buffer)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "\n* TODO Task Two\n"))
            (let ((result
                   (org-mcp-test--call-clock-in
                    (org-mcp-test--file-link file-2 "*Task One")
                    "2026-01-01T11:00:00" nil
                    (org-mcp-test--file-link file-1 "*Task One"))))
              (should (equal (alist-get 'clocked_in result) t))
              (should (eq (alist-get 'saved result) t)))
            (org-mcp-test--verify-file-matches
             file-2 org-mcp-test--clock-in-at-eleven-expected-regex)
            (org-mcp-test--verify-file-matches
             file-1
             org-mcp-test--clock-closed-in-modified-buffer-expected-regex)
            (org-mcp-test--verify-no-modified-buffer file-1))
        (kill-buffer buffer)))))

;;; Tests for org-node-set-scheduled

(ert-deftest org-mcp-test-update-scheduled-set ()
  "Test setting SCHEDULED on entry without one."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (params `((link . ,link)
                     (after . "2026-03-27")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-scheduled" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'before result) ""))
      (should (string-match-p "<2026-03-27"
                              (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-set))))

(ert-deftest org-mcp-test-update-scheduled-update ()
  "Test updating an existing SCHEDULED timestamp."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (let* ((link (org-mcp-test--file-link test-file "*Scheduled Task"))
           (params `((link . ,link)
                     (after . "2026-04-15")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-scheduled" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (string-match-p "<2026-03-01"
                              (alist-get 'before result)))
      (should (string-match-p "<2026-04-15"
                              (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-update))))

(ert-deftest org-mcp-test-update-scheduled-remove ()
  "Test removing SCHEDULED timestamp."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (let* ((link (org-mcp-test--file-link test-file "*Scheduled Task"))
           (params `((link . ,link)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-scheduled" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'after result) ""))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-remove))))

(ert-deftest org-mcp-test-update-scheduled-invalid-date ()
  "Test that invalid date format triggers an error."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-node-set-scheduled" 1
                `((link . ,link)
                  (after . "not-a-date"))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-update-scheduled-id-link ()
  "Test setting SCHEDULED via an `id:' link."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((link (concat "id:" org-mcp-test--crud-test-id))
          (params `((link . ,link)
                    (after . "2026-03-27")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-node-set-scheduled" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

;;; Tests for org-node-set-deadline

(ert-deftest org-mcp-test-update-deadline-set ()
  "Test setting DEADLINE on entry without one."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (params `((link . ,link)
                     (after . "2026-03-27")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-deadline" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'before result) ""))
      (should (string-match-p "<2026-03-27"
                              (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-deadline-set))))

(ert-deftest org-mcp-test-update-deadline-update ()
  "Test updating an existing DEADLINE timestamp."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (let* ((link (org-mcp-test--file-link test-file "*Deadline Task"))
           (params `((link . ,link)
                     (after . "2026-04-15")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-deadline" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (string-match-p "<2026-03-15"
                              (alist-get 'before result)))
      (should (string-match-p "<2026-04-15"
                              (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-deadline-update))))

(ert-deftest org-mcp-test-update-deadline-remove ()
  "Test removing DEADLINE timestamp."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (let* ((link (org-mcp-test--file-link test-file "*Deadline Task"))
           (params `((link . ,link)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-deadline" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'after result) ""))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-deadline-remove))))

(ert-deftest org-mcp-test-update-deadline-invalid-date ()
  "Test that invalid date format triggers an error."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-node-set-deadline" 1
                `((link . ,link)
                  (after . "not-a-date"))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-update-deadline-id-link ()
  "Test setting DEADLINE via an `id:' link."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((link (concat "id:" org-mcp-test--crud-test-id))
          (params `((link . ,link)
                    (after . "2026-03-27")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-node-set-deadline" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

;;; Tests for org-node-set-tags

(ert-deftest org-mcp-test-set-tags-add ()
  "Test adding tags to a bare task."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((org-tag-alist '("work" "personal" "urgent"))
           (link (org-mcp-test--file-link test-file "*Simple Task"))
           (params `((link . ,link)
                     (after . ["work" "urgent"])))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-tags" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'before result) []))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-set))))

(ert-deftest org-mcp-test-set-tags-replace ()
  "Test replacing existing tags."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let* ((org-tag-alist '("work" "personal" "urgent"))
           (link (org-mcp-test--file-link test-file "*Task with Tags"))
           (params `((link . ,link)
                     (after . "personal")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-tags" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-replace))))

(ert-deftest org-mcp-test-set-tags-clear ()
  "Test clearing all tags."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let* ((link (org-mcp-test--file-link test-file "*Task with Tags"))
           (params `((link . ,link)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-tags" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'after result) []))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-clear))))

(ert-deftest org-mcp-test-set-tags-invalid-name ()
  "Test that invalid tag names are rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-node-set-tags" 1
                `((link . ,link)
                  (after . "invalid tag!"))))
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
           (link (org-mcp-test--file-link test-file "*Simple Task"))
           (params `((link . ,link)
                     (after . "nonexistent")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-tags" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'after result) ["nonexistent"])))))

(ert-deftest org-mcp-test-set-tags-mutex-violation ()
  "Test that mutually exclusive tags are rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-tag-alist '(:startgroup "work" "personal" :endgroup "urgent"))
          (link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-node-set-tags" 1
                `((link . ,link)
                  (after . ["work" "personal"]))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-set-tags-id-link ()
  "Test setting tags via an `id:' link."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((link (concat "id:" org-mcp-test--crud-test-id))
          (params `((link . ,link)
                    (after . "work")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-node-set-tags" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

;;; Tests for org-node-set-priority

(ert-deftest org-mcp-test-set-priority-set ()
  "Test setting priority on a bare task."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (params `((link . ,link)
                     (after . "A")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-priority" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'before result) ""))
      (should (equal (alist-get 'after result) "A"))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-priority-set))))

(ert-deftest org-mcp-test-set-priority-change ()
  "Test changing existing priority."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (let* ((link (org-mcp-test--file-link test-file "*Priority Task"))
           (params `((link . ,link)
                     (after . "C")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-priority" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'before result) "B"))
      (should (equal (alist-get 'after result) "C"))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-priority-change))))

(ert-deftest org-mcp-test-set-priority-remove ()
  "Test removing priority."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (let* ((link (org-mcp-test--file-link test-file "*Priority Task"))
           (params `((link . ,link)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-priority" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'after result) ""))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-priority-remove))))

(ert-deftest org-mcp-test-set-priority-out-of-range ()
  "Test that out-of-range priority is rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-node-set-priority" 1
                `((link . ,link)
                  (after . "Z"))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-set-priority-multi-char ()
  "Test that multi-character priority is rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-node-set-priority" 1
                `((link . ,link)
                  (after . "AB"))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-set-priority-id-link ()
  "Test setting priority via an `id:' link."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((link (concat "id:" org-mcp-test--crud-test-id))
          (params `((link . ,link)
                    (after . "A")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-node-set-priority" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

;;; Tests for org-node-set-content append mode

(ert-deftest org-mcp-test-edit-body-append ()
  "Test appending to existing body."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (params `((link . ,link)
                     (before . "")
                     (after . "Appended line.")
                     (append . t)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-content" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should
       (equal (alist-get 'link result)
              (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-append-body))))

(ert-deftest org-mcp-test-edit-body-append-empty-entry ()
  "Test appending to entry with no body."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-empty-body))
    (let* ((link (org-mcp-test--file-link test-file "*Empty Body Task"))
           (params `((link . ,link)
                     (before . "")
                     (after . "New body content.")
                     (append . t)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-content" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-append-body-empty))))

(ert-deftest org-mcp-test-edit-body-append-empty-after ()
  "Append mode refuses an empty or whitespace-only after.
The refusal says what after is on an append call, so a client reading
it knows which argument to fix and what it is for."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-empty-body))
    (let ((link (org-mcp-test--file-link test-file "*Empty Body Task")))
      (dolist (after '("" "  " "\n\t"))
        (should
         (string=
          "after is the content to append and cannot be empty or \
whitespace-only"
          (org-mcp-test--call-tool-expecting-error
           test-file "org-node-set-content"
           `((link . ,link)
             (before . "")
             (after . ,after)
             (append . t)))))))))

(ert-deftest org-mcp-test-edit-body-append-before-children ()
  "Test that appended content goes before child headlines."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-children))
    (let* ((link (org-mcp-test--file-link test-file "*Parent Task"))
           (params `((link . ,link)
                     (before . "")
                     (after . "Appended text.")
                     (append . t)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-content" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-append-body-with-children))))

(defconst org-mcp-test--content-empty-body-before-child
  "* Parent\n** Child\n*** Grandchild\nDeep.\n"
  "Parent with no body, followed directly by its child and grandchild.")

(defconst org-mcp-test--regex-empty-body-before-child-set
  (concat
   "\\`\\* Parent\n"
   "Parent body\\.\n"
   "\\*\\* Child\n"
   "\\*\\*\\* Grandchild\n"
   "Deep\\.\n"
   "\\'")
  "Regex matching the whole file once Parent's body is set.")

(defconst org-mcp-test--content-drawer-no-body-before-child
  "* Parent\n:PROPERTIES:\n:CUSTOM_ID: parent\n:END:\n** Child\nChild body.\n"
  "Parent with a property drawer and no body, followed by its child.")

(defconst org-mcp-test--regex-drawer-no-body-before-child-set
  (concat
   "\\`\\* Parent\n"
   ":PROPERTIES:\n"
   ":CUSTOM_ID: parent\n"
   ":END:\n"
   "Parent body\\.\n"
   "\\*\\* Child\n"
   "Child body\\.\n"
   "\\'")
  "Regex matching the whole drawer file once Parent's body is set.")

(defconst org-mcp-test--content-empty-body-before-sibling
  "* Parent\n* Sibling\n** Sibling child\nSibling body.\n"
  "Parent with no body and no child, followed by a sibling with a child.")

(defconst org-mcp-test--regex-empty-body-before-sibling-set
  (concat
   "\\`\\* Parent\n"
   "Parent body\\.\n"
   "\\* Sibling\n"
   "\\*\\* Sibling child\n"
   "Sibling body\\.\n"
   "\\'")
  "Regex matching the whole sibling file once Parent's body is set.")

(ert-deftest org-mcp-test-edit-body-empty-body-before-next-heading ()
  "org-node-set-content sets an empty body followed directly by another heading.
The body lies between the heading's meta data and its first child, or
the end of its subtree, whichever heading follows it.  Appending, and
replacing with an empty before, each put the text under Parent: when
a child follows at once, when a property drawer comes first, and when
a sibling with a child of its own follows.  Nothing lands in the next
heading's body, and the response links to Parent."
  (pcase-dolist (`(,content ,search ,expected)
                 `((,org-mcp-test--content-empty-body-before-child
                    "*Parent"
                    ,org-mcp-test--regex-empty-body-before-child-set)
                   (,org-mcp-test--content-drawer-no-body-before-child
                    "#parent"
                    ,org-mcp-test--regex-drawer-no-body-before-child-set)
                   (,org-mcp-test--content-empty-body-before-sibling
                    "*Parent"
                    ,org-mcp-test--regex-empty-body-before-sibling-set)))
    (pcase-dolist (`(,old-body ,append) '((nil t) ("" nil)))
      (org-mcp-test--with-temp-org-files
          ((test-file content))
        (let ((link (org-mcp-test--file-link test-file search)))
          (org-mcp-test--call-edit-body-and-check
           test-file link old-body "Parent body." expected append link))))))

(defconst org-mcp-test--content-body-before-sibling
  "* Parent\nParent body.\n\n* Sibling\nSibling body.\n"
  "Parent with a body and no child, a blank line, then a sibling.")

(defconst org-mcp-test--regex-body-before-sibling-appended
  (concat
   "\\`\\* Parent\n"
   "Parent body\\.\n"
   "Appended\\.\n"
   "\n"
   "\\* Sibling\n"
   "Sibling body\\.\n"
   "\\'")
  "Regex matching the whole sibling file after appending to Parent.")

(defconst org-mcp-test--content-body-to-edit "* Task\nold text\n"
  "Task whose body is one line, for replacing and appending.")

(defconst org-mcp-test--regex-body-to-edit-replaced
  "\\`\\* Task\nnew text\n\\'"
  "Regex matching the whole file once the body line is replaced.")

(defconst org-mcp-test--regex-body-to-edit-appended
  "\\`\\* Task\nold text\nnew text\n\\'"
  "Regex matching the whole file once a line is appended to the body.")

(ert-deftest org-mcp-test-edit-body-append-false-replaces ()
  "append false, as JSON false or any other false spelling, replaces.
JSON false, \"false\", null, \"\" and a missing parameter each replace
before with after; only JSON true and \"true\" append."
  (pcase-dolist (`(,append ,expected)
                 `((:json-false ,org-mcp-test--regex-body-to-edit-replaced)
                   ("false" ,org-mcp-test--regex-body-to-edit-replaced)
                   ("" ,org-mcp-test--regex-body-to-edit-replaced)
                   (nil ,org-mcp-test--regex-body-to-edit-replaced)
                   (t ,org-mcp-test--regex-body-to-edit-appended)
                   ("true" ,org-mcp-test--regex-body-to-edit-appended)))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-body-to-edit))
      (let ((link (org-mcp-test--file-link test-file "*Task")))
        (org-mcp-test--call-edit-body-and-check
         test-file link "old text" "new text" expected append link)))))

(ert-deftest org-mcp-test-edit-body-refuses-unknown-append ()
  "append that is neither true nor false is refused, changing nothing."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-body-to-edit))
    (should
     (string-match-p
      "\\`append must be true or false: \"yes\"\\'"
      (org-mcp-test--call-tool-expecting-error
       test-file "org-node-set-content"
       `((link . ,(org-mcp-test--file-link test-file "*Task"))
         (before . "old text")
         (after . "new text")
         (append . "yes")))))))

(defconst org-mcp-test--content-body-mixed-case
  "* Task\nFoo bar first.\nThen foo bar again.\n"
  "Task whose body holds before once as written and once capitalized.")

(defconst org-mcp-test--regex-body-mixed-case-replaced
  (concat
   "\\`\\* Task\n"
   "Foo bar first\\.\n"
   "Then baz again\\.\n"
   "\\'")
  "Regex matching the whole file once the lowercase occurrence is replaced.")

(ert-deftest org-mcp-test-edit-body-replace-matches-case ()
  "Replacing before changes the occurrence that matches it in case.
The body holds \"foo bar\" once, after a capitalized \"Foo bar\", so the
unique occurrence is the lowercase one, and only it changes, whatever
`case-fold-search' is in the buffer."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-body-mixed-case))
    (let ((link (org-mcp-test--file-link test-file "*Task")))
      (org-mcp-test--call-edit-body-and-check
       test-file link "foo bar" "baz"
       org-mcp-test--regex-body-mixed-case-replaced nil link))))

(ert-deftest org-mcp-test-edit-body-append-before-sibling ()
  "Appending to a body followed by a sibling adds the text after it.
The text goes on the line after the body's last line, and the blank
line before the sibling stays the only one."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-body-before-sibling))
    (let ((link (org-mcp-test--file-link test-file "*Parent")))
      (org-mcp-test--call-edit-body-and-check
       test-file link nil "Appended."
       org-mcp-test--regex-body-before-sibling-appended t link))))

(ert-deftest org-mcp-test-edit-body-append-headline-error ()
  "Test that content with headlines is rejected in append mode."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-node-set-content" 1
                `((link . ,link)
                  (before . "")
                  (after . "* A headline")
                  (append . t))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-edit-body-append-unbalanced-blocks-error ()
  "Test that unbalanced blocks are rejected in append mode."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-node-set-content" 1
                `((link . ,link)
                  (before . "")
                  (after . "#+BEGIN_SRC\ncode\n")
                  (append . t))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-edit-body-append-id-link ()
  "Test appending body via an `id:' link."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((link (concat "id:" org-mcp-test--crud-test-id))
          (params `((link . ,link)
                    (before . "")
                    (after . "Appended.")
                    (append . t)))
          (result-text
           (mcp-server-lib-ert-call-tool "org-node-set-content" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

;;; Tests for org-node-add-note

(ert-deftest org-mcp-test-add-logbook-note-new ()
  "Test adding logbook note to task without LOGBOOK."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-log-into-drawer t))
      (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
             (params `((link . ,link)
                       (note . "This is my note.")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-node-add-note" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should
         (equal (alist-get 'link result)
                (org-mcp-test--file-link test-file "*Simple Task")))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-logbook-note-new)))))

(ert-deftest org-mcp-test-add-logbook-note-existing ()
  "Test adding logbook note to task with existing LOGBOOK."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-logbook))
    (let ((org-log-into-drawer t))
      (let* ((link (org-mcp-test--file-link test-file "*Task with Logbook"))
             (params `((link . ,link)
                       (note . "Another note.")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-node-add-note" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-logbook-note-existing)))))

(ert-deftest org-mcp-test-add-logbook-note-multiline ()
  "Test adding multiline logbook note with proper indentation."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-log-into-drawer t))
      (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
             (params `((link . ,link)
                       (note . "First line.\nSecond line.")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-node-add-note" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-logbook-note-multiline)))))

(ert-deftest org-mcp-test-add-logbook-note-whitespace-only-error ()
  "Test that whitespace-only note is rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--assert-error-and-file
       test-file
       (let* ((request
               (mcp-server-lib-create-tools-call-request
                "org-node-add-note" 1
                `((link . ,link)
                  (note . "   "))))
              (response (mcp-server-lib-process-jsonrpc-parsed
                         request mcp-server-lib-ert-server-id))
              (result (mcp-server-lib-ert-process-tool-response response)))
         (error "Expected error but got success: %s" result))))))

(ert-deftest org-mcp-test-add-logbook-note-id-link ()
  "Test adding logbook note via an `id:' link."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((link (concat "id:" org-mcp-test--crud-test-id))
          (params `((link . ,link)
                    (note . "Test note.")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-node-add-note" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

(ert-deftest org-mcp-test-add-logbook-note-special-chars ()
  "Test adding logbook note containing special characters."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-log-into-drawer t))
      (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
             (params
              `((link . ,link)
                (note
                 . "Quotes \"like this\", backslash \\, percent %, asterisk *.")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-node-add-note" params))
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
      (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
             (params `((link . ,link)
                       (note . "Plain note.")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-node-add-note" params))
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
      (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
             (params `((link . ,link)
                       (note . "My note.")))
             (result-text
              (mcp-server-lib-ert-call-tool "org-node-add-note" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-logbook-note-custom-heading)))))

;; Helper functions for testing org-query MCP tool

(defun org-mcp-test--call-ql-query (query)
  "Call org-query tool via JSON-RPC and return the parsed result.
QUERY is the org-ql query sexp as a string."
  (let* ((params `((query . ,query)))
         (result-text
          (mcp-server-lib-ert-call-tool "org-query" params)))
    (json-read-from-string result-text)))

(ert-deftest org-mcp-test-ql-query-link-with-id ()
  "Test that org-query links a headline with an ID by `id:'."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-with-id-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'children result))
           (first-match (aref matches 0))
           (link (alist-get 'link first-match)))
      (should (equal (alist-get 'total result) 1))
      (should (equal link (concat "id:" org-mcp-test--content-with-id-id))))))

(ert-deftest org-mcp-test-ql-query-link-without-id ()
  "Test that org-query links a headline without an ID by its title."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'children result))
           (first-match (aref matches 0))
           (link (alist-get 'link first-match)))
      (should (equal (alist-get 'total result) 1))
      (should
       (equal link (org-mcp-test--file-link test-file "*Simple Task"))))))

(defconst org-mcp-test--content-ql-tags-scheduled-deadline
  "* TODO Tagged Task                                                 :work:home:
SCHEDULED: <2024-03-15 Fri> DEADLINE: <2024-03-20 Wed>"
  "TODO task with tags, scheduled, and deadline for org-ql query tests.")

(ert-deftest org-mcp-test-ql-query-exports-tags ()
  "Test that org-query includes tags in match results."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ql-tags-scheduled-deadline))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'children result))
           (match (aref matches 0)))
      (should (equal (alist-get 'tags match) ["work" "home"])))))

(ert-deftest org-mcp-test-ql-query-exports-scheduled ()
  "Test that org-query includes scheduled date in match results."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ql-tags-scheduled-deadline))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'children result))
           (match (aref matches 0)))
      (should (stringp (alist-get 'scheduled match)))
      (should (string-match-p "2024-03-15" (alist-get 'scheduled match))))))

(ert-deftest org-mcp-test-ql-query-exports-deadline ()
  "Test that org-query includes deadline in match results."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ql-tags-scheduled-deadline))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'children result))
           (match (aref matches 0)))
      (should (stringp (alist-get 'deadline match)))
      (should (string-match-p "2024-03-20" (alist-get 'deadline match))))))

(ert-deftest org-mcp-test-ql-query-no-tags-absent ()
  "Test that tags key is absent when headline has no tags."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'children result))
           (match (aref matches 0)))
      (should-not (assq 'tags match)))))

(ert-deftest org-mcp-test-ql-query-no-scheduled-absent ()
  "Test that scheduled key is absent when headline has no scheduled date."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'children result))
           (match (aref matches 0)))
      (should-not (assq 'scheduled match)))))

(ert-deftest org-mcp-test-ql-query-no-deadline-absent ()
  "Test that deadline key is absent when headline has no deadline."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'children result))
           (match (aref matches 0)))
      (should-not (assq 'deadline match)))))

(defconst org-mcp-test--content-ql-priority-closed
  "* DONE [#A] Closed Task
CLOSED: [2024-04-01 Mon 15:30]"
  "DONE task with priority A and a CLOSED timestamp for ql tests.")

(ert-deftest org-mcp-test-ql-query-exports-priority ()
  "Test that org-query returns priority as a one-character string."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ql-priority-closed))
    (let* ((result (org-mcp-test--call-ql-query "(done)"))
           (matches (alist-get 'children result))
           (match (aref matches 0)))
      (should (equal (alist-get 'priority match) "A")))))

(ert-deftest org-mcp-test-ql-query-exports-closed ()
  "Test that org-query includes CLOSED timestamp in match results."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ql-priority-closed))
    (let* ((result (org-mcp-test--call-ql-query "(done)"))
           (matches (alist-get 'children result))
           (match (aref matches 0)))
      (should (stringp (alist-get 'closed match)))
      (should (string-match-p "2024-04-01" (alist-get 'closed match))))))

(ert-deftest org-mcp-test-ql-query-no-priority-absent ()
  "Test that priority key is absent when headline has no priority."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'children result))
           (match (aref matches 0)))
      (should-not (assq 'priority match)))))

(ert-deftest org-mcp-test-ql-query-no-closed-absent ()
  "Test that closed key is absent when headline has no CLOSED timestamp."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'children result))
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
  "Test that org-query includes non-filtered PROPERTIES drawer values."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ql-with-custom-prop))
    (let* ((result (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'children result))
           (match (aref matches 0))
           (props (alist-get 'properties match)))
      (should (equal (alist-get 'EFFORT props) "2:00"))
      (should (equal (alist-get 'CONTEXT props) "laptop")))))

;;; Tests for org-node-read structured metadata extraction
;;
;; These verify `org-mcp--extract-structured-heading' (via the org-node-read
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
  "DONE task with CLOSED timestamp for org-node-read tests.")

(defun org-mcp-test--read-structured (file headline)
  "Return parsed JSON alist for HEADLINE in FILE via org-node-read.
HEADLINE is the heading's title, reached through its title link."
  (let* ((link (org-mcp-test--file-link file (concat "*" headline)))
         (result-text (org-mcp-test--call-read link)))
    (json-parse-string result-text :object-type 'alist)))

(defconst org-mcp-test--bold-parent-body
  "*bold* opens the body.\nMore text after it.\n"
  "Body of Parent, whose first line starts with an emphasized word.")

(defconst org-mcp-test--bold-solo-body
  "Intro line.\n*emphasis* starts this line.\nLast line.\n"
  "Body of Solo, whose second line starts with an emphasized word.")

(defconst org-mcp-test--content-bold-body-lines
  (concat
   "* Parent\n"
   ":PROPERTIES:\n:CUSTOM_ID: parent\n:END:\n"
   org-mcp-test--bold-parent-body
   "** Child\nChild body.\n"
   "* Solo\n"
   org-mcp-test--bold-solo-body)
  "File whose bodies hold lines starting with `*' that are no headings.
Parent has a property drawer and a child; Solo has no child.")

(ert-deftest org-mcp-test-read-content-keeps-lines-starting-with-star ()
  "org-node-read's content is the whole body up to the first child heading.
A body line starting with `*bold*' is text, not a heading, so it and
the lines after it stay in `content'.  Parent's content stops at its
child, which is listed in `children' instead; Solo, with no child,
has its body up to the end of its subtree.  The file stays unchanged."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bold-body-lines))
    (let ((parent (org-mcp-test--read-structured test-file "Parent"))
          (solo (org-mcp-test--read-structured test-file "Solo")))
      (should
       (equal (alist-get 'content parent)
              (string-trim org-mcp-test--bold-parent-body)))
      (should
       (equal (mapcar (lambda (child)
                        (list (alist-get 'title child)
                              (alist-get 'level child)))
                      (alist-get 'children parent))
              '(("Child" 2))))
      (should
       (equal (alist-get 'content solo)
              (string-trim org-mcp-test--bold-solo-body)))
      (should (equal (alist-get 'children solo) [])))
    (should
     (string= (org-mcp-test--read-file test-file)
              org-mcp-test--content-bold-body-lines))))

(ert-deftest org-mcp-test-read-exports-priority ()
  "Test that org-node-read returns priority as a one-character string."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-read-full-metadata))
    (let ((result (org-mcp-test--read-structured
                   test-file "Full Metadata Task")))
      (should (equal (alist-get 'priority result) "B")))))

(ert-deftest org-mcp-test-read-exports-scheduled-deadline ()
  "Test that org-node-read includes scheduled and deadline timestamps."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-read-full-metadata))
    (let ((result (org-mcp-test--read-structured
                   test-file "Full Metadata Task")))
      (should (equal (alist-get 'scheduled result) "<2024-05-01 Wed>"))
      (should (equal (alist-get 'deadline result) "<2024-05-08 Wed>")))))

(ert-deftest org-mcp-test-read-exports-id ()
  "Test that org-node-read includes ID from PROPERTIES drawer."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-read-full-metadata))
    (let ((result (org-mcp-test--read-structured
                   test-file "Full Metadata Task")))
      (should (equal (alist-get 'id result) "full-meta-task-id")))))

(ert-deftest org-mcp-test-read-exports-tags-with-inheritance ()
  "Test that org-node-read includes the heading's tag list."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-read-full-metadata))
    (let ((result (org-mcp-test--read-structured
                   test-file "Full Metadata Task")))
      (should (equal (alist-get 'tags result) ["work" "home"])))))

(ert-deftest org-mcp-test-read-exports-closed ()
  "Test that org-node-read includes CLOSED timestamp."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-read-closed-task))
    (let ((result (org-mcp-test--read-structured
                   test-file "Closed Read Task")))
      (should (equal (alist-get 'closed result)
                     "[2024-05-15 Wed 09:00]")))))

(ert-deftest org-mcp-test-read-bare-omits-optional-fields ()
  "Test that org-node-read omits optional fields when absent on bare heading."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((result (org-mcp-test--read-structured
                   test-file "Simple Task")))
      (should-not (assq 'priority result))
      (should-not (assq 'scheduled result))
      (should-not (assq 'deadline result))
      (should-not (assq 'closed result))
      (should-not (assq 'id result))
      (should-not (assq 'tags result))
      (should-not (assq 'local_tags result)))))

;;; Tag inheritance tests
;;
;; Tags follow Org's own configuration, so these bind
;; `org-use-tag-inheritance' and `org-tags-exclude-from-inheritance'
;; around the call and assert what every read path returns under it.

(defconst org-mcp-test--content-inherited-tags
  "#+FILETAGS: filetag
* Tagged Parent :ptag:
** TODO Tagged Child :ctag:
Child body."
  "A file tag, a tagged parent and a tagged child below it.")

(defun org-mcp-test--read-tags (file headline field)
  "Return FIELD of HEADLINE in FILE as a list, read through org-node-read.
FIELD is `tags' or `local_tags'."
  (append
   (alist-get field (org-mcp-test--read-structured file headline))
   nil))

(defun org-mcp-test--ql-tags (title)
  "Return the tags org-query reports for the match titled TITLE.
The query matches every TODO heading in the allowed files."
  (let* ((result (org-mcp-test--call-ql-query "(todo)"))
         (match
          (seq-find
           (lambda (m) (equal (alist-get 'title m) title))
           (alist-get 'children result))))
    (should match)
    (append (alist-get 'tags match) nil)))

(ert-deftest org-mcp-test-tags-inherited-agree-across-read-paths ()
  "org-node-read and org-query report the same tags for one heading.
The child inherits a file tag and its parent's tag, and both paths
return that effective set."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance t)
          (org-tags-exclude-from-inheritance nil))
      (should
       (equal (org-mcp-test--read-tags test-file "Tagged Child" 'tags)
              '("filetag" "ptag" "ctag")))
      (should
       (equal (org-mcp-test--ql-tags "Tagged Child")
              '("filetag" "ptag" "ctag"))))))

(ert-deftest org-mcp-test-tags-without-inheritance-agree-across-read-paths ()
  "With inheritance off, both read paths return the heading's own tags."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance nil)
          (org-tags-exclude-from-inheritance nil))
      (should
       (equal (org-mcp-test--read-tags test-file "Tagged Child" 'tags)
              '("ctag")))
      (should
       (equal (org-mcp-test--ql-tags "Tagged Child") '("ctag"))))))

(ert-deftest org-mcp-test-tags-inheritance-list-form ()
  "A list `org-use-tag-inheritance' inherits only the tags it names."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance '("ptag"))
          (org-tags-exclude-from-inheritance nil))
      (should
       (equal (org-mcp-test--read-tags test-file "Tagged Child" 'tags)
              '("ptag" "ctag")))
      (should
       (equal (org-mcp-test--ql-tags "Tagged Child")
              '("ptag" "ctag"))))))

(ert-deftest org-mcp-test-tags-inheritance-regexp-form ()
  "A regexp `org-use-tag-inheritance' inherits only matching tags."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance "\\`p")
          (org-tags-exclude-from-inheritance nil))
      (should
       (equal (org-mcp-test--read-tags test-file "Tagged Child" 'tags)
              '("ptag" "ctag")))
      (should
       (equal (org-mcp-test--ql-tags "Tagged Child")
              '("ptag" "ctag"))))))

(ert-deftest org-mcp-test-tags-excluded-from-inheritance ()
  "An excluded tag stays on the parent and never reaches the child."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance t)
          (org-tags-exclude-from-inheritance '("ptag")))
      (should
       (equal (org-mcp-test--read-tags test-file "Tagged Parent" 'tags)
              '("filetag" "ptag")))
      (should
       (equal (org-mcp-test--read-tags test-file "Tagged Child" 'tags)
              '("filetag" "ctag"))))))

(ert-deftest org-mcp-test-read-local-tags-are-written-on-the-heading ()
  "`local_tags' carries only the tags written on the heading itself."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance t)
          (org-tags-exclude-from-inheritance nil))
      (should
       (equal (org-mcp-test--read-tags
               test-file "Tagged Child" 'local_tags)
              '("ctag")))
      (should
       (equal (org-mcp-test--read-tags
               test-file "Tagged Parent" 'local_tags)
              '("ptag"))))))

(ert-deftest org-mcp-test-read-local-tags-equal-tags-without-inheritance ()
  "With inheritance off, `tags' and `local_tags' are the same list."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance nil)
          (org-tags-exclude-from-inheritance nil))
      (let ((result
             (org-mcp-test--read-structured test-file "Tagged Child")))
        (should (equal (alist-get 'tags result) ["ctag"]))
        (should
         (equal (alist-get 'tags result)
                (alist-get 'local_tags result)))))))

;;; View tool tests

(defmacro org-mcp-test--with-configured-server
    (file-specs bindings &rest body)
  "Create temp org files and enable org-mcp with BINDINGS in force.
FILE-SPECS are (VAR CONTENT) pairs.  BINDINGS is a list of let-style
bindings for the settings that must be set before `org-mcp-enable'."
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
               (org-mcp-test--with-enabled
                 ,@body)))
         ,@cleanups))))

(defconst org-mcp-test--content-views
  "* TODO [#A] Alpha :work:

* TODO [#B] Beta :private:

* TODO Gamma :#inbox:
:PROPERTIES:
:EFFORT:   1:00
:END:

* TODO Delta :tangling:"
  "Items the org-view tests run over.
Gamma carries a drawer, so one match has a property to answer with
and a view that dropped the drawer fails rather than passing on a
file that had none to lose.")

(defun org-mcp-test--view-query-inbox ()
  "Return the query of a test view that takes no parameters."
  '(tags "#inbox"))

(defun org-mcp-test--view-query-stuck (filter)
  "Return the query of a test view taking FILTER alone."
  (if filter
      `(and (todo "TODO") ,filter)
    '(todo "TODO")))

(defun org-mcp-test--view-query-next (filter range)
  "Return the query of a test view taking FILTER and RANGE.
RANGE picks the band, so a test sees which range reached the query:
`sprint' is the A items, anything else every TODO."
  (let ((band
         (if (eq range 'sprint)
             '(priority "A")
           '(todo "TODO"))))
    (if filter
        `(and ,band ,filter)
      band)))

(defconst org-mcp-test--views
  '((inbox :name "Inbox" :query org-mcp-test--view-query-inbox)
    (stuck
     :name "Stuck Projects"
     :query org-mcp-test--view-query-stuck
     :filter t)
    (next
     :name "Next Actions"
     :query org-mcp-test--view-query-next
     :filter t
     :range (sprint all))
    (tangling :name "Tangling" :query (tags "tangling")))
  "The views the org-view tests are configured with.
One view per arity: none, a filter, a filter and a range, and one
carrying a literal query rather than a function.")

(defconst org-mcp-test--filters
  '((work . (tags "work")) (private . (tags "private")))
  "The filters the org-view tests are configured with.")

(defmacro org-mcp-test--with-views (&rest body)
  "Run BODY over `org-mcp-test--content-views' with the test views."
  (declare (indent defun) (debug t))
  `(org-mcp-test--with-configured-server
       ((test-file org-mcp-test--content-views))
       ((org-mcp-views org-mcp-test--views)
        (org-mcp-filters org-mcp-test--filters)
        (org-mcp-query-sort-fn nil))
     ,@body))

(defun org-mcp-test--view-matches (params)
  "Return the nodes org-view returns for PARAMS, in its own order."
  (append
   (alist-get
    'children
    (json-read-from-string
     (mcp-server-lib-ert-call-tool "org-view" params)))
   nil))

(defun org-mcp-test--view-titles (params)
  "Return the titles org-view returns for PARAMS, in its own order."
  (mapcar
   (lambda (match) (alist-get 'title match))
   (org-mcp-test--view-matches params)))

(defun org-mcp-test--view-refused (params message)
  "Assert org-view refuses PARAMS with exactly MESSAGE."
  (org-mcp-test--call-tool-refused
   "org-view" params
   (concat "\\`" (regexp-quote message) "\\'")))

(ert-deftest org-mcp-test-view-runs-by-name ()
  "A view runs by name and answers with nodes in the standard shape."
  (org-mcp-test--with-views
    (let ((matches (org-mcp-test--view-matches '((view . "inbox")))))
      (should (= (length matches) 1))
      (let ((match (car matches)))
        (should (equal (alist-get 'title match) "Gamma"))
        (should (equal (alist-get 'todo match) "TODO"))
        (should (equal (alist-get 'level match) 1))
        (should (stringp (alist-get 'link match)))))))

(ert-deftest org-mcp-test-view-runs-a-literal-query ()
  "A view taking no parameters may carry its query rather than a function."
  (org-mcp-test--with-views
    (should
     (equal (org-mcp-test--view-titles '((view . "tangling")))
            '("Delta")))))

(ert-deftest org-mcp-test-view-filter-restricts-it ()
  "Naming a filter restricts the view to what the filter matches."
  (org-mcp-test--with-views
    (should
     (equal
      (sort (org-mcp-test--view-titles '((view . "stuck"))) #'string<)
      '("Alpha" "Beta" "Delta" "Gamma")))
    (should
     (equal
      (org-mcp-test--view-titles '((view . "stuck") (filter . "work")))
      '("Alpha")))
    (should
     (equal
      (org-mcp-test--view-titles
       '((view . "stuck") (filter . "private")))
      '("Beta")))))

(ert-deftest org-mcp-test-view-range-defaults-to-the-first-declared ()
  "A view that takes a range runs at the one it declares first."
  (org-mcp-test--with-views
    (should
     (equal (org-mcp-test--view-titles '((view . "next"))) '("Alpha")))
    (should
     (equal
      (sort
       (org-mcp-test--view-titles '((view . "next") (range . "all")))
       #'string<)
      '("Alpha" "Beta" "Delta" "Gamma")))
    (should
     (equal
      (org-mcp-test--view-titles
       '((view . "next") (range . "all") (filter . "private")))
      '("Beta")))))

(ert-deftest org-mcp-test-view-refuses-an-unknown-view ()
  "An unknown view is refused, and the refusal lists the configured ones."
  (org-mcp-test--with-views
    (org-mcp-test--view-refused
     '((view . "nope"))
     "Unknown view: nope.  Configured views: inbox, stuck, next, \
tangling")))

(ert-deftest org-mcp-test-view-refuses-an-unknown-filter ()
  "An unknown filter is refused, and the refusal lists the valid names."
  (org-mcp-test--with-views
    (org-mcp-test--view-refused
     '((view . "stuck") (filter . "nope"))
     "Unknown filter: nope.  Configured filters: work, private")))

(ert-deftest org-mcp-test-view-declares-one-range-without-parentheses ()
  "A view taking a single range may name it without the parentheses."
  (org-mcp-test--with-configured-server
      ((test-file org-mcp-test--content-views))
      ((org-mcp-views
        '((next
           :name "Next Actions"
           :query org-mcp-test--view-query-next
           :filter t
           :range sprint)))
       (org-mcp-filters org-mcp-test--filters)
       (org-mcp-query-sort-fn nil))
    (should
     (equal (org-mcp-test--view-titles '((view . "next"))) '("Alpha")))
    (should
     (equal
      (org-mcp-test--view-titles '((view . "next") (range . "sprint")))
      '("Alpha")))
    (org-mcp-test--view-refused
     '((view . "next") (range . "all"))
     "Unknown range for the next view: all.  Its ranges: sprint")))

(ert-deftest org-mcp-test-view-refuses-an-unknown-range ()
  "An unknown range is refused, and the refusal lists the view's own."
  (org-mcp-test--with-views
    (org-mcp-test--view-refused
     '((view . "next") (range . "decade"))
     "Unknown range for the next view: decade.  Its ranges: sprint, \
all")))

(ert-deftest org-mcp-test-view-refuses-a-parameter-it-does-not-take ()
  "A parameter a view does not take is refused, not ignored.
The refusal names what that view does take, so a caller that
narrowed nothing learns it rather than reading a full answer as a
narrow one."
  (org-mcp-test--with-views
    (org-mcp-test--view-refused
     '((view . "stuck") (range . "sprint"))
     "The stuck view takes no range.  It takes: filter")
    (org-mcp-test--view-refused
     '((view . "inbox") (filter . "work"))
     "The inbox view takes no filter.  It takes no parameters")
    (org-mcp-test--view-refused
     '((view . "inbox") (range . "sprint"))
     "The inbox view takes no range.  It takes no parameters")))

(ert-deftest org-mcp-test-view-refuses-a-literal-query-it-must-feed ()
  "A view declaring a parameter its literal query cannot take is refused."
  (org-mcp-test--with-configured-server
      ((test-file org-mcp-test--content-views))
      ((org-mcp-views '((broken :query (todo "TODO") :filter t)))
       (org-mcp-filters org-mcp-test--filters)
       (org-mcp-query-sort-fn nil))
    (org-mcp-test--view-refused
     '((view . "broken"))
     "The broken view carries a literal query, which the parameters \
it declares cannot reach")))

(ert-deftest org-mcp-test-view-takes-fields ()
  "A view takes `fields' as the other node-returning endpoints do.
`properties' is turned off here so that what is left is the answer
to `fields' alone: the drawer is a namespace of its own and arrives
unasked, which `org-mcp-test-view-answers-in-every-namespace' pins."
  (org-mcp-test--with-views
    (should
     (equal
      (org-mcp-test--view-matches
       '((view . "inbox")
         (fields . ["title"])
         (properties . "none")))
      '(((title . "Gamma")))))
    (org-mcp-test--call-tool-refused
     "org-view" '((view . "inbox") (fields . ["nonesuch"]))
     (concat
      "\\`"
      (regexp-quote "Unknown node field: nonesuch.  Valid fields: ")))))

(ert-deftest org-mcp-test-view-answers-in-every-namespace ()
  "A view answers in the three namespaces org-query answers in.
A view is a query with a name, so a client that learned to read one
reads the other: the drawer and the computed values arrive unasked,
under their own keys, and \"none\" turns either off for a match list
that does not want it.  A view that answered in fewer would be a
second vocabulary for the same question, which is the thing this
epic removes."
  (let ((org-mcp-computed-fields
         (list (cons 'view-probe (lambda () "seen")))))
    (org-mcp-test--with-views
      (let ((match (car (org-mcp-test--view-matches
                         '((view . "inbox"))))))
        (should (equal (alist-get 'computed match)
                       '((view-probe . "seen"))))
        (should (alist-get 'properties match)))
      ;; And either is turned off by name, as on org-query.
      (let ((match (car (org-mcp-test--view-matches
                         '((view . "inbox")
                           (properties . "none")
                           (computed . "none"))))))
        (should-not (alist-get 'computed match))
        (should-not (alist-get 'properties match)))
      (org-mcp-test--call-tool-refused
       "org-view" '((view . "inbox") (computed . ["nonesuch"]))
       (concat
        "\\`"
        (regexp-quote "Unknown computed field: nonesuch."))))))

(ert-deftest org-mcp-test-view-refuses-files ()
  "A view never takes a scope override: it declares no `files'."
  (org-mcp-test--with-views
    (org-mcp-test--call-tool-refused
     "org-view"
     `((view . "inbox") (files . ,(vector test-file)))
     "Unexpected parameter: files")))

(ert-deftest org-mcp-test-view-sorts-by-the-configured-comparator ()
  "A view answers in the order `org-mcp-query-sort-fn' puts matches in."
  (org-mcp-test--with-configured-server
      ((test-file org-mcp-test--content-views))
      ((org-mcp-views org-mcp-test--views)
       (org-mcp-filters org-mcp-test--filters)
       (org-mcp-query-sort-fn
        (lambda (a b)
          (string>
           (org-element-property :raw-value a)
           (org-element-property :raw-value b)))))
    (should
     (equal (org-mcp-test--view-titles '((view . "stuck")))
            '("Gamma" "Delta" "Beta" "Alpha")))))

(ert-deftest org-mcp-test-view-tool-is-the-only-tool-views-add ()
  "Configuring views adds exactly the org-view tool."
  (let ((org-mcp-views org-mcp-test--views))
    (org-mcp-test--with-enabled
      (should
       (equal
        (org-mcp-test--registered-tool-ids)
        (sort
         (cons "org-view" (copy-sequence org-mcp-test--unconditional-tool-ids))
         #'string<))))))

(ert-deftest org-mcp-test-view-tool-not-registered-without-views ()
  "The org-view tool stays away while no view is configured."
  (org-mcp-test--with-configured-server
      ((test-file org-mcp-test--content-views))
      ((org-mcp-views nil))
    (org-mcp-test--call-tool-refused
     "org-view" '((view . "inbox")) "\\`Tool not found: org-view\\'")))

(ert-deftest org-mcp-test-view-tool-description-names-the-vocabulary ()
  "The org-view description carries the views, what each takes and the filters.
A closed vocabulary is only closed to a client that can see it, and
a view's range default is stated where the caller reads it."
  (org-mcp-test--with-views
    (let ((description
           (org-mcp-test--registered-tool-description "org-view")))
      (dolist (line
               '("inbox (Inbox) - takes no parameters"
                 "stuck (Stuck Projects) - takes filter"
                 "next (Next Actions) - takes filter, range \
(sprint, all; sprint unasked)"
                 "tangling (Tangling) - takes no parameters"
                 "Configured filters: work, private"))
        (should (string-match-p (regexp-quote line) description))))))

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
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-03-23 [A-Za-z]\\{2,3\\} 14:30\\]"
   "--\\[2026-03-23 [A-Za-z]\\{2,3\\} 16:45\\] =>  2:15\n"
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
   "--\\[2026-03-23 [A-Za-z]\\{2,3\\} 16:45\\] =>  2:15\n"
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
  "org-node-read resolves a link to a heading and a file link to the file."
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
         test-file "org-node-set-tags"
         `((link . ,(format "file:%s" test-file)) (after . "work")))))
      (org-mcp-test--add-todo-and-check
       "New Task" "TODO" nil nil (format "file:%s" test-file) nil
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-links-top-level-added))))

(defconst org-mcp-test--file-level-id-link "id:file-id"
  "Link to the file-level ID of `org-mcp-test--content-file-drawer'.")

(ert-deftest org-mcp-test-link-file-level-id-addresses-file ()
  "An `id:' link to a file-level property drawer addresses the whole file.
The ID is found through Emacs's ID index, or, without that index, in
the file the call names in `files'.  Either way org-node-read and
org-node-text read the file exactly as its `file:' link reads,
and so does the org://{link} resource, which takes no `files'.  With a
search part, the link searches the whole file for a heading.
org-node-set-tags refuses the link as it refuses a `file:' link without a
search part, leaving the file unchanged, and org-node-create adds a
heading at the top level of the file."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (link org-mcp-test--file-level-id-link))
    (dolist (named '(nil t))
      (org-mcp-test--with-temp-org-files
          ((test-file org-mcp-test--content-file-drawer))
        (org-mcp-test--with-id-tracking
            (list test-file)
            (unless named
              `(("file-id" . ,test-file)))
          (let* ((files (and named (vector test-file)))
                 (file-link (concat "file:" test-file))
                 (with-files
                  (lambda (params)
                    (append params (and files `((files . ,files))))))
                 (calls
                  (lambda ()
                    (should
                     (string=
                      (org-mcp-test--call-read-headline link files)
                      org-mcp-test--content-file-drawer))
                    (should
                     (string=
                      (mcp-server-lib-ert-call-tool
                       "org-node-read" (funcall with-files `((link . ,link))))
                      (org-mcp-test--call-read file-link)))
                    (should
                     (string=
                      (org-mcp-test--call-read-headline
                       (concat link "::*Existing") files)
                      "* Existing"))
                    (org-mcp-test--call-tool-refused
                     "org-node-set-tags"
                     (funcall with-files `((link . ,link) (after . "work")))
                     (concat
                      "\\`Link does not point to a heading: "
                      (regexp-quote link) "\\'")
                     test-file)
                    (mcp-server-lib-ert-call-tool
                     "org-node-create"
                     (funcall with-files
                              `((title . "New")
                                (todo . "TODO")
                                (content . nil)
                                (parent . ,link)
                                (previous_sibling . nil)))))))
            (if named
                (org-mcp-test--without-id-index
                  (funcall calls))
              (should
               (string=
                (org-mcp-test--read-resource (concat "org://" link))
                (org-mcp-test--read-resource (concat "org://" file-link))))
              (funcall calls))
            (org-mcp-test--verify-file-matches
             test-file org-mcp-test--regex-file-drawer-top-level-added)))))))

(ert-deftest org-mcp-test-clock-in-file-level-id-keeps-running-clock ()
  "Test clock-in refuses a file-level ID before closing the running clock.
The link addresses the whole file, not a heading, so clock-in refuses
it although clock_out names the running clock correctly.  Neither
file, the buffer of the running clock, nor the running clock changes."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-file-drawer)
       (running-file
        org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-session-clock running-file
      (let ((position (marker-position org-clock-marker)))
        (org-mcp-test--without-id-index
          (should
           (string-match-p
            (concat
             "\\`Link does not point to a heading: "
             (regexp-quote org-mcp-test--file-level-id-link) "\\'")
            (org-mcp-test--call-tool-expecting-error
             running-file "org-clock-in"
             `((link . ,org-mcp-test--file-level-id-link)
               (files . ,(vector test-file))
               (start_time . "2026-01-01T11:00:00")
               (clock_out
                . ,(org-mcp-test--file-link running-file "*Task One")))))))
        (should (string= (org-mcp-test--read-file test-file)
                         org-mcp-test--content-file-drawer))
        (org-mcp-test--verify-no-modified-buffer running-file)
        (should (eq (org-clock-is-active) (find-buffer-visiting running-file)))
        (should (= (marker-position org-clock-marker) position))))))

(ert-deftest org-mcp-test-link-title-search-resolves-to-first-match ()
  "A title search that matches several headings resolves to the first."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (let ((link (format "file:%s::*Review" test-file)))
      (should
       (string=
        (org-mcp-test--call-read-headline link) "** Review\nAlpha review."))
      (mcp-server-lib-ert-call-tool
       "org-node-set-title"
       `((link . ,link)
         (before . "Review")
         (after . "First Review")))
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
       test-file "org-node-text"
       `((link
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
       test-file "org-node-text"
       `((link . ,(format "file:%s::7" test-file))))))))

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
               "org-node-set-tags" `((link . ,link) (after . "work"))))))
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
               "org-node-set-tags"
               `((link . ,(format form test-file)) (after . "work"))))))
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
               "org-node-set-tags"
               `((link . ,(format form test-file)) (after . "work"))))))
        (should (equal (alist-get 'success result) t))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--regex-links-gamma-tagged)))))

(ert-deftest org-mcp-test-link-update-todo-state ()
  "org-node-set-todo accepts a title link."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (let ((result
           (org-mcp-test--call-update-todo-state
            (format "file:%s::*Gamma" test-file) "DONE" "TODO")))
      (should (equal (alist-get 'before result) "TODO"))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-links-gamma-done))))

(ert-deftest org-mcp-test-link-edit-body ()
  "org-node-set-content accepts a bracketed custom ID link."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (org-mcp-test--call-edit-body-and-check
     test-file
     (format "[[file:%s::#alpha-slug][Alpha]]" test-file)
     nil
     "Alpha appended."
     org-mcp-test--regex-links-alpha-body-appended
     t
     (org-mcp-test--file-link test-file "#alpha-slug"))))

(ert-deftest org-mcp-test-link-heading-tools ()
  "Every other tool that changes a heading accepts a link."
  (dolist (case
           '(("org-node-set-properties" (after . ((FOO . "bar"))))
             ("org-node-set-scheduled" (after . "2026-03-27"))
             ("org-node-set-deadline" (after . "2026-03-27"))
             ("org-node-set-priority" (after . "A"))
             ("org-node-add-note" (note . "Checked"))))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-links))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               (car case)
               (cons
                `(link . ,(format "[[file:%s::*Gamma][Gamma]]" test-file))
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
     `((link . ,(format "file:%s" test-file))
       (end_time . "2026-03-23T16:45:00")))
    (org-mcp-test--verify-file-matches
     test-file org-mcp-test--regex-links-gamma-clocked)))

(ert-deftest org-mcp-test-link-add-todo-after-sibling ()
  "org-node-create takes its parent and the sibling to follow as links."
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
  "org-node-create refuses a sibling link that is not a child of the parent."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
        (list org-mcp-test--link-beta-id)
      (should
       (string-match-p
        "not found under parent"
        (org-mcp-test--call-tool-expecting-error
         test-file "org-node-create"
         `((title . "New Task")
           (todo . "TODO")
           (tags . nil)
           (content . nil)
           (parent . ,(format "id:%s" org-mcp-test--link-beta-id))
           (previous_sibling . ,(format "file:%s::*Review" test-file)))))))))

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
                     `(("org-node-text" (link . ,link))
                       ("org-node-set-tags" (link . ,link) (after . "work"))))
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
               `(("org-node-text" (link . ,link))
                 ("org-node-set-tags" (link . ,link) (after . "work"))))
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
         'after
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
         "org-node-text" `((link . ,link)) "not in allowed list")
        (org-mcp-test--call-tool-refused
         "org-node-set-todo" `((link . ,link) (after . "DONE"))
         "not in allowed list"
         out))
      (org-mcp-test--with-id-tracking
          (list allowed)
          `((,org-mcp-test--content-with-id-id . ,with-id))
        (let ((link (format "id:%s" org-mcp-test--content-with-id-id)))
          (org-mcp-test--call-tool-refused
           "org-node-text" `((link . ,link)) "not in allowed list")
          (org-mcp-test--call-tool-refused
           "org-node-set-todo" `((link . ,link) (after . "DONE"))
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
a write, and as the parent and the sibling of `org-node-create'.  Every
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
                     `(("org-node-text" (link . ,link))
                       ("org-node-set-tags" (link . ,link) (after . "work"))
                       ("org-node-create"
                        (title . "New Task")
                        (todo . "TODO")
                        (tags . nil)
                        (content . nil)
                        (parent . ,link))
                       ("org-node-create"
                        (title . "New Task")
                        (todo . "TODO")
                        (tags . nil)
                        (content . nil)
                        (parent . ,(format "file:%s::*Gamma" test-file))
                        (previous_sibling . ,link))))
              (org-mcp-test--call-tool-refused
               (car call) (cdr call) "Send a full path" test-file))))
        (should (null ops))))))

(ert-deftest org-mcp-test-link-bracketed-outline-path-refused ()
  "A bracketed path with an outline path is refused with the link forms.
Org reads `[[/path.org#Parent/Child]]' as a `file:' link to a file
named with `#', which does not exist, so the call is refused as for a
file outside the allowed files, and the message names the link forms
to send.  Every refused path holding `#' carries that hint, also one
naming an existing file, such as an auto-save file, so the refusal
never tells whether a file outside the scope exists.  A refused path
without `#' carries no hint."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links)
       (other-file org-mcp-test--content-links))
    (let* ((org-mcp-allowed-files (list test-file))
           (dir (file-name-directory other-file))
           (auto-save
            (org-mcp-test--write-file
             dir (format "#%s#" (file-name-nondirectory other-file))
             org-mcp-test--content-links))
           (outline-path (format "[[%s#Beta/Review]]" test-file))
           (outside (format "[[%s::*Beta]]" other-file)))
      (unwind-protect
          (dolist (link
                   (list outline-path
                         (format "[[%s]]" auto-save)
                         (format "file:%s" auto-save)
                         (format "[[%s#missing#]]" dir)))
            (dolist (call
                     `(("org-node-text" (link . ,link))
                       ("org-node-set-tags" (link . ,link) (after . "work"))))
              (should
               (string-match-p
                (concat
                 "\\`'" (regexp-quote link)
                 "': the referenced file not in allowed list\\.  "
                 (regexp-quote org-mcp--link-forms-hint) "\\'")
                (org-mcp-test--call-tool-expecting-error
                 test-file (car call) (cdr call))))))
        (delete-file auto-save))
      (should
       (string-match-p
        (concat
         "\\`'" (regexp-quote outside)
         "': the referenced file not in allowed list\\'")
        (org-mcp-test--call-tool-expecting-error
         other-file "org-node-text" `((link . ,outside))))))))

(defconst org-mcp-test--content-block-star-line
  (concat
   ":PROPERTIES:\n:ID:       file-level-id\n:END:\n#+TITLE: Block\n"
   "#+begin_example\n* In block\n#+end_example\n"
   "* Real\nReal body.\n")
  "File whose preamble opens a block holding a line starting with `* '.
Org parses that line as a heading, which ends the block unclosed.")

(defconst org-mcp-test--content-block-escaped-preamble
  (concat
   ":PROPERTIES:\n:ID:       file-level-id\n:END:\n#+TITLE: Block\n"
   "#+begin_example\n,* Escaped\n#+end_example\n")
  "Preamble holding a block whose `* ' line is escaped with a comma.")

(defconst org-mcp-test--content-block-escaped
  (concat org-mcp-test--content-block-escaped-preamble "* Real\nReal body.\n")
  "File whose only heading follows a block with an escaped `* ' line.")

(ert-deftest org-mcp-test-read-lists-headings-org-parses ()
  "org-node-read lists a file's top-level headings as Org's parser finds them.
It agrees with org-read-outline.  A line starting with `* ' is a
heading even inside a block, as Org parses it, and is listed.  A line
escaped with a comma, as Org writes one inside a block, is no heading:
it stays in the preamble, and a title link to it is refused by a read
and by a write, leaving the file unchanged."
  (org-mcp-test--with-temp-org-files
      ((star-file org-mcp-test--content-block-star-line)
       (escaped-file org-mcp-test--content-block-escaped))
    (cl-flet
        ((titles
          (headings)
          (mapcar (lambda (heading) (alist-get 'title heading)) headings))
         (read-file
          (file)
          (json-read-from-string
           (org-mcp-test--call-read (concat "file:" file)))))
      (pcase-dolist (`(,file ,expected)
                     `((,star-file ("In block" "Real"))
                       (,escaped-file ("Real"))))
        (should (equal (titles (alist-get 'children (read-file file)))
                       expected))
        (should
         (equal (titles
                 (alist-get 'children (org-mcp-test--call-read-outline file)))
                expected)))
      (should
       (equal (alist-get 'content (read-file escaped-file))
              (string-trim org-mcp-test--content-block-escaped-preamble)))
      (let ((escaped (org-mcp-test--file-link escaped-file "*Escaped")))
        (org-mcp-test--call-tool-refused
         "org-node-text" `((link . ,escaped))
         (concat "\\`Cannot resolve link " (regexp-quote escaped))
         escaped-file)
        (org-mcp-test--call-tool-refused
         "org-node-set-tags" `((link . ,escaped) (after . "oops"))
         (concat "\\`Cannot resolve link " (regexp-quote escaped))
         escaped-file)))))

(ert-deftest org-mcp-test-link-refuses-regexp-search ()
  "A regexp search is refused, since Org answers it with a sparse tree."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (should
     (string-match-p
      "Regexp search is not supported"
      (org-mcp-test--call-tool-expecting-error
       test-file "org-node-text"
       `((link . ,(format "file:%s::/Gam.*/" test-file))))))
    (should-not (find-buffer-visiting test-file))))

(ert-deftest org-mcp-test-link-file-outside-allowed-files-refused ()
  "A file link naming a file outside the allowed files is refused."
  (org-mcp-test--with-temp-org-files
      ((allowed-file "* Allowed\n")
       (other-file org-mcp-test--content-links))
    (let ((org-mcp-allowed-files (list allowed-file))
          (link (format "file:%s::*Gamma" other-file)))
      (dolist (call
               `(("org-node-text" (link . ,link))
                 ("org-node-set-tags" (link . ,link) (after . "work"))))
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
                 `(("org-node-text" (link . ,link))
                   ("org-node-set-tags" (link . ,link) (after . "work"))))
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
                               '(("org-node-text"
                                  (link . "id:no-such-id"))
                                 ("org-node-set-tags"
                                  (link . "[[id:no-such-id][Gone]]")
                                  (after . "work"))))
                        (should
                         (string=
                          (org-mcp-test--call-tool-expecting-error
                           current (car call) (cdr call))
                          "Cannot find ID 'no-such-id'"))))
                  (kill-buffer buf))))
          (delete-file org-id-locations-file))))))

(ert-deftest org-mcp-test-link-id-indexed-remote-file-untouched ()
  "An ID the index places in a remote file is refused before TRAMP runs.
The fake remote method records every file operation but
`file-remote-p'; none may run.  The ID's `id:' link is refused, bare
and with a search, by a read, a write and the resource, as outside the
allowed files and without naming the file, even when the remote file
is listed among them, and the calls run from a buffer visiting an
allowed file."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (let ((remote (concat org-mcp-test--remote-prefix "/x/notes.org")))
      (org-mcp-test--with-id-tracking
          (list test-file remote)
          `(("remote-id" . ,remote))
        (let ((buf (find-file-noselect test-file)))
          (unwind-protect
              (with-current-buffer buf
                (org-mcp-test--with-remote-probe ops
                  (dolist (link '("id:remote-id" "[[id:remote-id::*Task]]"))
                    (let ((refusal
                           (concat
                            "\\`'" (regexp-quote link)
                            "': the referenced file not in allowed list\\'")))
                      (org-mcp-test--call-tool-refused
                       "org-node-text" `((link . ,link)) refusal)
                      (org-mcp-test--call-tool-refused
                       "org-node-set-tags" `((link . ,link) (after . "work"))
                       refusal test-file)
                      (should
                       (string-match-p
                        refusal
                        (org-mcp-test--resource-error
                         (concat "org://" (url-hexify-string link)))))))
                  (should (null ops))))
            (kill-buffer buf)))))))

(ert-deftest org-mcp-test-link-id-rescans-stale-index ()
  "An ID the index places in a file that lacks it is found by a rescan.
As `org-id-find' does, a miss rescans Emacs's ID index once, and the
ID resolves in the allowed file that holds it: a read returns the
heading, a write changes that file alone, and the index then names it."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-temp-org-files
        ((stale-file "* Other\nOther body.\n")
         (test-file org-mcp-test--content-links))
      (org-mcp-test--with-id-tracking
          (list stale-file test-file)
          `((,org-mcp-test--link-beta-id . ,stale-file))
        (let ((org-agenda-files (list stale-file test-file))
              (org-id-locations-file
               (make-temp-file "org-mcp-test-id-locations"))
              (link (format "id:%s" org-mcp-test--link-beta-id)))
          (unwind-protect
              (progn
                (should
                 (string=
                  (org-mcp-test--call-read-headline link)
                  (string-trim-right org-mcp-test--content-links-beta)))
                (org-mcp-test--update-todo-state-and-check
                 link "" "TODO" test-file
                 org-mcp-test--regex-links-beta-changed)
                (should
                 (string= (org-mcp-test--read-file stale-file)
                          "* Other\nOther body.\n"))
                (should
                 (file-equal-p
                  (gethash org-mcp-test--link-beta-id org-id-locations)
                  test-file)))
            (delete-file org-id-locations-file)))))))

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
                       `(("org-node-set-title"
                          ,(format "[[id:%s::*Second Child]]" parent)
                          ((before . "Second Child")
                           (after . "Renamed Second Child")))
                         ("org-node-text"
                          ,(format "id:%s::*Third Child #3" parent)
                          nil
                          "** Third Child #3")
                         ("org-node-text"
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
                (let ((link (nth 1 call))
                      (expected (nth 3 call)))
                  (let ((result
                         (mcp-server-lib-ert-call-tool
                          (nth 0 call) (cons `(link . ,link) (nth 2 call)))))
                    (when expected
                      (should (string= result expected))))
                  (should
                   (equal
                    (cons link (org-mcp-test--view-state))
                    (cons link before)))))
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
  "The resource reads every link org-node-read takes and changes nothing.
Each native link is sent raw and in each spelling of
`org-mcp-test--resource-uris', and every read returns what org-node-read
returns for the same link.  A bare ID, a bare path and an outline path
are no links: the resource refuses each, sent raw, with the message
org-node-read refuses it with.  The reads run once with no buffer on the
file and once with one; the file stays byte-for-byte unchanged and
the buffer unmodified."
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
                  (let ((message
                         (org-mcp-test--call-tool-expecting-error
                          test-file "org-node-read" `((link . ,address)))))
                    (should (string-prefix-p "Not an Org link: " message))
                    (should
                     (equal
                      (org-mcp-test--resource-error
                       (concat "org://" address))
                      message))))
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
`org-mcp-test--resource-uris' reads the heading org-node-read reads for the
same link.  Sent without encoding, `%41' and `%25' are percent
escapes, so the URI names another file, and the resource refuses it as
org-node-read refuses that file.  `%0A' and `%0D' decode to a line feed and
a carriage return, so the resource refuses a title holding them as
org-node-read refuses the decoded link."
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
         test-file "org-node-read" `((link . ,(cdr case)))))))))

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
   "Größe body\\.\n"
   "\\'")
  "Regex matching `org-mcp-test--content-non-ascii-titles' after DONE.")

(defconst org-mcp-test--regex-non-ascii-titles-added
  (concat
   "\\`\\* Ärger\n"
   "Ärger body\\.\n"
   "\\*\\* DONE Größe #3\n"
   "Größe body\\.\n"
   "\\*\\* TODO New Task *\n"
   "\\'")
  "Regex matching the DONE file after adding a TODO under Ärger.")

(ert-deftest org-mcp-test-title-link-non-ascii-titles ()
  "A title link reaches headings with non-ASCII titles.
org-node-read, the resource with the link sent raw and in each spelling
of `org-mcp-test--resource-uris', which encode the titles as UTF-8 or
leave them raw, org-node-text, org-node-set-todo and
org-node-create's parent all reach the heading.  The same titles in a
percent-encoded outline path are no link and are refused."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-non-ascii-titles))
      (let* ((parent (org-mcp-test--file-link test-file "*Ärger"))
             (child (org-mcp-test--file-link test-file "*Größe #3"))
             (outline-path
              (format "%s#%s/%s" test-file (url-hexify-string "Ärger")
                      (url-hexify-string "Größe #3")))
             (expected (org-mcp-test--call-read child)))
        (should
         (equal
          (alist-get 'title (json-read-from-string expected)) "Größe #3"))
        (dolist (uri
                 (cons (concat "org://" child)
                       (org-mcp-test--resource-uris child)))
          (should
           (equal
            (cons uri (org-mcp-test--read-resource uri))
            (cons uri expected))))
        (should
         (string-prefix-p
          "Not an Org link: "
          (org-mcp-test--call-tool-expecting-error
           test-file "org-node-read" `((link . ,outline-path)))))
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

(ert-deftest org-mcp-test-resource-decodes-slash-in-title-once ()
  "A `%2F' in the resource URI is a slash inside the title, decoded once.
The title link to Parent/Child, with its slash sent raw or as `%2F',
reads the heading Parent/Child, not Real Child under Parent.  The
outline path that spelled the title with `%2F' is no link: the
resource decodes it once and refuses it as org-node-read refuses the
decoded path."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-slash-not-nested-before))
    (let* ((address (org-mcp-test--file-link test-file "*Parent/Child"))
           (outline-path (format "%s#Parent%%2FChild" test-file))
           (expected (org-mcp-test--call-read address)))
      (should
       (equal (alist-get 'title (json-read-from-string expected)) "Parent/Child"))
      (should
       (string-match-p "%2F" (concat "org://" (url-hexify-string address))))
      (dolist (uri (list (concat "org://" address)
                         (concat "org://" (url-hexify-string address))))
        (should
         (equal (cons uri (org-mcp-test--read-resource uri))
                (cons uri expected))))
      (should
       (equal
        (org-mcp-test--resource-error (concat "org://" outline-path))
        (org-mcp-test--call-tool-expecting-error
         test-file "org-node-read"
         `((link . ,(format "%s#Parent/Child" test-file)))))))))

(ert-deftest org-mcp-test-resource-refuses-as-tools-do ()
  "The resource refuses a link with the message the read tools give.
The links run code or open something, name no local file by its full
path, search by regexp, or name what the call may not reach: a file
outside the allowed files, a missing file, an ID in a file outside
them and an unknown ID.  Others are no links at all: a bare ID, a
bare path, an outline path and a link behind a second org://.  Each
goes to org-node-read and org-node-text,
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
                        "id:no-such-id"
                        org-mcp-test--link-beta-id
                        test-file
                        (format "%s#Alpha/Review" test-file)
                        "org://id:no-such-id"))
                (let ((message
                       (org-mcp-test--call-tool-expecting-error
                        test-file "org-node-read" `((link . ,link)))))
                  (should
                   (equal
                    (org-mcp-test--call-tool-expecting-error
                     test-file "org-node-text" `((link . ,link)))
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

;; Heading tools taking a file set

(defconst org-mcp-test--scope-task-with-id-done-regex
  (concat
   "\\`\\* DONE Task\n"
   ":PROPERTIES:\n"
   ":ID:       " (regexp-quote org-mcp-test--content-with-id-id) "\n"
   ":END:\n"
   "Body\n"
   "\\'")
  "Regex matching the whole ID scope-test file after Task becomes DONE.")

(defconst org-mcp-test--regex-links-beta-changed
  (concat
   "\\`"
   (regexp-quote
    (concat
     org-mcp-test--content-links-preamble
     org-mcp-test--content-links-alpha))
   "\\* [^\n]*Beta[^\n]*\n"
   "\\(?:.\\|\n\\)*"
   ":ID: +" org-mcp-test--link-beta-id "\n"
   "\\(?:.\\|\n\\)*"
   (regexp-quote org-mcp-test--content-links-gamma)
   "\\'")
  "Regex matching the links file after any change inside Beta only.")

(defun org-mcp-test--id-locations ()
  "Return the entries of `org-id-locations' as an alist of files and IDs."
  (and (hash-table-p org-id-locations)
       (org-id-hash-to-alist org-id-locations)))

(defmacro org-mcp-test--without-id-index (&rest body)
  "Run BODY and assert that it never used Org's ID index.
While BODY runs, `org-id-find', `org-id-find-id-file' and
`org-id-update-id-locations', which consult or rescan the index,
record each call and fail loudly; no call may be recorded.
`org-id-locations' and `org-id-files' must hold the same entries
afterwards as before."
  (declare (indent 0) (debug t))
  (let ((calls (make-symbol "calls"))
        (before (make-symbol "before"))
        (files-before (make-symbol "files-before")))
    `(let ((,calls nil)
           (,before (org-mcp-test--id-locations))
           (,files-before (copy-sequence org-id-files)))
       (cl-letf ,(mapcar
                  (lambda (fn)
                    `((symbol-function ',fn)
                      (lambda (&rest _)
                        (push ',fn ,calls)
                        (error "%s ran" ',fn))))
                  '(org-id-find
                    org-id-find-id-file org-id-update-id-locations))
         ,@body)
       (should (null ,calls))
       (should (equal (org-mcp-test--id-locations) ,before))
       (should (equal org-id-files ,files-before)))))

(defun org-mcp-test--assert-id-task-permitted (file files)
  "Assert that Task in FILE is read and then made DONE by its `id:' link.
FILE holds `org-mcp-test--scope-task-with-id-content'.  FILES, when
non-nil, is sent as the `files' parameter, and the calls must then
leave Org's ID index alone."
  (let ((read-and-write
         (lambda ()
           (should
            (string=
             (org-mcp-test--call-read-headline
              org-mcp-test--scope-id-link files)
             (string-trim-right org-mcp-test--scope-task-with-id-content)))
           (should
            (equal
             (alist-get
              'after
              (org-mcp-test--call-update-todo-state
               org-mcp-test--scope-id-link "DONE" "TODO" nil files))
             "DONE")))))
    (if files
        (org-mcp-test--without-id-index
          (funcall read-and-write))
      (funcall read-and-write))
    (org-mcp-test--verify-file-matches
     file org-mcp-test--scope-task-with-id-done-regex)))

(defun org-mcp-test--assert-id-task-refused (file files refusal)
  "Assert that reading and writing Task in FILE by its `id:' link is refused.
FILE holds `org-mcp-test--scope-task-with-id-content' and stays
unchanged.  FILES, when non-nil, is sent as the `files' parameter.
Each refusal must match the regexp REFUSAL."
  (let ((params
         `((link . ,org-mcp-test--scope-id-link)
           ,@(when files `((files . ,files))))))
    (org-mcp-test--call-tool-refused "org-node-text" params refusal)
    (org-mcp-test--call-tool-refused
     "org-node-set-todo"
     (append params '((before . "TODO") (after . "DONE")))
     refusal file)))

(ert-deftest org-mcp-test-file-set-id-in-allowed-file ()
  "An ID in an allowed file resolves whether or not the call names its file.
This holds for a read and a write under every override setting.  The
file lies outside every root, so only being allowed makes it
reachable."
  (dolist (kind '(nil roots t))
    (dolist (named '(nil t))
      (org-mcp-test--with-scope-dirs (if (eq kind 'roots)
                                         (list root)
                                       kind)
        (let ((file (org-mcp-test--write-file
                     outside "task.org"
                     org-mcp-test--scope-task-with-id-content)))
          (org-mcp-test--with-id-tracking
              (list file)
              `((,org-mcp-test--content-with-id-id . ,file))
            (org-mcp-test--assert-id-task-permitted
             file (and named (vector file)))))))))

(ert-deftest org-mcp-test-file-set-id-outside-allowed-files ()
  "An ID outside the allowed files resolves only once the call names its file.
The ID is in Emacs's index.  Without `files' it is refused under every
override setting, by an error naming the link, not the file, and so is
the org://{link} resource, which takes no `files'.  With
`files' naming its file, it is refused under nil and for a file
outside every root, by an error naming the file as the call sent it,
and it resolves for a file under a root and under t."
  (pcase-dolist (`(,kind ,under-root ,permitted)
                 '((nil t nil) (roots t t) (roots nil nil) (t nil t)))
    (dolist (named '(nil t))
      (org-mcp-test--with-scope-dirs (if (eq kind 'roots)
                                         (list root)
                                       kind)
        (let ((file (org-mcp-test--write-file
                     (if under-root
                         root
                       outside)
                     "task.org" org-mcp-test--scope-task-with-id-content)))
          (org-mcp-test--with-id-tracking
              (list allowed)
              `((,org-mcp-test--content-with-id-id . ,file))
            (cond
             ((not named)
              (org-mcp-test--assert-id-task-refused
               file nil
               (org-mcp-test--refused-path-regexp
                org-mcp-test--scope-id-link))
              ;; A resource URI names no file set, so the resource
              ;; refuses the ID under every setting.
              (should
               (string-match-p
                (org-mcp-test--refused-path-regexp
                 org-mcp-test--scope-id-link)
                (org-mcp-test--resource-error
                 (concat "org://" org-mcp-test--scope-id-link)))))
             (permitted
              (org-mcp-test--assert-id-task-permitted file (vector file)))
             (t
              (org-mcp-test--assert-id-task-refused
               file (vector file)
               (org-mcp-test--refused-path-regexp file))))))))))

(ert-deftest org-mcp-test-file-set-id-in-unindexed-file ()
  "An ID in a file Emacs never indexed resolves once the call names its file.
The index holds no entry for the ID, before or after.  The file is
named directly or through its directory, which t lets the call
search, and it is an allowed file or lies outside the allowed files."
  (dolist (via-directory '(nil t))
    (dolist (in-allowed '(nil t))
      (org-mcp-test--with-scope-dirs t
        (let ((file (org-mcp-test--write-file
                     outside "task.org"
                     org-mcp-test--scope-task-with-id-content)))
          (org-mcp-test--with-id-tracking
              (list
               (if in-allowed
                   file
                 allowed))
              nil
            (org-mcp-test--assert-id-task-permitted
             file
             (vector
              (if via-directory
                  outside
                file)))
            (should-not
             (org-mcp-test--id-registered-p
              org-mcp-test--content-with-id-id))))))))

(ert-deftest org-mcp-test-file-set-id-first-match-in-order ()
  "An ID held by several named files resolves in the first of them.
The order is that of `files', not that of the allowed files."
  (org-mcp-test--with-scope-dirs nil
    (let ((a-file (org-mcp-test--write-file
                   root "a.org" org-mcp-test--scope-task-with-id-content))
          (b-file (org-mcp-test--write-file
                   outside "b.org" org-mcp-test--scope-task-with-id-content)))
      (org-mcp-test--with-id-tracking (list a-file b-file) nil
        (org-mcp-test--without-id-index
          (org-mcp-test--call-update-todo-state
           org-mcp-test--scope-id-link "DONE" "TODO" nil
           (vector b-file a-file)))
        (org-mcp-test--verify-file-matches
         b-file org-mcp-test--scope-task-with-id-done-regex)
        (should
         (string=
          (org-mcp-test--read-file a-file)
          org-mcp-test--scope-task-with-id-content))))))

(ert-deftest org-mcp-test-file-set-id-not-in-named-files ()
  "A named set without the ID is an error naming the ID and the set as sent.
The set is a file and a directory, as an array or a single path, and
the error never names a file found under the directory.  The ID is in
Emacs's index, in an allowed file, and is not looked up there."
  (org-mcp-test--with-scope-dirs t
    (let ((holder (org-mcp-test--write-file
                   root "holder.org"
                   org-mcp-test--scope-task-with-id-content))
          (other (org-mcp-test--write-file
                  outside "other.org" org-mcp-test--scope-task-content))
          (dir (file-name-as-directory (expand-file-name "dir" outside)))
          (refusal
           (lambda (&rest entries)
             (concat
              "\\`Cannot find ID '"
              (regexp-quote org-mcp-test--content-with-id-id)
              "' in files: "
              (regexp-quote (string-join entries ", "))
              "\\'"))))
      (org-mcp-test--write-file dir "inner.org" org-mcp-test--scope-task-content)
      (org-mcp-test--with-id-tracking
          (list holder)
          `((,org-mcp-test--content-with-id-id . ,holder))
        (org-mcp-test--without-id-index
          (org-mcp-test--assert-id-task-refused
           other (vector other dir) (funcall refusal other dir))
          (org-mcp-test--assert-id-task-refused
           other other (funcall refusal other)))
        (should
         (string=
          (org-mcp-test--read-file holder)
          org-mcp-test--scope-task-with-id-content))))))

(ert-deftest org-mcp-test-file-set-refused-with-address-naming-file ()
  "`files' applies only to an `id:' link; any other link refuses it.
A `file:' link names its file already, and a custom ID search is no
`id:' link.  Each is refused, before any file is opened, for a read, a
write, and as the parent of org-node-create.  A path with an outline
path and a bare ID are no links, and are refused as such with `files'
as without.  The file stays unchanged.  org-node-create's sibling never
uses `files', so next to an `id:' parent it may be any link: a
`file:' title or custom ID link, bare or bracketed, inserts after it,
and a bare ID is refused as no link, leaving its file unchanged."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-scope-dirs t
      (let* ((file (org-mcp-test--write-file
                    outside "task.org"
                    org-mcp-test--scope-task-with-id-content))
             (files (vector file))
             (refusal
              (lambda (address)
                (if (member address
                            (list (format "%s#Task" file)
                                  org-mcp-test--content-with-id-id
                                  "file-set-sibling-id"))
                    (concat "\\`Not an Org link: " (regexp-quote address))
                  (concat
                   "\\`files applies only to an id: link: "
                   (regexp-quote address)
                   "\\'")))))
        (org-mcp-test--with-id-tracking
            (list file)
            `((,org-mcp-test--content-with-id-id . ,file))
          (dolist (address
                   (list
                    (format "file:%s::*Task" file)
                    (format "[[file:%s::*Task][Task]]" file)
                    (format "file:%s" file)
                    (format "%s#Task" file)
                    org-mcp-test--content-with-id-id
                    "[[#task-slug]]"))
            (dolist (call
                     `(("org-node-text" (link . ,address))
                       ("org-node-set-todo"
                        (link . ,address) (after . "DONE"))
                       ("org-node-create"
                        (title . "New Task")
                        (todo . "TODO")
                        (content . nil)
                        (parent . ,address))))
              (org-mcp-test--call-tool-refused
               (car call)
               (append (cdr call) `((files . ,files)))
               (funcall refusal address)
               file)))
          (should-not (find-buffer-visiting file))
          (let ((count 0))
            (dolist (after
                     '("file:%s::*Sibling"
                       "file:%s::#sibling-slug"
                       "[[file:%s::*Sibling][Sibling]]"
                       "file-set-sibling-id"))
              (let* ((parent-file
                      (org-mcp-test--write-file
                       outside (format "parent-%d.org" (cl-incf count))
                       org-mcp-test--content-sibling-parent))
                     (after-link (format after parent-file))
                     (params
                      `((title . "New Task")
                        (todo . "TODO")
                        (content . nil)
                        (parent . "id:file-set-parent-id")
                        (previous_sibling . ,after-link)
                        (properties . ((ID . "new-task-id")))
                        (files . ,(vector parent-file)))))
                (if (string-prefix-p "file-set" after-link)
                    (org-mcp-test--call-tool-refused
                     "org-node-create" params (funcall refusal after-link)
                     parent-file)
                  (mcp-server-lib-ert-call-tool "org-node-create" params)
                  (org-mcp-test--verify-file-matches
                   parent-file
                   org-mcp-test--regex-sibling-parent-added))))))))))

(ert-deftest org-mcp-test-file-set-every-heading-tool ()
  "Every tool that names a heading looks an `id:' link up in the files named.
Beta's ID is not in Emacs's index, so only the named file finds it.
The reads return Beta, and each write changes Beta alone.  The set
serves org-node-create's parent; the sibling it inserts after is looked
up in the parent's file."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-links))
      (org-mcp-test--with-id-tracking (list test-file) nil
        (let* ((link (format "id:%s" org-mcp-test--link-beta-id))
               (files (vector test-file))
               (call
                (lambda (tool &rest params)
                  (mcp-server-lib-ert-call-tool
                   tool (append params `((files . ,files)))))))
          (org-mcp-test--without-id-index
            (should
             (string=
              (funcall call "org-node-text" `(link . ,link))
              (string-trim-right org-mcp-test--content-links-beta)))
            (should
             (equal
              (alist-get
               'title
               (json-read-from-string
                (funcall call "org-node-read" `(link . ,link))))
              "Beta"))
            (dolist (write
                     `(("org-node-set-todo"
                        (link . ,link) (after . "TODO"))
                       ("org-node-set-title"
                        (link . ,link)
                        (before . "Beta")
                        (after . "Beta Renamed"))
                       ("org-node-set-content"
                        (link . ,link)
                        (before . nil)
                        (after . "Beta appended.")
                        (append . t))
                       ("org-node-set-properties"
                        (link . ,link) (after . ((EFFORT . "1:00"))))
                       ("org-node-set-scheduled"
                        (link . ,link) (after . "2026-03-27"))
                       ("org-node-set-deadline"
                        (link . ,link) (after . "2026-03-28"))
                       ("org-node-set-tags" (link . ,link) (after . "work"))
                       ("org-node-set-priority" (link . ,link) (after . "A"))
                       ("org-node-add-note" (link . ,link) (note . "Checked"))
                       ("org-clock-add"
                        (link . ,link)
                        (start . "2026-03-23T10:00:00")
                        (end . "2026-03-23T11:00:00"))
                       ("org-clock-in"
                        (link . ,link) (start_time . "2026-03-23T14:30:00"))
                       ("org-clock-out"
                        (link . ,link) (end_time . "2026-03-23T16:45:00"))
                       ("org-clock-delete"
                        (link . ,link) (start . "2026-03-23T10:00:00"))
                       ("org-node-create"
                        (title . "New Task")
                        (todo . "TODO")
                        (tags . nil)
                        (content . nil)
                        (parent . ,link)
                        (previous_sibling . ,(concat link "::*Review"))
                        (properties . ((ID . "new-task-id"))))))
              (let ((before (org-mcp-test--read-file test-file)))
                (should
                 (equal
                  (alist-get
                   'success (json-read-from-string (apply call write)))
                  t))
                (should-not
                 (string= (org-mcp-test--read-file test-file) before))
                (org-mcp-test--verify-file-matches
                 test-file org-mcp-test--regex-links-beta-changed)))))))))

(defconst org-mcp-test--scope-task-with-id-child-added-regex
  (concat
   "\\`\\* DONE Task\n"
   ":PROPERTIES:\n"
   ":ID:       " (regexp-quote org-mcp-test--content-with-id-id) "\n"
   ":END:\n"
   "Body\n"
   "\n?"
   "\\*\\* TODO New Task *\n"
   " *:PROPERTIES:\n"
   " *:ID: +new-task-id\n"
   " *:END:\n"
   "\\'")
  "Regex matching the ID scope-test file with Task DONE and a new child.")

(ert-deftest org-mcp-test-file-set-blank-means-none ()
  "A blank `files' on a heading tool means the call names no files.
Some clients send null, false, \"\" or [] for every optional parameter
they do not use.  With each, an `id:' link and a `file:' link, bare
and bracketed, resolve as without `files', for a read, a write and
the parent of org-node-create, and a bare ID is refused as no link, as
without `files'."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (dolist (blank '(null "" [] :json-false))
      (org-mcp-test--with-scope-dirs nil
        (let* ((file (org-mcp-test--write-file
                      outside "task.org"
                      org-mcp-test--scope-task-with-id-content))
               (param
                `((files
                   .
                   ,(unless (eq blank 'null)
                      blank)))))
          (org-mcp-test--with-id-tracking
              (list file)
              `((,org-mcp-test--content-with-id-id . ,file))
            (should
             (string-prefix-p
              "Not an Org link: "
              (org-mcp-test--call-tool-expecting-error
               file "org-node-text"
               `((link . ,org-mcp-test--content-with-id-id) ,@param))))
            (dolist (address
                     (list
                      org-mcp-test--scope-id-link
                      (format "file:%s::*Task" file)
                      (format "[[%s][Task]]" org-mcp-test--scope-id-link)
                      (format "[[file:%s::*Task]]" file)))
              (should
               (string=
                (mcp-server-lib-ert-call-tool
                 "org-node-text" `((link . ,address) ,@param))
                (string-trim-right
                 org-mcp-test--scope-task-with-id-content)))
              (should
               (equal
                (alist-get
                 'title
                 (json-read-from-string
                  (mcp-server-lib-ert-call-tool
                   "org-node-read" `((link . ,address) ,@param))))
                "Task")))
            (mcp-server-lib-ert-call-tool
             "org-node-set-todo"
             `((link . ,org-mcp-test--scope-id-link)
               (after . "DONE")
               ,@param))
            (mcp-server-lib-ert-call-tool
             "org-node-create"
             `((title . "New Task")
               (todo . "TODO")
               (content . nil)
               (parent . ,org-mcp-test--scope-id-link)
               (previous_sibling . "")
               (properties . ((ID . "new-task-id")))
               ,@param))
            (org-mcp-test--verify-file-matches
             file org-mcp-test--scope-task-with-id-child-added-regex)))))))

(defconst org-mcp-test--content-sibling-parent
  "* Parent
:PROPERTIES:
:ID:       file-set-parent-id
:END:
** Sibling
:PROPERTIES:
:ID:       file-set-sibling-id
:CUSTOM_ID: sibling-slug
:END:
** Last
"
  "File holding a parent and its child Sibling, both with an ID.
Sibling also has a custom ID.")

(defconst org-mcp-test--content-sibling-elsewhere
  "* Sibling elsewhere
:PROPERTIES:
:ID:       file-set-sibling-id
:END:
"
  "File holding a heading with the ID of Sibling in another file.")

(defconst org-mcp-test--regex-sibling-parent-added
  (concat
   "\\`"
   (regexp-quote
    (string-remove-suffix "** Last\n" org-mcp-test--content-sibling-parent))
   "\n?"
   "\\*\\* TODO New Task *\n"
   " *:PROPERTIES:\n"
   " *:ID: +new-task-id\n"
   " *:END:\n"
   "\\*\\* Last\n"
   "\\'")
  "Regex matching the parent file after adding a TODO after Sibling.")

(ert-deftest org-mcp-test-add-todo-after-link-without-index ()
  "org-node-create resolves the sibling to insert after in the parent's file.
The parent lies in a file the scope override permits, outside the
allowed files, and neither ID is in Emacs's ID index.  Without
`files', the sibling's `id:' link is still found, in the parent's
buffer, with no index lookup or rescan; a custom ID link and a
bracketed title link to the sibling work the same.  Each call inserts
after Sibling."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (dolist (after '("id:file-set-sibling-id"
                     "file:%s::#sibling-slug"
                     "[[file:%s::*Sibling][Sibling]]"))
      (org-mcp-test--with-scope-dirs (list root)
        (let ((a-file (org-mcp-test--write-file
                       root "a.org" org-mcp-test--content-sibling-parent)))
          (org-mcp-test--with-id-tracking (list allowed) nil
            (org-mcp-test--without-id-index
              (mcp-server-lib-ert-call-tool
               "org-node-create"
               `((title . "New Task")
                 (todo . "TODO")
                 (content . nil)
                 (parent . ,(format "file:%s::*Parent" a-file))
                 (previous_sibling . ,(format after a-file))
                 (properties . ((ID . "new-task-id"))))))
            (org-mcp-test--verify-file-matches
             a-file org-mcp-test--regex-sibling-parent-added)))))))

(ert-deftest org-mcp-test-add-todo-blank-after-link ()
  "A blank previous_sibling means none: the TODO goes after the last child.
Clients may send null, false, \"\" or blanks for an optional parameter
they do not use; none of them is an error."
  (dolist (blank '(null :json-false "" "  "))
    (org-mcp-test--with-add-todo-setup test-file
        org-mcp-test--content-nested-siblings
      (org-mcp-test--add-todo-and-check
       "Child Task"
       "TODO"
       '("work")
       nil
       (org-mcp-test--file-link test-file "*Parent Task")
       (unless (eq blank 'null)
         blank)
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-child-under-parent))))

(ert-deftest org-mcp-test-file-set-after-link-in-parent-file ()
  "With `files', org-node-create looks the sibling's ID up in the parent's file.
The sibling must be a child of the parent, so the other file holding
the same ID, named first, is not searched for it.  No ID index is
used."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-scope-dirs nil
      (let ((a-file (org-mcp-test--write-file
                     root "a.org" org-mcp-test--content-sibling-parent))
            (b-file (org-mcp-test--write-file
                     outside "b.org"
                     org-mcp-test--content-sibling-elsewhere)))
        (org-mcp-test--with-id-tracking (list a-file b-file) nil
          (org-mcp-test--without-id-index
            (mcp-server-lib-ert-call-tool
             "org-node-create"
             `((title . "New Task")
               (todo . "TODO")
               (content . nil)
               (parent . "id:file-set-parent-id")
               (previous_sibling . "id:file-set-sibling-id")
               (properties . ((ID . "new-task-id")))
               (files . ,(vector b-file a-file)))))
          (org-mcp-test--verify-file-matches
           a-file org-mcp-test--regex-sibling-parent-added)
          (should
           (string=
            (org-mcp-test--read-file b-file)
            org-mcp-test--content-sibling-elsewhere)))))))

(defconst org-mcp-test--content-other-file-heading
  "* Elsewhere
:PROPERTIES:
:ID:       elsewhere-id
:END:
"
  "File holding a heading whose ID no other test file carries.")

(ert-deftest org-mcp-test-add-todo-after-link-in-other-file ()
  "An previous_sibling `id:' link to a heading in another file is no sibling.
The heading lies in an allowed file and its ID is in Emacs's index,
but the sibling is only looked for in the parent's file, so the call
is refused as naming no sibling under the parent, bare and with a
search part, and neither file changes."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-temp-org-files
        ((a-file org-mcp-test--content-sibling-parent)
         (b-file org-mcp-test--content-other-file-heading))
      (org-mcp-test--with-id-tracking
          (list a-file b-file)
          `(("elsewhere-id" . ,b-file))
        (dolist (after '("id:elsewhere-id" "[[id:elsewhere-id::*Elsewhere]]"))
          (org-mcp-test--without-id-index
            (org-mcp-test--call-tool-refused
             "org-node-create"
             `((title . "New Task")
               (todo . "TODO")
               (content . nil)
               (parent . ,(format "file:%s::*Parent" a-file))
               (previous_sibling . ,after))
             (concat "\\`Sibling " (regexp-quote after)
                     " not found under parent\\'")
             a-file))
          (should
           (string= (org-mcp-test--read-file b-file)
                    org-mcp-test--content-other-file-heading)))))))

(defconst org-mcp-test--content-custom-id-children-first
  "* Parent
:PROPERTIES:
:ID:       file-set-parent-id
:END:
** First
:PROPERTIES:
:CUSTOM_ID: first
:END:
First body.
"
  "Parent with an ID and its child First, which has a custom ID only.")

(defconst org-mcp-test--content-custom-id-children-second
  "** Second\nSecond body.\n"
  "Second child of Parent, with no identifier.")

(defconst org-mcp-test--content-custom-id-children
  (concat
   org-mcp-test--content-custom-id-children-first
   org-mcp-test--content-custom-id-children-second)
  "File holding Parent, with an ID, and two children without one.")

(defconst org-mcp-test--regex-custom-id-children-after-first
  (concat
   "\\`"
   (regexp-quote org-mcp-test--content-custom-id-children-first)
   "\n?"
   "\\*\\* TODO New Task *\n"
   (regexp-quote org-mcp-test--content-custom-id-children-second)
   "\\'")
  "Regex matching the whole file after adding a TODO after First.")

(defconst org-mcp-test--regex-custom-id-children-after-second
  (concat
   "\\`"
   (regexp-quote org-mcp-test--content-custom-id-children)
   "\n?"
   "\\*\\* TODO New Task *\n"
   "\\'")
  "Regex matching the whole file after adding a TODO after Second.")

(ert-deftest org-mcp-test-file-set-parent-id-with-file-sibling ()
  "With `files' naming the parent's file, a `file:' sibling link works.
The parent, reached by its `id:' link through `files', lies in a file
outside the allowed files that Emacs never indexed.  Its children have
no ID, so org-node-read returns a custom ID link for First and a title link
for Second.  Each, sent back as previous_sibling next to the parent's `id:'
link and `files', inserts the new TODO after that child, and Emacs's
ID index is never consulted."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-scope-dirs t
      (pcase-dolist (`(,title ,search ,name ,expected)
                     `(("First" "#first" "first.org"
                        ,org-mcp-test--regex-custom-id-children-after-first)
                       ("Second" "*Second" "second.org"
                        ,org-mcp-test--regex-custom-id-children-after-second)))
        (let* ((file (org-mcp-test--write-file
                      outside name org-mcp-test--content-custom-id-children))
               (files (vector file)))
          (org-mcp-test--without-id-index
            (let* ((parent
                    (json-read-from-string
                     (mcp-server-lib-ert-call-tool
                      "org-node-read"
                      `((link . "id:file-set-parent-id") (files . ,files)))))
                   (after-link
                    (alist-get
                     'link
                     (seq-find
                      (lambda (child) (equal (alist-get 'title child) title))
                      (alist-get 'children parent)))))
              (should
               (equal after-link (org-mcp-test--file-link file search)))
              (mcp-server-lib-ert-call-tool
               "org-node-create"
               `((title . "New Task")
                 (todo . "TODO")
                 (content . nil)
                 (parent . "id:file-set-parent-id")
                 (previous_sibling . ,after-link)
                 (files . ,files)))))
          (org-mcp-test--verify-file-matches file expected))))))

(defconst org-mcp-test--content-top-level-preamble "#+TITLE: Top\n\n"
  "Header lines of `org-mcp-test--content-top-level'.")

(defconst org-mcp-test--content-top-level-first
  "* First
:PROPERTIES:
:ID:       top-level-first-id
:END:
First body.
** Nested
Nested body.
"
  "Top-level heading First, with an ID and a child.")

(defconst org-mcp-test--content-top-level-second
  "* Second
:PROPERTIES:
:CUSTOM_ID: second-slug
:END:
Second body.
"
  "Top-level heading Second, with a custom ID.")

(defconst org-mcp-test--content-top-level-third "* Third\nThird body.\n"
  "Top-level heading Third, with no identifier.")

(defconst org-mcp-test--content-top-level
  (concat
   org-mcp-test--content-top-level-preamble
   org-mcp-test--content-top-level-first
   org-mcp-test--content-top-level-second
   org-mcp-test--content-top-level-third)
  "File with three top-level headings after its header lines.")

(defconst org-mcp-test--regex-top-level-after-first
  (concat
   "\\`"
   (regexp-quote
    (concat
     org-mcp-test--content-top-level-preamble
     org-mcp-test--content-top-level-first))
   "\n?"
   "\\* TODO New Task *\n"
   (regexp-quote
    (concat
     org-mcp-test--content-top-level-second
     org-mcp-test--content-top-level-third))
   "\\'")
  "Regex matching the whole file after adding a TODO after First.")

(defconst org-mcp-test--regex-top-level-after-second
  (concat
   "\\`"
   (regexp-quote
    (concat
     org-mcp-test--content-top-level-preamble
     org-mcp-test--content-top-level-first
     org-mcp-test--content-top-level-second))
   "\n?"
   "\\* TODO New Task *\n"
   (regexp-quote org-mcp-test--content-top-level-third)
   "\\'")
  "Regex matching the whole file after adding a TODO after Second.")

(defconst org-mcp-test--regex-top-level-after-third
  (concat
   "\\`"
   (regexp-quote org-mcp-test--content-top-level)
   "\n?"
   "\\* TODO New Task *\n"
   "\\'")
  "Regex matching the whole file after adding a TODO after Third.")

(ert-deftest org-mcp-test-add-todo-top-level-after-sibling ()
  "At the top level, org-node-create inserts after the heading previous_sibling names.
parent names the whole file.  An `id:' link, a custom ID link and
a bracketed title link each name a top-level heading, and the new TODO
goes after that heading and its subtree, not after the header lines.
The `id:' link is looked up in the file, with no ID index."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (pcase-dolist (`(,after ,expected)
                   `(("id:top-level-first-id"
                      ,org-mcp-test--regex-top-level-after-first)
                     ("file:%s::#second-slug"
                      ,org-mcp-test--regex-top-level-after-second)
                     ("[[file:%s::*Third][Third]]"
                      ,org-mcp-test--regex-top-level-after-third)))
      (org-mcp-test--with-temp-org-files
          ((test-file org-mcp-test--content-top-level))
        (org-mcp-test--with-id-tracking (list test-file) nil
          (org-mcp-test--without-id-index
            (org-mcp-test--add-todo-and-check
             "New Task" "TODO" nil nil (concat "file:" test-file)
             (format after test-file)
             (file-name-nondirectory test-file)
             test-file
             expected)))))))

(ert-deftest org-mcp-test-add-todo-top-level-after-link-refused ()
  "At the top level, a bad previous_sibling is refused and the file is unchanged.
A level-2 heading is not a top-level sibling, so its link is refused
as not found under the parent.  A bare UUID is no link, and a
`shell:' link is a type org-mcp does not resolve; neither runs or is
looked up, and the shell command never runs."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-top-level))
      (let* ((canary
              (expand-file-name "org-mcp-test-top-level-canary"
                                (file-name-directory test-file)))
             (nested (org-mcp-test--file-link test-file "*Nested"))
             (uuid "0f1e2d3c-4b5a-4968-8776-a5b4c3d2e1f0")
             (shell (format "shell:touch %s" canary)))
        (unwind-protect
            (pcase-dolist (`(,after ,refusal)
                           `((,nested
                              ,(concat
                                "\\`Sibling " (regexp-quote nested)
                                " not found under parent\\'"))
                             (,uuid
                              ,(concat
                                "\\`Not an Org link: " (regexp-quote uuid)))
                             (,shell "\\`Link type 'shell' is not supported")))
              (org-mcp-test--call-tool-refused
               "org-node-create"
               `((title . "New Task")
                 (todo . "TODO")
                 (content . nil)
                 (parent . ,(concat "file:" test-file))
                 (previous_sibling . ,after))
               refusal
               test-file)
              (org-mcp-test--verify-file-matches
               test-file
               (concat
                "\\`" (regexp-quote org-mcp-test--content-top-level) "\\'"))
              (org-mcp-test--verify-no-modified-buffer test-file))
          (when (file-exists-p canary)
            (delete-file canary)))
        (should-not (file-exists-p canary))))))

(ert-deftest org-mcp-test-link-non-links-refused ()
  "A string that is no Org link is refused before any lookup.
A bare ID, known or unknown, a bare path, a path with an outline path,
a title on its own, and an ID, a path or an `id:' link behind
`org://', are refused by the read tools, a write, org-node-create as its
parent and as its sibling, and the resource.  The error names the
link forms to send, and for an `org://' string also says to drop the
prefix; the resource carries it too, for `org://' plus an unknown ID
and for `org://org://'.  The calls run from a buffer visiting an allowed Org
file, which Org's ID lookup falls back to for an ID it does not know.
Nothing is read from it, no ID lookup or rescan runs, and neither the
file nor the buffer changes."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (org-mcp-test--with-id-tracking
        (list test-file)
        `((,org-mcp-test--link-beta-id . ,test-file))
      (let* ((unknown "0f1e2d3c-4b5a-4968-8776-a5b4c3d2e1f0")
             (forms
              (list
               unknown
               org-mcp-test--link-beta-id
               test-file
               (format "%s#Beta/Review" test-file)
               "Gamma"
               (concat "org://" unknown)
               (concat "org://" test-file)
               (concat "org://id:" org-mcp-test--link-beta-id)))
             (buf (find-file-noselect test-file)))
        (unwind-protect
            (with-current-buffer buf
              (org-mcp-test--without-id-index
                (dolist (form forms)
                  (let ((expected
                         (concat
                          "\\`Not an Org link: " (regexp-quote form) "\\.  "
                          (if (string-prefix-p "org://" form)
                              "Drop org://, which only a resource URI \
starts with\\.  "
                            "")
                          "Send id:<uuid>, file:<path>::#<custom-id>, "
                          "file:<path>::\\*<title> or file:<path>, "
                          "with the file's full path\\'")))
                    (dolist (call
                             `(("org-node-read" (link . ,form))
                               ("org-node-text" (link . ,form))
                               ("org-node-set-tags" (link . ,form) (after . "work"))
                               ("org-node-create"
                                (title . "New Task")
                                (todo . "TODO")
                                (content . nil)
                                (parent . ,form))
                               ("org-node-create"
                                (title . "New Task")
                                (todo . "TODO")
                                (content . nil)
                                (parent
                                 . ,(org-mcp-test--file-link test-file "*Beta"))
                                (previous_sibling . ,form))))
                      (should
                       (string-match-p
                        expected
                        (org-mcp-test--call-tool-expecting-error
                         test-file (car call) (cdr call)))))
                    (should
                     (string-match-p
                      expected
                      (org-mcp-test--resource-error
                       (concat "org://" (url-hexify-string form))))))))
              (should-not (buffer-modified-p buf)))
          (kill-buffer buf))))))

;;; Returned link tests

(defconst org-mcp-test--link-both-id "0c7e2f4a-5b6d-4e8f-9a1b-2c3d4e5f6a7b"
  "ID of the Both heading in `org-mcp-test--content-link-kinds'.")

(defconst org-mcp-test--content-link-kinds
  (concat
   "#+TITLE: Links\n"
   "\n"
   "Notes before the first heading.\n"
   org-mcp-test--content-links-alpha
   org-mcp-test--content-links-beta
   org-mcp-test--content-links-gamma
   "* Both\n"
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--link-both-id "\n"
   ":CUSTOM_ID: both-slug\n"
   ":END:\n"
   "* Blank ID\n"
   ":PROPERTIES:\n"
   ":ID:\n"
   ":END:\n"
   "* TODO [#A] Decorated [1/2] :tag:\n")
  "Org file with a heading of every kind a link is chosen for.
Alpha has a custom ID, Beta an ID, Gamma neither, Both an ID and a
custom ID, Blank ID an empty ID, and Decorated a priority, a
statistics cookie and a tag.  A line of text precedes the headings.")

(defconst org-mcp-test--content-read-tools
  (concat
   org-mcp-test--content-link-kinds
   "* TODO Clocked :#inbox:\n"
   ":LOGBOOK:\n"
   "CLOCK: [2026-01-01 Thu 10:00]\n"
   ":END:\n")
  "The link kinds file plus a heading whose clock is still running.")

(defconst org-mcp-test--regex-links-review-and-gamma-seen
  (concat
   "\\`"
   (regexp-quote
    (concat
     org-mcp-test--content-links-preamble
     org-mcp-test--content-links-alpha))
   "\\* Beta\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--link-beta-id "\n"
   ":END:\n"
   "Beta body\\.\n"
   "\\*\\* Review\n"
   " *:PROPERTIES:\n"
   " *:SEEN: +yes\n"
   " *:END:\n"
   "Beta review\\.\n"
   "\\* TODO Gamma\n"
   " *:PROPERTIES:\n"
   " *:SEEN: +yes\n"
   " *:END:\n"
   "Gamma body\\.\n"
   "\\'")
  "Regex matching the links file after marking Beta's Review and Gamma.
Neither heading gains an ID.")

(defconst org-mcp-test--content-entry-with-targets
  (concat
   "* Heading\n"
   "Text with a <<target>> in it.\n"
   "#+NAME: named-block\n"
   "#+begin_src emacs-lisp\n"
   "(+ 1 2)\n"
   "#+end_src\n")
  "A heading whose body holds a target and a named element.")

(defconst org-mcp-test--regex-delta-done
  "\\`\\* DONE Delta\n\\'"
  "Regex matching a file holding only the heading Delta, marked DONE.")

(defconst org-mcp-test--content-parent-with-child
  "* TODO Parent Task\nParent body.\n** Child One\nChild body.\n"
  "A heading without identifiers whose body is followed by a child.")

(defconst org-mcp-test--regex-parent-body-replaced
  "\\`\\* TODO Parent Task\nNew parent body\\.\n\\*\\* Child One\nChild body\\.\n\\'"
  "Regex matching the parent file after replacing the parent's body.")

(defconst org-mcp-test--content-targets-before-headings
  (concat
   "* Alpha <<alpha-anchor>>\n"
   "* Beta\n"
   "Beta body.\n"
   "* Gamma\n"
   "See <<gamma-anchor>>\n"
   "* Delta\n"
   ":PROPERTIES:\n"
   ":CUSTOM_ID: delta-slug\n"
   ":END:\n")
  "Org file in which a target ends the line before a heading.
Alpha's heading line ends with one before Beta, which has no
identifier, and a body line ends with one before Delta, which has a
custom ID.")

(defconst org-mcp-test--regex-targets-beta-noted-delta-seen
  (concat
   "\\`\\* Alpha <<alpha-anchor>>\n"
   "\\* Beta[ \t]+:work:\n"
   ":LOGBOOK:\n"
   "- Note taken on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  Beta note\\.\n"
   ":END:\n"
   "Beta body\\.\n"
   "\\* Gamma\n"
   "See <<gamma-anchor>>\n"
   "\\* Delta\n"
   " *:PROPERTIES:\n"
   " *:CUSTOM_ID: +delta-slug\n"
   " *:SEEN: +yes\n"
   " *:END:\n"
   "\\'")
  "Regex matching the targets file after writes to Beta and Delta.
Alpha and Gamma, whose lines end with the targets, are unchanged.")

(defconst org-mcp-test--content-notes-by-search
  "* TODO Noted Task\nTask body.\n* Other Task <<other-anchor>>\nOther body.\n"
  "Org file with two headings without identifiers.
Other Task's heading line ends with a target.")

(defconst org-mcp-test--regex-notes-by-search
  (concat
   "\\`\\* TODO Noted Task\n"
   ":LOGBOOK:\n"
   "- Note taken on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  Note by title\\.\n"
   ":END:\n"
   "Task body\\.\n"
   "\\* Other Task <<other-anchor>>\n"
   ":LOGBOOK:\n"
   "- Note taken on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  Note by target\\.\n"
   ":END:\n"
   "Other body\\.\n"
   "\\'")
  "Regex matching the notes file after a note on each heading.")

(defun org-mcp-test--set-seen (link)
  "Set the property SEEN on the heading LINK names and return the response."
  (json-read-from-string
   (mcp-server-lib-ert-call-tool
    "org-node-set-properties" `((link . ,link) (after . ((SEEN . "yes")))))))

(defun org-mcp-test--links-in (value)
  "Return every `link' string in VALUE, a parsed JSON result, in order."
  (cond
   ((vectorp value)
    (apply #'append (mapcar #'org-mcp-test--links-in value)))
   ((and (consp value) (consp (car value)))
    (apply #'append
           (mapcar
            (lambda (field)
              (if (and (eq (car field) 'link) (stringp (cdr field)))
                  (list (cdr field))
                (org-mcp-test--links-in (cdr field))))
            value)))
   (t
    nil)))

(defun org-mcp-test--call-tool-with-error (tool params)
  "Call TOOL with PARAMS expecting a tool error and return its message."
  (cadr
   (should-error
    (mcp-server-lib-ert-process-tool-response
     (mcp-server-lib-process-jsonrpc-parsed
      (mcp-server-lib-create-tools-call-request tool 1 params)
      mcp-server-lib-ert-server-id))
    :type 'mcp-server-lib-tool-error)))

(ert-deftest org-mcp-test-returned-link-forms ()
  "Reads and writes link a heading by its ID, else custom ID, else title.
A heading with both an ID and a custom ID is linked by the ID, an empty
ID counts as none, and a title link leaves out the TODO keyword, the
priority, the statistics cookie and the tags, as Org does.  The title
the node reports leaves out the same, so the title a read returns is
the search a later call sends.  The form does not follow the link the
call was sent: every write here is addressed by a title link.
org-node-read's `id' field is the ID its `link' names."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-link-kinds
      (list org-mcp-test--link-beta-id org-mcp-test--link-both-id)
    (let ((expected
           `(("Alpha" . ,(org-mcp-test--file-link test-file "#alpha-slug"))
             ("Beta" . ,(concat "id:" org-mcp-test--link-beta-id))
             ("Gamma" . ,(org-mcp-test--file-link test-file "*Gamma"))
             ("Both" . ,(concat "id:" org-mcp-test--link-both-id))
             ("Blank ID" . ,(org-mcp-test--file-link test-file "*Blank ID"))
             ("Decorated"
              . ,(org-mcp-test--file-link test-file "*Decorated")))))
      (should
       (equal
        (mapcar
         (lambda (child)
           (cons (alist-get 'title child) (alist-get 'link child)))
         (alist-get
          'children
          (json-read-from-string
           (org-mcp-test--call-read (format "file:%s" test-file)))))
        expected))
      (pcase-dolist (`(,title . ,link) expected)
        (let* ((search
                (org-mcp-test--file-link test-file (concat "*" title)))
               (heading
                (json-read-from-string (org-mcp-test--call-read search))))
          (should (equal (alist-get 'link heading) link))
          (should
           (equal (alist-get 'id heading)
                  (and (string-prefix-p "id:" link) (substring link 3))))
          (should
           (equal (alist-get 'link (org-mcp-test--set-seen search)) link)))))))

(ert-deftest org-mcp-test-returned-link-off-heading ()
  "Before the first heading, the link searches for the line's text.
An empty line gives the link to the bare file.  No tool returns a link
for a position off a heading, since every tool links a heading, so
the link builder is called directly."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-link-kinds))
    (let ((buffer (find-file-noselect test-file)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (forward-line 2)
            (should
             (equal
              (org-mcp--link-at-point)
              (org-mcp-test--file-link
               test-file "Notes before the first heading.")))
            (forward-line -1)
            (let ((here (point)))
              (should
               (equal
                (org-mcp--link-at-point)
                (concat "file:" (abbreviate-file-name test-file))))
              (should (= (point) here)))
            (should-not (buffer-modified-p)))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-returned-link-inside-entry ()
  "Anywhere in a heading's entry, the link is the heading's own.
Org would link a target or a named element at point instead, and a
tool can leave point on either after an edit.  The link builder is
called directly with point on each."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-entry-with-targets))
    (let ((buffer (find-file-noselect test-file)))
      (unwind-protect
          (with-current-buffer buffer
            (dolist (place '("<<ta" "(+ 1"))
              (goto-char (point-min))
              (search-forward place)
              (should
               (equal (org-mcp--link-at-point)
                      (org-mcp-test--file-link test-file "*Heading")))))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-returned-link-round-trip ()
  "A link a tool returns reaches the same heading in a later call.
Links of each form come back from writes addressed by title link:
an `id:' link, a custom ID link and a title link.  Reading through
each finds the heading and returns the same link.  The title link of
a heading a call creates is then sent to a write, which changes that
heading."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
        (list org-mcp-test--link-beta-id)
      (pcase-dolist (`(,title . ,link)
                     `(("Alpha"
                        . ,(org-mcp-test--file-link test-file "#alpha-slug"))
                       ("Beta" . ,(concat "id:" org-mcp-test--link-beta-id))
                       ("Gamma"
                        . ,(org-mcp-test--file-link test-file "*Gamma"))))
        (should
         (equal
          (alist-get
           'link
           (org-mcp-test--set-seen
            (org-mcp-test--file-link test-file (concat "*" title))))
          link))
        (org-mcp-test--should-resolve-to link title)))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-empty))
      (let ((link
             (alist-get
              'link
              (org-mcp-test--add-todo-and-check
               "Delta" "TODO" nil nil (format "file:%s" test-file) nil
               (file-name-nondirectory test-file)
               test-file "\\`\\* TODO Delta\n\\'"))))
        (org-mcp-test--should-resolve-to link "Delta")
        (org-mcp-test--update-todo-state-and-check
         link "TODO" "DONE" test-file org-mcp-test--regex-delta-done
         link)))))

(ert-deftest org-mcp-test-returned-link-id-round-trip-with-files ()
  "The `id:' link returned for a heading in another file leads back with `files'.
Task, with an ID, is in a file outside the allowed files that the
override permits.  Read through a title link, it is linked by its ID;
that link, sent with `files' naming the file, reads Task again and
returns the same link, and Emacs's ID index is never consulted."
  (org-mcp-test--with-scope-dirs t
    (let ((file (org-mcp-test--write-file
                 outside "task.org" org-mcp-test--scope-task-with-id-content)))
      (org-mcp-test--without-id-index
        (should
         (equal
          (alist-get
           'link
           (json-read-from-string
            (org-mcp-test--call-read
             (org-mcp-test--file-link file "*Task"))))
          org-mcp-test--scope-id-link))
        (let ((heading
               (json-read-from-string
                (mcp-server-lib-ert-call-tool
                 "org-node-read"
                 `((link . ,org-mcp-test--scope-id-link)
                   (files . ,(vector file)))))))
          (should (equal (alist-get 'title heading) "Task"))
          (should
           (equal (alist-get 'link heading) org-mcp-test--scope-id-link)))))))

(ert-deftest org-mcp-test-returned-link-ignores-link-config ()
  "The user's link settings change neither the returned link nor the file.
Every setting that makes `org-store-link' create an ID or return
another link is active: `org-id-link-to-org-use-id' is t, and
`find-file-hook' makes it `create-if-interactive' in the buffer
org-mcp visits, as Doom's org-roam module does; Beta's ID would be
inherited through `org-id-link-consider-parent-id'; file links carry
no context; a search function supplies its own search string; and an
active region spans a line of the buffer.  Beta's Review still gets its
title link, Gamma too, neither gains an ID, and the buffer-local
setting is intact afterwards."
  (let ((local-hook
         (lambda ()
           (setq-local org-id-link-to-org-use-id 'create-if-interactive)))
        (org-id-link-to-org-use-id t)
        (org-id-link-consider-parent-id t)
        (org-id-link-use-context t)
        (org-link-context-for-files nil)
        (org-create-file-search-functions (list (lambda () "custom-search")))
        (transient-mark-mode t))
    (add-hook 'find-file-hook local-hook)
    (unwind-protect
        (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
            (list org-mcp-test--link-beta-id)
          (should-not (find-buffer-visiting test-file))
          (should
           (equal
            (alist-get
             'link
             (org-mcp-test--set-seen
              (format "id:%s::*Review" org-mcp-test--link-beta-id)))
            (org-mcp-test--file-link test-file "*Review")))
          (let ((buffer (find-buffer-visiting test-file)))
            (should
             (eq (buffer-local-value 'org-id-link-to-org-use-id buffer)
                 'create-if-interactive))
            (with-current-buffer buffer
              (goto-char (point-min))
              (forward-line 3)
              (push-mark (point) t t)
              (end-of-line)
              (should (use-region-p)))
            (should
             (equal
              (alist-get
               'link
               (org-mcp-test--set-seen
                (org-mcp-test--file-link test-file "*Gamma")))
              (org-mcp-test--file-link test-file "*Gamma")))
            (should
             (eq (buffer-local-value 'org-id-link-to-org-use-id buffer)
                 'create-if-interactive)))
          (org-mcp-test--verify-file-matches
           test-file org-mcp-test--regex-links-review-and-gamma-seen))
      (remove-hook 'find-file-hook local-hook))))

(ert-deftest org-mcp-test-returned-link-competing-store-function ()
  "Another package's link store function does not take the link over.
A store function claims every Org buffer.  Alone, it would replace the
title link, and together with Org's `id:' store function it would make
Org 9.7 prompt and Org 9.8 pick it.  Reads and writes still return the
`id:', custom ID and title links."
  (let ((org-link-parameters
         (cons
          (list
           "probe"
           :store
           (lambda (&optional _interactive)
             (when (derived-mode-p 'org-mode)
               (org-link-store-props :type "probe" :link "probe:taken-over")
               t)))
          org-link-parameters)))
    (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
        (list org-mcp-test--link-beta-id)
      (let ((expected
             `(,(org-mcp-test--file-link test-file "#alpha-slug")
               ,(concat "id:" org-mcp-test--link-beta-id)
               ,(org-mcp-test--file-link test-file "*Gamma"))))
        (should
         (equal
          (org-mcp-test--links-in
           (alist-get
            'children
            (json-read-from-string
             (org-mcp-test--call-read (format "file:%s" test-file)))))
          expected))
        (should
         (equal
          (mapcar
           (lambda (title)
             (alist-get
              'link
              (org-mcp-test--set-seen
               (org-mcp-test--file-link test-file (concat "*" title)))))
           '("Alpha" "Beta" "Gamma"))
          expected))))))

(ert-deftest org-mcp-test-returned-link-refuses-foreign-link ()
  "A link that comes back in another form fails the call, which says so.
Advice on `org-store-link' that returns a link of another type, or a
`file:' link to something other than the heading, makes a read fail.
It makes a write fail with a message that the change was made, and so
does advice that signals an error of its own; the change is saved.
Advice that edits the buffer while a link is stored fails the call
too."
  (let ((foreign (lambda (&rest _) "[[probe:taken-over]]"))
        (target (lambda (&rest _) "[[file:/tmp/notes.org::anchor]]"))
        (failing (lambda (&rest _) (error "Store failed")))
        (editing
         (lambda (store &rest args)
           (org-entry-put nil "STORED" "yes")
           (apply store args))))
    (pcase-dolist (`(,advice . ,reason)
                   `((,foreign
                      . "org-store-link made \\[\\[probe:taken-over\\]\\], \
not an id: or file: link to the heading")
                     (,target
                      . "org-store-link made \\[\\[file:/tmp/notes.org::anchor\\]\\], \
not an id: or file: link to the heading")
                     (,failing . "Store failed\\'")))
      (org-mcp-test--with-temp-org-files
          ((test-file org-mcp-test--content-links))
        (let ((gamma (org-mcp-test--file-link test-file "*Gamma")))
          (advice-add 'org-store-link :override advice)
          (unwind-protect
              (progn
                (unless (eq advice failing)
                  (should
                   (string-match-p
                    reason
                    (org-mcp-test--call-tool-expecting-error
                     test-file "org-node-read" `((link . ,gamma))))))
                (should
                 (string-match-p
                  (concat
                   "\\`The change was made, but no link to it could be made: "
                   reason)
                  (org-mcp-test--call-tool-with-error
                   "org-node-set-tags" `((link . ,gamma) (after . "work")))))
                (org-mcp-test--verify-file-matches
                 test-file org-mcp-test--regex-links-gamma-tagged)
                (org-mcp-test--verify-no-modified-buffer test-file))
            (advice-remove 'org-store-link advice))
          (should-not (advice-member-p advice 'org-store-link)))))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-links))
      (let ((gamma (org-mcp-test--file-link test-file "*Gamma")))
        (advice-add 'org-store-link :around editing)
        (unwind-protect
            (should
             (string-match-p
              "org-store-link changed"
              (org-mcp-test--call-tool-expecting-error
               test-file "org-node-read" `((link . ,gamma)))))
          (advice-remove 'org-store-link editing)
          (with-current-buffer (find-buffer-visiting test-file)
            (set-buffer-modified-p nil)
            (kill-buffer)))
        (should-not (advice-member-p editing 'org-store-link))))))

(ert-deftest org-mcp-test-returned-link-after-target-line ()
  "A target ending the line before a heading does not become its link.
At the start of a heading's line, Org would link a <<target>> that ends
the previous line, a heading's or a body line.  Beta, without an
identifier, and Delta, with a custom ID, still get their own links, and
each link leads a later write to its own heading."
  (let ((org-log-into-drawer t))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-targets-before-headings))
      (let ((beta
             (alist-get
              'link
              (json-read-from-string
               (mcp-server-lib-ert-call-tool
                "org-node-set-tags"
                `((link . ,(org-mcp-test--file-link test-file "*Beta")) (after . "work"))))))
            (delta
             (alist-get
              'link (org-mcp-test--set-seen (org-mcp-test--file-link test-file "*Delta")))))
        (should (equal beta (org-mcp-test--file-link test-file "*Beta")))
        (should
         (equal delta (org-mcp-test--file-link test-file "#delta-slug")))
        (org-mcp-test--should-resolve-to delta "Delta")
        (should
         (equal
          (alist-get
           'link
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-add-note" `((link . ,beta) (note . "Beta note.")))))
          beta))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--regex-targets-beta-noted-delta-seen)))))

(ert-deftest org-mcp-test-link-search-reaches-heading-start ()
  "A link whose search lands on a heading's line leads to its start.
A note sent through a `::*Title' link, and one sent through a link to
a target inside a heading's line, each land whole in that heading's
LOGBOOK, which they do only when the write starts at the heading."
  (let ((org-log-into-drawer t))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-notes-by-search))
      (pcase-dolist (`(,search . ,note)
                     '(("*Noted Task" . "Note by title.")
                       ("other-anchor" . "Note by target.")))
        (mcp-server-lib-ert-call-tool
         "org-node-add-note"
         `((link . ,(org-mcp-test--file-link test-file search))
           (note . ,note))))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-notes-by-search))))

(ert-deftest org-mcp-test-goto-heading-leaves-point-at-heading-start ()
  "Resolving a link that lands inside a heading's line ends at its start.
The link's search finds a target in the middle of Other Task's line.
Every tool that takes a heading starts from `org-mcp--goto-heading', so
it is called directly to check where point ends."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-notes-by-search))
    (let ((target
           (org-mcp--link-target
            (org-mcp-test--file-link test-file "other-anchor"))))
      (org-mcp--with-org-file test-file
        (org-mcp--goto-heading target)
        (should (bolp))
        (should (looking-at-p "\\* Other Task <<other-anchor>>$"))))))

(ert-deftest org-mcp-test-returned-link-edited-heading-with-children ()
  "Replacing the body of a heading with children links to that heading.
The replacement ends where the first child starts, and the response
still links to the heading whose body changed."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-parent-with-child))
    (org-mcp-test--call-edit-body-and-check
     test-file
     (org-mcp-test--file-link test-file "*Parent Task")
     "Parent body."
     "New parent body."
     org-mcp-test--regex-parent-body-replaced
     nil
     (org-mcp-test--file-link test-file "*Parent Task"))))

(ert-deftest org-mcp-test-returned-link-clock-tools ()
  "Clock tools link the clocked heading, wherever they leave point.
Adding, starting, finding, stopping and deleting a clock all return
Gamma's title link, although the edit ends on a CLOCK line."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (let ((gamma (org-mcp-test--file-link test-file "*Gamma")))
      (should
       (equal
        (alist-get
         'link
         (org-mcp-test--call-clock-add
          gamma "2026-03-23T10:00:00" "2026-03-23T11:00:00"))
        gamma))
      (should
       (equal
        (alist-get
         'link (org-mcp-test--call-clock-in gamma "2026-03-23T14:30:00"))
        gamma))
      (should (equal (alist-get 'link (org-mcp-test--call-clock-get-active))
                     gamma))
      (should
       (equal
        (org-mcp-test--links-in (org-mcp-test--call-clock-find-dangling))
        (list gamma)))
      (should
       (equal
        (alist-get 'link (org-mcp-test--call-clock-out "2026-03-23T16:45:00"))
        gamma))
      (should
       (equal
        (alist-get
         'link (org-mcp-test--call-clock-delete gamma "2026-03-23T10:00:00"))
        gamma))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-links-gamma-clocked))))

(ert-deftest org-mcp-test-returned-link-query-in-narrowed-buffer ()
  "Queries read headings outside the user's narrowing where they are.
The user's buffer is narrowed to Alpha.  org-query and org-view both
return Gamma with its own title and link, and the narrowing is
unchanged afterwards."
  (org-mcp-test--with-configured-server
      ((test-file org-mcp-test--content-links))
      ((org-mcp-views '((todo :name "Todo" :query (todo))))
       (org-mcp-query-sort-fn nil))
    (let ((buffer (find-file-noselect test-file)))
      (unwind-protect
          (let ((restriction
                 (with-current-buffer buffer
                   (goto-char (point-min))
                   (re-search-forward "^\\* Alpha")
                   (org-narrow-to-subtree)
                   (list (point-min) (point-max)))))
            (dolist (call '(("org-query" (query . "(todo)"))
                            ("org-view" (view . "todo"))))
              (let ((matches
                     (alist-get
                      'children
                      (json-read-from-string
                       (mcp-server-lib-ert-call-tool
                        (car call) (cdr call))))))
                (should (= (length matches) 1))
                (should (equal (alist-get 'title (aref matches 0)) "Gamma"))
                (should
                 (equal (alist-get 'link (aref matches 0))
                        (org-mcp-test--file-link test-file "*Gamma")))))
            (should
             (equal (with-current-buffer buffer
                      (list (point-min) (point-max)))
                    restriction)))
        (kill-buffer buffer)))))

(ert-deftest org-mcp-test-returned-link-session-clock-in-narrowed-buffer ()
  "The session's clock is read where it is, outside the user's narrowing.
The Emacs clock runs in Clocked while the user's buffer is narrowed to
Alpha.  org-clock-active names Clocked and links it, and the
narrowing is unchanged afterwards."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-read-tools))
    (org-mcp-test--with-session-clock test-file
      (let* ((buffer (find-buffer-visiting test-file))
             (restriction
              (with-current-buffer buffer
                (goto-char (point-min))
                (re-search-forward "^\\* Alpha")
                (org-narrow-to-subtree)
                (list (point-min) (point-max))))
             (active (org-mcp-test--call-clock-get-active)))
        (should (equal (alist-get 'heading active) "Clocked"))
        (should
         (equal (alist-get 'link active)
                (org-mcp-test--file-link test-file "*Clocked")))
        (should
         (equal (with-current-buffer buffer
                  (list (point-min) (point-max)))
                restriction))))))

(ert-deftest org-mcp-test-read-tools-leave-files-unchanged ()
  "Every read tool and resource leaves the file byte-for-byte unchanged.
The reads cover each read tool, with and without a file set where it
takes one, and the org://{link} resource with links, encoded and raw.
Each read runs once with no buffer visiting the file and once with a
clean one.  Afterwards the file on disk is the before image, the
buffer is unmodified with its text untouched, and Org's ID locations
have not grown.  Every node in a result carries its link, the file's
own among them, and each link reads back to itself through
org-node-read."
  (org-mcp-test--with-configured-server
      ((test-file org-mcp-test--content-read-tools))
      ((org-mcp-views '((todo :name "Todo" :query (todo))))
       (org-mcp-query-sort-fn nil)
       (org-tag-alist nil)
       (org-tag-persistent-alist nil))
    (org-mcp-test--with-id-tracking
        (list test-file)
        `((,org-mcp-test--link-beta-id . ,test-file)
          (,org-mcp-test--link-both-id . ,test-file))
      (let* ((beta (concat "id:" org-mcp-test--link-beta-id))
             (reads
              (list
               (lambda () (org-mcp-test--call-read (format "file:%s" test-file)))
               (lambda ()
                 (org-mcp-test--call-read (format "[[file:%s][Links]]" test-file)))
               (lambda () (org-mcp-test--call-read beta))
               (lambda () (org-mcp-test--call-read (format "[[%s][Beta]]" beta)))
               (lambda () (org-mcp-test--call-read (org-mcp-test--file-link test-file "*Alpha")))
               (lambda ()
                 (org-mcp-test--call-read
                  (org-mcp-test--file-link test-file "#alpha-slug")))
               (lambda ()
                 (org-mcp-test--call-read-headline
                  (org-mcp-test--file-link test-file "*Gamma")))
               (lambda ()
                 (org-mcp-test--call-read-headline (format "file:%s" test-file)))
               (lambda ()
                 (mcp-server-lib-ert-call-tool
                  "org-read-outline" `((file . ,test-file))))
               (lambda ()
                 (mcp-server-lib-ert-call-tool
                  "org-query"
                  `((query . "(todo)") (files . ,(vector test-file)))))
               (lambda ()
                 (mcp-server-lib-ert-call-tool
                  "org-view" '((view . "todo"))))
               (lambda ()
                 (mcp-server-lib-ert-call-tool "org-config-tag-candidates" nil))
               (lambda ()
                 (mcp-server-lib-ert-call-tool
                  "org-config-tag-candidates" `((files . ,(vector test-file)))))
               (lambda ()
                 (mcp-server-lib-ert-call-tool "org-clock-active" nil))
               (lambda ()
                 (mcp-server-lib-ert-call-tool "org-clock-dangling" nil))
               (lambda ()
                 (mcp-server-lib-ert-call-tool
                  "org-clock-dangling" `((files . ,(vector test-file)))))
               (lambda ()
                 (org-mcp-test--read-resource
                  (car
                   (org-mcp-test--resource-uris (format "file:%s" test-file)))))
               (lambda ()
                 (org-mcp-test--read-resource
                  (car (org-mcp-test--resource-uris beta))))
               (lambda ()
                 (org-mcp-test--read-resource
                  (car
                   (org-mcp-test--resource-uris
                    (org-mcp-test--file-link test-file "#alpha-slug")))))
               (lambda ()
                 (org-mcp-test--read-resource
                  (car
                   (org-mcp-test--resource-uris
                    (org-mcp-test--file-link test-file "*Gamma")))))
               (lambda ()
                 (org-mcp-test--read-resource (concat "org://file:" test-file)))
               (lambda ()
                 (org-mcp-test--read-resource
                  (concat "org://" (org-mcp-test--file-link test-file "*Beta"))))
               (lambda ()
                 (org-mcp-test--read-resource (concat "org://" beta)))))
             (ids (hash-table-count org-id-locations))
             (links nil))
        (should-not (find-buffer-visiting test-file))
        (dolist (visited '(nil t))
          (dolist (read reads)
            (let ((buffer (find-buffer-visiting test-file)))
              (when (and buffer (not visited))
                (kill-buffer buffer))
              (when visited
                (setq buffer (find-file-noselect test-file)))
              (let ((ticks (and visited (buffer-chars-modified-tick buffer)))
                    (result (funcall read)))
                (when (string-prefix-p "{" result)
                  (setq links
                        (append
                         links
                         (org-mcp-test--links-in
                          (json-read-from-string result)))))
                (should
                 (string= (org-mcp-test--read-file test-file)
                          org-mcp-test--content-read-tools))
                (setq buffer (find-buffer-visiting test-file))
                (should-not (and buffer (buffer-modified-p buffer)))
                (when visited
                  (should
                   (= ticks (buffer-chars-modified-tick buffer))))))))
        (should (= ids (hash-table-count org-id-locations)))
        (should
         (equal
          (sort (delete-dups (copy-sequence links)) #'string<)
          (sort
           (list
            beta
            (concat "id:" org-mcp-test--link-both-id)
            (org-mcp-test--file-link test-file "#alpha-slug")
            (org-mcp-test--file-link test-file "*Review")
            (org-mcp-test--file-link test-file "*Gamma")
            (org-mcp-test--file-link test-file "*Blank ID")
            (org-mcp-test--file-link test-file "*Decorated")
            (org-mcp-test--file-link test-file "*Clocked")
            (concat "file:" (abbreviate-file-name test-file)))
           #'string<)))
        (dolist (link (delete-dups (copy-sequence links)))
          (should
           (equal
            (alist-get
             'link (json-read-from-string (org-mcp-test--call-read link)))
            link)))
        (should
         (string= (org-mcp-test--read-file test-file)
                  org-mcp-test--content-read-tools))
        (should-not (buffer-modified-p (find-buffer-visiting test-file)))))))

;;; Refusal class tests

;; The markers are spelled out here rather than read from org-mcp, so
;; that a change to either one fails a test instead of passing
;; unnoticed.  docs/writing.org publishes them.

(defconst org-mcp-test--conflict-marker "conflict: "
  "Marker a stale-belief refusal carries, as a client matches it.")

(defconst org-mcp-test--blocked-marker "blocked: "
  "Marker an Org-veto refusal carries, as a client matches it.")

(defun org-mcp-test--refusal-class (message)
  "Return the class MESSAGE announces.
One of `conflict', `blocked' or `validation', the unmarked default."
  (cond
   ((string-prefix-p org-mcp-test--conflict-marker message) 'conflict)
   ((string-prefix-p org-mcp-test--blocked-marker message) 'blocked)
   (t 'validation)))

(defconst org-mcp-test--content-unfinished-child
  "* TODO Parent\n** TODO Child\n"
  "A parent whose child is unfinished, so Org vetoes finishing it.")

(defconst org-mcp-test--content-unchecked-box
  "* TODO Task One\n- [ ] Not yet\n"
  "A task holding an unchecked box, so Org vetoes finishing it.")

(defconst org-mcp-test--content-ordered-parent
  (concat "* TODO Parent\n"
          ":PROPERTIES:\n"
          ":ORDERED: t\n"
          ":END:\n"
          "** TODO First\n")
  "An ordered parent whose first child is unfinished.")

(defconst org-mcp-test--expected-unfinished-child-done-regex
  "\\`\\* TODO Parent\n\\*\\* DONE Child\n\\'"
  "Regex matching the whole file once the child is finished.")

(ert-deftest org-mcp-test-refusal-conflict-marks-a-stale-precondition ()
  "A precondition that no longer holds is refused as a conflict.
`before' and `before' say what the client believes the
file holds; when it holds something else, the refusal carries the
conflict marker, so the client reads again rather than sending the
same call.  A malformed call is refused unmarked, the validation
default."
  (org-mcp-test--with-temp-org-files
      ((test-file "* TODO Task One\nTask description.\n"))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (link (org-mcp-test--file-link test-file "*Task One")))
      (should
       (equal
        (org-mcp-test--call-tool-expecting-error
         test-file "org-node-set-todo"
         `((link . ,link)
           (before . "DONE")
           (after . "DONE")))
        (concat org-mcp-test--conflict-marker
                "State mismatch: expected 'DONE', found 'TODO'")))
      (should
       (equal
        (org-mcp-test--call-tool-expecting-error
         test-file "org-node-set-title"
         `((link . ,link)
           (before . "Task Two")
           (after . "Task Three")))
        (concat org-mcp-test--conflict-marker
                "Title mismatch: expected 'Task Two', found 'Task One'")))
      (should
       (eq
        (org-mcp-test--refusal-class
         (org-mcp-test--call-tool-expecting-error
          test-file "org-node-set-todo"
          `((link . ,link) (after . "NOPE"))))
        'validation)))))

(ert-deftest org-mcp-test-clock-out-refuses-without-a-running-clock ()
  "org-clock-out with no clock running is refused as a conflict.
The client believed a clock ran; reading the active clock again is
what puts that right, so the refusal is marked."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (should
     (equal
      (org-mcp-test--call-tool-expecting-error
       test-file "org-clock-out" nil)
      (concat org-mcp-test--conflict-marker "No active clock to stop")))
    (org-mcp-test--verify-no-modified-buffer test-file)))

(ert-deftest org-mcp-test-refusal-class-survives-the-resource-path ()
  "The resource refuses with the message the tool refuses with.
The transport carries no error code, so a class lives in the message
itself; the org://{link} resource re-signals the tool error's own
string as invalid-params, and a marker survives only because that
string is passed through untouched.  A link naming no heading is
refused unmarked on both paths: org-mcp cannot tell a heading that
has gone, a conflict, from one a client invented, so the refusal
stays in the validation default."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-links))
    (dolist (link
             (list "id:no-such-id"
                   (org-mcp-test--file-link test-file "*Nope")))
      (let ((message
             (org-mcp-test--call-tool-expecting-error
              test-file "org-node-read" `((link . ,link)))))
        (should (eq (org-mcp-test--refusal-class message) 'validation))
        (should
         (equal (org-mcp-test--resource-error (concat "org://" link))
                message))))))

(ert-deftest org-mcp-test-update-todo-state-refuses-an-org-veto ()
  "A TODO change Org vetoes is refused and nothing is written.
`org-enforce-todo-dependencies' blocks finishing a parent whose
child is unfinished.  The refusal carries the veto marker and Org's
own reason, the blocking heading, so the client can relay it; the
file and the buffer are untouched, and no save happens."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-unfinished-child))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-enforce-todo-dependencies t)
          (org-blocker-hook
           '(org-block-todo-from-children-or-siblings-or-parent)))
      (should
       (equal
        (org-mcp-test--call-tool-expecting-error
         test-file "org-node-set-todo"
         `((link . ,(org-mcp-test--file-link test-file "*Parent"))
           (after . "DONE")))
        (concat org-mcp-test--blocked-marker
                "TODO state change from TODO to DONE blocked "
                "(by \"TODO Child\")")))
      (org-mcp-test--verify-no-modified-buffer test-file))))

(ert-deftest org-mcp-test-update-todo-state-refuses-a-checkbox-veto ()
  "An unchecked box vetoes finishing the task that holds it.
`org-enforce-todo-checkbox-dependencies' names no heading, so the
refusal carries the reason Org gives instead."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-unchecked-box))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-enforce-todo-checkbox-dependencies t)
          (org-blocker-hook '(org-block-todo-from-checkboxes)))
      (should
       (equal
        (org-mcp-test--call-tool-expecting-error
         test-file "org-node-set-todo"
         `((link . ,(org-mcp-test--file-link test-file "*Task One"))
           (after . "DONE")))
        (concat org-mcp-test--blocked-marker
                "TODO state change from TODO to DONE blocked "
                "(by contained checkboxes)")))
      (org-mcp-test--verify-no-modified-buffer test-file))))

(ert-deftest org-mcp-test-update-todo-state-unvetoed-change-unaffected ()
  "With the blocker installed, a change Org allows goes through.
The child has nothing under it to block on, so it is finished and
the response reports the state Org left it in."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-unfinished-child))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-enforce-todo-dependencies t)
          (org-blocker-hook
           '(org-block-todo-from-children-or-siblings-or-parent)))
      (let ((result
             (org-mcp-test--call-update-todo-state
              (org-mcp-test--file-link test-file "*Child") "DONE")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'after result) "DONE")))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--expected-unfinished-child-done-regex))))

(ert-deftest org-mcp-test-add-todo-refuses-an-org-veto ()
  "A new heading in a state Org vetoes is refused, and none is added.
The parent is ordered and its first child is unfinished, so Org
blocks a second child that arrives already done.  The heading has no
state to move from, which the refusal says."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-ordered-parent))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-enforce-todo-dependencies t)
          (org-blocker-hook
           '(org-block-todo-from-children-or-siblings-or-parent)))
      (should
       (string-prefix-p
        (concat org-mcp-test--blocked-marker
                "TODO state change from (no state) to DONE blocked (by ")
        (org-mcp-test--call-tool-expecting-error
         test-file "org-node-create"
         `((title . "Second")
           (todo . "DONE")
           (content . nil)
           (parent
            . ,(org-mcp-test--file-link test-file "*Parent"))))))
      (org-mcp-test--verify-no-modified-buffer test-file))))

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

;;; The node shape

;; A file and a heading come back in the same shape, from every
;; endpoint that returns either: a heading, a file, a child, a query
;; result.  These tests pin what each endpoint carries for one file, at
;; the seam a client calls, so that consolidating the builders behind
;; them cannot quietly change an answer.

(defconst org-mcp-test--node-shape-parent-id
  "11111111-2222-3333-4444-555555555555"
  "ID of Parent in `org-mcp-test--content-node-shape'.")

(defconst org-mcp-test--content-node-shape
  (concat
   "#+TITLE: Node Shapes\n"
   "Preamble text.\n"
   "\n"
   "* TODO [#A] Parent :work:\n"
   "SCHEDULED: <2026-03-26 Thu> DEADLINE: <2026-04-01 Wed>\n"
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--node-shape-parent-id "\n"
   ":Effort:   1:00\n"
   ":END:\n"
   "Parent body.\n"
   "** DONE Child One :urgent:\n"
   "CLOSED: [2026-03-20 Fri 10:00]\n"
   "** Child Two\n"
   "*** Grandchild\n"
   "* Second\n")
  "A file carrying every field a node reports.
Parent has a TODO state, a priority, a tag of its own, both planning
timestamps, an ID, a property and a body.  Child One is closed and
tagged, Child Two carries an inherited tag only and a child of its
own, and Second is an empty sibling of Parent.")

(defun org-mcp-test--node-shape-read (link)
  "Return the node `org-node-read' serves for LINK, parsed."
  (json-read-from-string (org-mcp-test--call-read link)))

(ert-deftest org-mcp-test-node-shape-heading ()
  "A heading node carries each field the heading has, and no other.
A field the heading lacks is left out rather than sent as null, which
`closed' stands for here."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((node
           (org-mcp-test--node-shape-read
            (concat "id:" org-mcp-test--node-shape-parent-id))))
      (should (equal (alist-get 'title node) "Parent"))
      (should (equal (alist-get 'todo node) "TODO"))
      (should (equal (alist-get 'priority node) "A"))
      (should (equal (alist-get 'tags node) ["work"]))
      (should (equal (alist-get 'local_tags node) ["work"]))
      (should (equal (alist-get 'scheduled node) "<2026-03-26 Thu>"))
      (should (equal (alist-get 'deadline node) "<2026-04-01 Wed>"))
      (should-not (alist-get 'closed node))
      (should
       (equal (alist-get 'id node) org-mcp-test--node-shape-parent-id))
      (should (= (alist-get 'level node) 1))
      (should
       (equal (alist-get 'link node)
              (concat "id:" org-mcp-test--node-shape-parent-id)))
      (should (equal (alist-get 'file node) test-file))
      (should (equal (alist-get 'content node) "Parent body.")))))

(ert-deftest org-mcp-test-node-shape-children ()
  "A child is a node, asked for with few fields.
It carries its title, its TODO state when it has one, its level and
its link — enough to show the outline and to address the child in the
call that reads it in full."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((node
           (org-mcp-test--node-shape-read
            (concat "id:" org-mcp-test--node-shape-parent-id))))
      (should
       (equal
        (append (alist-get 'children node) nil)
        `(((title . "Child One")
           (todo . "DONE")
           (level . 2)
           (link
            . ,(org-mcp-test--file-link test-file "*Child One")))
          ((title . "Child Two")
           (level . 2)
           (link
            . ,(org-mcp-test--file-link test-file "*Child Two")))))))))

(ert-deftest org-mcp-test-node-shape-inherited-tag ()
  "A node reports the tags in effect on it and the tags of its own.
Child Two has no tag of its own, so `local_tags' is left out while
`tags' carries the one it inherits from Parent."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((node
           (org-mcp-test--node-shape-read
            (org-mcp-test--file-link test-file "*Child Two"))))
      (should (equal (alist-get 'title node) "Child Two"))
      (should (equal (alist-get 'tags node) ["work"]))
      (should-not (alist-get 'local_tags node))
      (should (= (alist-get 'level node) 2))
      (should-not (alist-get 'content node))
      (should
       (equal
        (append (alist-get 'children node) nil)
        `(((title . "Grandchild")
           (level . 3)
           (link
            . ,(org-mcp-test--file-link
                test-file "*Grandchild")))))))))

(ert-deftest org-mcp-test-node-shape-closed ()
  "A closed node reports its closing timestamp and both tag sets."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((node
           (org-mcp-test--node-shape-read
            (org-mcp-test--file-link test-file "*Child One"))))
      (should (equal (alist-get 'todo node) "DONE"))
      (should (equal (alist-get 'closed node) "[2026-03-20 Fri 10:00]"))
      (should (equal (alist-get 'tags node) ["work" "urgent"]))
      (should (equal (alist-get 'local_tags node) ["urgent"]))
      (should (equal (append (alist-get 'children node) nil) nil)))))

(ert-deftest org-mcp-test-node-shape-file ()
  "A file is a node: its preamble is its content, its headings its children."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((node
           (org-mcp-test--node-shape-read (concat "file:" test-file))))
      (should (equal (alist-get 'title node) "Node Shapes"))
      (should (equal (alist-get 'file node) test-file))
      (should (= (alist-get 'level node) 0))
      (should
       (equal (alist-get 'link node) (concat "file:" test-file)))
      (should
       (equal (alist-get 'content node)
              "#+TITLE: Node Shapes\nPreamble text."))
      (should
       (equal
        (append (alist-get 'children node) nil)
        `(((title . "Parent")
           (todo . "TODO")
           (level . 1)
           (link . ,(concat "id:" org-mcp-test--node-shape-parent-id)))
          ((title . "Second")
           (level . 1)
           (link
            . ,(org-mcp-test--file-link test-file "*Second")))))))))

(ert-deftest org-mcp-test-node-shape-resource-is-the-read ()
  "The org:// resource serves the node org-node-read returns."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let* ((link (concat "id:" org-mcp-test--node-shape-parent-id))
           (response
            (json-parse-string
             (mcp-server-lib-process-jsonrpc
              (mcp-server-lib-create-resources-read-request
               (concat "org://" link))
              mcp-server-lib-ert-server-id)
             :object-type 'alist))
           (contents
            (alist-get 'contents (alist-get 'result response))))
      (should-not (alist-get 'error response))
      (should
       (equal (alist-get 'text (aref contents 0))
              (org-mcp-test--call-read link))))))

(ert-deftest org-mcp-test-node-shape-query-match ()
  "A query result is a node, carrying the fields a read carries.
The Org property drawer comes with it, under its own key, with the
values Org computes rather than stores left out."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let* ((result
            (org-mcp-test--call-ql-query "(todo \"TODO\")"))
           (matches (alist-get 'children result))
           (node (aref matches 0)))
      (should (= (length matches) 1))
      (should (= (alist-get 'total result) 1))
      (should (equal (alist-get 'title node) "Parent"))
      (should (equal (alist-get 'todo node) "TODO"))
      (should (equal (alist-get 'priority node) "A"))
      (should (equal (alist-get 'tags node) ["work"]))
      (should (equal (alist-get 'local_tags node) ["work"]))
      (should (equal (alist-get 'scheduled node) "<2026-03-26 Thu>"))
      (should (equal (alist-get 'deadline node) "<2026-04-01 Wed>"))
      (should (equal (alist-get 'file node) test-file))
      (should
       (equal (alist-get 'id node) org-mcp-test--node-shape-parent-id))
      (should (= (alist-get 'level node) 1))
      (should
       (equal (alist-get 'link node)
              (concat "id:" org-mcp-test--node-shape-parent-id)))
      (should
       (equal
        (alist-get 'properties node)
        `((EFFORT . "1:00")
          (ID . ,org-mcp-test--node-shape-parent-id)))))))


(defconst org-mcp-test--node-shape-file-id
  "99999999-8888-7777-6666-555555555555"
  "ID in the file-level drawer of `org-mcp-test--content-file-node-id'.")

(defconst org-mcp-test--content-file-node-id
  (concat
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--node-shape-file-id "\n"
   ":END:\n"
   "* Only\n")
  "A file whose own property drawer carries an ID and which sets no title.")

(ert-deftest org-mcp-test-node-shape-file-linked-by-its-id ()
  "A file carrying an ID of its own reports that ID as its link.
Reading the path and reading the ID return the same node, so a file is
addressed the way every other node is.  Setting no `#+TITLE:' leaves
the file's own name as its title."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-file-node-id
      (list org-mcp-test--node-shape-file-id)
    (let* ((link (concat "id:" org-mcp-test--node-shape-file-id))
           (node (org-mcp-test--node-shape-read link)))
      (should (equal (alist-get 'link node) link))
      (should (= (alist-get 'level node) 0))
      (should
       (equal (alist-get 'title node)
              (file-name-nondirectory test-file)))
      (should (equal (alist-get 'file node) test-file))
      (should
       (equal (org-mcp-test--node-shape-read (concat "file:" test-file))
              node)))))

;;; Asking for the fields you want

;; A call says how much of a node it wants and gets exactly that.
;; These tests ask at the seam a client asks at, so what they pin is
;; the contract rather than the resolution behind it.

(defun org-mcp-test--read-fields (link fields)
  "Return the node `org-node-read' serves for LINK asking for FIELDS.
FIELDS is sent as the `fields' parameter, as a client sends it."
  (json-read-from-string
   (mcp-server-lib-ert-call-tool
    "org-node-read" `((link . ,link) (fields . ,fields)))))

(defun org-mcp-test--query-fields (query fields)
  "Return the nodes `org-query' matches QUERY with, asking for FIELDS.
The drawer a query carries unasked is turned off, so what comes back
is the fields and nothing else."
  (alist-get
   'children
   (json-read-from-string
    (mcp-server-lib-ert-call-tool
     "org-query"
     `((query . ,query)
       (fields . ,fields)
       (properties . "none"))))))

(ert-deftest org-mcp-test-fields-named-explicitly ()
  "A call naming the fields it wants receives those and no others.
The node lists them in the order the call did, and a field named
twice is one key, since a node cannot carry the same key twice."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((link (concat "id:" org-mcp-test--node-shape-parent-id)))
      (should
       (equal
        (org-mcp-test--read-fields link ["todo" "title"])
        '((todo . "TODO") (title . "Parent"))))
      (should
       (equal
        (org-mcp-test--read-fields link ["title" "title"])
        '((title . "Parent")))))))

(ert-deftest org-mcp-test-fields-absent-when-empty ()
  "A field asked for that the node has no value for is left out.
An absent key therefore means the node has nothing there, whether
the call asked for the field or not; the call itself says which of
the two it is, because it knows what it asked for."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (should
     (equal
      (org-mcp-test--read-fields
       (concat "id:" org-mcp-test--node-shape-parent-id)
       ["title" "closed" "todo"])
      '((title . "Parent") (todo . "TODO"))))))

(ert-deftest org-mcp-test-fields-every-field-is-askable ()
  "Every field a node can carry is a field a call may ask for.
The request is built from the list a call is checked against, so a
field named there that the builder does not build fails here rather
than reaching a client as a refusal.

A heading carries every field but `closed' here, which stands for
the fields left out when empty.  A file carries the ones a file has.
No field of either is the Org drawer: that is a namespace of the
user's, asked for in its own parameter.  Both digests are there for
either: a region always has one, even when it is empty."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((every (vconcat (mapcar #'symbol-name org-mcp--node-fields))))
      (should
       (equal
        (mapcar
         #'car
         (org-mcp-test--read-fields
          (concat "id:" org-mcp-test--node-shape-parent-id) every))
        (remq 'closed org-mcp--node-fields)))
      (should
       (equal
        (mapcar
         #'car
         (org-mcp-test--read-fields (concat "file:" test-file) every))
        '(title
          file
          level
          link
          content
          content_digest
          digest
          children))))))

(ert-deftest org-mcp-test-fields-one-field-is-a-reference ()
  "A node asked for with one field is a reference to it.
Asking for a child's own fields returns the child as its parent
carries it, so a reference is a node with few fields rather than a
shape of its own, and asking for more of the same node is how it
grows into a whole one."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let* ((parent
            (org-mcp-test--node-shape-read
             (concat "id:" org-mcp-test--node-shape-parent-id)))
           (child (aref (alist-get 'children parent) 0))
           (link (org-mcp-test--file-link test-file "*Child One")))
      (should
       (equal
        (org-mcp-test--read-fields
         link ["title" "todo" "level" "link"])
        child))
      (should
       (equal
        (org-mcp-test--read-fields link ["link"])
        `((link . ,link)))))))

(ert-deftest org-mcp-test-fields-named-list ()
  "A call names a configured list instead of listing the fields.
The two lists org-mcp is configured with out of the box are the two
shapes the surface has a word for: a reference, and enough of a node
to show it in an outline."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((link (concat "id:" org-mcp-test--node-shape-parent-id)))
      (should
       (equal
        (org-mcp-test--read-fields link "reference")
        `((link . ,link))))
      (should
       (equal
        (org-mcp-test--read-fields link "outline")
        `((title . "Parent")
          (todo . "TODO")
          (level . 1)
          (link . ,link)))))))

(ert-deftest org-mcp-test-fields-lists-are-user-configuration ()
  "The set of named lists is the user's, not one baked into the server.
A list configured here is a list a call can name, and the lists
org-mcp ships with are gone once the user replaces them."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((org-mcp-node-field-lists '((planning title scheduled deadline)))
          (link (concat "id:" org-mcp-test--node-shape-parent-id)))
      (should
       (equal
        (org-mcp-test--read-fields link "planning")
        '((title . "Parent")
          (scheduled . "<2026-03-26 Thu>")
          (deadline . "<2026-04-01 Wed>"))))
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (fields . "outline"))
       "Unknown field list: outline\\.  Configured lists: planning\\."))))

(ert-deftest org-mcp-test-fields-blank-asks-for-the-default ()
  "A blank `fields' means the call does not send one.
Clients fill an optional parameter they do not use with an empty
value, so an empty array and an empty string ask for what the
endpoint carries unasked, as they do everywhere else in the
surface."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let* ((link (concat "id:" org-mcp-test--node-shape-parent-id))
           (default (org-mcp-test--node-shape-read link)))
      (should (equal (org-mcp-test--read-fields link []) default))
      (should (equal (org-mcp-test--read-fields link "") default)))))

(ert-deftest org-mcp-test-fields-unknown-name-refused ()
  "A field that does not exist is refused, naming the ones that do.
Silently leaving it out would hand a client a node missing the field
it asked for, with nothing to tell it why."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((link (concat "id:" org-mcp-test--node-shape-parent-id)))
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (fields . ["titel"]))
       "Unknown node field: titel\\.  Valid fields: title, todo, ")
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (fields . "summary"))
       "Unknown field list: summary\\.  \
Configured lists: reference, outline\\.")
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (fields . "link"))
       "Unknown field list: link\\.")
      (org-mcp-test--call-tool-refused
       "org-query" `((query . "(todo \"TODO\")") (fields . ["bodyy"]))
       "Unknown node field: bodyy\\."))))

(ert-deftest org-mcp-test-fields-query-carries-no-unasked-body ()
  "A match list carries a body only when the call asked for one.
Filling `content' means reading every matched subtree, so a query
leaves it out unasked — and returns it for the asking, which is the
whole of what the parameter is for."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((query "(todo \"TODO\")"))
      (should-not
       (alist-get
        'content
        (aref (alist-get 'children
                         (org-mcp-test--call-ql-query query))
              0)))
      (should
       (equal
        (aref (org-mcp-test--query-fields query ["title" "content"]) 0)
        '((title . "Parent") (content . "Parent body.")))))))

(ert-deftest org-mcp-test-fields-mean-the-same-on-both-endpoints ()
  "The same `fields' asks the same thing of a read and of a query.
One heading reached two ways comes back as one node, so a client
that learns the parameter on either endpoint has learned it on
both."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((fields ["title" "todo" "local_tags" "link"]))
      (should
       (equal
        (aref (org-mcp-test--query-fields "(todo \"TODO\")" fields) 0)
        (org-mcp-test--read-fields
         (concat "id:" org-mcp-test--node-shape-parent-id)
         fields))))))

;;; The digests a client asserts with

;; Two fields a client asks for when it means to change something:
;; `digest' over the whole subtree and `content_digest' over the body.
;; Each is a token over a buffer region, and these tests cut the region
;; out of the file itself and hash it the way the documentation says
;; org-mcp hashes it, so a token taken over the wrong region, or built
;; some other way, fails here rather than reaching a client.

(defun org-mcp-test--digest-of (text)
  "Return the digest org-mcp emits for a region holding TEXT.
The token is built here from the published recipe — `sha256:' and the
first 16 hexadecimal characters of the SHA-256 of the region's UTF-8
bytes — rather than by calling org-mcp, so that a change to how
org-mcp builds one is a failure and not a silent agreement."
  (concat
   "sha256:"
   (substring
    (secure-hash 'sha256 (encode-coding-string text 'utf-8 t)) 0 16)))

(defun org-mcp-test--region-of (file from to)
  "Return the text of FILE between the strings FROM and TO.
The region runs from where FROM begins to where TO begins, or to the
end of FILE when TO is nil, so a test names the region a digest
covers by what bounds it in the file."
  (let* ((text (org-mcp-test--read-file file))
         (begin (string-search from text))
         (end (if to (string-search to text) (length text))))
    (should begin)
    (should end)
    (substring text begin end)))

(defconst org-mcp-test--content-digest-trim
  "* One\nSame body.\n** Child\n* Two\nSame body.\n"
  "Two headings whose bodies read alike and hash apart.
One has a child, so its body region ends at that child and keeps the
newline the body ends with.  Two has none, so its region ends where
its last line does.  Both report the same `content', which is
trimmed.")

(ert-deftest org-mcp-test-digest-absent-until-asked-for ()
  "Neither digest comes back unless the call asks for it.
A digest is for a client about to change something; a read that only
wants to see the node is not made to hash it."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let* ((link (concat "id:" org-mcp-test--node-shape-parent-id))
           (unasked (org-mcp-test--node-shape-read link))
           (asked
            (org-mcp-test--read-fields
             link ["digest" "content_digest"])))
      (should-not (alist-get 'digest unasked))
      (should-not (alist-get 'content_digest unasked))
      (should-not
       (alist-get
        'digest
        (aref
         (alist-get
          'children (org-mcp-test--call-ql-query "(todo \"TODO\")"))
         0)))
      (should (alist-get 'digest asked))
      (should (alist-get 'content_digest asked)))))

(ert-deftest org-mcp-test-digest-covers-its-region ()
  "Each digest is the hash of the region it is defined over.
`digest' covers the subtree from the heading's stars to the next
heading, descendants and drawers included.  `content_digest' covers
the body between the drawer and the first child — the region
`content' is read from and org-node-set-content writes within."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((node
           (org-mcp-test--read-fields
            (concat "id:" org-mcp-test--node-shape-parent-id)
            ["digest" "content_digest"])))
      (should
       (equal
        (alist-get 'digest node)
        (org-mcp-test--digest-of
         (org-mcp-test--region-of
          test-file "* TODO [#A] Parent" "* Second"))))
      (should
       (equal
        (alist-get 'content_digest node)
        (org-mcp-test--digest-of
         (org-mcp-test--region-of
          test-file "Parent body." "** DONE Child One")))))))

(ert-deftest org-mcp-test-digest-of-a-file-covers-the-file ()
  "A file is a node, and its two regions are the file and its preamble.
Nothing about a digest is particular to a heading: the subtree of a
file is all of it, and its body is what lies before its first
heading."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((node
           (org-mcp-test--read-fields
            (concat "file:" test-file) ["digest" "content_digest"])))
      (should
       (equal
        (alist-get 'digest node)
        (org-mcp-test--digest-of
         (org-mcp-test--read-file test-file))))
      (should
       (equal
        (alist-get 'content_digest node)
        (org-mcp-test--digest-of
         (org-mcp-test--region-of
          test-file "#+TITLE:" "* TODO [#A] Parent")))))))

(ert-deftest org-mcp-test-digest-of-an-empty-body ()
  "A body with nothing in it has no content and a digest all the same.
`content' is absent because there is nothing to read; the region is
still a region, and a client asserting that a body is empty has a
token to send back for it."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((node
           (org-mcp-test--read-fields
            (org-mcp-test--file-link test-file "*Child Two")
            ["content" "content_digest"])))
      (should-not (alist-get 'content node))
      (should
       (equal
        (alist-get 'content_digest node)
        (org-mcp-test--digest-of ""))))))

(ert-deftest org-mcp-test-digest-is-not-taken-over-trimmed-content ()
  "The token covers the body's region, not the body a read returns.
`content' is trimmed for its reader, so hashing what a read returns
would make a presentation decision into a safety boundary.  Two
headings whose bodies differ only in that trimming therefore carry
one `content' and two tokens, which is the buffer telling the truth
about itself."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-digest-trim))
    (let* ((fields ["content" "content_digest"])
           (one
            (org-mcp-test--read-fields
             (org-mcp-test--file-link test-file "*One") fields))
           (two
            (org-mcp-test--read-fields
             (org-mcp-test--file-link test-file "*Two") fields)))
      (should (equal (alist-get 'content one) "Same body."))
      (should (equal (alist-get 'content two) "Same body."))
      (should
       (equal
        (alist-get 'content_digest one)
        (org-mcp-test--digest-of "Same body.\n")))
      (should
       (equal
        (alist-get 'content_digest two)
        (org-mcp-test--digest-of "Same body.")))
      (should-not
       (equal
        (alist-get 'content_digest one)
        (alist-get 'content_digest two))))))

(ert-deftest org-mcp-test-digest-covers-every-descendant ()
  "A change to a grandchild moves the subtree digest and nothing else.
The call asks for the two tokens alone — no children, no body — and
the subtree digest still moves when a grandchild is tagged: it covers
the whole subtree whatever the call asked to see of it.  The body the
change never touched keeps its token, which is why there are two."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let* ((link (concat "id:" org-mcp-test--node-shape-parent-id))
           (fields ["digest" "content_digest"])
           (before (org-mcp-test--read-fields link fields)))
      (mcp-server-lib-ert-call-tool
       "org-node-set-tags"
       `((link
          .
          ,(org-mcp-test--file-link test-file "*Grandchild"))
         (after . ["later"])))
      (let ((after (org-mcp-test--read-fields link fields)))
        (should-not
         (equal (alist-get 'digest after) (alist-get 'digest before)))
        (should
         (equal
          (alist-get 'content_digest after)
          (alist-get 'content_digest before)))))))

(ert-deftest org-mcp-test-digest-follows-the-body-it-guards ()
  "The body's token names the body the editor left behind.
org-node-set-content writes within the region `content_digest'
covers, so a token read before the write no longer matches after it,
and the token read afterwards is the one over what was written.  A
guard and the edit it guards cannot drift apart while they share a
region."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let* ((link (concat "id:" org-mcp-test--node-shape-parent-id))
           (fields ["content_digest"])
           (before (org-mcp-test--read-fields link fields)))
      (mcp-server-lib-ert-call-tool
       "org-node-set-content"
       `((link . ,link)
         (before . "Parent body.")
         (after . "Rewritten body.")))
      (let ((after (org-mcp-test--read-fields link fields)))
        (should-not (equal after before))
        (should
         (equal
          (alist-get 'content_digest after)
          (org-mcp-test--digest-of
           (org-mcp-test--region-of
            test-file "Rewritten body." "** DONE Child One"))))))))
;;; Asking for the properties you want

;; A node's Org drawer is a namespace of the user's, asked for in a
;; parameter of its own and answered under one key.  These tests ask at
;; the seam a client asks at.

(defconst org-mcp-test--content-property-namespace
  (concat
   "* Parent\n"
   ":PROPERTIES:\n"
   ":TITLE:    a property, not the heading\n"
   ":Effort:   1:00\n"
   ":END:\n")
  "A heading whose drawer holds a property named like a node field.")

(defun org-mcp-test--read-properties (link properties)
  "Return the node `org-node-read' serves for LINK asking for PROPERTIES.
PROPERTIES is sent as the `properties' parameter, as a client sends
it, and the node is cut down to the one field the drawer could
collide with, so what the test reads is the two namespaces side by
side."
  (json-read-from-string
   (mcp-server-lib-ert-call-tool
    "org-node-read"
    `((link . ,link)
      (fields . ["title"])
      (properties . ,properties)))))

(defun org-mcp-test--query-properties (query properties)
  "Return the nodes `org-query' matches QUERY with, asking for PROPERTIES."
  (alist-get
   'children
   (json-read-from-string
    (mcp-server-lib-ert-call-tool
     "org-query"
     `((query . ,query)
       (fields . ["title"])
       (properties . ,properties))))))

(ert-deftest org-mcp-test-properties-named-explicitly ()
  "A call naming the properties it wants receives those and no others.
A name the drawer does not hold contributes nothing, as a field with
no value does, so the answer says what the node has rather than what
the call asked about."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((link (concat "id:" org-mcp-test--node-shape-parent-id)))
      (should
       (equal
        (org-mcp-test--read-properties link ["Effort"])
        '((title . "Parent") (properties . ((EFFORT . "1:00"))))))
      (should
       (equal
        (org-mcp-test--read-properties link ["Owner"])
        '((title . "Parent")))))))

(ert-deftest org-mcp-test-properties-match-as-org-matches-them ()
  "A property name is matched the way Org matches one, ignoring case.
The drawer of the file under test writes `Effort'; a call naming it
in any case asks for the same property, and reads it back under the
name Org keeps."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((link (concat "id:" org-mcp-test--node-shape-parent-id))
          (answer
           '((title . "Parent") (properties . ((EFFORT . "1:00"))))))
      (should (equal (org-mcp-test--read-properties link ["effort"])
                     answer))
      (should (equal (org-mcp-test--read-properties link ["EFFORT"])
                     answer)))))

(ert-deftest org-mcp-test-properties-whole-drawer-or-none ()
  "A call takes the whole drawer, or none of it, by naming a group.
Naming each property is the way to ask for some of them; \"all\" is
how a call that does not know the names reaches them, and \"none\"
is how one that has the fields it came for leaves them behind."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((link (concat "id:" org-mcp-test--node-shape-parent-id)))
      (should
       (equal
        (org-mcp-test--read-properties link "all")
        `((title . "Parent")
          (properties
           . ((EFFORT . "1:00")
              (ID . ,org-mcp-test--node-shape-parent-id))))))
      (should
       (equal
        (org-mcp-test--read-properties link "none")
        '((title . "Parent")))))))

(ert-deftest org-mcp-test-properties-kept-apart-from-fields ()
  "A property cannot shadow the node field of the same name.
A drawer holds names the user chose, so one of them is called TITLE
here.  It arrives under `properties', beside the field `title', and
neither has anything to say about the other."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-property-namespace))
    (let ((link (org-mcp-test--file-link test-file "*Parent")))
      (should
       (equal
        (org-mcp-test--read-properties link "all")
        '((title . "Parent")
          (properties
           . ((EFFORT . "1:00")
              (TITLE . "a property, not the heading"))))))
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (fields . ["TITLE"]))
       "Unknown node field: TITLE\\.  Valid fields: title, todo, "))))

(ert-deftest org-mcp-test-properties-file-drawer-is-a-drawer ()
  "A file's own property drawer is a drawer like any other.
A file is a node, so the properties parameter reaches its top-level
drawer the way it reaches a heading's."
  (org-mcp-test--with-id-setup
      test-file org-mcp-test--content-file-node-id
      (list org-mcp-test--node-shape-file-id)
    (should
     (equal
      (org-mcp-test--read-properties (concat "file:" test-file) "all")
      `((title . ,(file-name-nondirectory test-file))
        (properties
         . ((ID . ,org-mcp-test--node-shape-file-id))))))))

(ert-deftest org-mcp-test-properties-default-is-the-endpoint-s ()
  "What a call carries unasked is what that endpoint is for.
A read carries the whole node and no drawer: a drawer holds what the
user put there, and a client asks for the properties it knows what
to do with.  A query is the call that asks about properties, so it
hands the drawer back with every match."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((link (concat "id:" org-mcp-test--node-shape-parent-id))
          (drawer
           `((EFFORT . "1:00")
             (ID . ,org-mcp-test--node-shape-parent-id))))
      (should-not
       (alist-get 'properties (org-mcp-test--node-shape-read link)))
      (should
       (equal
        (alist-get
         'properties
         (aref
          (alist-get
           'children
           (org-mcp-test--call-ql-query "(todo \"TODO\")"))
          0))
        drawer))
      (should
       (equal
        (org-mcp-test--read-properties link [])
        '((title . "Parent"))))
      (should
       (equal
        (alist-get
         'properties
         (aref (org-mcp-test--query-properties "(todo \"TODO\")" []) 0))
        drawer)))))

(ert-deftest org-mcp-test-properties-mean-the-same-on-both-endpoints ()
  "The same `properties' asks the same thing of a read and of a query.
One heading reached two ways comes back as one node, drawer
included, so a client that learns the parameter on either endpoint
has learned it on both."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (dolist (asked (list ["Effort"] "all" "none"))
      (should
       (equal
        (aref (org-mcp-test--query-properties "(todo \"TODO\")" asked) 0)
        (org-mcp-test--read-properties
         (concat "id:" org-mcp-test--node-shape-parent-id) asked))))))

(ert-deftest org-mcp-test-properties-special-property-refused ()
  "A special property is refused rather than answered empty.
Org computes those rather than storing them, so no drawer holds one,
and a call that asked for one and got nothing back would read that
as a node without it."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((link (concat "id:" org-mcp-test--node-shape-parent-id)))
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (properties . ["TODO"]))
       "Not a drawer property: TODO\\.  Org computes it rather than \
storing it; the node's own fields carry what it says\\.  Special \
properties: TODO, TAGS, ")
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (properties . ["deadline"]))
       "Not a drawer property: deadline\\.")
      (org-mcp-test--call-tool-refused
       "org-query"
       `((query . "(todo \"TODO\")") (properties . ["ALLTAGS"]))
       "Not a drawer property: ALLTAGS\\."))))

(ert-deftest org-mcp-test-properties-malformed-refused ()
  "A `properties' that is neither names nor a group is refused.
The refusal names both ways of writing one, since a call that sent
something else cannot tell which it got wrong."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((link (concat "id:" org-mcp-test--node-shape-parent-id)))
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (properties . "everything"))
       "properties takes an array of names, or \"all\" or \"none\" \
as a string, not: \"everything\"")
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (properties . [42]))
       "A property name is a string, not: 42")
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (properties . ["not a name"]))
       "Invalid property name: 'not a name'"))))

;;; Asking for the computed fields you want

;; A computed field is a configured function's answer at the moment of
;; reading.  It belongs to no drawer, so it arrives apart from one.

(defconst org-mcp-test--content-computed-clash
  (concat
   "* TODO Parent\n"
   ":PROPERTIES:\n"
   ":RANK:     written down\n"
   ":END:\n")
  "A heading whose drawer holds a property named like a computed field.")

(defmacro org-mcp-test--with-computed-fields (&rest body)
  "Run BODY with two computed fields configured.
`rank' answers for every node and `nothing' for none, so one test
can tell a field with a value from a field without one."
  (declare (indent 0) (debug t))
  `(let ((org-mcp-computed-fields
          (list (cons 'rank (lambda () 12))
                (cons 'nothing (lambda () nil)))))
     ,@body))

(defun org-mcp-test--read-computed (link computed)
  "Return the node `org-node-read' serves for LINK asking for COMPUTED.
COMPUTED is sent as the `computed' parameter, as a client sends it,
and the node is cut down to its title so that what the test reads is
the computed fields beside one field of the node's own."
  (json-read-from-string
   (mcp-server-lib-ert-call-tool
    "org-node-read"
    `((link . ,link)
      (fields . ["title"])
      (computed . ,computed)))))

(defun org-mcp-test--query-computed (query computed)
  "Return the nodes `org-query' matches QUERY with, asking for COMPUTED."
  (alist-get
   'children
   (json-read-from-string
    (mcp-server-lib-ert-call-tool
     "org-query"
     `((query . ,query)
       (fields . ["title"])
       (properties . "none")
       (computed . ,computed))))))

(ert-deftest org-mcp-test-computed-named-explicitly ()
  "A call naming the computed fields it wants receives those.
A function that answers with nothing leaves its field out, the way a
node field with no value is left out."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (org-mcp-test--with-computed-fields
      (let ((link (concat "id:" org-mcp-test--node-shape-parent-id)))
        (should
         (equal
          (org-mcp-test--read-computed link ["rank"])
          '((title . "Parent") (computed . ((rank . 12))))))
        (should
         (equal
          (org-mcp-test--read-computed link ["nothing"])
          '((title . "Parent"))))
        (should
         (equal
          (org-mcp-test--read-computed link "all")
          '((title . "Parent") (computed . ((rank . 12))))))
        (should
         (equal
          (org-mcp-test--read-computed link "none")
          '((title . "Parent"))))))))

(ert-deftest org-mcp-test-computed-is-the-user-s-configuration ()
  "The computed fields are the user's, not a set baked into the server.
Out of the box nothing is configured, so there is nothing to ask
for and every name is refused, naming what is configured."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (let ((link (concat "id:" org-mcp-test--node-shape-parent-id)))
      (should
       (equal
        (org-mcp-test--read-computed link "all")
        '((title . "Parent"))))
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (computed . ["rank"]))
       "Unknown computed field: rank\\.  Configured computed \
fields: none")
      (org-mcp-test--with-computed-fields
        (org-mcp-test--call-tool-refused
         "org-node-read" `((link . ,link) (computed . ["renk"]))
         "Unknown computed field: renk\\.  Configured computed \
fields: rank, nothing")
        (org-mcp-test--call-tool-refused
         "org-query"
         `((query . "(todo \"TODO\")") (computed . "every"))
         "computed takes an array of names, or \"all\" or \"none\" \
as a string, not: \"every\"")))))

(ert-deftest org-mcp-test-computed-kept-apart-from-properties ()
  "A computed value never arrives where a stored one does.
The heading's drawer holds a RANK the user wrote down and the
workflow computes a `rank' of its own.  They come back under
separate keys, so a client writing the drawer back writes what the
file said rather than what this server worked out."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-computed-clash))
    (org-mcp-test--with-computed-fields
      (should
       (equal
        (json-read-from-string
         (mcp-server-lib-ert-call-tool
          "org-node-read"
          `((link . ,(org-mcp-test--file-link test-file "*Parent"))
            (fields . ["title"])
            (properties . "all")
            (computed . "all"))))
        '((title . "Parent")
          (properties . ((RANK . "written down")))
          (computed . ((rank . 12)))))))))

(ert-deftest org-mcp-test-computed-default-is-the-endpoint-s ()
  "A query carries the computed fields unasked; a read carries none.
A workflow configures them for the matches it ranks and groups, so
they come with a match list without being asked for, and a read that
wants one says so."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (org-mcp-test--with-computed-fields
      (let ((link (concat "id:" org-mcp-test--node-shape-parent-id)))
        (should-not
         (alist-get 'computed (org-mcp-test--node-shape-read link)))
        (should
         (equal
          (alist-get
           'computed
           (aref
            (alist-get
             'children
             (org-mcp-test--call-ql-query "(todo \"TODO\")"))
            0))
          '((rank . 12))))
        (should
         (equal
          (aref (org-mcp-test--query-computed "(todo \"TODO\")" []) 0)
          '((title . "Parent") (computed . ((rank . 12))))))
        (should
         (equal
          (aref
           (org-mcp-test--query-computed "(todo \"TODO\")" "none")
           0)
          '((title . "Parent"))))))))

(ert-deftest org-mcp-test-computed-means-the-same-on-both-endpoints ()
  "The same `computed' asks the same thing of a read and of a query."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (org-mcp-test--with-computed-fields
      (dolist (asked (list ["rank"] "all" "none"))
        (should
         (equal
          (aref (org-mcp-test--query-computed "(todo \"TODO\")" asked)
                0)
          (org-mcp-test--read-computed
           (concat "id:" org-mcp-test--node-shape-parent-id)
           asked)))))))
;;; Reading a subtree in one call

;; `depth' expands that many generations of children in place, and the
;; generation past it comes back as references.  These tests read at
;; the seam a client reads at, so what they pin is that an expanded
;; child and a read of that child by its own link are one node.

(defconst org-mcp-test--depth-project-id
  "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  "ID of Project in `org-mcp-test--content-depth'.")

(defconst org-mcp-test--content-depth
  (concat
   "#+TITLE: Depth\n"
   "Preamble.\n"
   "* TODO Project :work:\n"
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--depth-project-id "\n"
   ":END:\n"
   "Project body.\n"
   "** TODO Task One\n"
   "Task One body.\n"
   "*** Step A\n"
   "*** Step B\n"
   "** Task Two\n"
   "* Other\n")
  "A file four generations deep, counting the file itself as the first.
Project has two children, Task One two of its own and Task Two none,
so one walk covers an expanded node with children, an expanded node
without, and the references that end the walk.")

(defun org-mcp-test--read-depth (link depth &optional fields)
  "Return the node `org-node-read' serves for LINK, DEPTH deep.
DEPTH is sent as the `depth' parameter and FIELDS, when non-nil, as
the `fields' parameter, the way a client sends them."
  (json-read-from-string
   (mcp-server-lib-ert-call-tool
    "org-node-read"
    `((link . ,link)
      (depth . ,depth)
      ,@(when fields `((fields . ,fields)))))))

(ert-deftest org-mcp-test-depth-none-returns-references ()
  "A read asking for no depth carries its children as references.
Sending no `depth', sending zero, sending the string a client
following the tool schema sends and sending the blank an optional
parameter is filled with are one call: a walk that expands nothing
ends where it starts, and each child is the address of the read that
opens it."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
      (list org-mcp-test--depth-project-id)
    (let* ((link (concat "id:" org-mcp-test--depth-project-id))
           (node
            (json-read-from-string (org-mcp-test--call-read link))))
      (should (equal (org-mcp-test--read-depth link 0) node))
      (should (equal (org-mcp-test--read-depth link "0") node))
      (should (equal (org-mcp-test--read-depth link "") node))
      (should (equal (org-mcp-test--read-depth link []) node))
      (should
       (equal
        (append (alist-get 'children node) nil)
        `(((title . "Task One")
           (todo . "TODO")
           (level . 2)
           (link . ,(org-mcp-test--file-link test-file "*Task One")))
          ((title . "Task Two")
           (level . 2)
           (link
            . ,(org-mcp-test--file-link test-file "*Task Two")))))))))

(ert-deftest org-mcp-test-depth-expands-one-generation ()
  "Depth one returns children as nodes whose own children are references.
An expanded child carries the fields the call asked of the node
itself, body text included; the generation past the depth carries a
reference's four, so the walk ends in an address rather than in a
node that looks whole and is not."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
      (list org-mcp-test--depth-project-id)
    (let* ((link (concat "id:" org-mcp-test--depth-project-id))
           (children
            (alist-get 'children (org-mcp-test--read-depth link 1)))
           (task-one (aref children 0))
           (task-two (aref children 1)))
      (should (equal (alist-get 'title task-one) "Task One"))
      (should (equal (alist-get 'todo task-one) "TODO"))
      (should (equal (alist-get 'content task-one) "Task One body."))
      (should (equal (alist-get 'file task-one) test-file))
      (should (equal (alist-get 'tags task-one) ["work"]))
      (should
       (equal
        (append (alist-get 'children task-one) nil)
        `(((title . "Step A")
           (level . 3)
           (link . ,(org-mcp-test--file-link test-file "*Step A")))
          ((title . "Step B")
           (level . 3)
           (link . ,(org-mcp-test--file-link test-file "*Step B"))))))
      ;; Task Two has no children, and reports that the way a read of
      ;; Task Two on its own reports it.
      (should
       (equal (append (alist-get 'children task-two) nil) nil)))))

(ert-deftest org-mcp-test-depth-expands-two-generations ()
  "Depth two goes one generation further than depth one.
Step A arrives as a whole node where depth one left a reference: it
gains the fields a reference does not carry and the empty `children'
of a node with none."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
      (list org-mcp-test--depth-project-id)
    (let* ((link (concat "id:" org-mcp-test--depth-project-id))
           (step-of
            (lambda (depth)
              (aref
               (alist-get
                'children
                (aref
                 (alist-get
                  'children (org-mcp-test--read-depth link depth))
                 0))
               0))))
      (should-not (alist-get 'file (funcall step-of 1)))
      (should (equal (alist-get 'title (funcall step-of 2)) "Step A"))
      (should (equal (alist-get 'file (funcall step-of 2)) test-file))
      (should
       (equal
        (append (alist-get 'children (funcall step-of 2)) nil) nil)))))

(ert-deftest org-mcp-test-depth-expanded-child-is-a-read-of-that-child ()
  "An expanded child is the node a read of its own link returns.
This is what makes depth a join rather than a second shape: one
builder, one node, so a client that expands a subtree and a client
that walks it link by link cannot tell their answers apart.  It
holds a generation down, where the child of an expanded child equals
that child read one generation shallower, and it holds for a field
list the call names as it does for the default."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
      (list org-mcp-test--depth-project-id)
    (let ((project (concat "id:" org-mcp-test--depth-project-id))
          (task-one (org-mcp-test--file-link test-file "*Task One"))
          (fields ["title" "level" "link" "children"]))
      (should
       (equal
        (aref
         (alist-get 'children (org-mcp-test--read-depth project 1)) 0)
        (json-read-from-string
         (org-mcp-test--call-read task-one))))
      (should
       (equal
        (aref
         (alist-get 'children (org-mcp-test--read-depth project 2)) 0)
        (org-mcp-test--read-depth task-one 1)))
      (should
       (equal
        (aref
         (alist-get
          'children (org-mcp-test--read-depth project 1 fields))
         0)
        (org-mcp-test--read-depth task-one 0 fields))))))

(ert-deftest org-mcp-test-depth-expanded-child-carries-every-namespace ()
  "An expanded child carries the drawer and computed values too.
A node answers in three namespaces -- its fields, its Org drawer and
what the configured functions work out -- and `depth\=' expands nodes,
not field lists.  A child expanded under a call that asked for
properties therefore answers with them, and equals a read of its own
link asking for the same, which is what \"indistinguishable from a
direct read\" means once there is more than one namespace to carry.
The generation past the depth carries neither: a reference is an
address, and an address has no drawer."
  (let ((org-mcp-computed-fields
         (list (cons 'depth-probe (lambda () "seen")))))
    (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
        (list org-mcp-test--depth-project-id)
      (let* ((project (concat "id:" org-mcp-test--depth-project-id))
             (task-one
              (org-mcp-test--file-link test-file "*Task One"))
             (args
              '((fields . ["title" "level" "link" "children"])
                (properties . "all")
                (computed . "all")))
             (expanded
              (json-read-from-string
               (mcp-server-lib-ert-call-tool
                "org-node-read"
                `((link . ,project) (depth . 1) ,@args))))
             (child (aref (alist-get 'children expanded) 0)))
        ;; The node the call addressed answers in all three.
        (should (equal (alist-get 'computed expanded)
                       '((depth-probe . "seen"))))
        (should (alist-get 'properties expanded))
        ;; So does the child the call expanded, and it is the node a
        ;; read of that child asking the same returns.
        (should (equal (alist-get 'computed child)
                       '((depth-probe . "seen"))))
        (should
         (equal
          child
          (json-read-from-string
           (mcp-server-lib-ert-call-tool
            "org-node-read"
            `((link . ,task-one) (depth . 0) ,@args)))))
        ;; The generation past the depth is a reference and carries
        ;; neither namespace.
        (let ((reference (aref (alist-get 'children child) 0)))
          (should (alist-get 'link reference))
          (should-not (alist-get 'properties reference))
          (should-not (alist-get 'computed reference)))))))

(ert-deftest org-mcp-test-depth-the-walk-ends-in-an-address ()
  "The generation past the depth is a reference that resolves.
A walk that stopped is continued by reading the link it stopped at,
so no depth leaves a client holding a node it cannot reach."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
      (list org-mcp-test--depth-project-id)
    (let* ((link (concat "id:" org-mcp-test--depth-project-id))
           (task-one
            (aref
             (alist-get 'children (org-mcp-test--read-depth link 1))
             0))
           (step-a (aref (alist-get 'children task-one) 0)))
      (org-mcp-test--should-resolve-to
       (alist-get 'link step-a) "Step A"))))

(ert-deftest org-mcp-test-depth-one-field-list-renders-every-level ()
  "The fields a call asks for render every generation it expands.
`depth' says how many nodes come back and `fields' how much of each,
and the two do not interact.  The last generation is the one place
they meet: a reference carries the four fields a child carries,
whatever the call asked of its parent, and a field it has no value
for is left out there as everywhere."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
      (list org-mcp-test--depth-project-id)
    (let* ((node
            (org-mcp-test--read-depth
             (concat "file:" test-file) 2 ["title" "link" "children"]))
           (project (aref (alist-get 'children node) 0))
           (task-one (aref (alist-get 'children project) 0))
           (step-a (aref (alist-get 'children task-one) 0)))
      (should (equal (mapcar #'car node) '(title link children)))
      (should (equal (mapcar #'car project) '(title link children)))
      (should (equal (mapcar #'car task-one) '(title link children)))
      (should (equal (mapcar #'car step-a) '(title level link)))
      (should (equal (alist-get 'title step-a) "Step A")))))

(ert-deftest org-mcp-test-depth-without-children-changes-nothing ()
  "Depth expands the `children' field and asks for nothing else.
A call that did not ask for children has nothing to expand, so depth
costs it nothing and changes nothing: the two parameters compose
without interacting."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
      (list org-mcp-test--depth-project-id)
    (let ((link (concat "id:" org-mcp-test--depth-project-id)))
      (should
       (equal
        (org-mcp-test--read-depth link 3 ["title" "todo"])
        '((title . "Project") (todo . "TODO")))))))

(ert-deftest org-mcp-test-depth-not-a-count-is-refused ()
  "A depth that is not a whole number of generations is refused.
Reading it as none would expand nothing and say nothing about it,
leaving a client to conclude that the file is flat."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
      (list org-mcp-test--depth-project-id)
    (let ((link (concat "id:" org-mcp-test--depth-project-id)))
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (depth . "deep"))
       "depth must be a whole number of generations, not: \"deep\"")
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (depth . -1))
       "depth must be a whole number of generations, not: -1")
      (org-mcp-test--call-tool-refused
       "org-node-read" `((link . ,link) (depth . 1.5))
       "depth must be a whole number of generations, not: 1.5"))))

(ert-deftest org-mcp-test-depth-ceiling-refuses-and-never-trims ()
  "A walk past the ceiling is refused, not cut short.
The refusal names the node the walk stopped at, which is a link the
caller can read on its own, and the setting that raises the
ceiling.  It is a validation refusal and carries no marker, because
the call itself asked for too much and the recovery is to ask for
less.  A walk that fits returns the whole subtree, so the ceiling
never quietly takes anything out of one."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
      (list org-mcp-test--depth-project-id)
    (let* ((link (concat "id:" org-mcp-test--depth-project-id))
           (whole (org-mcp-test--read-depth link 2)))
      (let ((org-mcp-read-max-nodes 5))
        (should (equal (org-mcp-test--read-depth link 2) whole)))
      (let ((org-mcp-read-max-nodes 4))
        (org-mcp-test--call-tool-refused
         "org-node-read" `((link . ,link) (depth . 2))
         (concat
          "\\`Too many nodes: more than 4\\.  The walk stops at "
          (regexp-quote
           (org-mcp-test--file-link test-file "*Task Two"))
          ": ask for a shallower depth, or read that node on its "
          "own\\.  org-mcp-read-max-nodes sets the ceiling"))))))

(ert-deftest org-mcp-test-depth-ceiling-counts-every-node-returned ()
  "The ceiling counts the references that end a walk as well.
They are nodes the response carries and a client pays for, so a read
that asks for no depth at all is bounded by the same number: the
count is what comes back, not what was expanded."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
      (list org-mcp-test--depth-project-id)
    (let ((link (concat "id:" org-mcp-test--depth-project-id)))
      (let ((org-mcp-read-max-nodes 3))
        (should (org-mcp-test--read-depth link 0)))
      (let ((org-mcp-read-max-nodes 2))
        (org-mcp-test--call-tool-refused
         "org-node-read" `((link . ,link) (depth . 0))
         "\\`Too many nodes: more than 2\\.")))))

(ert-deftest org-mcp-test-depth-ceiling-bounds-one-node-not-a-match-list ()
  "The ceiling is what one node read returns, not what a call returns.
Every node a query matches is read on its own and gets the whole
ceiling to itself, so bounding how deep a match may be read does not
bound how many matches there are."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
      (list org-mcp-test--depth-project-id)
    (let ((org-mcp-read-max-nodes 1)
          (result (org-mcp-test--call-ql-query "(todo \"TODO\")")))
      (should (= (alist-get 'total result) 2)))))

(ert-deftest org-mcp-test-depth-resource-carries-references ()
  "The org:// resource serves the node alone, its children references.
A resource URI has nowhere to carry a depth, and it is picked from a
client's UI rather than by a model deciding how much to fetch, where
an expansion would be a surprise.  It serves what a read asking for
no depth serves, over a file that has three generations to expand."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-depth
      (list org-mcp-test--depth-project-id)
    (let* ((link (concat "id:" org-mcp-test--depth-project-id))
           (response
            (json-parse-string
             (mcp-server-lib-process-jsonrpc
              (mcp-server-lib-create-resources-read-request
               (concat "org://" link))
              mcp-server-lib-ert-server-id)
             :object-type 'alist))
           (contents
            (alist-get 'contents (alist-get 'result response))))
      (should-not (alist-get 'error response))
      (should
       (equal
        (alist-get 'text (aref contents 0))
        (mcp-server-lib-ert-call-tool
         "org-node-read" `((link . ,link) (depth . 0))))))))

;;; One definition of a title

(defconst org-mcp-test--content-cookie-title
  "* TODO [#A] Ship  v2 [1/3]\nBody.\n"
  "A heading whose title carries doubled whitespace and a cookie.")

(defconst org-mcp-test--regex-cookie-title-renamed
  "\\`\\* TODO \\[#A\\] Ship v3\nBody\\.\n\\'"
  "Regex matching the cookie-title file after the rename.")

(ert-deftest org-mcp-test-title-normalization-is-org-s ()
  "The title a node reports is normalized by Org's own predicate.
`org-link--normalize-string' removes statistics cookies and collapses
runs of whitespace, and `org-link-search' normalizes a heading that
way before matching a `::*title' link against it.  Pinning it here
makes a change in Org fail loudly rather than drift through every read
and every write precondition."
  (should
   (equal (org-link--normalize-string "Ship  v2 [1/3]") "Ship v2"))
  (should (equal (org-link--normalize-string "Done [50%]") "Done"))
  (should (equal (org-link--normalize-string " Ship\tv2 ") "Ship v2"))
  (should (equal (org-link--normalize-string "50% Done") "50% Done")))

(ert-deftest org-mcp-test-title-resolved-is-title-accepted ()
  "A title a link resolves by is a title a write accepts.
The heading is reached by a title link differing from the heading as
written in letter case, in spacing and by a statistics cookie.  The
node reports the title Org compares against, and sending a title back
as `before' renames the heading instead of being refused — the
refusal a byte-exact comparison produced for a call the link had just
resolved."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-cookie-title))
    (let ((link (org-mcp-test--file-link test-file "*ship v2")))
      (should
       (equal
        (alist-get
         'title (json-read-from-string (org-mcp-test--call-read link)))
        "Ship v2"))
      (org-mcp-test--call-rename-headline-and-check
       link "SHIP  V2" "Ship v3" test-file
       org-mcp-test--regex-cookie-title-renamed))))

(provide 'org-mcp-test)
;;; org-mcp-test.el ends here
