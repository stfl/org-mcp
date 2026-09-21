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

(defconst org-mcp-test--content-inherited-tags
  "#+FILETAGS: filetag
* Tagged Parent :ptag:
** TODO Tagged Child :ctag:
Child body."
  "A file tag, a tagged parent and a tagged child below it.")

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

(defconst org-mcp-test--expected-task-one-no-keyword-regex
  "\\`\\* Task One\nTask description\\.\n?\\'"
  "The whole file after Task One stops being a task.
The title and the body stay; only the keyword goes.")

(defconst org-mcp-test--expected-task-one-todo-regex
  (concat
   "\\`\\* TODO Task One"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching complete buffer with Task One in TODO state.")

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
   "\\*\\* TODO Child via ID +:work:\n"
   "- State \"TODO\" +from +\\[[^]]+\\]\n\\'")
  "Pattern for TODO added via the parent's `id:' link.
The keywords carry `!', so entering TODO is a transition Org records;
the entry names no state to have come from because the heading is
new.")

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
EXPECTED-FINAL is the decoded JSON value of `isFinal': t for a final
state and :json-false for one before the bar.  It is the wire value
rather than an elisp boolean because nil would also match a null,
and a null is what a client testing `isFinal === false' trips on.
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
    (test-file link properties asserted)
  "Call org-node-set-properties expecting an error, verify nothing changed.
TEST-FILE is the file that must stay unchanged on disk and in any
buffer visiting it.  LINK is the link to the headline.  PROPERTIES is the
alist sent as the `after' parameter and ASSERTED the one sent as
`before'."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((request
           (mcp-server-lib-create-tools-call-request
            "org-node-set-properties" nil
            `((link . ,link)
              (before . ,asserted)
              (after . ,properties))))
          (response
           (mcp-server-lib-process-jsonrpc-parsed
            request mcp-server-lib-ert-server-id))
          (result (mcp-server-lib-ert-process-tool-response response)))
     (error "Expected error but got success: %s" result)))
  (org-mcp-test--verify-no-modified-buffer test-file))

;; Helper functions for testing org-node-set-todo MCP tool

(defun org-mcp-test--call-update-todo-state
    (link new-state current-state &optional note files)
  "Call org-node-set-todo tool via JSON-RPC and return the result.
LINK is the link to the headline, NEW-STATE is the new TODO state to set.
CURRENT-STATE is the TODO state the headline is asserted to hold,
\"\" for a headline that has none.
NOTE, when provided, is a note to attach to the state transition.
FILES, when provided, is sent as the `files' parameter."
  (let* ((params
          `((link . ,link)
            (after . ,new-state)
            (before . ,current-state)
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
CURRENT-STATE is the TODO state the headline is asserted to hold.
NEW-STATE is the new TODO state to set."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((request
            (mcp-server-lib-create-tools-call-request
             "org-node-set-todo" 1
             `((link . ,link)
               (before . ,current-state)
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
    (test-file link old-body new-body expected-pattern expected-link)
  "Call org-node-set-content tool and check result structure and file content.
TEST-FILE is the path to the file to check.
LINK is the link to the node to edit.
OLD-BODY is what the body holds now, the value `before' asserts.
NEW-BODY is the replacement text.
EXPECTED-PATTERN is a regexp that the file content should match.
EXPECTED-LINK is the link to the edited heading the response carries."
  (let* ((params
          `((link . ,link)
            (before . ,old-body)
            (after . ,new-body)))
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

;; Helper functions for testing org-node-text MCP tool

(defun org-mcp-test--call-read-headline (link &optional files)
  "Call org-node-text tool via JSON-RPC and return the result.
LINK is the native Org link sent as the `link' parameter.
FILES, when provided, is sent as the `files' parameter."
  (let ((params `((link . ,link) ,@(when files `((files . ,files))))))
    (mcp-server-lib-ert-call-tool "org-node-text" params)))

;; Helpers for writing into a buffer the user is editing
;;
;; org-mcp writes through the buffer the user is editing and leaves
;; the file alone until the user saves it (docs/adr/0002).  So a test
;; that reads the file back cannot tell a write that did nothing from
;; one that did everything in the buffer, which is the damage that
;; reaches disk at the user's next save.  A test reaching into the
;; buffer itself cannot tell either, because it never asks the server
;; what it believes the content is: a call that wrote around the live
;; buffer would pass it while handing a client stale bytes.  So a
;; write into a dirty buffer is read back through the server.

(defconst org-mcp-test--user-edit "Typed by hand, not saved.\n"
  "An edit of the user's own, parting a buffer from its file.
It is unrelated to anything a call under test touches, so a call
that writes around the buffer, or that loses the edit, shows up.")

(defun org-mcp-test--content-with-user-edit (content)
  "Return CONTENT as a buffer holds it once the user has typed into it.
`org-mcp-test--with-dirty-buffer' types the edit on a line of its own
at the end, so a file whose last line has no newline gains one."
  (concat
   content
   (if (or (string-empty-p content) (string-suffix-p "\n" content))
       ""
     "\n")
   org-mcp-test--user-edit))

(defun org-mcp-test--served-text (file)
  "Return the text org-node-text serves for FILE, as a client reads it.
The read goes through the server, which answers from the buffer
visiting FILE when there is one."
  (org-mcp-test--call-read-headline (concat "file:" file)))

(defun org-mcp-test--verify-served-matches (file expected-pattern)
  "Verify the whole text the server serves for FILE matches EXPECTED-PATTERN."
  (should (string-match-p expected-pattern (org-mcp-test--served-text file))))

(defun org-mcp-test--served-regex (file-regex)
  "Return FILE-REGEX, a regexp over a whole Org file, as the server serves it.
The buffer `org-mcp-test--with-dirty-buffer' leaves holds the file
and the user's own line after it, so a regexp pinning the file image
pins what the server answers with once it is extended by that line."
  (concat
   (string-remove-suffix "\\'" file-regex)
   (regexp-quote org-mcp-test--user-edit)
   "\\'"))

(defmacro org-mcp-test--with-dirty-buffer (spec file &rest body)
  "Run BODY over FILE with a buffer visiting it the user has edited.
SPEC is (BUFFER-VAR ON-DISK-VAR).  BUFFER-VAR is bound to the buffer,
ON-DISK-VAR to the bytes FILE held before it was opened, which stay
its bytes for as long as nothing saves the buffer.  The buffer holds
`org-mcp-test--user-edit' and nothing else that FILE does not, so BODY
can pin what it gained.  It is killed afterwards, unmodified, so no
test leaves an unsaved buffer behind for the next one."
  (declare (indent 2) (debug t))
  (let ((buffer-var (nth 0 spec))
        (on-disk-var (nth 1 spec)))
    `(let ((,buffer-var nil))
       (unwind-protect
           (let ((,on-disk-var (org-mcp-test--read-file ,file)))
             (setq ,buffer-var (find-file-noselect ,file))
             (with-current-buffer ,buffer-var
               (save-restriction
                 (widen)
                 (goto-char (point-max))
                 (unless (bolp)
                   (insert "\n"))
                 (insert org-mcp-test--user-edit))
               (should (buffer-modified-p)))
             ,@body)
         (when ,buffer-var
           (with-current-buffer ,buffer-var
             (set-buffer-modified-p nil))
           (kill-buffer ,buffer-var))))))

(defun org-mcp-test--assert-unsaved (result file on-disk buffer)
  "Assert RESULT says the write has not reached disk, and it has not.
FILE still holds ON-DISK byte for byte, and BUFFER is still the
user's to save, so the change is waiting in it."
  (should (eq (alist-get 'saved result) :json-false))
  (should (string= (org-mcp-test--read-file file) on-disk))
  (should (buffer-modified-p buffer)))

(defun org-mcp-test--assert-content-unmoved (buffer file on-disk)
  "Assert a refused write moved nothing, in FILE or in BUFFER.
The server serves ON-DISK and the user's edit and nothing else, FILE
holds ON-DISK, and BUFFER is still the user's to save."
  (should
   (string=
    (org-mcp-test--served-text file)
    (org-mcp-test--content-with-user-edit on-disk)))
  (should (string= (org-mcp-test--read-file file) on-disk))
  (should (buffer-modified-p buffer)))

(defun org-mcp-test--write-through-dirty-buffer
    (content tool params fields served)
  "Call TOOL over a buffer of CONTENT the user has unsaved edits in.
CONTENT is what the file holds before the call.  PARAMS is a function
of that file, called once the buffer is dirty, returning the tool\='s
parameters; an endpoint asserting a digest reads it there, from the
buffer, as a client planning the call does.  FIELDS is an alist of
response fields the call is to answer with, beside the `success' and
`saved' every write answers.  SERVED is a regexp the whole text the
server serves for the file matches afterwards.

Every write endpoint is put through this one shape: the response is
what a client acts on, the served text is what the server believes
the file holds, and the bytes on disk are what a save would change.
The three together are what a buffer-blind write cannot fake."
  (org-mcp-test--with-temp-org-files ((test-file content))
    (org-mcp-test--with-dirty-buffer (buffer on-disk) test-file
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               tool (funcall params test-file)))))
        (should (eq (alist-get 'success result) t))
        (pcase-dolist (`(,field . ,value) fields)
          (should (equal (alist-get field result) value)))
        (org-mcp-test--verify-served-matches test-file served)
        (org-mcp-test--assert-unsaved result test-file on-disk buffer)))))

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

(defun org-mcp-test--call-clock-out (link &optional end-time note)
  "Call org-clock-out tool via JSON-RPC and return the parsed result.
LINK names the heading the running clock is on.  END-TIME is an
optional ISO 8601 end timestamp.  NOTE, when non-nil, is sent as the
`note' parameter, the prose to record against the closed clock; the
blanks a client can spell -- \"\", a string of whitespace, false and
[] -- are all non-nil here, so each is sent as the call wrote it and
only nil leaves the parameter out."
  (let* ((params
          (append
           `((link . ,link))
           (when end-time `((end_time . ,end-time)))
           (when note `((note . ,note)))))
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
     (aref semantics 0) "TODO" :json-false "sequence")
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
     (aref semantics 0) "TODO" :json-false "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "NEXT" :json-false "sequence")
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
     (aref semantics 0) "Fred" :json-false "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "Sara" :json-false "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 2) "Lucy" :json-false "type")
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
     (aref semantics 0) "TODO" :json-false "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "DONE" t "sequence")
    ;; Semantics from second sequence
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 2) "BUG" :json-false "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 3) "FEATURE" :json-false "type")
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
     (aref semantics 0) "TODO" :json-false "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "NEXT" :json-false "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-type-no-separator ()
  "Test org-config-todo with type keywords and no separator."
  (org-mcp-test--with-get-todo-config-result '((type "BUG" "FEATURE" "ENHANCEMENT"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "type" ["BUG" "FEATURE" "|" "ENHANCEMENT"])
    (should (= (length semantics) 3))
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "BUG" :json-false "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "FEATURE" :json-false "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 2) "ENHANCEMENT" t "type")))

(ert-deftest org-mcp-test-todo-config-sends-false-never-null ()
  "A keyword before the bar reports `isFinal\=' as false, not null.
The published description calls `isFinal\=' a boolean, and
`json-encode\=' writes an elisp nil as null, so the value is spelled
:json-false at the source.  A client asking whether a keyword is
done tests the wire text against false, which is what this pins."
  (let ((org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE"))))
    (org-mcp-test--with-enabled
     (let ((text (mcp-server-lib-ert-call-tool "org-config-todo" nil)))
       (should (string-match-p "\"isFinal\":false" text))
       (should-not (string-match-p ":null" text))))))

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

(defun org-mcp-test--refusal-message (tool-name params)
  "Call TOOL-NAME with PARAMS, assert it is refused, return the message.
The refusal arrives as a tool error or, from the resource-style
validation, as a JSON-RPC error; the message reads the same either
way, which is what a client acts on."
  (let* ((response
          (mcp-server-lib-process-jsonrpc-parsed
           (mcp-server-lib-create-tools-call-request tool-name 1 params)
           mcp-server-lib-ert-server-id))
         (result (alist-get 'result response))
         (message
          (if (eq (alist-get 'isError result) t)
              (alist-get 'text (aref (alist-get 'content result) 0))
            (alist-get 'message (alist-get 'error response)))))
    (should (stringp message))
    message))

(defun org-mcp-test--call-tool-refused
    (tool-name params expected-message &optional file)
  "Call TOOL-NAME with PARAMS and assert it is refused.
The refusal\='s message must match the regexp EXPECTED-MESSAGE.  When
FILE is non-nil, it must be byte-for-byte unchanged afterwards.

A buffer holding the user\='s unsaved edits must be unchanged too, and
the file alone cannot say so: a refusal that damaged such a buffer
leaves the file exactly as it found it, and the damage reaches disk
at the user\='s next save.  So while a modified buffer visits FILE,
what the server serves for it is pinned across the call as well.  An
unmodified buffer holds what FILE holds, which the bytes already say."
  (let* ((before (and file (org-mcp-test--read-file-raw file)))
         (served-before
          (and file
               (let ((visiting (find-buffer-visiting file)))
                 (and visiting (buffer-modified-p visiting)))
               (org-mcp-test--served-text file))))
    (should
     (string-match-p
      expected-message
      (org-mcp-test--refusal-message tool-name params)))
    (when file
      (should (string= (org-mcp-test--read-file-raw file) before)))
    (when served-before
      (should (string= (org-mcp-test--served-text file) served-before)))))

(defun org-mcp-test--call-tool-leaving-file (tool-name params file)
  "Call TOOL-NAME with PARAMS, assert FILE is unchanged, return the result.
A removal of a field a headline does not carry has nothing to take
away, so the call is accepted and the file is left byte for byte as
it was.  The result is the parsed JSON response, which says the same
thing in the words a client reads."
  (let* ((before (org-mcp-test--read-file-raw file))
         (result
          (json-read-from-string
           (mcp-server-lib-ert-call-tool tool-name params))))
    (should (string= (org-mcp-test--read-file-raw file) before))
    result))

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
             `((link . ,heading-link)
               (before . "TODO")
               (after . "DONE"))
             "names no local file by its full path")
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
           `((link . ,(concat "file:" link "::*Task"))
             (before . "TODO")
             (after . "DONE"))
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
    "org-node-add-tags"
    "org-node-archive"
    "org-node-create"
    "org-node-delete"
    "org-node-read"
    "org-node-refile"
    "org-node-remove-tags"
    "org-node-set-content"
    "org-node-set-deadline"
    "org-node-set-priority"
    "org-node-set-properties"
    "org-node-set-scheduled"
    "org-node-set-tags"
    "org-node-set-title"
    "org-node-set-todo"
    "org-node-text"
    "org-query")
  "Every tool id org-mcp registers regardless of configuration, sorted.")

(defun org-mcp-test--registered-tools ()
  "Return the tools of the tools/list response."
  (append
   (alist-get
    'tools
    (mcp-server-lib-ert-get-success-result
     "tools/list" (mcp-server-lib-create-tools-list-request)))
   nil))

(defun org-mcp-test--registered-tool (id)
  "Return the tools/list entry for the tool ID."
  (cl-find
   id (org-mcp-test--registered-tools)
   :key (lambda (tool) (alist-get 'name tool))
   :test #'string=))

(defun org-mcp-test--registered-tool-description (id)
  "Return the description tools/list gives for the tool ID."
  (alist-get 'description (org-mcp-test--registered-tool id)))

(defun org-mcp-test--registered-tool-required (id)
  "Return the required parameter names tools/list publishes for tool ID."
  (append
   (alist-get
    'required
    (alist-get 'inputSchema (org-mcp-test--registered-tool id)))
   nil))

(defun org-mcp-test--registered-tool-properties (id)
  "Return the parameter names tools/list publishes for the tool ID.
A JSON object decodes with symbols for its keys, and a schema names
its parameters as strings, so the keys come back as the client reads
them."
  (mapcar
   (lambda (property) (symbol-name (car property)))
   (alist-get
    'properties
    (alist-get 'inputSchema (org-mcp-test--registered-tool id)))))

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

(ert-deftest org-mcp-test-guarded-writes-publish-before-as-required ()
  "Every guarded write tool publishes `before\=' among its required parameters.
A client discovers the guard from the schema and never from the
handler, so a parameter published as optional is a guard that is
off, whatever the handler then does with it."
  (org-mcp-test--with-enabled
    (dolist (id
             '("org-node-set-content"
               "org-node-set-deadline"
               "org-node-set-priority"
               "org-node-set-properties"
               "org-node-set-scheduled"
               "org-node-set-title"
               "org-node-set-todo"))
      (let ((required (org-mcp-test--registered-tool-required id)))
        (should (member "link" required))
        (should (member "before" required))
        (should (member "after" required))
        (should-not (member "files" required))))))

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

(ert-deftest org-mcp-test-set-todo-null-after-takes-the-keyword-off ()
  "A null `after\=' leaves the headline with no TODO keyword.
The heading stops being a task and keeps its title, and the response
reports the keyword destroyed under `before\='.  Its `after\=' is \"\",
the state the field is now in; a read of the headline carries no
`todo\=' key at all, which is why \"\" is no keyword to ask for."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let* ((org-todo-keywords
              '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
             (link (org-mcp-test--file-link test-file "*Task One"))
             (result
              (json-read-from-string
               (mcp-server-lib-ert-call-tool
                "org-node-set-todo"
                `((link . ,link) (before . "TODO") (after))))))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'before result) "TODO"))
        (should (equal (alist-get 'after result) ""))
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--expected-task-one-no-keyword-regex)))))

(ert-deftest org-mcp-test-set-todo-null-after-refuses-a-stale-before ()
  "A keyword the headline does not hold refuses taking it off."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        (org-mcp-test--call-tool-refused
         "org-node-set-todo"
         `((link . ,(org-mcp-test--file-link test-file "*Task One"))
           (before . "IN-PROGRESS")
           (after))
         "\\`conflict: State mismatch: expected 'IN-PROGRESS', \
found 'TODO'\\'"
         test-file)))))

(ert-deftest org-mcp-test-set-todo-empty-after-is-no-keyword ()
  "\"\" is no TODO keyword and is refused as one; false is left out.
Null is the one spelling that takes a keyword off, because null is
JSON's word for no value.  An empty string is a value, and this
field has none — no state named in `org-todo-keywords\=' is \"\", and
a read of a headline carrying no keyword reports no state at all
rather than an empty one.  So \"\" reaches the field's own check and
is refused there, naming the states there are and the null that
asks for none.  The message can say that only because a blank
`todo\=' means the same thing on `org-node-create\=': while the two
callers of this check disagreed about null, it could name it for
neither."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
            (link (org-mcp-test--file-link test-file "*Task One")))
        (org-mcp-test--call-tool-refused
         "org-node-set-todo"
         `((link . ,link) (before . "TODO") (after . ""))
         "\\`Invalid TODO state: '' - valid states: \
TODO, IN-PROGRESS, DONE, or null for no keyword\\'"
         test-file)
        (dolist (blank '(:json-false []))
          (org-mcp-test--call-tool-refused
           "org-node-set-todo"
           `((link . ,link) (before . "TODO") (after . ,blank))
           "\\`Missing required parameter: after\\'"
           test-file))))))

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

(defconst org-mcp-test--dirty-task-one-in-progress-regex
  (concat
   "\\`\\* IN-PROGRESS Task One\n"
   "Task description\\.\n"
   "\\* TODO Task Two\n"
   "Another task description\\.\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "Regex matching the whole file the server serves after Task One moves on.
The file is served from the buffer the user is editing, so it carries
that user's own unsaved edit as well as the new TODO state.")

(ert-deftest org-mcp-test-update-todo-state-with-modified-buffer ()
  "A TODO state lands in the buffer the user is editing, not around it.
The buffer held the user's own unsaved edit before the call, so the
change is the user's to save: the response says so, the file on disk
keeps the state it had, and the server answers from the buffer with
the new state and the user's edit both in it."
  (let ((org-todo-keywords
         '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
    (org-mcp-test--write-through-dirty-buffer
     org-mcp-test--content-two-todo-tasks
     "org-node-set-todo"
     (lambda (file)
       `((link . ,(org-mcp-test--file-link file "*Task One"))
         (before . "TODO")
         (after . "IN-PROGRESS")))
     '((before . "TODO") (after . "IN-PROGRESS"))
     org-mcp-test--dirty-task-one-in-progress-regex)))

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

(ert-deftest org-mcp-test-update-todo-state-matching-before ()
  "A before the headline holds is accepted and the change goes through.
The keywords log their transitions, so the assertion is checked on
the same path a logging change takes."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "|" "DONE(d!)"))))
        (let ((link
               (org-mcp-test--file-link test-file "*Task One")))
          (let ((result
                 (org-mcp-test--call-update-todo-state
                  link "IN-PROGRESS" "TODO")))
            (should (= (length result) 5))
            (should (equal (alist-get 'success result) t))
            (should (eq (alist-get 'saved result) t))
            (should (equal (alist-get 'before result) "TODO"))
            (should (equal (alist-get 'after result) "IN-PROGRESS"))
            (should
             (equal (alist-get 'link result)
                    (org-mcp-test--file-link test-file "*Task One")))
            (org-mcp-test--verify-file-matches
             test-file org-mcp-test--expected-task-one-in-progress-regex)))))))

(ert-deftest org-mcp-test-update-todo-state-refuses-an-omitted-before ()
  "A call that sends no before is refused and writes nothing.
The precondition is the parameter itself, so a client that leaves it
out has sent a malformed call rather than asked for an unguarded
write."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        (org-mcp-test--call-tool-refused
         "org-node-set-todo"
         `((link . ,(org-mcp-test--file-link test-file "*Task One"))
           (after . "IN-PROGRESS"))
         "\\`Missing required parameter: before\\'"
         test-file)))))

(ert-deftest org-mcp-test-update-todo-state-blank-before-asserts-no-state ()
  "An empty before asserts the headline carries no TODO keyword.
A required parameter never means \"not sent\", so the empty string is
free to name the state a headline without a keyword is in."
  (let ((test-content "* Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO(t!)" "|" "DONE(d!)"))))
        (let ((link
               (org-mcp-test--file-link test-file "*Task One")))
          (let ((result
                 (org-mcp-test--call-update-todo-state link "TODO" "")))
            (should (= (length result) 5))
            (should (equal (alist-get 'success result) t))
            (should (eq (alist-get 'saved result) t))
            (should (equal (alist-get 'before result) ""))
            (should (equal (alist-get 'after result) "TODO"))
            (should
             (equal (alist-get 'link result)
                    (org-mcp-test--file-link test-file "*Task One")))
            (org-mcp-test--verify-file-matches
             test-file org-mcp-test--expected-task-one-todo-regex)))))))

(ert-deftest org-mcp-test-update-todo-state-blank-before-on-a-keyword ()
  "An empty before is refused on a headline that carries a keyword.
The empty string asserts there is none, so a headline that has one
is a conflict like any other mismatch."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "|" "DONE"))))
        (org-mcp-test--call-tool-refused
         "org-node-set-todo"
         `((link . ,(org-mcp-test--file-link test-file "*Task One"))
           (before . "")
           (after . "DONE"))
         "\\`conflict: State mismatch: expected '', found 'TODO'\\'"
         test-file)))))

(ert-deftest org-mcp-test-update-todo-state-keyword-before-on-no-state ()
  "A keyword in before is refused on a headline that carries none.
The refusal names what it found, which for a headline without a
keyword reads `(no state)'; the value that asserts it is \"\"."
  (let ((test-content "* Task One\nTask description."))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "|" "DONE"))))
        (org-mcp-test--call-tool-refused
         "org-node-set-todo"
         `((link . ,(org-mcp-test--file-link test-file "*Task One"))
           (before . "TODO")
           (after . "DONE"))
         "\\`conflict: State mismatch: expected 'TODO', found '(no state)'\\'"
         test-file)))))

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

(defconst org-mcp-test--content-todo-with-two-props
  "* TODO Task with Two Properties
:PROPERTIES:
:EFFORT:   1:00
:OWNER:    ada
:END:
Some body."
  "TODO task with two properties this server may write.")

(defconst org-mcp-test--content-todo-with-scheduled
  "* TODO Scheduled Task
SCHEDULED: <2026-03-01 Sun>
Task body."
  "TODO task with a SCHEDULED timestamp.")

(defconst org-mcp-test--content-repeating-scheduled
  "* TODO Repeating Task
SCHEDULED: <2026-06-20 Sat +1w -3d>
Task body."
  "TODO task whose SCHEDULED carries a repeater and a delay.")

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
  "TODO task with a child heading, for writes bounded by that child.")

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

(defconst org-mcp-test--pattern-set-properties-one-of-two
  (concat
   "\\`\\* TODO Task with Two Properties\n"
   " *:PROPERTIES:\n"
   " *:EFFORT: +3:00\n"
   " *:OWNER: +ada\n"
   " *:END:\n"
   "Some body\\.\n?\\'")
  "Pattern after EFFORT alone is written and OWNER left alone.")

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

(defconst org-mcp-test--pattern-repeating-scheduled-moved
  (concat
   "\\`\\* TODO Repeating Task\n"
   "SCHEDULED: <2026-06-27 [^ ]+ \\+1w -3d>\n"
   "Task body\\.\n?\\'")
  "Pattern after the repeating SCHEDULED is moved a week on.
Org carries the repeater and the delay to the new date.")

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

(defconst org-mcp-test--pattern-bare-todo
  (concat "\\`\\* TODO Simple Task\n" "Task body text\\.\n?\\'")
  "The bare task as it stands, nothing added and nothing taken away.")

(defconst org-mcp-test--content-todo-with-empty-property
  "* TODO Simple Task
:PROPERTIES:
:EMPTY:
:OWNER:    ada
:END:
Task body text."
  "A task whose drawer carries a line with nothing after the name.
It reads back as \"\" exactly as a property the drawer does not carry
asserts, and the two are told apart only by the file.")

(defconst org-mcp-test--pattern-blank-line-kept
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:EMPTY:\n"
   " *:OWNER: +grace\n"
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern once OWNER is rewritten and the blank EMPTY line stands.")

(defconst org-mcp-test--pattern-blank-line-intact
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:EMPTY:\n"
   " *:OWNER: +ada\n"
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern once a call has written the blank line back as it found it.")

(defconst org-mcp-test--pattern-blank-line-written
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:BLANK:\n"
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern once a line carrying no value is written into a bare task.")

(defconst org-mcp-test--pattern-empty-property-removed
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:OWNER: +ada\n"
   " *:END:\n"
   "Task body text\\.\n?\\'")
  "Pattern after the empty line goes and the drawer keeps OWNER.")

(defconst org-mcp-test--pattern-remove-properties-both
  (concat
   "\\`\\* TODO Task with Two Properties\n" "Some body\\.\n?\\'")
  "Pattern after both properties are removed and the drawer with them.")

(defconst org-mcp-test--pattern-body-line-added
  (concat
   "\\`\\* TODO Simple Task\n"
   "Task body text\\.\n"
   "Appended line\\.\n?\\'")
  "Pattern once the body holds the line that was added to it.")

(defconst org-mcp-test--pattern-body-written-into-empty
  (concat
   "\\`\\* TODO Empty Body Task\n"
   "New body content\\.\n?\\'")
  "Pattern once an entry that had no body holds one.")

(defconst org-mcp-test--pattern-body-added-before-children
  (concat
   "\\`\\* TODO Parent Task\n"
   "Parent body\\.\n"
   "Appended text\\.\n"
   "\\*\\* Child One\n"
   "Child body\\.\n?\\'")
  "Pattern once the added text stands between the body and the children.")

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
                           (before . "TODO")
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
                           (before . "TODO")
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
             (result
              (org-mcp-test--call-update-todo-state
               link "DONE" "TODO")))
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
             (result
              (org-mcp-test--call-update-todo-state
               link "DONE" "TODO")))
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

(defconst org-mcp-test--regex-todo-without-body "\\`\\* TODO Task\n\\'"
  "The whole file after a create that writes a heading and no body.")

(ert-deftest org-mcp-test-node-create-publishes-its-required-parameters ()
  "org-node-create asks for a title and a place, and nothing else.
The schema is where a client learns what a call has to carry, and
the page tells a reader the same, so the two are pinned together
here.  A creation destroys nothing, so nothing is guarded by
insisting the caller name a body, a tag, a property or a state it
does not want; a node that names no state is a heading rather than
a task."
  (org-mcp-test--with-enabled
    (should
     (equal (org-mcp-test--registered-tool-required "org-node-create")
            '("title" "parent")))
    (should
     (equal
      (sort
       (copy-sequence
        (org-mcp-test--registered-tool-properties "org-node-create"))
       #'string<)
      '("content"
        "files"
        "parent"
        "previous_sibling"
        "properties"
        "tags"
        "title"
        "todo")))))

(defconst org-mcp-test--regex-heading-without-keyword "\\`\\* Task\n\\'"
  "The whole file after a create that names no TODO state.")

(ert-deftest org-mcp-test-node-create-makes-a-heading-without-a-keyword ()
  "A create that names no state writes a heading that is not a task.
`todo\=' is optional, so leaving it out and every spelling a client
fills an unused parameter with mean one thing: this node is not a
task.  A read of such a headline carries no `todo\=' key, so the
response carries none either, and the file holds `* Task\=' with no
keyword each time."
  (dolist (params
           '(()
             ((todo . nil))
             ((todo . ""))
             ((todo . :json-false))
             ((todo . []))))
    (org-mcp-test--with-add-todo-setup test-file
        org-mcp-test--content-empty
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-create"
               `((title . "Task")
                 (parent . ,(concat "file:" test-file))
                 ,@params)))))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'title result) "Task"))
        (should
         (equal (alist-get 'link result)
                (org-mcp-test--file-link test-file "*Task"))))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-heading-without-keyword))))

(ert-deftest org-mcp-test-node-create-still-takes-a-keyword ()
  "A named state still makes a task, and a state that is none is refused.
The parameter going optional does not widen what a non-blank value
may be: it is a keyword from `org-todo-keywords\=' or the call is
refused, and the file is left as it was."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-create"
             `((title . "Task")
               (todo . "IN-PROGRESS")
               (parent . ,(concat "file:" test-file)))))))
      (should (equal (alist-get 'success result) t)))
    (org-mcp-test--verify-file-matches
     test-file "\\`\\* IN-PROGRESS Task\n\\'"))
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (org-mcp-test--call-tool-refused
     "org-node-create"
     `((title . "Task")
       (todo . "NOTAKEYWORD")
       (parent . ,(concat "file:" test-file)))
     (concat "\\`Invalid TODO state: 'NOTAKEYWORD' - valid states: "
             "TODO, IN-PROGRESS, DONE, or null for no keyword\\'")
     test-file)))

(ert-deftest org-mcp-test-node-create-writes-no-body-for-every-blank ()
  "A create that names no body writes the heading and nothing under it.
`content\=' is optional, so leaving it out and every spelling a client
fills an unused parameter with mean one thing: a new heading has no
body until something is written to it.  The file holds the heading
alone each time, and the response reports the same node."
  (dolist (params
           '(()
             ((content . nil))
             ((content . ""))
             ((content . :json-false))
             ((content . []))))
    (org-mcp-test--with-add-todo-setup test-file
        org-mcp-test--content-empty
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-create"
               `((title . "Task")
                 (todo . "TODO")
                 (parent . ,(concat "file:" test-file))
                 ,@params)))))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'title result) "Task"))
        (should
         (equal (alist-get 'link result)
                (org-mcp-test--file-link test-file "*Task"))))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-todo-without-body))))

(ert-deftest org-mcp-test-node-create-refuses-a-content-that-is-not-a-string ()
  "A create is refused when `content\=' is not text, and writes nothing.
The body is inserted and checked as text, so a number or an array
would reach that as a wrong type and cross the MCP boundary as an
internal error, which names no parameter.  The refusal names it, and
the file is left as it was."
  (dolist (case '((42 . "42") (["a"] . "an array")))
    (org-mcp-test--with-add-todo-setup test-file
        org-mcp-test--content-empty
      (org-mcp-test--call-tool-refused
       "org-node-create"
       `((title . "Task")
         (todo . "TODO")
         (content . ,(car case))
         (parent . ,(concat "file:" test-file)))
       (concat "\\`content must be a string: " (cdr case) "\\'")
       test-file))))

(ert-deftest org-mcp-test-node-create-refuses-a-blank-title ()
  "A create whose `title\=' is blank is refused as a parameter left out.
An empty `title\=' is not blank -- it is text, and the title validator
refuses it in its own words -- so the two are told apart, and neither
crosses the MCP boundary as an internal error naming no parameter."
  (dolist (blank '(nil :json-false []))
    (org-mcp-test--with-add-todo-setup test-file
        org-mcp-test--content-empty
      (org-mcp-test--call-tool-refused
       "org-node-create"
       `((title . ,blank)
         (todo . "TODO")
         (parent . ,(concat "file:" test-file)))
       "\\`Missing required parameter: title\\'"
       test-file)))
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (org-mcp-test--call-tool-refused
     "org-node-create"
     `((title . "")
       (todo . "TODO")
       (parent . ,(concat "file:" test-file)))
     "\\`Headline title cannot be empty or contain only whitespace\\'"
     test-file)))

(defconst org-mcp-test--content-create-examples
  "* Projects\n** Draft the plan\n* Plan the kickoff\n"
  "A project with one child, and a second top-level heading after it.
The before image for the two examples docs/writing.org gives for
org-node-create, each of which names a sibling and no body.")

(defconst org-mcp-test--regex-create-examples
  (concat
   "\\`\\* Projects\n"
   "\\*\\* Draft the plan\n"
   "\\*\\* TODO Review the budget\n"
   "\\* TODO Plan the offsite\n"
   "\\* Plan the kickoff\n"
   "\\'")
  "The whole file after both documented examples have run.
The child lands after its sibling, and the top-level heading after
the sibling's whole subtree.")

(ert-deftest org-mcp-test-node-create-runs-the-documented-examples ()
  "The two examples on the page name no body, and each creates a node.
Each is a call a reader copies, so each runs here as it is written:
a child after a sibling, then a top-level heading after a top-level
heading and its subtree."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-create-examples
    (dolist (case
             (list
              (list "Review the budget"
                    (org-mcp-test--file-link test-file "*Projects")
                    (org-mcp-test--file-link
                     test-file "*Draft the plan"))
              (list "Plan the offsite"
                    (concat "file:" test-file)
                    (org-mcp-test--file-link test-file "*Projects"))))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-create"
               `((title . ,(nth 0 case))
                 (todo . "TODO")
                 (parent . ,(nth 1 case))
                 (previous_sibling . ,(nth 2 case)))))))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'title result) (nth 0 case)))
        (should
         (equal (alist-get 'link result)
                (org-mcp-test--file-link
                 test-file (concat "*" (nth 0 case)))))))
    (org-mcp-test--verify-file-matches
     test-file org-mcp-test--regex-create-examples)))

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
     '((ITEMS . ["a" "b"])) '((ITEMS)))))

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

;;; A title is the title, not a line of Org grammar

;; A title is written raw into the headline line, and that line has a
;; grammar: a trailing `:word:' is tags, a leading COMMENT comments the
;; heading out of export and the agenda, a leading keyword is the TODO
;; state and a leading `[#A]' is the priority.  A title claimed by any
;; of them is not the title the call named, and the response would
;; report success.  So the line is parsed before it is written and a
;; title Org reads as something else is refused.

(defconst org-mcp-test--titles-org-would-claim
  '(("Buy milk :fresh:" . "would become tags")
    ("Buy milk :a:b:" . "would become tags")
    ("Buy milk :fresh: " . "would become tags")
    ("COMMENT the code" . "would comment")
    ("COMMENT" . "would comment")
    ("TODO buy milk" . "would become the TODO keyword")
    ("[#A] buy milk" . "would become the priority"))
  "Titles Org's headline grammar claims, each with what it claims.
The cdr is the clause the refusal carries, which is what tells a
client which part of the title it has to spell another way.")

(defconst org-mcp-test--titles-org-leaves-alone
  '("10:30 standup"
    "Meeting: notes"
    "Buy milk:fresh:"
    "Buy milk :not a tag:"
    "COMMENTARY on it"
    "the COMMENT code"
    "Buy milk :fresh:x")
  "Titles carrying a colon, a bracket or the word COMMENT and no grammar.
Each writes, and reads back exactly as it was sent.")

(ert-deftest org-mcp-test-set-title-refuses-a-title-org-would-claim ()
  "A title Org reads as something else is refused, and nothing is written.
The refusal names what Org would have made of it, because that is the
part a client cannot see from its own call."
  (org-mcp-test--with-temp-org-files
      ((test-file "* TODO Original\nBody.\n"))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (link (org-mcp-test--file-link test-file "*Original")))
      (pcase-dolist (`(,title . ,clause)
                     org-mcp-test--titles-org-would-claim)
        (org-mcp-test--call-tool-refused
         "org-node-set-title"
         `((link . ,link) (before . "Original") (after . ,title))
         (concat "\\`Not a title: '" (regexp-quote title)
                 "'\\.  It " (regexp-quote clause))
         test-file)))))

(ert-deftest org-mcp-test-node-create-refuses-a-title-org-would-claim ()
  "org-node-create refuses the same titles through the same validator."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (pcase-dolist (`(,title . ,clause)
                   org-mcp-test--titles-org-would-claim)
      (org-mcp-test--call-tool-refused
       "org-node-create"
       `((title . ,title)
         (todo . "TODO")
         (parent . ,(concat "file:" test-file)))
       (concat "\\`Not a title: '" (regexp-quote title)
               "'\\.  It " (regexp-quote clause))
       test-file))))

(ert-deftest org-mcp-test-set-title-takes-a-colon-that-is-no-tag ()
  "A colon, a bracket or the word COMMENT inside a title is just text.
The check is what Org makes of the whole headline line, not a rule
about punctuation, so a title is refused exactly when Org would read
it as something else.  Each of these is read back through the server
as the title that was sent."
  (dolist (title org-mcp-test--titles-org-leaves-alone)
    (org-mcp-test--with-temp-org-files
        ((test-file "* TODO Original\nBody.\n"))
      (let* ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
             (link (org-mcp-test--file-link test-file "*Original"))
             (result
              (json-read-from-string
               (mcp-server-lib-ert-call-tool
                "org-node-set-title"
                `((link . ,link)
                  (before . "Original")
                  (after . ,title))))))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'before result) "Original"))
        (should (equal (alist-get 'after result) title))
        ;; What the server serves for the node is the title sent, so
        ;; the round trip a client makes comes back whole.
        (should
         (equal
          (alist-get
           'title
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-read"
             `((link
                .
                ,(org-mcp-test--file-link
                  test-file (concat "*" title)))))))
          title))))))

(ert-deftest org-mcp-test-node-create-takes-a-colon-that-is-no-tag ()
  "org-node-create writes the same titles and reads them back whole."
  (dolist (title org-mcp-test--titles-org-leaves-alone)
    (org-mcp-test--with-add-todo-setup test-file
        org-mcp-test--content-empty
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-create"
               `((title . ,title)
                 (todo . "TODO")
                 (parent . ,(concat "file:" test-file)))))))
        (should (equal (alist-get 'success result) t))
        (should
         (equal
          (alist-get
           'title
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-read"
             `((link . ,(alist-get 'link result))))))
          title))))))

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
     "\\`conflict: Text appears 3 times (must be unique)\\'"
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
       (org-mcp-test--file-link test-file "*Third Child #3")))))

(ert-deftest org-mcp-test-edit-body-empty-old-non-empty-body ()
  "An empty before is refused when the node already has content.
The refusal names the assertion the call made, what the tool found
instead and what to send in its place, because a client has only the
message to act on.  It is a conflict: the client read a body it
believed was empty, and the node has one."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (should
     (string=
      "conflict: An empty before asserts the node has no content, \
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

(defconst org-mcp-test--regex-block-body-escaped-written
  (concat
   "\\`\\* Task\nTask body\\.\n- item\n  #\\+begin_example\n"
   "  ,\\*\\* x\n  #\\+end_example\n\\'")
  "Regex matching the target once its body holds an escaped block.")

(ert-deftest org-mcp-test-body-refuses-star-line-in-indented-block ()
  "A body whose indented block holds a star line is refused.
Org parses the star line as a heading, which breaks the block, so the
body would add a heading.  The refusal holds for a line deeper than
the heading the body goes under, in org-node-create and in both forms
of org-node-set-content's before, and leaves the file unchanged."
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
                  (before . "Task body.")
                  (after . "  #+begin_example\n** z\n  #+end_example"))
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
  "A body whose indented block escapes its star line is written as is.
The escape is what keeps Org from reading the line as a heading, so
the body goes in with the comma still on it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-block-body-target))
    (let ((link (org-mcp-test--file-link test-file "*Task")))
      (org-mcp-test--call-edit-body-and-check
       test-file link "Task body."
       (concat
        "Task body.\n"
        "- item\n  #+begin_example\n  ,** x\n  #+end_example")
       org-mcp-test--regex-block-body-escaped-written link))))

;;; The two forms of org-node-set-content's before

;; What a replacement overwrites follows the form of what it asserts: a
;; substring names a part of the body and replaces that part, and a
;; content_digest names the region entire and replaces the region.  The
;; prefix is the whole of the discrimination, so these tests go through
;; the tool the way a client does, and each pins the response as well as
;; the file: a wrong before, after or saved reaches a client through the
;; response and never through the bytes.

(defconst org-mcp-test--set-content-id
  "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  "ID of Target in `org-mcp-test--content-set-content'.")

(defconst org-mcp-test--content-set-content
  (concat
   "* TODO [#B] Target :work:\n"
   "SCHEDULED: <2026-04-02 Thu> DEADLINE: <2026-04-09 Thu>\n"
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--set-content-id "\n"
   ":Effort:   2:00\n"
   ":END:\n"
   "First line of the body.\n"
   "Second line of the body.\n"
   "** Child\n"
   "* Sibling\n")
  "A node with a body to rewrite and every field a digest must not guard.
Target carries a TODO state, a priority, a tag, both planning
timestamps and a property, so a call asserting the subtree where a
field's value belongs is refused by each setter in turn.  Sibling
has no body, which is a region with a token of its own.")

(defconst org-mcp-test--set-content-heading
  (concat
   "\\`\\* TODO \\[#B\\] Target :work:\n"
   "SCHEDULED: <2026-04-02 Thu> DEADLINE: <2026-04-09 Thu>\n"
   ":PROPERTIES:\n"
   ":ID:       "
   org-mcp-test--set-content-id
   "\n"
   ":Effort:   2:00\n"
   ":END:\n")
  "Everything above Target's body, as a regexp, unchanged by a body write.")

(defconst org-mcp-test--set-content-rewritten
  (concat
   org-mcp-test--set-content-heading
   "The body, written afresh\\.\n"
   "\\*\\* Child\n"
   "\\* Sibling\n\\'")
  "The complete file after Target's body is replaced entire.")

(defconst org-mcp-test--set-content-substring-replaced
  (concat
   org-mcp-test--set-content-heading
   "First line of the body\\.\n"
   "Second line, edited\\.\n"
   "\\*\\* Child\n"
   "\\* Sibling\n\\'")
  "The complete file after one line of Target's body is replaced.")

(defconst org-mcp-test--set-content-line-added
  (concat
   org-mcp-test--set-content-heading
   "First line of the body\\.\n"
   "Second line of the body\\.\n"
   "Appended line\\.\n"
   "\\*\\* Child\n"
   "\\* Sibling\n\\'")
  "The complete file once a line is added to the end of Target's body.")

(defconst org-mcp-test--set-content-sibling-filled
  (concat
   org-mcp-test--set-content-heading
   "First line of the body\\.\n"
   "Second line of the body\\.\n"
   "\\*\\* Child\n"
   "\\* Sibling\n"
   "Sibling body\\.\n\\'")
  "The complete file after the empty body of Sibling is written into.")

(defun org-mcp-test--content-digest-of (link)
  "Return the `content_digest' a read of LINK returns, as a client reads it."
  (alist-get
   'content_digest
   (org-mcp-test--read-fields link ["content_digest"])))

(defun org-mcp-test--set-content-link ()
  "Return the link to Target in `org-mcp-test--content-set-content'."
  (concat "id:" org-mcp-test--set-content-id))

(defmacro org-mcp-test--with-set-content-file (file-var &rest body)
  "Bind FILE-VAR to a temp file of `org-mcp-test--content-set-content'."
  (declare (indent 1) (debug t))
  `(org-mcp-test--with-id-setup ,file-var
       org-mcp-test--content-set-content
       (list org-mcp-test--set-content-id)
     ,@body))

(ert-deftest org-mcp-test-set-content-digest-replaces-the-body-entire ()
  "A content_digest in before replaces the whole body with after.
A client holding a token over the region has said what it believed
was there, so it echoes no part of the region back, and what the
write covers is what the token covers."
  (org-mcp-test--with-set-content-file test-file
    (let ((link (org-mcp-test--set-content-link)))
      (org-mcp-test--call-edit-body-and-check
       test-file
       link
       (org-mcp-test--content-digest-of link)
       "The body, written afresh."
       org-mcp-test--set-content-rewritten
       link))))

(ert-deftest org-mcp-test-set-content-substring-mode-is-unchanged ()
  "A before that is no token still names the substring it replaces.
The same node takes both forms; which one a call sends is the whole
of what decides how much of the body it overwrites."
  (org-mcp-test--with-set-content-file test-file
    (let ((link (org-mcp-test--set-content-link)))
      (org-mcp-test--call-edit-body-and-check
       test-file
       link
       "Second line of the body."
       "Second line, edited."
       org-mcp-test--set-content-substring-replaced
       link))))

(ert-deftest org-mcp-test-set-content-digest-writes-an-empty-body ()
  "The token over an empty body is a token, and asserts with it.
A region with nothing in it is still a region, so a client saying
\"this body is empty and I am replacing it\" has a value to say it
with."
  (org-mcp-test--with-set-content-file test-file
    (let ((link (org-mcp-test--file-link test-file "*Sibling")))
      (org-mcp-test--call-edit-body-and-check
       test-file
       link
       (org-mcp-test--content-digest-of link)
       "Sibling body."
       org-mcp-test--set-content-sibling-filled
       link))))

(ert-deftest org-mcp-test-set-content-digest-round-trips ()
  "The token a read hands back is the token the next write takes.
A client reads, writes against what it read, and reads again for the
next write.  The token the write leaves behind is the one over the
body it wrote, so the loop closes without the client computing
anything; the token it planned the first write from is spent."
  (org-mcp-test--with-set-content-file test-file
    (let* ((link (org-mcp-test--set-content-link))
           (first (org-mcp-test--content-digest-of link)))
      (should
       (eq
        t
        (alist-get
         'success
         (json-read-from-string
          (mcp-server-lib-ert-call-tool
           "org-node-set-content"
           `((link . ,link)
             (before . ,first)
             (after . "One rewrite.")))))))
      (let ((second (org-mcp-test--content-digest-of link)))
        (should-not (string= first second))
        (org-mcp-test--call-edit-body-and-check
         test-file
         link
         second
         "The body, written afresh."
         org-mcp-test--set-content-rewritten
         link)))))

(ert-deftest org-mcp-test-set-content-refuses-a-stale-digest ()
  "A token the body no longer carries refuses the write, file untouched.
The refusal is a conflict: the call was well formed, and the body
has moved on from what the call asserted about it."
  (org-mcp-test--with-set-content-file test-file
    (let* ((link (org-mcp-test--set-content-link))
           (stale (org-mcp-test--content-digest-of link)))
      (mcp-server-lib-ert-call-tool
       "org-node-set-content"
       `((link . ,link)
         (before . "Second line of the body.")
         (after . "Second line, edited.")))
      (org-mcp-test--call-tool-refused
       "org-node-set-content"
       `((link . ,link) (before . ,stale) (after . "Rewritten."))
       "\\`conflict: Content mismatch: .*nothing was written\\'"
       test-file)
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--set-content-substring-replaced))))

(ert-deftest org-mcp-test-set-content-withholds-the-digest-it-found ()
  "The conflict names the token the call sent, never the current one.
The current token is the one value that would make the same call
succeed, so a refusal carrying it would make resending the call the
cheapest recovery there is — and a caller asserting a token it never
read asserts nothing.  The refusal sends the caller back to a read."
  (org-mcp-test--with-set-content-file test-file
    (let* ((link (org-mcp-test--set-content-link))
           (stale (org-mcp-test--content-digest-of link)))
      (mcp-server-lib-ert-call-tool
       "org-node-set-content"
       `((link . ,link)
         (before . "Second line of the body.")
         (after . "Second line, edited.")))
      (let ((fresh (org-mcp-test--content-digest-of link))
            (message
             (org-mcp-test--refusal-message
              "org-node-set-content"
              `((link . ,link)
                (before . ,stale)
                (after . "Rewritten.")))))
        (should-not (string= stale fresh))
        (should (string-match-p (regexp-quote stale) message))
        (should-not (string-match-p (regexp-quote fresh) message))
        (should (string-match-p "read the node again" message))))))

(defconst org-mcp-test--content-hex-body
  "* Target\n1b4f0e9851971998\n"
  "A node whose whole body reads like a token and is not one.")

(ert-deftest org-mcp-test-set-content-hex-body-is-a-value ()
  "A body of sixteen hexadecimal characters is a value, not a token.
The prefix is the whole of what tells the two forms of before apart,
and this is why a token carries one: told apart by shape alone, the
shortest bodies would be the ones a client could not assert."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-hex-body))
    (let ((link (org-mcp-test--file-link test-file "*Target")))
      (org-mcp-test--call-edit-body-and-check
       test-file
       link
       "1b4f0e9851971998"
       "A plain body."
       "\\`\\* Target\nA plain body\\.\n\\'"
       link))))

(defconst org-mcp-test--content-set-content-refusals
  (concat
   "* Target\n"
   "A line, and a line, and a line.\n"
   "* Bare\n")
  "A body repeating a substring, beside a node with no body at all.")

(ert-deftest org-mcp-test-set-content-before-refusals-are-conflicts ()
  "Every refusal over set-content's before says the file moved on.
The client read a body and planned against it, and the body is not
what it planned against: the substring it named is missing, or is
there more than once, or the node has no body, or it has one where
the call said it had none.  Each answers a belief about the file
rather than a malformed call, so each carries the marker whose
recovery is to read the node again and retry against what is there."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-set-content-refusals))
    (let ((target (org-mcp-test--file-link test-file "*Target"))
          (bare (org-mcp-test--file-link test-file "*Bare")))
      (pcase-dolist
          (`(,link ,before ,refusal)
           `((,target
              "no such text"
              "\\`conflict: Body text not found: no such text\\'")
             (,target
              "a line"
              "\\`conflict: Text appears 2 times (must be unique)\\'")
             (,bare "anything" "\\`conflict: Node has no body content\\'")
             (,target
              ""
              "\\`conflict: An empty before asserts the node has no content,")))
        (org-mcp-test--call-tool-refused
         "org-node-set-content"
         `((link . ,link) (before . ,before) (after . "Replaced."))
         refusal
         test-file)))))

(ert-deftest org-mcp-test-field-setters-refuse-a-digest ()
  "A setter that changes one field takes a value and never a token.
A token covers a region, and a region takes in what the call does
not touch: a priority set asserted with one would be refused because
a clock line moved or a descendant was edited, which is the
over-sensitivity the field-scoped assertion exists to avoid.  The
refusal is unmarked, the validation class, because no version of the
file makes a token the value of a field — reading the node again and
sending the token back refuses the call again.

org-node-set-tags asserts a set rather than a single value, and it
refuses a token wherever in that set the token arrives: as the whole
parameter, as the one member of it, or beside tags that are real.
There is no position in a tag set where a token belongs."
  (org-mcp-test--with-set-content-file test-file
    (let* ((link (org-mcp-test--set-content-link))
           (node
            (org-mcp-test--read-fields
             link ["digest" "content_digest"])))
      (dolist (token
               (list (alist-get 'digest node)
                     (alist-get 'content_digest node)))
        (pcase-dolist
            (`(,tool ,params ,field)
             `(("org-node-set-todo"
                ((before . ,token) (after . "DONE"))
                "State")
               ("org-node-set-title"
                ((before . ,token) (after . "Renamed"))
                "Title")
               ("org-node-set-scheduled"
                ((before . ,token) (after . "2026-05-01"))
                "SCHEDULED")
               ("org-node-set-deadline"
                ((before . ,token) (after . "2026-05-08"))
                "DEADLINE")
               ("org-node-set-priority"
                ((before . ,token) (after . "A"))
                "Priority")
               ("org-node-set-properties"
                ((before . ((Effort . ,token)))
                 (after . ((Effort . "3:00"))))
                "Property 'Effort'")
               ("org-node-set-tags"
                ((before . ,token) (after . ["later"]))
                "Tags")
               ("org-node-set-tags"
                ((before . [,token]) (after . ["later"]))
                "Tags")
               ("org-node-set-tags"
                ((before . ["work" ,token]) (after . ["later"]))
                "Tags")))
          (org-mcp-test--call-tool-refused
           tool
           (cons `(link . ,link) params)
           (concat
            "\\`"
            (regexp-quote field)
            " is asserted with the value it holds, not with a digest")
           test-file))))))

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

(defconst org-mcp-test--dirty-read-content "* Task One\nOriginal body\n"
  "A one-heading file for reads of a buffer the user is editing.")

(ert-deftest org-mcp-test-tool-read-file-prefers-modified-buffer ()
  "org-node-text serves a file from the buffer the user is editing.
What comes back is the whole buffer: the file's own text and the
user's unsaved edit, which the file on disk does not carry."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--dirty-read-content))
    (org-mcp-test--with-dirty-buffer (_buffer on-disk) test-file
      (should
       (string=
        (org-mcp-test--served-text test-file)
        (org-mcp-test--content-with-user-edit on-disk)))
      (should-not
       (string-match-p
        "Typed by hand"
        (org-mcp-test--read-file test-file))))))

(ert-deftest org-mcp-test-tool-read-node-prefers-modified-buffer ()
  "org-node-read serves a node from the buffer the user is editing.
The structured read walks the same buffer the verbatim one does, so
the node's body carries the user's unsaved edit and the two agree on
what the file holds."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--dirty-read-content))
    (org-mcp-test--with-dirty-buffer (_buffer _on-disk) test-file
      ;; A node's content comes back without the line break that
      ;; separates it from what follows it, so the user's edit is its
      ;; last line rather than a line and a break.
      (should
       (string=
        (org-mcp-test--read-content test-file "Task One")
        (concat
         "Original body\n"
         (string-trim-right org-mcp-test--user-edit))))
      (should
       (string-match-p
        "Typed by hand"
        (org-mcp-test--call-read-headline
         (org-mcp-test--file-link test-file "*Task One")))))))

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

(defconst org-mcp-test--dirty-clock-add-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "Regex matching the whole file the server serves after a clock-add.
The file is served from the buffer the user is editing, so it carries
that user's own unsaved edit as well as the new CLOCK line.")

(defconst org-mcp-test--dirty-clock-in-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\n"
   ":END:\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "Regex matching the whole file the server serves after a clock-in.
The file is served from the buffer the user is editing, so it carries
that user's own unsaved edit as well as the open CLOCK line.")

(defconst org-mcp-test--dirty-clock-out-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "Regex matching the whole file the server serves after a clock-out.
The file is served from the buffer the user is editing, so it carries
that user's own unsaved edit as well as the closed CLOCK line.")

(ert-deftest org-mcp-test-clock-add-modified-buffer-no-auto-save ()
  "A clock-add lands in the buffer the user is editing, not around it.
The buffer held the user's own unsaved edit before the call, so the
CLOCK line is the user's to save: the response says so, the file on
disk keeps the content it had, and the server answers from the buffer
with the CLOCK line and the user's edit both in it."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--clock-task-content
   "org-clock-add"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Task One"))
       (start . "2026-01-01T10:00:00")
       (end . "2026-01-01T11:00:00")))
   '((added . t))
   org-mcp-test--dirty-clock-add-regex))

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
  "A clock-in lands in the buffer the user is editing, not around it.
The buffer held the user's own unsaved edit before the call, so the
open CLOCK line is the user's to save: the response says so, the file
on disk keeps the content it had, and the server answers from the
buffer with the CLOCK line and the user's edit both in it."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--clock-task-content
   "org-clock-in"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Task One"))
       (start_time . "2026-01-01T10:00:00")))
   '((clocked_in . t))
   org-mcp-test--dirty-clock-in-regex))

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

(defconst org-mcp-test--clock-resolve-child-content
  (concat
   "* TODO Task One\n"
   "** TODO Child\n:LOGBOOK:\n"
   "CLOCK: [2025-12-30 Tue 09:00]\n"
   ":END:\n")
  "Dangling CLOCK under a child of Task One, the resolve target.")

(defconst org-mcp-test--clock-resolve-parent-and-child-content
  (concat
   "* TODO Task One\n:LOGBOOK:\n"
   "CLOCK: [2025-12-29 Mon 09:00]\n"
   ":END:\n"
   "** TODO Child\n:LOGBOOK:\n"
   "CLOCK: [2025-12-30 Tue 09:00]\n"
   ":END:\n")
  "Task One and its child each holding a dangling CLOCK entry.")

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

(defconst org-mcp-test--clock-in-resolve-child-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\n"
   ":END:\n"
   "\\*\\* TODO Child\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2025-12-30 [A-Za-z]\\{2,3\\} 09:00\\]\n"
   ":END:\n"
   "\\'")
  "After resolve=true on Task One, its child's dangling CLOCK survives.")

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
RESOLVED deleted clocks, or nil for none.  Whatever RESOLVE deleted,
the response names Task One as the heading clocked in to, since that
is the heading the call named."
  (org-mcp-test--with-temp-org-files
      ((test-file content)
       (running-file org-mcp-test--clock-running-elsewhere-content))
    (org-mcp-test--with-session-clock running-file
      (let* ((link (org-mcp-test--file-link test-file "*Task One"))
             (result
              (org-mcp-test--call-clock-in
               link "2026-01-01T10:00:00" resolve
               (org-mcp-test--file-link running-file "*Running Task"))))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'clocked_in result) t))
        (should (equal (alist-get 'heading result) "Task One"))
        (should (equal (alist-get 'link result) link))
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

(ert-deftest org-mcp-test-clock-in-resolve-leaves-a-sibling-alone ()
  "Test resolve=true deletes no dangling clock in a sibling heading.
Task Two's dangling CLOCK lies outside Task One, the heading clocked in
to, so it survives."
  (org-mcp-test--check-clock-in-resolve
   org-mcp-test--clock-resolve-other-heading-content "true"
   org-mcp-test--clock-in-resolve-other-heading-expected-regex nil))

(ert-deftest org-mcp-test-clock-in-resolve-leaves-a-descendant-alone ()
  "A dangling clock on a child is not the parent's to cancel.
Task One carries none of its own, so resolve=true finds nothing to
delete, its child keeps its dangling CLOCK, and the response reports
no `resolved' count at all."
  (org-mcp-test--check-clock-in-resolve
   org-mcp-test--clock-resolve-child-content "true"
   org-mcp-test--clock-in-resolve-child-expected-regex nil))

(ert-deftest org-mcp-test-clock-in-resolve-counts-the-named-heading ()
  "`resolved' counts what was cancelled on the heading the call named.
Task One and its child each hold a dangling CLOCK.  Only Task One's is
cancelled, so the count is one, and the child's survives."
  (org-mcp-test--check-clock-in-resolve
   org-mcp-test--clock-resolve-parent-and-child-content "true"
   org-mcp-test--clock-in-resolve-child-expected-regex 1))

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
    (let ((result
           (org-mcp-test--call-clock-out
            (org-mcp-test--file-link test-file "*Task One")
            "2026-01-01T11:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'clocked_out result) t)))
    (org-mcp-test--verify-file-matches
     test-file org-mcp-test--clock-out-expected-regex)))

(ert-deftest org-mcp-test-clock-out-modified-buffer-no-auto-save ()
  "A clock-out lands in the buffer the user is editing, not around it.
The buffer held the user's own unsaved edit before the call, so the
closed CLOCK line is the user's to save: the response says so, the
file on disk keeps the open clock it had, and the server answers from
the buffer with the close and the user's edit both in it."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--clock-task-with-open-clock
   "org-clock-out"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Task One"))
       (end_time . "2026-01-01T11:00:00")))
   '((clocked_out . t))
   org-mcp-test--dirty-clock-out-regex))

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
             "org-clock-out"
             `((link
                . ,(org-mcp-test--file-link test-file "*Task One"))
               (end_time . "2026-01-01T11:00:00"))
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

(defconst org-mcp-test--dirty-clock-in-close-same-regex
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
   "Typed by hand, not saved\\.\n"
   "\\'")
  "Regex matching the whole file served after clock-in to Task Two at 11:00.
The file is served from the buffer the user is editing, so it carries
Task One's closed clock, Task Two's new clock, and that user's own
unsaved edit.")

(ert-deftest org-mcp-test-clock-in-closes-active-same-modified-buffer ()
  "A clock-in closing a clock in the same dirty buffer lands in it.
Both the close and the new clock are the user's to save, because the
buffer holds the user's own unsaved edit: the response says so, the
file on disk keeps the open clock it had, and the server answers from
the buffer with both changes and the user's edit in it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-dirty-buffer (buffer on-disk) test-file
      (let ((result
             (org-mcp-test--call-clock-in
              (org-mcp-test--file-link test-file "*Task Two")
              "2026-01-01T11:00:00" nil
              (org-mcp-test--file-link test-file "*Task One"))))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'clocked_in result) t))
        (org-mcp-test--verify-served-matches
         test-file org-mcp-test--dirty-clock-in-close-same-regex)
        (org-mcp-test--assert-unsaved
         result test-file on-disk buffer)))))

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

(defconst org-mcp-test--dirty-clock-closed-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "Regex matching the whole file whose open clock clock-in closed at 11:00.
The file carries the user's own unsaved edit as well as the close.")

(ert-deftest org-mcp-test-clock-in-closes-active-in-modified-buffer ()
  "Test clock-in reports `saved' false when the clock it closes stays unsaved.
The active clock sits in another allowed file whose buffer already has
unsaved edits.  The target file reaches disk, but the closed clock
only lands in that buffer, so the response covers both edits and the
server answers for that file from the buffer."
  (org-mcp-test--with-temp-org-files
      ((file-1 org-mcp-test--clock-task-with-open-clock)
       (file-2 org-mcp-test--clock-task-content))
    (org-mcp-test--with-dirty-buffer (buffer on-disk) file-1
      (let ((result
             (org-mcp-test--call-clock-in
              (org-mcp-test--file-link file-2 "*Task One")
              "2026-01-01T11:00:00" nil
              (org-mcp-test--file-link file-1 "*Task One"))))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'clocked_in result) t))
        (org-mcp-test--verify-file-matches
         file-2 org-mcp-test--clock-in-at-eleven-expected-regex)
        (org-mcp-test--verify-served-matches
         file-1 org-mcp-test--dirty-clock-closed-regex)
        (org-mcp-test--assert-unsaved
         result file-1 on-disk buffer)))))

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
                     (org-mcp-test--file-link test-file "*Task One")
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
      (let ((result
             (org-mcp-test--call-clock-out
              (org-mcp-test--file-link test-file "*Task One")
              "2026-01-01T11:00:00")))
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
               (org-mcp-test--call-clock-out
                (org-mcp-test--file-link test-file "*Task Two")
                "2026-01-01T10:00:00")))
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
               (org-mcp-test--call-clock-out
                (org-mcp-test--file-link test-file "*Task One")
                "2026-01-01T11:00:00")))
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

;;; The log entry closing a clock means to leave

;; `org-log-note-clock-out' asks Org to record a clock closing, and
;; `org-clock-out-switch-to-state' makes the close a TODO change with
;; log settings of its own.  Org's route to either entry arms
;; `org-add-log-note' on the global `post-command-hook'; an MCP call
;; has no command loop to run it, so the entry would reach the user as
;; a prompt at their next unrelated command.

(defconst org-mcp-test--clock-out-logged-heading
  "Clocked out on %t"
  "A heading for the `clock-out' purpose, which Org leaves empty.")

(defconst org-mcp-test--clock-out-note-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   "- Clocked out on \\[[^]]+\\]\n"
   ":END:\n"
   "\\'")
  "File contents once a clock-out heading of one's own records the close.
Org places the entry against the clock it belongs to rather than at
the top of the drawer.")

(ert-deftest org-mcp-test-clock-out-records-the-close-without-asking ()
  "`org-log-note-clock-out' records the close and waits for no one.
Org's own route to the entry arms `post-command-hook' and opens an
`*Org Note*' buffer for a person to type in, which an MCP call has
nobody to finish.  `org-log-note-headings' leaves the `clock-out'
purpose an empty heading, so the record Org writes for it is nothing
at all, and the file carries the closed CLOCK line alone."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-log-note-clock-out t)
          (org-log-into-drawer t))
      (org-mcp-test--with-session-clock test-file
        (let ((result
               (org-mcp-test--call-clock-out
                (org-mcp-test--file-link test-file "*Task One")
                "2026-01-01T11:00:00")))
          (should (equal (alist-get 'success result) t))
          (should (eq (alist-get 'saved result) t))
          (should (equal (alist-get 'clocked_out result) t))
          (should (equal (alist-get 'heading result) "Task One"))
          (should (equal (alist-get 'duration result) "1:00")))
        (org-mcp-test--should-leave-no-log-prompt)
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--clock-add-expected-regex)))))

(ert-deftest org-mcp-test-clock-out-writes-the-heading-given-for-it ()
  "A `clock-out' heading of one's own is written, against its clock.
The empty entry Org ships for the purpose is a default, not the
whole of what the setting can record: a user who gives `clock-out' a
heading gets it here as they would from a clock-out by hand."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-log-note-clock-out t)
          (org-log-into-drawer t)
          (org-log-note-headings
           (cons
            (cons 'clock-out org-mcp-test--clock-out-logged-heading)
            (assq-delete-all 'clock-out
                             (copy-alist org-log-note-headings)))))
      (org-mcp-test--with-session-clock test-file
        (let ((result
               (org-mcp-test--call-clock-out
                (org-mcp-test--file-link test-file "*Task One")
                "2026-01-01T11:00:00")))
          (should (equal (alist-get 'success result) t))
          (should (eq (alist-get 'saved result) t))
          (should (equal (alist-get 'clocked_out result) t)))
        (org-mcp-test--should-leave-no-log-prompt)
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--clock-out-note-expected-regex)))))

;;; The note a clock-out carries

;; `org-log-note-clock-out' asks Org to record a clock closing, and Org
;; asks a person for the prose.  A `note' is that prose, sent with the
;; call that closes the clock.

(defconst org-mcp-test--clock-out-prose "Stopped to take the call."
  "The prose these tests send as a clock-out's `note'.")

(ert-deftest org-mcp-test-clock-out-publishes-note-as-optional ()
  "org-clock-out publishes `note\=' as a parameter a call may carry.
A client discovers the note from the schema and never from the
handler, so prose no published parameter carries is prose nothing
will ever send.  It is optional: a close says nothing unless the
caller has something to say."
  (org-mcp-test--with-enabled
    (should
     (member
      "note"
      (org-mcp-test--registered-tool-properties "org-clock-out")))
    (should-not
     (member
      "note"
      (org-mcp-test--registered-tool-required "org-clock-out")))))

(defconst org-mcp-test--clock-out-prose-regex
  "- Stopped to take the call\\.\n"
  "The entry `org-mcp-test--clock-out-prose' is written as.
`org-log-note-headings' leaves the `clock-out' purpose an empty
heading, so the prose is the whole entry and no heading line
precedes it.")

(defconst org-mcp-test--clock-out-with-note-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   org-mcp-test--clock-out-prose-regex
   ":END:\n"
   "\\'")
  "File contents once a clock-out carrying a note records it.
The entry sits under the CLOCK line it is about rather than at the
top of the drawer, which is where Org puts a clock-out entry.")

(defun org-mcp-test--should-report-the-closed-hour (result link)
  "Assert RESULT is the response closing the test hour returns.
LINK is the link the call named the heading with, which the response
carries back.  A note changes no field of it: the response reports
the close that was asked for, and the note is in the file."
  (should (equal (alist-get 'success result) t))
  (should (eq (alist-get 'saved result) t))
  (should (equal (alist-get 'clocked_out result) t))
  (should (equal (alist-get 'heading result) "Task One"))
  (should
   (string-match-p "\\`2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\'"
                   (alist-get 'start result)))
  (should
   (string-match-p "\\`\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\]\\'"
                   (alist-get 'end result)))
  (should (equal (alist-get 'duration result) "1:00"))
  (should (equal (alist-get 'link result) link)))

(ert-deftest org-mcp-test-clock-out-note-is-written-against-its-clock ()
  "A `note' becomes the entry `org-log-note-clock-out' asks for.
Org gets that prose by prompting, which an MCP call has nobody to
answer; sent with the call, it is written where Org writes a
clock-out entry -- under the CLOCK line it closed, in the drawer that
line sits in."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-log-note-clock-out t)
          (org-log-into-drawer t))
      (org-mcp-test--with-session-clock test-file
        (let ((link (org-mcp-test--file-link test-file "*Task One")))
          (org-mcp-test--should-report-the-closed-hour
           (org-mcp-test--call-clock-out
            link "2026-01-01T11:00:00"
            org-mcp-test--clock-out-prose)
           link))
        (org-mcp-test--should-leave-no-log-prompt)
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--clock-out-with-note-regex)))))

(ert-deftest org-mcp-test-clock-out-note-outlives-the-setting-being-off ()
  "A `note' is recorded whether or not `org-log-note-clock-out' is on.
The setting says whether Org asks for prose of its own accord.  A
call that sends prose has already said it has some, and it is
recorded, as `org-node-set-todo' records a note under log settings
that asked for none."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-log-note-clock-out nil)
          (org-log-into-drawer t))
      (org-mcp-test--with-session-clock test-file
        (let ((link (org-mcp-test--file-link test-file "*Task One")))
          (org-mcp-test--should-report-the-closed-hour
           (org-mcp-test--call-clock-out
            link "2026-01-01T11:00:00"
            org-mcp-test--clock-out-prose)
           link))
        (org-mcp-test--should-leave-no-log-prompt)
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--clock-out-with-note-regex)))))

(ert-deftest org-mcp-test-clock-out-blank-note-records-nothing ()
  "Every blank `note' is a note the call did not send.
\"\", a string of whitespace, false and [] all leave the close
recorded the way a call with no note at all records it: the closed
CLOCK line alone, since `org-log-note-headings' gives the `clock-out'
purpose an empty heading and there is no prose to go under it."
  (dolist (blank '("" "   " :json-false []))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--clock-task-with-open-clock))
      (let ((org-log-note-clock-out t)
            (org-log-into-drawer t))
        (org-mcp-test--with-session-clock test-file
          (let ((link
                 (org-mcp-test--file-link test-file "*Task One")))
            (org-mcp-test--should-report-the-closed-hour
             (org-mcp-test--call-clock-out
              link "2026-01-01T11:00:00" blank)
             link))
          (org-mcp-test--should-leave-no-log-prompt)
          (org-mcp-test--verify-file-matches
           test-file org-mcp-test--clock-add-expected-regex))))))

(ert-deftest org-mcp-test-clock-out-note-arms-no-prompt ()
  "A `note' changes nothing about the prompt a clock-out must not arm.
Org reaches its clock-out entry by pushing `org-add-log-note' onto
the global `post-command-hook', which inside an MCP call would pop a
note prompt at the user's next unrelated command.  The note rides
that entry, so it is written here and the hook is left as it was
found, whether the call carried prose or not."
  (dolist (note (list nil org-mcp-test--clock-out-prose))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--clock-task-with-open-clock))
      (let ((org-log-note-clock-out t)
            (org-log-into-drawer t))
        (org-mcp-test--with-session-clock test-file
          (let ((link
                 (org-mcp-test--file-link test-file "*Task One")))
            (org-mcp-test--should-report-the-closed-hour
             (org-mcp-test--call-clock-out
              link "2026-01-01T11:00:00" note)
             link))
          (org-mcp-test--should-leave-no-log-prompt)
          (org-mcp-test--verify-file-matches
           test-file
           (if note
               org-mcp-test--clock-out-with-note-regex
             org-mcp-test--clock-add-expected-regex)))))))

(defconst org-mcp-test--clock-open-in-named-drawer
  (concat
   "* TODO Task One\n"
   ":PROPERTIES:\n"
   ":LOG_INTO_DRAWER: NOTES\n"
   ":END:\n"
   ":NOTES:\n"
   "CLOCK: [2026-01-01 Thu 10:00]\n"
   ":END:\n")
  "An open clock in the drawer the node's LOG_INTO_DRAWER names.
`org-clock-into-drawer' reads that property through
`org-log-into-drawer', so this is the drawer clocking in put the line
in.")

(defconst org-mcp-test--clock-out-note-in-named-drawer-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":PROPERTIES:\n"
   ":LOG_INTO_DRAWER:[ \t]+NOTES\n"
   ":END:\n"
   ":NOTES:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   org-mcp-test--clock-out-prose-regex
   ":END:\n"
   "\\'")
  "File contents once a note followed its clock into the NOTES drawer.
The entry is under the CLOCK line, not at the top of the drawer:
a clock-out entry is placed against its clock, so the drawer it lands
in is the one holding that clock.")

(ert-deftest org-mcp-test-clock-out-note-follows-its-clock-into-the-drawer ()
  "A note lands in the drawer the clock it closes sits in.
A node carrying LOG_INTO_DRAWER keeps its clocks somewhere other
than LOGBOOK, and the entry marking a close belongs with the line it
marks, so it goes there too.  `org-log-into-drawer' is off globally
here, so the property is the only thing naming that drawer."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-open-in-named-drawer))
    (let ((org-log-note-clock-out t)
          (org-log-into-drawer nil))
      (org-mcp-test--with-session-clock test-file
        (let ((link (org-mcp-test--file-link test-file "*Task One")))
          (org-mcp-test--should-report-the-closed-hour
           (org-mcp-test--call-clock-out
            link "2026-01-01T11:00:00"
            org-mcp-test--clock-out-prose)
           link))
        (org-mcp-test--should-leave-no-log-prompt)
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--clock-out-note-in-named-drawer-regex)))))

(ert-deftest org-mcp-test-clock-out-note-goes-with-a-removed-clock-line ()
  "A note has nothing to mark once Org takes the clock line away.
`org-clock-out-remove-zero-time-clocks' deletes a CLOCK line of no
length and the drawer it empties, and Org records no close for a line
it removed.  A note sent with such a call goes the same way, leaving
the file a close with no note leaves."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-out-zero-time-content))
    (let ((org-log-note-clock-out t)
          (org-log-into-drawer t)
          (org-clock-out-remove-zero-time-clocks t))
      (org-mcp-test--with-session-clock test-file
        (let* ((link (org-mcp-test--file-link test-file "*Task Two"))
               (result
                (org-mcp-test--call-clock-out
                 link "2026-01-01T10:00:00"
                 org-mcp-test--clock-out-prose)))
          (should (equal (alist-get 'success result) t))
          (should (eq (alist-get 'saved result) t))
          (should (equal (alist-get 'clocked_out result) t))
          (should (equal (alist-get 'heading result) "Task Two"))
          (should (equal (alist-get 'duration result) "0:00"))
          (should (equal (alist-get 'link result) link)))
        (org-mcp-test--should-leave-no-log-prompt)
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--clock-out-zero-time-expected-regex)))))

(defconst org-mcp-test--clock-out-switch-state-logged-regex
  (concat
   "\\`\\* DONE Task One\n"
   "CLOSED: \\[[^]]+\\]\n"
   ":LOGBOOK:\n"
   "- CLOSING NOTE \\[[^]]+\\]\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\'")
  "File contents once a clock-out that switches state records it too.
`org-clock-out-switch-to-state' makes the close a TODO change, and
`org-log-done' records that change as it records any other.")

(ert-deftest org-mcp-test-clock-out-switching-state-logs-without-asking ()
  "`org-clock-out-switch-to-state' is a TODO change, and it is recorded.
It is the second way a clock-out reaches a log entry: Org moves the
heading with `org-todo', whose own settings decide what that writes.
The entry lands here rather than on the hook."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-clock-out-switch-to-state "DONE")
          (org-log-done 'note)
          (org-log-into-drawer t))
      (org-mcp-test--with-session-clock test-file
        (let ((result
               (org-mcp-test--call-clock-out
                (org-mcp-test--file-link test-file "*Task One")
                "2026-01-01T11:00:00")))
          (should (equal (alist-get 'success result) t))
          (should (equal (alist-get 'clocked_out result) t))
          (should (equal (alist-get 'heading result) "Task One")))
        (org-mcp-test--should-leave-no-log-prompt)
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--clock-out-switch-state-logged-regex)))))

(ert-deftest org-mcp-test-clock-in-closing-a-clock-asks-no-one ()
  "The clock org-clock-in closes is recorded the same way, and asks no one.
A clock-in closes whatever clock is running before it opens its own,
and that close reaches the same log settings as a clock-out does."
  (org-mcp-test--with-temp-org-files
      ((file-1 org-mcp-test--clock-task-with-open-clock)
       (file-2 org-mcp-test--clock-task-content))
    (let ((org-log-note-clock-out t)
          (org-log-into-drawer t))
      (let ((result
             (org-mcp-test--call-clock-in
              (org-mcp-test--file-link file-2 "*Task One")
              "2026-01-01T11:00:00" nil
              (org-mcp-test--file-link file-1 "*Task One"))))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'clocked_in result) t))
        (should (eq (alist-get 'saved result) t)))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       file-1 org-mcp-test--clock-add-expected-regex)
      (org-mcp-test--verify-file-matches
       file-2 org-mcp-test--clock-in-at-eleven-expected-regex))))

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
outside the allowed files.  It comes before `link' is looked at, so a
link into the allowed files and one naming the running clock's own
file are answered alike and neither confirms where that clock is.
Neither the file, the buffer of the running clock, nor the running
clock changes."
  (org-mcp-test--with-temp-org-files
      ((allowed-file org-mcp-test--clock-task-content)
       (outside-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-mcp-allowed-files (list allowed-file)))
      (org-mcp-test--with-session-clock outside-file
        (let ((position (marker-position org-clock-marker)))
          (dolist (file (list allowed-file outside-file))
            (should
             (string-match-p
              "\\`A clock is running in a file outside the allowed files\\.  \
Ask the user to clock out of it in Emacs\\'"
              (org-mcp-test--call-tool-expecting-error
               outside-file "org-clock-out"
               `((link
                  . ,(org-mcp-test--file-link file "*Task One"))
                 (end_time . "2026-01-01T11:00:00"))))))
          (should (string= (org-mcp-test--read-file outside-file)
                           org-mcp-test--clock-task-with-open-clock))
          (org-mcp-test--verify-no-modified-buffer outside-file)
          (should (eq (org-clock-is-active) (find-buffer-visiting outside-file)))
          (should (= (marker-position org-clock-marker) position)))))))

(ert-deftest org-mcp-test-clock-out-publishes-link-as-required ()
  "org-clock-out publishes `link\=' as the one parameter a call must carry.
A clock operation asserts which clock it changes rather than a value
it overwrites, so `link\=' is this tool\='s guard.  A client discovers a
guard from the schema and never from the handler, so one published as
optional is a guard that is off."
  (org-mcp-test--with-enabled
    (should
     (equal (org-mcp-test--registered-tool-required "org-clock-out")
            '("link")))))

(ert-deftest org-mcp-test-clock-out-refuses-without-a-link ()
  "A clock-out that names no clock closes none.
Without `link\=' the call would close whichever clock happens to be
running, which may be one the user started in Emacs and the client
never saw.  The refusal comes before any clock is found, so the file,
the buffer and the running clock are all left as they were."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-session-clock test-file
      (let ((position (marker-position org-clock-marker)))
        (org-mcp-test--call-tool-refused
         "org-clock-out" '((end_time . "2026-01-01T11:00:00"))
         "\\`Missing required parameter: link\\'" test-file)
        (org-mcp-test--verify-no-modified-buffer test-file)
        (should
         (eq (org-clock-is-active) (find-buffer-visiting test-file)))
        (should (= (marker-position org-clock-marker) position))))))

(ert-deftest org-mcp-test-clock-out-refuses-a-link-naming-another-heading ()
  "A clock-out is refused when `link\=' names a heading no clock runs on.
The heading named sits in the running clock\='s own file, so matching
the file alone would close a clock the call never named.  The client
believed the clock ran where it did not, so the refusal is a conflict
and names the clock that is running; nothing is closed."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-session-clock test-file
      (let ((position (marker-position org-clock-marker))
            (running (org-mcp-test--file-link test-file "*Task One"))
            (named (org-mcp-test--file-link test-file "*Task Two")))
        (should
         (equal
          (org-mcp-test--call-tool-expecting-error
           test-file "org-clock-out"
           `((link . ,named) (end_time . "2026-01-01T11:00:00")))
          (concat
           org-mcp-test--conflict-marker
           "link does not name the running clock: " named
           ".  The clock runs on 'Task One' (" running
           ") since 2026-01-01 Thu 10:00")))
        (org-mcp-test--verify-no-modified-buffer test-file)
        (should
         (eq (org-clock-is-active) (find-buffer-visiting test-file)))
        (should (= (marker-position org-clock-marker) position))))))

(ert-deftest org-mcp-test-clock-out-refuses-a-whole-file-link ()
  "A clock-out is refused when `link\=' names a file rather than a heading.
A file holds any number of headings and so names no one clock.  The
guard is which clock, so the call is refused as one naming another
heading is, and the file keeps its open CLOCK line."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-session-clock test-file
      (org-mcp-test--call-tool-refused
       "org-clock-out"
       `((link . ,(concat "file:" (abbreviate-file-name test-file)))
         (end_time . "2026-01-01T11:00:00"))
       (concat
        "\\`" org-mcp-test--conflict-marker
        "link does not name the running clock: ")
       test-file)
      (should
       (eq (org-clock-is-active) (find-buffer-visiting test-file))))))

(defconst org-mcp-test--clock-out-close-same-file-expected-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\* TODO Task Two\n"
   "\\'")
  "File contents after clock-out closes Task One's clock at 11:00.
Task Two stands unclocked beside it, so a close that reached the
wrong heading shows here.")

(ert-deftest org-mcp-test-clock-out-accepts-the-link-of-the-running-clock ()
  "A clock-out that names the running clock closes it and reports it.
The link `org-clock-active\=' hands back for the running clock is what
a client echoes here, and the response names the same heading and
link it did."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--clock-in-close-same-file-open-clock-content))
    (org-mcp-test--with-session-clock test-file
      (let* ((active (org-mcp-test--call-clock-get-active))
             (result
              (org-mcp-test--call-clock-out
               (alist-get 'link active) "2026-01-01T11:00:00")))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'clocked_out result) t))
        (should (equal (alist-get 'heading result) "Task One"))
        (should
         (equal (alist-get 'link result) (alist-get 'link active)))
        (should
         (equal (alist-get 'start result) (alist-get 'start active)))
        (should (equal (alist-get 'duration result) "1:00")))
      (should-not (org-clock-is-active)))
    (org-mcp-test--verify-file-matches
     test-file
     org-mcp-test--clock-out-close-same-file-expected-regex)))

(ert-deftest org-mcp-test-clock-out-accepts-an-id-link ()
  "A clock-out names the running clock by `id\=:' as readily as by title.
`org-clock-active\=' hands back an `id:' link for a heading that has an
ID, so that is the form a client echoes most often.  It is looked up
the way any other `id:' link is, and the guard finds the same heading
through it."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-links
      (list org-mcp-test--link-beta-id)
    (let ((link (format "id:%s" org-mcp-test--link-beta-id)))
      (org-mcp-test--call-clock-in link "2026-03-23T14:30:00")
      (should
       (equal
        (alist-get 'link (org-mcp-test--call-clock-get-active)) link))
      (let ((result
             (org-mcp-test--call-clock-out
              link "2026-03-23T16:45:00")))
        (should (equal (alist-get 'clocked_out result) t))
        (should (equal (alist-get 'heading result) "Beta"))
        (should (equal (alist-get 'link result) link))
        (should (equal (alist-get 'duration result) "2:15")))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-links-beta-clocked))))

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
      (org-mcp-test--call-clock-out
       (org-mcp-test--file-link test-file "*Task One")
       "2026-01-01T10:30:00")
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

;;; A date has to be a date, and a time a time

;; A shape is not an existence: `2026-02-30' is four digits, two and
;; two, and it is no day of any year.  Org reads such a value by
;; rolling it over — `2026-13-45' becomes 2027-02-14 — so a call that
;; sent one got a success reporting a date eighteen months from the
;; one it named.  Every date and time parameter is therefore read the
;; way the write will read it and compared with what was sent, before
;; a link is resolved or a buffer opened.

(defconst org-mcp-test--dates-that-are-not-dates
  '(("2026-02-30" . "2026-03-02 [^ >]+")
    ("2026-13-45" . "2027-02-14 [^ >]+")
    ("2026-00-00" . "2025-11-30 [^ >]+")
    ("2026-02-29" . "2026-03-01 [^ >]+"))
  "Dates whose fields name no day, each with the day Org reads instead.
The day Org reads is written as a regexp, because the day name in it
is Org\='s to choose.  The last is a leap day of a year that has none;
`2024-02-29\=' is the same date in a year that does, and it writes.
A year below 100 is refused for its own reason and is not here; see
`org-mcp-test-set-scheduled-refuses-a-two-digit-year\='.")

(defconst org-mcp-test--times-that-are-not-times
  '(("2026-03-27 25:99" . "2026-03-28 [^ >]+ 02:39")
    ("2026-03-27 10:99" . "2026-03-27 [^ >]+ 11:39"))
  "Times whose fields name no minute, each with the one Org reads.
Written as regexps, because the day name in them is Org\='s to choose.
The second rolls the hour without rolling the day, so a check
comparing dates alone would let it through.")

(ert-deftest org-mcp-test-planning-refuses-a-date-that-is-not-one ()
  "A date Org would roll over to another date is refused, not written.
The refusal names what the call would have got, because that is the
one thing the client cannot work out for itself and the one thing
that tells it the value was wrong rather than the file."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (pcase-dolist (`(,sent . ,rolled)
                     org-mcp-test--dates-that-are-not-dates)
        (dolist (tool '("org-node-set-scheduled" "org-node-set-deadline"))
          (org-mcp-test--call-tool-refused
           tool
           `((link . ,link) (before . "") (after . ,sent))
           (format
            "\\`Date '%s' does not exist - Org reads it as '<%s>'\\'"
            (regexp-quote sent) rolled)
           test-file))))))

(ert-deftest org-mcp-test-planning-refuses-a-time-that-is-not-one ()
  "An hour or minute Org would roll over is refused with the date.
`2026-03-27 10:99' keeps the day it names and moves the hour, so the
whole value is compared and not the date alone."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (pcase-dolist (`(,sent . ,rolled)
                     org-mcp-test--times-that-are-not-times)
        (org-mcp-test--call-tool-refused
         "org-node-set-scheduled"
         `((link . ,link) (before . "") (after . ,sent))
         (format
          "\\`Date '%s' does not exist - Org reads it as '<%s>'\\'"
          (regexp-quote sent) rolled)
         test-file)))))

(ert-deftest org-mcp-test-planning-takes-a-leap-day-that-exists ()
  "A leap day of a leap year is a date, and it is written.
The check is what Org makes of the value, not a rule about February,
so the day exists exactly in the years it exists in."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-set-scheduled"
              `((link . ,link) (before . "") (after . "2024-02-29"))))))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'before result) ""))
      (should
       (string-match-p "\\`<2024-02-29 " (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file
       (concat
        "\\`\\* TODO Simple Task\n"
        "SCHEDULED: <2024-02-29 [^>]+>\n"
        "Task body text\\.\n?\\'")))))

(ert-deftest org-mcp-test-planning-refuses-a-date-before-it-opens-a-file ()
  "The date is read before the link is, so a refusal reaches no file.
The link here names no heading.  A call checking the file first would
answer for the link; this one answers for the date, which is how a
refused write is known to have opened nothing."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (org-mcp-test--call-tool-refused
     "org-node-set-scheduled"
     `((link . ,(org-mcp-test--file-link test-file "*No Such Heading"))
       (before . "")
       (after . "2026-02-30"))
     "\\`Date '2026-02-30' does not exist - Org reads it as '<2026-03-02"
     test-file)))

(ert-deftest org-mcp-test-clock-refuses-a-time-that-is-not-one ()
  "A clock time Org would roll over is refused by every clock tool.
They take a different format from the planning setters and read it
with a different parser, and the same question is asked of it: is the
value the call sent the value Org makes of it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (let ((link (org-mcp-test--file-link test-file "*Task One")))
      (dolist (sent '("2026-02-30T14:30:00" "2026-13-45T14:30:00"
                      "2026-00-00T14:30:00" "2026-03-27T25:99:00"))
        (dolist (call
                 (list
                  (list "org-clock-in" `((link . ,link)
                                         (start_time . ,sent)))
                  (list "org-clock-add" `((link . ,link)
                                          (start . ,sent)
                                          (end . "2026-03-27T15:00:00")))
                  (list "org-clock-add" `((link . ,link)
                                          (start . "2026-03-27T14:00:00")
                                          (end . ,sent)))
                  (list "org-clock-delete" `((link . ,link)
                                             (start . ,sent)))))
          (org-mcp-test--call-tool-refused
           (car call) (cadr call)
           (concat "\\`Not a time: '" (regexp-quote sent) "'\\.")
           test-file))))))

(ert-deftest org-mcp-test-clock-out-refuses-a-time-that-is-not-one ()
  "`org-clock-out' asks the same question of its `end_time'.
It is the one clock tool whose timestamp needs a running clock to
reach, and the refusal comes first: the clock is still running
afterwards."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (org-mcp-test--with-session-clock test-file
      (org-mcp-test--call-tool-refused
       "org-clock-out"
       `((link . ,(org-mcp-test--file-link test-file "*Task One"))
         (end_time . "2026-02-30T14:30:00"))
       "\\`Not a time: '2026-02-30T14:30:00'\\."
       test-file))))

(defconst org-mcp-test--clock-on-the-rolled-over-day
  (concat
   "* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: [2026-03-02 Mon 14:30]--[2026-03-02 Mon 15:30] =>  1:00\n"
   ":END:\n")
  "A heading clocked on the day `2026-02-30' rolls over to.
A delete that rolled its `start' over would find this entry and take
away a clock the call never named.")

(ert-deftest org-mcp-test-clock-delete-takes-no-clock-a-rollover-finds ()
  "A `start' that is no time takes nothing away, not something else.
`org-clock-delete' only ever compares its `start' against the CLOCK
lines it finds, which is why a rolled-over value does not write a
wrong time — it matches a different clock and deletes that one.  The
entry the rollover lands on is still here afterwards."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-on-the-rolled-over-day))
    (org-mcp-test--call-tool-refused
     "org-clock-delete"
     `((link . ,(org-mcp-test--file-link test-file "*Task One"))
       (start . "2026-02-30T14:30:00"))
     "\\`Not a time: '2026-02-30T14:30:00'\\."
     test-file)))

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

(defconst org-mcp-test--clock-delete-child-only-content
  (concat
   "* TODO Parent\n"
   "** TODO Child\n:LOGBOOK:\n"
   "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:00] =>  1:00\n"
   ":END:\n")
  "A parent carrying no clock, over a child that carries one.")

(ert-deftest org-mcp-test-clock-delete-leaves-a-descendant-alone ()
  "A clock on a child is not the parent's to delete.
The call names the parent, whose own entry holds no CLOCK line, so
it is refused as no entry found and the child keeps its clock."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-delete-child-only-content))
    (org-mcp-test--call-tool-refused
     "org-clock-delete"
     `((link . ,(org-mcp-test--file-link test-file "*Parent"))
       (start . "2026-01-01T10:00:00"))
     "\\`No clock entry starting at \\[2026-01-01 [^]]+ 10:00\\] found\\'"
     test-file)))

(defconst org-mcp-test--clock-delete-both-levels-content
  (concat
   "* TODO Parent\n:LOGBOOK:\n"
   "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:00] =>  1:00\n"
   ":END:\n"
   "** TODO Child\n:LOGBOOK:\n"
   "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 12:00] =>  2:00\n"
   ":END:\n")
  "A parent and its child each clocked, both entries starting alike.")

(defconst org-mcp-test--clock-delete-child-kept-expected-regex
  (concat
   "\\`\\* TODO Parent\n"
   "\\*\\* TODO Child\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 12:00\\] =>  2:00\n"
   ":END:\n"
   "\\'")
  "The parent's CLOCK line and its emptied drawer go; the child's stays.")

(ert-deftest org-mcp-test-clock-delete-takes-the-heading-it-names ()
  "The entry deleted is the named heading's, and the response says so.
Parent and child hold entries of the same start, and the call names
the parent: the parent's one-hour entry goes, the child's two-hour
entry stays, and the response reports the parent's entry and links
the parent.  The shared start is also what pins the ambiguity check
to the entry: two such entries on one heading are refused, and these
two sit on headings of their own."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-delete-both-levels-content))
    (let* ((link (org-mcp-test--file-link test-file "*Parent"))
           (result
            (org-mcp-test--call-clock-delete
             link "2026-01-01T10:00:00")))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'deleted result) t))
      (should
       (string-match-p
        "\\`\\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]\\'"
        (alist-get 'start result)))
      (should
       (string-match-p
        "\\`\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\]\\'"
        (alist-get 'end result)))
      (should (equal (alist-get 'duration result) "1:00"))
      (should (equal (alist-get 'link result) link))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--clock-delete-child-kept-expected-regex))))

(defconst org-mcp-test--clock-delete-same-start-content
  (concat
   "* TODO Task One\n:LOGBOOK:\n"
   "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:00] =>  1:00\n"
   "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 12:00] =>  2:00\n"
   ":END:\n")
  "Two closed CLOCK entries on one heading, starting at the same time.")

(defconst org-mcp-test--clock-delete-ambiguous-regex
  (concat
   "\\`blocked: 2 clock entries on this heading start at "
   "\\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]: "
   "one ending \\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] and "
   "one ending \\[2026-01-01 [A-Za-z]\\{2,3\\} 12:00\\]\\.  "
   "start names no one of them, so delete the one you mean in Emacs\\'")
  "The refusal names both entries and what tells them apart.")

(ert-deftest org-mcp-test-clock-delete-same-start-is-ambiguous ()
  "Two entries sharing a start name no one entry, so nothing goes."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-delete-same-start-content))
    (org-mcp-test--call-tool-refused
     "org-clock-delete"
     `((link . ,(org-mcp-test--file-link test-file "*Task One"))
       (start . "2026-01-01T10:00:00"))
     org-mcp-test--clock-delete-ambiguous-regex
     test-file)))

(defconst org-mcp-test--clock-delete-same-start-open-content
  (concat
   "* TODO Task One\n:LOGBOOK:\n"
   "CLOCK: [2026-01-01 Thu 10:00]\n"
   "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:00] =>  1:00\n"
   ":END:\n")
  "An unclosed CLOCK entry sharing a start with a closed one.")

(ert-deftest org-mcp-test-clock-delete-same-start-names-an-open-clock ()
  "An unclosed entry among the ambiguous ones is named as still open."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-delete-same-start-open-content))
    (org-mcp-test--call-tool-refused
     "org-clock-delete"
     `((link . ,(org-mcp-test--file-link test-file "*Task One"))
       (start . "2026-01-01T10:00:00"))
     (concat
      "\\`blocked: 2 clock entries on this heading start at "
      "\\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]: "
      "one still open and "
      "one ending \\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\]\\.  ")
     test-file)))

(defconst org-mcp-test--clock-delete-rounded-pair-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 12:00\\] =>  2:00\n"
   "CLOCK: \\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "\\'")
  "Two adds of distinct starts that rounding wrote as one time.")

(defconst org-mcp-test--clock-delete-rounded-ambiguous-regex
  (concat
   "\\`blocked: 2 clock entries on this heading start at "
   "\\[2026-01-01 [A-Za-z]\\{2,3\\} 10:00\\]: "
   "one ending \\[2026-01-01 [A-Za-z]\\{2,3\\} 12:00\\] and "
   "one ending \\[2026-01-01 [A-Za-z]\\{2,3\\} 11:00\\]\\.  "
   "start names no one of them, so delete the one you mean in Emacs\\'")
  "The refusal names the pair in the order the LOGBOOK holds them.
A new entry goes to the top of the drawer, so the entry added second
is described first.")

(ert-deftest org-mcp-test-clock-delete-rounding-collapses-two-starts ()
  "Rounding can write two distinct starts as one, and start names neither.
`org-clock-rounding-minutes' of 5 rounds 10:01 and 10:02 to the same
10:00, so two adds leave two entries beginning there.  A delete
naming that start is refused and the file keeps both."
  (org-mcp-test--with-temp-org-files
      ((test-file "* TODO Task One\n"))
    (let ((org-clock-rounding-minutes 5)
          (link (org-mcp-test--file-link test-file "*Task One")))
      (org-mcp-test--call-clock-add
       link "2026-01-01T10:01:00" "2026-01-01T11:00:00")
      (org-mcp-test--call-clock-add
       link "2026-01-01T10:02:00" "2026-01-01T12:00:00")
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--clock-delete-rounded-pair-regex)
      (org-mcp-test--call-tool-refused
       "org-clock-delete"
       `((link . ,link) (start . "2026-01-01T10:01:00"))
       org-mcp-test--clock-delete-rounded-ambiguous-regex
       test-file))))

;;; Tests for org-node-set-properties

(ert-deftest org-mcp-test-set-properties-new ()
  "Test setting a new property on a bare task."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (params `((link . ,link)
                     (before . ((EFFORT)))
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
                     (before . ((EFFORT . "1:00")))
                     (after . ((EFFORT . "2:30")))))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-properties" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-set-properties-update))))

(ert-deftest org-mcp-test-set-properties-booleans ()
  "Test booleans set properties to t and nil.
JSON false overwrites EFFORT with the text nil and keeps the
property.  JSON true writes t, and the strings \"t\" and \"nil\" are
written as given."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (let* ((params
            `((link
               .
               ,(org-mcp-test--file-link test-file "*Task with Properties"))
              (before
               .
               ((EFFORT . "1:00")
                (ENABLED)
                (LITERAL_T)
                (LITERAL_NIL)))
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
      (should (equal (alist-get 'properties_deleted result) []))
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
                  (before . ((TODO)))
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
                    (before . ((EFFORT)))
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
                (before . ((ID) (CUSTOM_ID)))
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
       '((OK . "1") ("A\nB" . "v")) '((OK) ("A\nB" . ""))))))

(ert-deftest org-mcp-test-set-properties-multiline-value ()
  "Test a line break in a property value refuses the call."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (org-mcp-test--call-set-properties-expecting-error
     test-file (org-mcp-test--file-link test-file "*Simple Task")
     '((FOO . "x\r* Injected heading")) '((FOO)))))

(ert-deftest org-mcp-test-set-properties-asserts-only-what-it-writes ()
  "A call names the one property it writes and leaves the other alone.
OWNER is a property the tool could write, and this call neither
asserts nor writes it, so it survives untouched."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-two-props))
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-set-properties"
             `((link
                .
                ,(org-mcp-test--file-link
                  test-file "*Task with Two Properties"))
               (before . ((EFFORT . "1:00")))
               (after . ((EFFORT . "3:00"))))))))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'properties_set result) ["EFFORT"]))
      (should (equal (alist-get 'properties_deleted result) []))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-set-properties-one-of-two))))

(ert-deftest org-mcp-test-set-properties-sends-empty-arrays-never-null ()
  "The half of the response a call does not fill arrives as [], not null.
`properties_set\=' and `properties_deleted\=' are both published as
arrays of names.  A call that only sets fills neither, and
`json-encode\=' writes an elisp nil as null, so each is built with
`vconcat\='.  A client reading the length of either reads the wire
text, which is what this pins."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-two-props))
    (let* ((link
            (org-mcp-test--file-link
             test-file "*Task with Two Properties"))
           (text
            (mcp-server-lib-ert-call-tool
             "org-node-set-properties"
             `((link . ,link)
               (before . ((EFFORT . "1:00")))
               (after . ((EFFORT . "3:00")))))))
      (should (string-match-p "\"properties_deleted\":\\[\\]" text))
      (should-not (string-match-p ":null" text))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-set-properties-one-of-two))))

(ert-deftest org-mcp-test-set-properties-before-mismatch-refuses ()
  "A property whose asserted value is stale refuses the whole call.
EFFORT is asserted correctly and OWNER is not.  Every assertion is
checked before the first write, so EFFORT is not written either."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-two-props))
    (org-mcp-test--call-tool-refused
     "org-node-set-properties"
     `((link
        .
        ,(org-mcp-test--file-link
          test-file "*Task with Two Properties"))
       (before . ((EFFORT . "1:00") (OWNER . "grace")))
       (after . ((EFFORT . "3:00") (OWNER . "ada"))))
     "\\`conflict: Property 'OWNER' mismatch: expected 'grace', \
found 'ada'\\'"
     test-file)))

(ert-deftest org-mcp-test-set-properties-null-before-asserts-absent ()
  "A null `before\=' asserts the property is not on the heading.
The refusal names the state it expected rather than showing it as an
empty value, because \"\" is the neighbouring state and a client has
to be able to tell which of the two its assertion missed."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (org-mcp-test--call-tool-refused
     "org-node-set-properties"
     `((link
        . ,(org-mcp-test--file-link test-file "*Task with Properties"))
       (before . ((EFFORT)))
       (after . ((EFFORT . "3:00"))))
     "\\`conflict: Property 'EFFORT' mismatch: expected (absent), \
found '1:00'\\'"
     test-file)))

(ert-deftest org-mcp-test-set-properties-empty-before-asserts-a-blank-line ()
  "An empty `before\=' asserts a line that carries nothing, not absence.
The heading holds EFFORT with a value, so the assertion is stale
either way; what this pins is which stale belief the refusal reports
back.  Its sibling above sends null against the same heading and is
told `(absent)\='."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (org-mcp-test--call-tool-refused
     "org-node-set-properties"
     `((link
        . ,(org-mcp-test--file-link test-file "*Task with Properties"))
       (before . ((EFFORT . "")))
       (after . ((EFFORT . "3:00"))))
     "\\`conflict: Property 'EFFORT' mismatch: expected '', \
found '1:00'\\'"
     test-file)))

(ert-deftest org-mcp-test-set-properties-before-names-every-write ()
  "A property the call writes and `before\=' omits refuses the call.
The write would destroy a value no one vouched for, which is the one
thing the assertion exists to stop."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-two-props))
    (org-mcp-test--call-tool-refused
     "org-node-set-properties"
     `((link
        .
        ,(org-mcp-test--file-link
          test-file "*Task with Two Properties"))
       (before . ((EFFORT . "1:00")))
       (after . ((EFFORT . "3:00") (OWNER . "grace"))))
     "\\`before does not name the property 'OWNER' this call \
writes\\'"
     test-file)))

(ert-deftest org-mcp-test-set-properties-before-names-nothing-else ()
  "A property `before\=' names and the call does not write refuses it.
Asserting a property the call leaves alone misstates what the call
can touch."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-two-props))
    (org-mcp-test--call-tool-refused
     "org-node-set-properties"
     `((link
        .
        ,(org-mcp-test--file-link
          test-file "*Task with Two Properties"))
       (before . ((EFFORT . "1:00") (OWNER . "ada")))
       (after . ((EFFORT . "3:00"))))
     "\\`before names the property 'OWNER', which this call does \
not write\\'"
     test-file)))

;;; Removing a property through org-node-set-properties

(ert-deftest org-mcp-test-set-properties-null-after-deletes ()
  "A null `after\=' value takes the property off the headline.
`before\=' names the value that goes with it, so the call says what it
destroys and the response records it.  Null is the deleting spelling
because it is the one state a line cannot be in: \"\" is a line
carrying nothing, which is a line."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (let* ((link (org-mcp-test--file-link test-file "*Task with Properties"))
           (params `((link . ,link)
                     (before . ((EFFORT . "1:00")))
                     (after . ((EFFORT)))))
           (result-text
            (mcp-server-lib-ert-call-tool
             "org-node-set-properties" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'properties_deleted result) ["EFFORT"]))
      (should (equal (alist-get 'properties_set result) []))
      (should
       (equal (alist-get 'before result) '((EFFORT . "1:00"))))
      (should (equal (alist-get 'link result) link))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-set-properties-delete))))

(ert-deftest org-mcp-test-set-properties-deletes-two-at-once ()
  "Both properties named go, and the response lists them in order."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-two-props))
    (let* ((link
            (org-mcp-test--file-link
             test-file "*Task with Two Properties"))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-set-properties"
              `((link . ,link)
                (before . ((EFFORT . "1:00") (OWNER . "ada")))
                (after . ((EFFORT) (OWNER))))))))
      (should (equal (alist-get 'success result) t))
      (should
       (equal (alist-get 'properties_deleted result)
              ["EFFORT" "OWNER"]))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-remove-properties-both))))

(ert-deftest org-mcp-test-set-properties-reports-both-values-deleted ()
  "A call that deletes two properties reports both values it destroyed.
Nothing in the file records them once the call returns, so the
response is where they exist, and it is read by more than the client
that sent the request: the values come back under `before\=', the key
a field setter reports what it destroyed under."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-two-props))
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-set-properties"
             `((link
                .
                ,(org-mcp-test--file-link
                  test-file "*Task with Two Properties"))
               (before . ((EFFORT . "1:00") (OWNER . "ada")))
               (after . ((EFFORT) (OWNER))))))))
      (should (equal (alist-get 'success result) t))
      (should
       (equal (alist-get 'before result)
              '((EFFORT . "1:00") (OWNER . "ada")))))))

(ert-deftest org-mcp-test-set-properties-delete-refuses-a-stale-before ()
  "A value the headline does not hold refuses the deletion."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (org-mcp-test--call-tool-refused
     "org-node-set-properties"
     `((link
        .
        ,(org-mcp-test--file-link test-file "*Task with Properties"))
       (before . ((EFFORT . "3:00")))
       (after . ((EFFORT))))
     "\\`conflict: Property 'EFFORT' mismatch: expected '3:00', \
found '1:00'\\'"
     test-file)))

(ert-deftest org-mcp-test-set-properties-delete-refuses-when-one-moved ()
  "Two properties deleted and one moved: neither is removed.
Every assertion is checked before the first property goes, so a call
refused over its second has not taken its first away."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-two-props))
    (org-mcp-test--call-tool-refused
     "org-node-set-properties"
     `((link
        .
        ,(org-mcp-test--file-link
          test-file "*Task with Two Properties"))
       (before . ((EFFORT . "1:00") (OWNER . "bob")))
       (after . ((EFFORT) (OWNER))))
     "\\`conflict: Property 'OWNER' mismatch: expected 'bob', \
found 'ada'\\'"
     test-file)))

(ert-deftest org-mcp-test-set-properties-delete-refuses-a-blank-before ()
  "A `before\=' naming no value refuses the deletion, in either spelling.
Asserting that a property holds nothing when it holds something is
the stale belief the guard exists to catch, and it matters most on
the call that would destroy it: nothing is removed either time, and
the refusal says which of the two blanks arrived."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (let ((link
           (org-mcp-test--file-link test-file "*Task with Properties")))
      (org-mcp-test--call-tool-refused
       "org-node-set-properties"
       `((link . ,link) (before . ((EFFORT))) (after . ((EFFORT))))
       "\\`conflict: Property 'EFFORT' mismatch: expected (absent), \
found '1:00'\\'"
       test-file)
      (org-mcp-test--call-tool-refused
       "org-node-set-properties"
       `((link . ,link) (before . ((EFFORT . ""))) (after . ((EFFORT))))
       "\\`conflict: Property 'EFFORT' mismatch: expected '', \
found '1:00'\\'"
       test-file))))

(ert-deftest org-mcp-test-a-null-after-takes-a-blank-line-away ()
  "Null takes away a line carrying nothing, and the removal is reported.
The `before\=' of \"\" asserts the line as the read returned it, and the
`after\=' of null asks for the state a line cannot be in.  It is the
only spelling that empties the drawer of the name, since \"\" would
put the line back where it stood."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-empty-property))
    (let* ((link
            (org-mcp-test--file-link test-file "*Simple Task"))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-set-properties"
              `((link . ,link)
                (before . ((EMPTY . "")))
                (after . ((EMPTY))))))))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'properties_deleted result) ["EMPTY"]))
      (should (equal (alist-get 'properties_set result) []))
      (should (equal (alist-get 'before result) '((EMPTY . ""))))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-empty-property-removed))))

(ert-deftest org-mcp-test-an-empty-after-writes-a-blank-line ()
  "An empty `after\=' puts a line in the drawer that carries no value.
The heading has no such property, so `before\=' is null; the call
writes `:BLANK:\=' and reports it set, because a line is what it put
there.  A read then returns it as \"\", which is the state this
spelling exists to reach."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-set-properties"
              `((link . ,link)
                (before . ((BLANK)))
                (after . ((BLANK . ""))))))))
      (should (equal (alist-get 'properties_set result) ["BLANK"]))
      (should (equal (alist-get 'properties_deleted result) []))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-blank-line-written)
      (should
       (equal
        (alist-get
         'properties
         (json-read-from-string
          (mcp-server-lib-ert-call-tool
           "org-node-read"
           `((link . ,link) (properties . ["BLANK"])))))
        '((BLANK . "")))))))

(ert-deftest org-mcp-test-read-tells-an-empty-property-from-an-absent-one ()
  "A drawer line carrying nothing is read; one the drawer lacks is not.
`properties\=' names what the node has, so an empty line arrives under
its name with \"\" and a name the drawer never carried arrives not at
all, even when the call asked for it.  That is the distinction a
`before\=' of \"\" cannot make, and it is why the write surface reads
the drawer rather than the assertion."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-empty-property))
    (let* ((link
            (org-mcp-test--file-link test-file "*Simple Task"))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-read"
              `((link . ,link)
                (fields . ["title"])
                (properties . ["EMPTY" "NEVER_THERE"]))))))
      (should
       (equal (alist-get 'properties result) '((EMPTY . "")))))))

(ert-deftest org-mcp-test-set-properties-delete-of-an-absent-property ()
  "Deleting a property that is not there is a no-op success.
The empty `before\=' asserts the headline holds none of it, which it
does, so the assertion holds: not a conflict, and not a write
either.  The response leaves the name out of both `properties_set\='
and `properties_deleted\=', which arrive empty, because nothing was
set and nothing was deleted, and it echoes the assertion under
`before\='."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((result
           (org-mcp-test--call-tool-leaving-file
            "org-node-set-properties"
            `((link
               .
               ,(org-mcp-test--file-link test-file "*Simple Task"))
              (before . ((OWNER)))
              (after . ((OWNER))))
            test-file)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'properties_deleted result) []))
      (should (equal (alist-get 'properties_set result) []))
      (should (equal (alist-get 'before result) '((OWNER))))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-bare-todo))))

(ert-deftest org-mcp-test-set-properties-delete-forbids-special ()
  "A special property has its own tool and is refused here too."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (org-mcp-test--call-tool-refused
     "org-node-set-properties"
     `((link . ,(org-mcp-test--file-link test-file "*Scheduled Task"))
       (before . ((SCHEDULED . "<2026-03-01 Sun>")))
       (after . ((SCHEDULED))))
     "\\`Cannot set special property 'SCHEDULED' - use the dedicated \
tool\\'"
     test-file)))

(ert-deftest org-mcp-test-a-blank-line-survives-a-call-that-passes-over-it ()
  "A property carrying nothing is untouched by a call that does not name it.
The drawer holds EMPTY and OWNER; the call writes OWNER and names
only OWNER.  A property a call does not name is none of its business,
and the blank line is the one most easily lost to a tool that reads
\"\" as absence, so this is where that is pinned."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-empty-property))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-set-properties"
              `((link . ,link)
                (before . ((OWNER . "ada")))
                (after . ((OWNER . "grace"))))))))
      (should (equal (alist-get 'properties_set result) ["OWNER"]))
      (should (equal (alist-get 'properties_deleted result) []))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-blank-line-kept))))

(ert-deftest org-mcp-test-a-blank-property-round-trips-read-assert-write ()
  "What a read hands back for a blank line is what a write puts back.
The three states go round the loop the guard is for: a read returns
EMPTY as \"\", that value is the `before\=' the next call asserts with,
and an `after\=' of \"\" leaves the line where it stood.  A tool that
read \"\" as absence would break this at the assertion; one that wrote
\"\" as a deletion would break it at the write."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-empty-property))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (read-back
            (alist-get
             'properties
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-read"
               `((link . ,link) (properties . ["EMPTY"])))))))
      (should (equal read-back '((EMPTY . ""))))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-set-properties"
               `((link . ,link)
                 (before . ((EMPTY . ,(alist-get 'EMPTY read-back))))
                 (after . ((EMPTY . ,(alist-get 'EMPTY read-back)))))))))
        (should (equal (alist-get 'properties_set result) ["EMPTY"]))
        (should (equal (alist-get 'properties_deleted result) []))
        (should (equal (alist-get 'before result) '((EMPTY . ""))))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-blank-line-intact)))))

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
                 (before . ((FIRST) (SECOND)))
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
               (before . ((FIRST) (SECOND)))
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
                 (before . ((FIRST) (SECOND)))
                 (after . ((FIRST . "1") (SECOND . "2"))))
               "Property hook failed" test-file))
            (let ((result
                   (json-read-from-string
                    (mcp-server-lib-ert-call-tool
                     "org-node-set-properties"
                     `((link . ,link)
                       (before . ((FIRST)))
                       (after . ((FIRST . "1"))))))))
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
               (before . ((FIRST)))
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
               (before . ((FIRST) (SECOND)))
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
               (before . ((FIRST) (SECOND)))
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
             `((link . ,link)
               (before . ((FIRST)))
               (after . ((FIRST . "1"))))
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
                     `((link . ,link)
                       (before . ((SECOND)))
                       (after . ((SECOND . "2"))))))))
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
                        (before . ((FIRST)))
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
    (let ((org-clock-out-hook (list #'save-buffer)))
      (org-mcp-test--with-dirty-buffer (_buffer _on-disk) file-1
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
         file-1 org-mcp-test--dirty-clock-closed-regex)
        (org-mcp-test--verify-served-matches
         file-1 org-mcp-test--dirty-clock-closed-regex)
        (org-mcp-test--verify-no-modified-buffer file-1)))))

;;; Tests for org-node-set-scheduled

(ert-deftest org-mcp-test-update-scheduled-set ()
  "Test setting SCHEDULED on entry without one."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (params `((link . ,link)
                     (before . "")
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
                     (before . "<2026-03-01 Sun>")
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
                  (before . "")
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
                    (before . "")
                    (after . "2026-03-27")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-node-set-scheduled" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

(ert-deftest org-mcp-test-set-scheduled-before-is-the-raw-org-timestamp ()
  "`before\=' is the raw Org SCHEDULED, repeater and delay included.
The string a read hands back is the string the assertion takes, so a
repeating entry is rescheduled without the client reconstructing
anything.  Org carries the repeater on to the new date."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-repeating-scheduled))
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-set-scheduled"
             `((link
                .
                ,(org-mcp-test--file-link test-file "*Repeating Task"))
               (before . "<2026-06-20 Sat +1w -3d>")
               (after . "2026-06-27"))))))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should
       (equal (alist-get 'before result) "<2026-06-20 Sat +1w -3d>"))
      (should
       (string-match-p
        "\\`<2026-06-27 [^ ]+ \\+1w -3d>\\'"
        (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-repeating-scheduled-moved))))

(ert-deftest org-mcp-test-set-scheduled-refuses-iso-shorthand-in-before ()
  "`before\=' compares as the stored Org string, never the ISO shorthand.
The same date written the way `after\=' takes it is not what the file
holds, and org-mcp says so rather than accepting a second spelling:
comparing an input format against a stored one manufactures conflicts
on headings nobody touched."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-repeating-scheduled))
    (org-mcp-test--call-tool-refused
     "org-node-set-scheduled"
     `((link . ,(org-mcp-test--file-link test-file "*Repeating Task"))
       (before . "2026-06-20")
       (after . "2026-06-27"))
     "\\`conflict: SCHEDULED mismatch: expected '2026-06-20', \
found '<2026-06-20 Sat \\+1w -3d>'\\'"
     test-file)))

(ert-deftest org-mcp-test-set-scheduled-before-mismatch-refuses ()
  "A SCHEDULED the heading does not carry refuses the call."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (org-mcp-test--call-tool-refused
     "org-node-set-scheduled"
     `((link . ,(org-mcp-test--file-link test-file "*Scheduled Task"))
       (before . "<2026-03-08 Sun>")
       (after . "2026-04-15"))
     "\\`conflict: SCHEDULED mismatch: expected '<2026-03-08 Sun>', \
found '<2026-03-01 Sun>'\\'"
     test-file)))

(ert-deftest org-mcp-test-set-scheduled-empty-before-asserts-none ()
  "An empty `before\=' asserts the heading carries no SCHEDULED.
It is a value the assertion takes, never a parameter the call left
out, so a heading that does carry one refuses the write."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (org-mcp-test--call-tool-refused
     "org-node-set-scheduled"
     `((link . ,(org-mcp-test--file-link test-file "*Scheduled Task"))
       (before . "")
       (after . "2026-04-15"))
     "\\`conflict: SCHEDULED mismatch: expected '', \
found '<2026-03-01 Sun>'\\'"
     test-file)))

;;; Removing SCHEDULED through org-node-set-scheduled

(ert-deftest org-mcp-test-set-scheduled-null-after-takes-it-off ()
  "A null `after\=' takes the SCHEDULED timestamp away.
`before\=' is the timestamp destroyed and the response reports it,
because the response is the only record the call leaves of what was
there."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (let* ((link (org-mcp-test--file-link test-file "*Scheduled Task"))
           (params `((link . ,link)
                     (before . "<2026-03-01 Sun>")
                     (after)))
           (result-text
            (mcp-server-lib-ert-call-tool
             "org-node-set-scheduled" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'before result) "<2026-03-01 Sun>"))
      (should (equal (alist-get 'after result) ""))
      (should (equal (alist-get 'link result) link))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-remove))))

(ert-deftest org-mcp-test-set-scheduled-null-after-refuses-stale-before ()
  "A SCHEDULED the headline does not carry refuses the removal.
The call names the timestamp it destroys, so a belief that has gone
stale is a conflict: read the headline again and decide afresh."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (org-mcp-test--call-tool-refused
     "org-node-set-scheduled"
     `((link . ,(org-mcp-test--file-link test-file "*Scheduled Task"))
       (before . "<2026-03-08 Sun>")
       (after))
     "\\`conflict: SCHEDULED mismatch: expected '<2026-03-08 Sun>', \
found '<2026-03-01 Sun>'\\'"
     test-file)))

(ert-deftest org-mcp-test-set-scheduled-null-after-on-a-headline-without-one ()
  "A null `after\=' on a headline carrying no SCHEDULED writes nothing.
The empty `before\=' asserts the headline carries none, which it
does, so the assertion holds and the call is accepted with nothing
to do."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((result
           (org-mcp-test--call-tool-leaving-file
            "org-node-set-scheduled"
            `((link
               .
               ,(org-mcp-test--file-link test-file "*Simple Task"))
              (before . "")
              (after))
            test-file)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'before result) ""))
      (should (equal (alist-get 'after result) ""))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-bare-todo))))

(ert-deftest org-mcp-test-set-scheduled-empty-after-is-no-date ()
  "\"\" is no date and is refused as one; false is left out.
Null is the one spelling that takes a value away, because null is
JSON's word for no value.  An empty string is a value, and this
field has none — so it reaches the field's own check and is refused
there, naming what the field does accept.  False and [] are what a
client fills a parameter it is not using with, and are refused as
the parameter left out."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (let ((link (org-mcp-test--file-link test-file "*Scheduled Task")))
      (org-mcp-test--call-tool-refused
       "org-node-set-scheduled"
       `((link . ,link) (before . "<2026-03-01 Sun>") (after . ""))
       "\\`Invalid date '' - expected 2026-03-27, \
2026-03-27 09:00, or an Org timestamp"
       test-file)
      (dolist (blank '(:json-false []))
        (org-mcp-test--call-tool-refused
         "org-node-set-scheduled"
         `((link . ,link) (before . "<2026-03-01 Sun>") (after . ,blank))
         "\\`Missing required parameter: after\\'"
         test-file)))))

;;; Tests for org-node-set-deadline

(ert-deftest org-mcp-test-update-deadline-set ()
  "Test setting DEADLINE on entry without one."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (params `((link . ,link)
                     (before . "")
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
                     (before . "<2026-03-15 Sun>")
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
                  (before . "")
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
                    (before . "")
                    (after . "2026-03-27")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-node-set-deadline" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

(ert-deftest org-mcp-test-set-deadline-before-mismatch-refuses ()
  "A DEADLINE the heading does not carry refuses the call."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (org-mcp-test--call-tool-refused
     "org-node-set-deadline"
     `((link . ,(org-mcp-test--file-link test-file "*Deadline Task"))
       (before . "<2026-03-22 Sun>")
       (after . "2026-04-15"))
     "\\`conflict: DEADLINE mismatch: expected '<2026-03-22 Sun>', \
found '<2026-03-15 Sun>'\\'"
     test-file)))

(ert-deftest org-mcp-test-set-deadline-empty-before-asserts-none ()
  "An empty `before\=' asserts the heading carries no DEADLINE."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (org-mcp-test--call-tool-refused
     "org-node-set-deadline"
     `((link . ,(org-mcp-test--file-link test-file "*Deadline Task"))
       (before . "")
       (after . "2026-04-15"))
     "\\`conflict: DEADLINE mismatch: expected '', \
found '<2026-03-15 Sun>'\\'"
     test-file)))

;;; Removing DEADLINE through org-node-set-deadline

(ert-deftest org-mcp-test-set-deadline-null-after-takes-it-off ()
  "A null `after\=' takes the DEADLINE timestamp away.
`before\=' is the timestamp destroyed and the response reports it,
because the response is the only record the call leaves of what was
there."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (let* ((link (org-mcp-test--file-link test-file "*Deadline Task"))
           (params `((link . ,link)
                     (before . "<2026-03-15 Sun>")
                     (after)))
           (result-text
            (mcp-server-lib-ert-call-tool
             "org-node-set-deadline" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'before result) "<2026-03-15 Sun>"))
      (should (equal (alist-get 'after result) ""))
      (should (equal (alist-get 'link result) link))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-deadline-remove))))

(ert-deftest org-mcp-test-set-deadline-null-after-refuses-stale-before ()
  "A DEADLINE the headline does not carry refuses the removal.
The call names the timestamp it destroys, so a belief that has gone
stale is a conflict: read the headline again and decide afresh."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (org-mcp-test--call-tool-refused
     "org-node-set-deadline"
     `((link . ,(org-mcp-test--file-link test-file "*Deadline Task"))
       (before . "<2026-03-08 Sun>")
       (after))
     "\\`conflict: DEADLINE mismatch: expected '<2026-03-08 Sun>', \
found '<2026-03-15 Sun>'\\'"
     test-file)))

(ert-deftest org-mcp-test-set-deadline-null-after-on-a-headline-without-one ()
  "A null `after\=' on a headline carrying no DEADLINE writes nothing.
The empty `before\=' asserts the headline carries none, which it
does, so the assertion holds and the call is accepted with nothing
to do."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((result
           (org-mcp-test--call-tool-leaving-file
            "org-node-set-deadline"
            `((link
               .
               ,(org-mcp-test--file-link test-file "*Simple Task"))
              (before . "")
              (after))
            test-file)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'before result) ""))
      (should (equal (alist-get 'after result) ""))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-bare-todo))))

(ert-deftest org-mcp-test-set-deadline-empty-after-is-no-date ()
  "\"\" is no date and is refused as one; false is left out.
Null is the one spelling that takes a value away, because null is
JSON's word for no value.  An empty string is a value, and this
field has none — so it reaches the field's own check and is refused
there, naming what the field does accept.  False and [] are what a
client fills a parameter it is not using with, and are refused as
the parameter left out."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (let ((link (org-mcp-test--file-link test-file "*Deadline Task")))
      (org-mcp-test--call-tool-refused
       "org-node-set-deadline"
       `((link . ,link) (before . "<2026-03-15 Sun>") (after . ""))
       "\\`Invalid date '' - expected 2026-03-27, \
2026-03-27 09:00, or an Org timestamp"
       test-file)
      (dolist (blank '(:json-false []))
        (org-mcp-test--call-tool-refused
         "org-node-set-deadline"
         `((link . ,link) (before . "<2026-03-15 Sun>") (after . ,blank))
         "\\`Missing required parameter: after\\'"
         test-file)))))

;;; A date parameter is a timestamp Org parses
;;
;; `after' is validated by Org's own parser, so what it accepts is the
;; Org timestamp vocabulary a read already speaks: the ISO shorthand,
;; a repeater, a warning period, and the raw string a read returns.
;; What Org's parser reads as another date than the one written is
;; refused before the link is resolved, so the file is untouched.

(defconst org-mcp-test--content-repeating-deadline
  "* TODO Repeating Deadline
DEADLINE: <2026-06-20 Sat ++1m -2d>
Task body."
  "TODO task whose DEADLINE carries a repeater and a warning period.")

(defconst org-mcp-test--pattern-scheduled-with-repeater
  (concat
   "\\`\\* TODO Simple Task\n"
   "SCHEDULED: <2026-03-27 [^ >]+ \\+1w>\n"
   "Task body text\\.\n?\\'")
  "Pattern after a SCHEDULED carrying a repeater is written.")

(defconst org-mcp-test--pattern-scheduled-with-warning
  (concat
   "\\`\\* TODO Simple Task\n"
   "SCHEDULED: <2026-03-27 [^ >]+ -3d>\n"
   "Task body text\\.\n?\\'")
  "Pattern after a SCHEDULED carrying a warning period is written.")

(defconst org-mcp-test--pattern-deadline-with-repeater-and-warning
  (concat
   "\\`\\* TODO Simple Task\n"
   "DEADLINE: <2026-03-27 [^ >]+ 09:00 \\.\\+2d -1d>\n"
   "Task body text\\.\n?\\'")
  "Pattern after a DEADLINE carrying both a repeater and a warning.")

(defconst org-mcp-test--pattern-scheduled-round-tripped
  (concat
   "\\`\\* TODO Repeating Task\n"
   "SCHEDULED: <2026-06-20 [^ >]+ \\+1w -3d>\n"
   "Task body\\.\n?\\'")
  "Pattern after the string a read returned is sent back as `after'.
The heading carries the timestamp it carried before the call.")

(defconst org-mcp-test--pattern-repeating-deadline-moved
  (concat
   "\\`\\* TODO Repeating Deadline\n"
   "DEADLINE: <2026-07-20 [^ >]+ \\+\\+1m -2d>\n"
   "Task body\\.\n?\\'")
  "Pattern after a date-only change to a DEADLINE carrying a repeater.
The repeater and the warning period stand as they were.")

(defconst org-mcp-test--pattern-leap-day-deadline
  (concat
   "\\`\\* TODO Simple Task\n"
   "DEADLINE: <2028-02-29 [^ >]+>\n"
   "Task body text\\.\n?\\'")
  "Pattern after the leap day of a leap year is written.")

(defconst org-mcp-test--pattern-scheduled-past-2037
  (concat
   "\\`\\* TODO Simple Task\n"
   "SCHEDULED: <2050-06-15 [^ >]+>\n"
   "Task body text\\.\n?\\'")
  "Pattern after a date beyond the 32-bit era is written.")

(ert-deftest org-mcp-test-set-deadline-writes-a-real-leap-day ()
  "The leap day of a leap year is a date, and it is written.
The check asks Org whether the day exists, not whether February has
twenty-nine days in general."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-set-deadline"
             `((link
                .
                ,(org-mcp-test--file-link test-file "*Simple Task"))
               (before . "")
               (after . "2028-02-29"))))))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'before result) ""))
      (should
       (string-match-p "\\`<2028-02-29 [^ >]+>\\'"
                       (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-leap-day-deadline))))

(ert-deftest org-mcp-test-set-scheduled-refuses-a-two-digit-year ()
  "A year below 100 is refused: Org reads it as a two-digit year.
`0000-01-01' names the first day of year zero and Org's date reader
answers with the year 2000, so the value cannot be written as
written."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (org-mcp-test--call-tool-refused
     "org-node-set-scheduled"
     `((link . ,(org-mcp-test--file-link test-file "*Simple Task"))
       (before . "")
       (after . "0000-01-01"))
     "\\`Date '0000-01-01' has a year below 100, \
which Org reads as a two-digit year\\'"
     test-file)
    (org-mcp-test--verify-file-matches
     test-file org-mcp-test--pattern-bare-todo)))

(ert-deftest org-mcp-test-set-scheduled-writes-a-repeater ()
  "A repeater is written, so a repeating task can be created.
The response reports the timestamp the heading ends up carrying,
repeater included."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-set-scheduled"
             `((link
                .
                ,(org-mcp-test--file-link test-file "*Simple Task"))
               (before . "")
               (after . "2026-03-27 +1w"))))))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should
       (string-match-p "\\`<2026-03-27 [^ >]+ \\+1w>\\'"
                       (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-with-repeater))))

(ert-deftest org-mcp-test-set-scheduled-writes-a-warning-period ()
  "A warning period is written on its own, with no repeater beside it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-set-scheduled"
             `((link
                .
                ,(org-mcp-test--file-link test-file "*Simple Task"))
               (before . "")
               (after . "2026-03-27 -3d"))))))
      (should (equal (alist-get 'success result) t))
      (should
       (string-match-p "\\`<2026-03-27 [^ >]+ -3d>\\'"
                       (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-with-warning))))

(ert-deftest org-mcp-test-set-deadline-writes-a-repeater-and-a-warning ()
  "A repeater and a warning period are written together, beside a time.
Each of Org's three repeater forms is a repeater; this one is the
restart form `.+'."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-set-deadline"
             `((link
                .
                ,(org-mcp-test--file-link test-file "*Simple Task"))
               (before . "")
               (after . "2026-03-27 09:00 .+2d -1d"))))))
      (should (equal (alist-get 'success result) t))
      (should
       (string-match-p "\\`<2026-03-27 [^ >]+ 09:00 \\.\\+2d -1d>\\'"
                       (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--pattern-deadline-with-repeater-and-warning))))

(ert-deftest org-mcp-test-set-scheduled-round-trips-what-a-read-returns ()
  "The raw Org string a read returns is a value `after\=' takes.
A client that read a repeating SCHEDULED can send it back unchanged
— to restore it, or to write it on another heading — without taking
the string apart first."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-repeating-scheduled))
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-set-scheduled"
             `((link
                .
                ,(org-mcp-test--file-link test-file "*Repeating Task"))
               (before . "<2026-06-20 Sat +1w -3d>")
               (after . "<2026-06-20 Sat +1w -3d>"))))))
      (should (equal (alist-get 'success result) t))
      (should
       (equal (alist-get 'before result) "<2026-06-20 Sat +1w -3d>"))
      (should
       (string-match-p "\\`<2026-06-20 [^ >]+ \\+1w -3d>\\'"
                       (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-round-tripped))))

(ert-deftest org-mcp-test-set-deadline-date-only-change-keeps-the-repeater ()
  "Moving the date of a repeating DEADLINE leaves its repeater alone.
`after\=' names a date and nothing else, and the repeater and the
warning period the heading carried are carried to it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-repeating-deadline))
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-set-deadline"
             `((link
                .
                ,(org-mcp-test--file-link
                  test-file "*Repeating Deadline"))
               (before . "<2026-06-20 Sat ++1m -2d>")
               (after . "2026-07-20"))))))
      (should (equal (alist-get 'success result) t))
      (should
       (equal (alist-get 'before result) "<2026-06-20 Sat ++1m -2d>"))
      (should
       (string-match-p "\\`<2026-07-20 [^ >]+ \\+\\+1m -2d>\\'"
                       (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-repeating-deadline-moved))))

(ert-deftest org-mcp-test-set-scheduled-refuses-a-date-range ()
  "A date range is read and asserted but never written.
Org\='s planning writer keeps the first half of a range and drops the
second, so a range in `after\=' is refused rather than written short."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (org-mcp-test--call-tool-refused
     "org-node-set-scheduled"
     `((link . ,(org-mcp-test--file-link test-file "*Scheduled Task"))
       (before . "<2026-03-01 Sun>")
       (after . "<2026-06-20 Sat>--<2026-06-21 Sun>"))
     "\\`Date '<2026-06-20 Sat>--<2026-06-21 Sun>' is a date range - \
name the one date the field is to carry\\'"
     test-file)))

(ert-deftest org-mcp-test-set-scheduled-refuses-an-inactive-timestamp ()
  "An inactive timestamp is refused rather than written as an active one.
A SCHEDULED and a DEADLINE carry an active timestamp; Org would take
the brackets off silently, writing something other than what the
call sent."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (org-mcp-test--call-tool-refused
     "org-node-set-scheduled"
     `((link . ,(org-mcp-test--file-link test-file "*Simple Task"))
       (before . "")
       (after . "[2026-03-27 Fri]"))
     "\\`Date '\\[2026-03-27 Fri\\]' is an inactive timestamp - \
SCHEDULED and DEADLINE carry an active one, written <\\.\\.\\.>\\'"
     test-file)))

(ert-deftest org-mcp-test-set-scheduled-refuses-text-after-the-timestamp ()
  "Text Org does not read as part of the timestamp refuses the call.
Org reads the timestamp and stops; accepting the value would drop
the rest of it without saying so."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (org-mcp-test--call-tool-refused
     "org-node-set-scheduled"
     `((link . ,(org-mcp-test--file-link test-file "*Simple Task"))
       (before . "")
       (after . "<2026-03-27 Fri> and then some"))
     "\\`Invalid date '<2026-03-27 Fri> and then some' - expected"
     test-file)))

(ert-deftest org-mcp-test-set-scheduled-refuses-a-relative-date ()
  "A repeater with no date under it is refused, never read as a date.
`org-read-date' answers `+1w' with a date a week from today, so a
check that reached it would write a plausible wrong date under a
success.  Org\\='s timestamp parser reads no timestamp here at all,
and the value never gets that far."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (dolist (relative '("+1w" "-3d" "++1m" ".+2d" "+1w -3d"))
        (org-mcp-test--call-tool-refused
         "org-node-set-scheduled"
         `((link . ,link) (before . "") (after . ,relative))
         (format "\\`Invalid date '%s' - expected"
                 (regexp-quote relative))
         test-file))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-bare-todo))))

(ert-deftest org-mcp-test-set-scheduled-reads-seconds-without-recording-them ()
  "Seconds are read and not recorded, as the clock tools read them.
An Org timestamp has no seconds field, so a value carrying them is
not refused and not rounded on: the minute it names is the minute
written, and the response says which."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-set-scheduled"
             `((link
                .
                ,(org-mcp-test--file-link test-file "*Simple Task"))
               (before . "")
               (after . "2026-03-27 09:00:33"))))))
      (should (equal (alist-get 'success result) t))
      (should
       (string-match-p "\\`<2026-03-27 [^ >]+ 09:00>\\'"
                       (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file
       (concat
        "\\`\\* TODO Simple Task\n"
        "SCHEDULED: <2026-03-27 [^ >]+ 09:00>\n"
        "Task body text\\.\n?\\'")))))

(ert-deftest org-mcp-test-set-scheduled-writes-a-date-past-2037 ()
  "A date beyond the 32-bit era is written as it was sent.
Org\='s date reader pulls a year outside 1970-2037 into that range,
which would land the write thirteen years early; org-mcp writes the
year the call named."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-set-scheduled"
             `((link
                .
                ,(org-mcp-test--file-link test-file "*Simple Task"))
               (before . "")
               (after . "2050-06-15"))))))
      (should (equal (alist-get 'success result) t))
      (should
       (string-match-p "\\`<2050-06-15 [^ >]+>\\'"
                       (alist-get 'after result)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-past-2037))))

(defconst org-mcp-test--pattern-scheduled-update-logged
  (concat
   "\\`\\* TODO Scheduled Task\n"
   "SCHEDULED: <2026-04-15 [^>]+>\n"
   ":LOGBOOK:\n"
   "- Rescheduled from \"\\[2026-03-01 [^]]+\\]\" on \\[[^]]+\\]\n"
   ":END:\n"
   "Task body\\.\n?\\'")
  "Pattern after a SCHEDULED move `org-log-reschedule' records.")

(defconst org-mcp-test--pattern-scheduled-remove-logged
  (concat
   "\\`\\* TODO Scheduled Task\n"
   ":LOGBOOK:\n"
   "- Not scheduled, was \"\\[2026-03-01 [^]]+\\]\" on \\[[^]]+\\]\n"
   ":END:\n"
   "Task body\\.\n?\\'")
  "Pattern after a SCHEDULED removal `org-log-reschedule' records.")

(defconst org-mcp-test--pattern-deadline-update-logged
  (concat
   "\\`\\* TODO Deadline Task\n"
   "DEADLINE: <2026-04-15 [^>]+>\n"
   ":LOGBOOK:\n"
   "- New deadline from \"\\[2026-03-15 [^]]+\\]\" on \\[[^]]+\\]\n"
   ":END:\n"
   "Task body\\.\n?\\'")
  "Pattern after a DEADLINE move `org-log-redeadline' records.")

(defconst org-mcp-test--pattern-deadline-remove-logged
  (concat
   "\\`\\* TODO Deadline Task\n"
   ":LOGBOOK:\n"
   "- Removed deadline, was \"\\[2026-03-15 [^]]+\\]\" on \\[[^]]+\\]\n"
   ":END:\n"
   "Task body\\.\n?\\'")
  "Pattern after a DEADLINE removal `org-log-redeadline' records.")

(defun org-mcp-test--call-set-scheduled (file before after)
  "Move the SCHEDULED of the Scheduled Task in FILE from BEFORE to AFTER.
Returns the tool's parsed response."
  (json-read-from-string
   (mcp-server-lib-ert-call-tool
    "org-node-set-scheduled"
    `((link . ,(org-mcp-test--file-link file "*Scheduled Task"))
      (before . ,before)
      (after . ,after)))))

(defun org-mcp-test--call-set-deadline (file before after)
  "Move the DEADLINE of the Deadline Task in FILE from BEFORE to AFTER.
Returns the tool's parsed response."
  (json-read-from-string
   (mcp-server-lib-ert-call-tool
    "org-node-set-deadline"
    `((link . ,(org-mcp-test--file-link file "*Deadline Task"))
      (before . ,before)
      (after . ,after)))))

;;; A note the user is typing is none of a write's business

;; Org's own note prompt is one buffer, `*Org Note*', and one set of
;; `org-log-note-*' variables: the marker saying where the entry goes,
;; the purpose saying what it is, the states it names.  They belong to
;; whoever is typing.  A write here writes an entry of its own, so
;; every one of them is this call's own, and what the user has in
;; flight comes through a write untouched — the text they typed, the
;; place their entry is going, and the hook that will ask them for it.

(defconst org-mcp-test--users-note-text
  "Half a sentence the user is still"
  "Text a user has typed into Org's note prompt and not finished.")

(defun org-mcp-test--arm-users-log-note (file heading)
  "Open Org's note prompt on HEADING of FILE, as Org opens it.
Leaves `*Org Note*' holding `org-mcp-test--users-note-text', the
`org-log-note-*' variables pointing at that heading, and
`org-add-log-note' on `post-command-hook' — the state a user is in
between typing into the prompt and finishing it.

Returns what their entry is: the buffer and position it is going to,
and the purpose saying what it says."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (search-forward heading)
    (org-back-to-heading t)
    (org-add-log-setup 'note nil nil 'note))
  (with-current-buffer (get-buffer-create "*Org Note*")
    (erase-buffer)
    (insert org-mcp-test--users-note-text))
  (list (marker-buffer org-log-note-marker)
        (marker-position org-log-note-marker)
        org-log-note-purpose))

(defun org-mcp-test--disarm-users-log-note ()
  "Put the global log-note state back, whatever a test left in it.
These are Org's own globals, shared with every other test in the
process, so a test that arms a note puts it away again."
  (remove-hook 'post-command-hook #'org-add-log-note)
  (setq org-log-setup nil
        org-log-note-purpose nil
        org-log-note-state nil
        org-log-note-previous-state nil)
  (set-marker org-log-note-marker nil)
  (when-let* ((buffer (get-buffer "*Org Note*")))
    (kill-buffer buffer)))

(defun org-mcp-test--should-keep-users-log-note (armed)
  "Assert the note the user had in flight came through the call intact.
ARMED is what `org-mcp-test--arm-users-log-note' returned."
  (let ((buffer (get-buffer "*Org Note*")))
    (should buffer)
    (should
     (equal
      (with-current-buffer buffer (buffer-string))
      org-mcp-test--users-note-text)))
  ;; The entry is still theirs: where it goes and what it says.
  (should
   (equal
    (list (marker-buffer org-log-note-marker)
          (marker-position org-log-note-marker)
          org-log-note-purpose)
    armed))
  ;; And the hook that will ask them for it when they next act.
  (should org-log-setup)
  (should (memq 'org-add-log-note post-command-hook)))

(ert-deftest org-mcp-test-a-logged-write-keeps-the-note-being-typed ()
  "A write that records an entry leaves the user's half-typed one alone.
Org's note prompt is one buffer and one set of variables; a write
that took them over would erase what the user typed, and their
finishing key would write the server's entry instead of theirs.  The
write still records what `org-log-done' asked for."
  (org-mcp-test--with-temp-org-files
      ((test-file "* TODO Task One\nTask description."))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-log-done 'note)
          (org-log-into-drawer t))
      (let ((marker
             (org-mcp-test--arm-users-log-note test-file "Task One")))
        (unwind-protect
            (progn
              (let ((result
                     (org-mcp-test--call-update-todo-state
                      (org-mcp-test--file-link test-file "*Task One")
                      "DONE" "TODO")))
                (should (equal (alist-get 'success result) t))
                (should (equal (alist-get 'before result) "TODO"))
                (should (equal (alist-get 'after result) "DONE")))
              (org-mcp-test--should-keep-users-log-note marker)
              (org-mcp-test--verify-file-matches
               test-file
               org-mcp-test--pattern-task-one-closing-note))
          (org-mcp-test--disarm-users-log-note))))))

(ert-deftest org-mcp-test-a-note-of-ones-own-keeps-the-note-being-typed ()
  "org-node-add-note leaves a note the user is typing alone as well.
It is the tool that reached Org's note machinery before any other,
and it writes its entry the same way every write now does."
  (org-mcp-test--with-temp-org-files
      ((test-file "* TODO Task One\nTask description."))
    (let ((org-log-into-drawer t))
      (let ((marker
             (org-mcp-test--arm-users-log-note test-file "Task One")))
        (unwind-protect
            (progn
              (let ((result
                     (json-read-from-string
                      (mcp-server-lib-ert-call-tool
                       "org-node-add-note"
                       `((link
                          .
                          ,(org-mcp-test--file-link
                            test-file "*Task One"))
                         (note . "The server's own note."))))))
                (should (equal (alist-get 'success result) t))
                (should (eq (alist-get 'saved result) t)))
              (org-mcp-test--should-keep-users-log-note marker)
              (should
               (string-match-p
                "The server's own note\\."
                (org-mcp-test--read-file test-file))))
          (org-mcp-test--disarm-users-log-note))))))

(ert-deftest org-mcp-test-a-clock-out-note-keeps-the-note-being-typed ()
  "A clock-out carrying prose leaves a note the user is typing alone.
It is the write that puts the most of its own into Org's note
machinery -- prose, purpose and place -- so it is the one that would
take the user's over."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-with-open-clock))
    (let ((org-log-note-clock-out t)
          (org-log-into-drawer t))
      (org-mcp-test--with-session-clock test-file
        (let ((marker
               (org-mcp-test--arm-users-log-note test-file "Task One")))
          (unwind-protect
              (let ((link
                     (org-mcp-test--file-link test-file "*Task One")))
                (org-mcp-test--should-report-the-closed-hour
                 (org-mcp-test--call-clock-out
                  link "2026-01-01T11:00:00"
                  org-mcp-test--clock-out-prose)
                 link)
                (org-mcp-test--should-keep-users-log-note marker)
                (org-mcp-test--verify-file-matches
                 test-file org-mcp-test--clock-out-with-note-regex))
            (org-mcp-test--disarm-users-log-note)))))))

(ert-deftest org-mcp-test-a-planning-write-keeps-the-note-being-typed ()
  "The planning setters leave a note the user is typing alone too.
`org-log-reschedule' takes a write through the same machinery, on a
removal as much as on a move."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (let ((org-log-reschedule 'note)
          (org-log-into-drawer t))
      (let ((marker
             (org-mcp-test--arm-users-log-note
              test-file "Scheduled Task")))
        (unwind-protect
            (progn
              (let ((result
                     (org-mcp-test--call-set-scheduled
                      test-file "<2026-03-01 Sun>" nil)))
                (should (equal (alist-get 'success result) t))
                (should
                 (equal (alist-get 'before result) "<2026-03-01 Sun>"))
                (should (equal (alist-get 'after result) "")))
              (org-mcp-test--should-keep-users-log-note marker)
              (org-mcp-test--verify-file-matches
               test-file
               org-mcp-test--pattern-scheduled-remove-logged))
          (org-mcp-test--disarm-users-log-note))))))

(defun org-mcp-test--should-leave-no-log-prompt ()
  "Assert no log note is left waiting for a command loop that will not come.
`*Org Note*' is Org's own prompt buffer, which a write never opens
and never borrows; the other is the buffer a write puts its own prose
in, which `org-store-log-note' kills as it reads it."
  (should-not (memq 'org-add-log-note post-command-hook))
  (should-not (get-buffer "*Org Note*"))
  (should-not
   (seq-find
    (lambda (buffer)
      (string-prefix-p " *org-mcp-log-note*" (buffer-name buffer)))
    (buffer-list))))

(ert-deftest org-mcp-test-set-scheduled-logs-the-move-without-asking ()
  "`org-log-reschedule' set to `note' records the move and waits for no one.
Org's own route to the entry arms `post-command-hook' and opens an
`*Org Note*' buffer for a person to type in, which an MCP call has
nobody to finish.  The call returns, the hook is not armed, no note
buffer is left behind, and the entry written is the one the `time'
setting writes: a heading line with no note body under it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (let ((org-log-reschedule 'note)
          (org-log-into-drawer t))
      (let ((result
             (org-mcp-test--call-set-scheduled
              test-file "<2026-03-01 Sun>" "2026-04-15")))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'before result) "<2026-03-01 Sun>"))
        (should
         (string-match-p "\\`<2026-04-15 " (alist-get 'after result))))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-update-logged))))

(ert-deftest org-mcp-test-set-scheduled-logs-nothing-when-unset ()
  "`org-log-reschedule' unset leaves the move unrecorded.
A user who does not log reschedules gets no entry from org-mcp
either: the setting is the whole decision."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (let ((org-log-reschedule nil)
          (org-log-into-drawer t))
      (let ((result
             (org-mcp-test--call-set-scheduled
              test-file "<2026-03-01 Sun>" "2026-04-15")))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'before result) "<2026-03-01 Sun>"))
        (should
         (string-match-p "\\`<2026-04-15 " (alist-get 'after result))))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-update))))

(ert-deftest org-mcp-test-set-scheduled-null-after-logs-the-removal ()
  "`org-log-reschedule' records the removal a null `after\=' makes.
The entry names the timestamp destroyed, which is the record Org
writes when a person takes a SCHEDULED off by hand."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (let ((org-log-reschedule 'note)
          (org-log-into-drawer t))
      (let ((result
             (org-mcp-test--call-set-scheduled
              test-file "<2026-03-01 Sun>" nil)))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'before result) "<2026-03-01 Sun>"))
        (should (equal (alist-get 'after result) "")))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-remove-logged))))

(ert-deftest org-mcp-test-set-scheduled-null-after-logs-nothing-when-unset ()
  "`org-log-reschedule' unset leaves the removal unrecorded."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (let ((org-log-reschedule nil)
          (org-log-into-drawer t))
      (let ((result
             (org-mcp-test--call-set-scheduled
              test-file "<2026-03-01 Sun>" nil)))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'before result) "<2026-03-01 Sun>"))
        (should (equal (alist-get 'after result) "")))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-scheduled-remove))))

(ert-deftest org-mcp-test-set-deadline-logs-the-move-without-asking ()
  "`org-log-redeadline' set to `note' records the move and waits for no one."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (let ((org-log-redeadline 'note)
          (org-log-into-drawer t))
      (let ((result
             (org-mcp-test--call-set-deadline
              test-file "<2026-03-15 Sun>" "2026-04-15")))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'before result) "<2026-03-15 Sun>"))
        (should
         (string-match-p "\\`<2026-04-15 " (alist-get 'after result))))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-deadline-update-logged))))

(ert-deftest org-mcp-test-set-deadline-logs-nothing-when-unset ()
  "`org-log-redeadline' unset leaves the move unrecorded."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (let ((org-log-redeadline nil)
          (org-log-into-drawer t))
      (let ((result
             (org-mcp-test--call-set-deadline
              test-file "<2026-03-15 Sun>" "2026-04-15")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'before result) "<2026-03-15 Sun>"))
        (should
         (string-match-p "\\`<2026-04-15 " (alist-get 'after result))))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-deadline-update))))

(ert-deftest org-mcp-test-set-deadline-null-after-logs-the-removal ()
  "`org-log-redeadline' records the removal a null `after\=' makes."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (let ((org-log-redeadline 'note)
          (org-log-into-drawer t))
      (let ((result
             (org-mcp-test--call-set-deadline
              test-file "<2026-03-15 Sun>" nil)))
        (should (equal (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'before result) "<2026-03-15 Sun>"))
        (should (equal (alist-get 'after result) "")))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-deadline-remove-logged))))

(ert-deftest org-mcp-test-set-deadline-null-after-logs-nothing-when-unset ()
  "`org-log-redeadline' unset leaves the removal unrecorded."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (let ((org-log-redeadline nil)
          (org-log-into-drawer t))
      (let ((result
             (org-mcp-test--call-set-deadline
              test-file "<2026-03-15 Sun>" nil)))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'before result) "<2026-03-15 Sun>"))
        (should (equal (alist-get 'after result) "")))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-deadline-remove))))

(ert-deftest org-mcp-test-set-priority-arms-no-log-prompt ()
  "Writing and taking off a priority leaves nothing on the hook.
`org-priority' has no log setting and reaches no log note, under any
of the settings that make the other planning writes record one."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (let ((org-log-reschedule 'note)
          (org-log-redeadline 'note)
          (org-log-into-drawer t)
          (link (org-mcp-test--file-link test-file "*Priority Task")))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-set-priority"
               `((link . ,link) (before . "B") (after . "A"))))))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'before result) "B"))
        (should (equal (alist-get 'after result) "A")))
      (org-mcp-test--should-leave-no-log-prompt)
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-set-priority"
               `((link . ,link) (before . "A") (after))))))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'before result) "A"))
        (should (equal (alist-get 'after result) "")))
      (org-mcp-test--should-leave-no-log-prompt))))

;;; The log entry a TODO state change means to leave

(defconst org-mcp-test--pattern-task-one-closing-note
  (concat
   "\\`\\* DONE Task One\n"
   "CLOSED: \\[[^]]+\\]\n"
   ":LOGBOOK:\n"
   "- CLOSING NOTE \\[[^]]+\\]\n"
   ":END:\n"
   "Task description\\.\n?\\'")
  "Pattern after a DONE that `org-log-done' set to `note' records.")

(defconst org-mcp-test--pattern-task-one-closing-note-with-prose
  (concat
   "\\`\\* DONE Task One\n"
   "CLOSED: \\[[^]]+\\]\n"
   ":LOGBOOK:\n"
   "- CLOSING NOTE \\[[^]]+\\] \\\\\\\\\n"
   "  Shipped it\\.\n"
   ":END:\n"
   "Task description\\.\n?\\'")
  "Pattern after a DONE with a note, recorded as one entry.")

(ert-deftest org-mcp-test-set-todo-logs-the-state-change-without-asking ()
  "`org-log-done' set to `note' records the change and waits for no one.
The entry Org sets up is written here rather than left on
`post-command-hook' for the user's next command to run."
  (org-mcp-test--with-temp-org-files
      ((test-file "* TODO Task One\nTask description."))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-log-done 'note)
          (org-log-into-drawer t))
      (let ((result
             (org-mcp-test--call-update-todo-state
              (org-mcp-test--file-link test-file "*Task One")
              "DONE" "TODO")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'before result) "TODO"))
        (should (equal (alist-get 'after result) "DONE")))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-task-one-closing-note))))

(ert-deftest org-mcp-test-set-todo-note-rides-the-entry-org-sets-up ()
  "A `note\=' becomes the prose of the entry Org set up, not a second entry.
The client asked for one record of one transition, so the note goes
under the heading line Org chose for it."
  (org-mcp-test--with-temp-org-files
      ((test-file "* TODO Task One\nTask description."))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-log-done 'note)
          (org-log-into-drawer t))
      (let ((result
             (org-mcp-test--call-update-todo-state
              (org-mcp-test--file-link test-file "*Task One")
              "DONE" "TODO" "Shipped it.")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'before result) "TODO"))
        (should (equal (alist-get 'after result) "DONE")))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--pattern-task-one-closing-note-with-prose))))

(defconst org-mcp-test--pattern-weekly-task-repeat-logged
  (concat
   "\\`\\* TODO Weekly Task\n"
   "SCHEDULED: <[0-9]+-[0-9]+-[0-9]+[^>]*\\+1w[^>]*>\n"
   ":PROPERTIES:\n"
   ":LAST_REPEAT:[ \t]+\\[[^]]+\\]\n"
   ":END:\n"
   ":LOGBOOK:\n"
   "- State \"DONE\"[ \t]+from \"TODO\"[ \t]+\\[[^]]+\\]\n"
   ":END:\n?\\'")
  "Pattern after a repeat that `org-log-repeat' records.")

(ert-deftest org-mcp-test-set-todo-repeat-logs-without-asking ()
  "`org-log-repeat' records the repeat and waits for no one.
`org-auto-repeat-maybe' is the second way `org-todo' reaches a log
note, and a repeating entry moved to a done keyword takes it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-task-scheduled-repeat))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-log-repeat 'note)
          (org-log-into-drawer t))
      (let ((result
             (org-mcp-test--call-update-todo-state
              (org-mcp-test--file-link test-file "*Weekly Task")
              "DONE" "TODO")))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'before result) "TODO"))
        (should (equal (alist-get 'after result) "TODO")))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--pattern-weekly-task-repeat-logged))))

(ert-deftest org-mcp-test-node-create-done-logs-without-asking ()
  "A node created straight into a done keyword leaves no prompt behind.
`org-node-create' reaches `org-todo' the same way the setter does."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((org-log-done 'note)
          (org-log-into-drawer t))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-create"
               `((title . "Done on arrival")
                 (todo . "DONE")
                 (parent . ,(concat "file:" test-file))
                 (content . "Body."))))))
        (should (equal (alist-get 'success result) t)))
      (org-mcp-test--should-leave-no-log-prompt))))

(defconst org-mcp-test--pattern-weekly-task-archived-logged
  (concat
   "\\`\\* Archived\n"
   "\n"
   "\\*\\* TODO Weekly Task\n"
   "SCHEDULED: <[0-9-]+ [^>]*\\+1w>\n"
   ":PROPERTIES:\n"
   ":LAST_REPEAT: \\[[^]]+\\]\n"
   ":ARCHIVE_TIME: [^\n]+\n"
   ":ARCHIVE_FILE: [^\n]+\n"
   ":ARCHIVE_CATEGORY: [^\n]+\n"
   ":ARCHIVE_TODO: TODO\n"
   ":END:\n"
   ":LOGBOOK:\n"
   "- State \"DONE\" +from \"TODO\" +\\[[^]]+\\]\n"
   ":END:\n\\'")
  "Pattern after a repeating task is archived and marked done.
The repeat fires, so the heading arrives in its not-done keyword with
the date advanced, and the entry `org-log-repeat' asks for is in the
archived copy rather than waiting on a hook.")

(ert-deftest org-mcp-test-node-archive-logs-the-repeat-without-asking ()
  "Archiving a repeating task records the repeat and waits for no one.
`org-archive-mark-done' makes Org mark the archived subtree done
through `org-todo', which reaches a log entry through
`org-auto-repeat-maybe'.  Org marks it in the archive, so that is
where the entry goes."
  (org-mcp-test--with-temp-org-files
      ((test-file
        (concat org-mcp-test--content-task-scheduled-repeat "\n")))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-log-repeat 'note)
          (org-log-done 'note)
          (org-archive-mark-done t)
          (org-log-into-drawer t)
          (org-archive-location "::* Archived"))
      (let* ((link (org-mcp-test--file-link test-file "*Weekly Task"))
             (before
              (alist-get
               'digest (org-mcp-test--read-fields link ["digest"])))
             (result
              (json-read-from-string
               (mcp-server-lib-ert-call-tool
                "org-node-archive"
                `((link . ,link) (before . ,before))))))
        (should (eq (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'link result) link)))
      (org-mcp-test--should-leave-no-log-prompt)
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--pattern-weekly-task-archived-logged))))

;;; Tests for the three tag tools
;;
;; org-node-add-tags and org-node-remove-tags name what they change
;; and assert nothing; org-node-set-tags replaces the set and asserts
;; the whole of it.  Every test pins the tool's own response as well
;; as the file: a wrong `before', `after' or `inherited' is what a
;; client acts on, and a file assertion cannot see it.

(defconst org-mcp-test--pattern-tags-one-added
  (concat
   "\\`\\* TODO Simple Task[ \t]+:work:\n"
   "Task body text\\.\n?\\'")
  "Pattern after one tag is added to the bare task.")

(defconst org-mcp-test--pattern-tags-unseen-kept
  (concat
   "\\`\\* TODO Task with Tags[ \t]+:work:urgent:personal:\n"
   "Task description\\.\n?\\'")
  "Pattern after a third tag joins the two the task carries.")

(defconst org-mcp-test--pattern-tags-one-removed
  (concat
   "\\`\\* TODO Task with Tags[ \t]+:urgent:\n"
   "Task description\\.\n?\\'")
  "Pattern after one of the task's two tags is removed.")

(defconst org-mcp-test--pattern-tags-work-only
  (concat
   "\\`\\* TODO Task with Tags[ \t]+:work:\n"
   "Task description\\.\n?\\'")
  "Pattern after a replacement keeps one of the task's two tags.")

(defconst org-mcp-test--content-mutex-tagged
  "* TODO Task with Tags :work:personal:\nTask description."
  "A task carrying two tags of one mutually exclusive group.")

(defconst org-mcp-test--pattern-child-tag-restored
  (concat
   "\\`#\\+FILETAGS: filetag\n"
   "\\* Tagged Parent[ \t]+:ptag:\n"
   "\\*\\* TODO Tagged Child[ \t]+:ctag:\n"
   "Child body\\.\n?\\'")
  "Pattern after the child is taken through an add, a remove and a set.")

(defconst org-mcp-test--pattern-child-tag-added
  (concat
   "\\`#\\+FILETAGS: filetag\n"
   "\\* Tagged Parent[ \t]+:ptag:\n"
   "\\*\\* TODO Tagged Child[ \t]+:ctag:own:\n"
   "Child body\\.\n?\\'")
  "Pattern after the child is given a tag of its own.")

(defconst org-mcp-test--pattern-child-tag-copied-down
  (concat
   "\\`#\\+FILETAGS: filetag\n"
   "\\* Tagged Parent[ \t]+:ptag:\n"
   "\\*\\* TODO Tagged Child[ \t]+:ctag:ptag:\n"
   "Child body\\.\n?\\'")
  "Pattern after the parent's tag is written on the child as well.
Reached only with inheritance off, where the child does not have
that tag until it is written there.")

(defun org-mcp-test--call-tag-tool (tool params)
  "Call TOOL with PARAMS through the MCP boundary, parsing the response."
  (json-read-from-string (mcp-server-lib-ert-call-tool tool params)))

(defun org-mcp-test--should-report-tags (result before after inherited)
  "Assert RESULT reports a saved change carrying these three tag fields.
BEFORE and AFTER are the heading's own tags either side of the call
and INHERITED the tags in effect on it from elsewhere, each as the
vector the response carries."
  (should (equal (alist-get 'success result) t))
  (should (eq (alist-get 'saved result) t))
  (should (equal (alist-get 'before result) before))
  (should (equal (alist-get 'after result) after))
  (should (equal (alist-get 'inherited result) inherited)))

(ert-deftest org-mcp-test-add-tags-two-adds-in-a-row-both-land ()
  "Two calls adding different tags each keep what the other added.
Add destroys nothing, which is why it needs no assertion: the second
call plans from a heading the first has already changed, and it is
still right."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-add-tags" `((link . ,link) (after . "work")))
       [] ["work"] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-one-added)
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-add-tags" `((link . ,link) (after . "urgent")))
       ["work"] ["work" "urgent"] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-set))))

(ert-deftest org-mcp-test-add-tags-a-tag-already-there-is-a-no-op ()
  "Adding a tag the heading carries leaves the file byte for byte.
It is written once and not twice, and the call that asks for nothing
new writes nothing at all."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags"))
          (before (org-mcp-test--read-file test-file)))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-add-tags" `((link . ,link) (after . "work")))
       ["work" "urgent"] ["work" "urgent"] [])
      (should (string= (org-mcp-test--read-file test-file) before)))))

(ert-deftest org-mcp-test-add-tags-a-tag-named-twice-is-added-once ()
  "A repeated tag in one call is written once, as a set has it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-add-tags" `((link . ,link) (after . ["work" "work"])))
       [] ["work"] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-one-added))))

(ert-deftest org-mcp-test-add-tags-keeps-a-tag-the-client-never-saw ()
  "A tag the call does not name survives it.
The heading carries two tags and the call names a third, so the two
it says nothing about are still there afterwards."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-add-tags" `((link . ,link) (after . "personal")))
       ["work" "urgent"] ["work" "urgent" "personal"] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-unseen-kept))))

(ert-deftest org-mcp-test-add-tags-an-inherited-tag-is-not-copied-down ()
  "Adding a tag the heading inherits leaves the heading alone.
The heading has the tag already, and writing it there as well would
make a local copy of an inherited tag rather than add anything."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance t)
          (org-tags-exclude-from-inheritance nil)
          (link (org-mcp-test--file-link test-file "*Tagged Child"))
          (before (org-mcp-test--read-file test-file)))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-add-tags" `((link . ,link) (after . "ptag")))
       ["ctag"] ["ctag"] ["filetag" "ptag"])
      (should (string= (org-mcp-test--read-file test-file) before)))))

(ert-deftest org-mcp-test-add-tags-writes-an-uninherited-tag ()
  "With inheritance off the parent's tag is not the child's until written.
The policy is Org's, so the same call writes the tag here and leaves
it alone where the child already inherits it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance nil)
          (link (org-mcp-test--file-link test-file "*Tagged Child")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-add-tags" `((link . ,link) (after . "ptag")))
       ["ctag"] ["ctag" "ptag"] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-child-tag-copied-down))))

(ert-deftest org-mcp-test-remove-tags-takes-only-what-it-names ()
  "Removing one tag leaves every other tag where it is."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-remove-tags" `((link . ,link) (after . "work")))
       ["work" "urgent"] ["urgent"] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-one-removed))))

(ert-deftest org-mcp-test-remove-tags-a-tag-that-is-absent-is-a-no-op ()
  "Removing a tag the heading does not have changes nothing."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags"))
          (before (org-mcp-test--read-file test-file)))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-remove-tags" `((link . ,link) (after . "personal")))
       ["work" "urgent"] ["work" "urgent"] [])
      (should (string= (org-mcp-test--read-file test-file) before)))))

(ert-deftest org-mcp-test-remove-tags-clears-the-tags-a-read-returned ()
  "Naming every tag a read reported as the heading's own clears them.
That is one of the two spellings of clearing, and the one that
asserts nothing."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-remove-tags"
        `((link . ,link) (after . ["work" "urgent"])))
       ["work" "urgent"] [] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-clear))))

(ert-deftest org-mcp-test-remove-tags-refuses-an-inherited-tag ()
  "A tag the heading only inherits cannot be removed here.
The refusal names the heading the tag is written on, which is where
removing it would have to happen."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance t)
          (org-tags-exclude-from-inheritance nil))
      (org-mcp-test--call-tool-refused
       "org-node-remove-tags"
       `((link
          .
          ,(org-mcp-test--file-link test-file "*Tagged Child"))
         (after . "ptag"))
       (concat
        "\\`"
        (regexp-quote
         (concat
          "Cannot remove tag 'ptag': the heading inherits it from "
          "'Tagged Parent' and does not carry it itself"))
        "\\'")
       test-file))))

(ert-deftest org-mcp-test-remove-tags-refuses-a-file-tag ()
  "A tag the file gives every heading is refused the same way.
No heading carries it, so the refusal names the file's own line
rather than an ancestor."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance t)
          (org-tags-exclude-from-inheritance nil))
      (org-mcp-test--call-tool-refused
       "org-node-remove-tags"
       `((link
          .
          ,(org-mcp-test--file-link test-file "*Tagged Child"))
         (after . "filetag"))
       (concat
        "\\`"
        (regexp-quote
         (concat
          "Cannot remove tag 'filetag': the heading inherits it from "
          "the file's #+FILETAGS: and does not carry it itself"))
        "\\'")
       test-file))))

(ert-deftest org-mcp-test-set-tags-replaces-the-asserted-set ()
  "A set the heading carries is accepted and the replacement is written."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((org-tag-alist '("work" "personal" "urgent"))
          (link (org-mcp-test--file-link test-file "*Task with Tags")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-set-tags"
        `((link . ,link)
          (before . ["work" "urgent"])
          (after . "personal")))
       ["work" "urgent"] ["personal"] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-replace))))

(ert-deftest org-mcp-test-set-tags-before-is-compared-as-a-set ()
  "The order tags are asserted in makes no difference.
Org writes tags in an order, but that order says nothing, so the
assertion is a comparison of sets."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((org-tag-alist '("work" "personal" "urgent"))
          (link (org-mcp-test--file-link test-file "*Task with Tags")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-set-tags"
        `((link . ,link)
          (before . ["urgent" "work"])
          (after . "personal")))
       ["work" "urgent"] ["personal"] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-replace))))

(ert-deftest org-mcp-test-set-tags-destroys-a-tag-the-client-never-saw ()
  "Replacement takes away every tag the call does not list.
That is the whole difference between the deltas and this tool, and
it is why this one asserts the entire prior set: a client that sent
a stale set would be destroying a tag it had never read."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-set-tags"
        `((link . ,link)
          (before . ["work" "urgent"])
          (after . "work")))
       ["work" "urgent"] ["work"] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-work-only))))

(ert-deftest org-mcp-test-set-tags-refuses-a-stale-before ()
  "A prior set the heading does not carry is a conflict.
The refusal names both sets in one order, since what it reports is a
comparison of sets and an order in the message would invite a reader
to look for a difference that is not there."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (org-mcp-test--call-tool-refused
     "org-node-set-tags"
     `((link
        .
        ,(org-mcp-test--file-link test-file "*Task with Tags"))
       (before . ["work"])
       (after . ["personal"]))
     (concat
      "\\`"
      (regexp-quote
       "conflict: Tags mismatch: expected 'work', found 'urgent, work'")
      "\\'")
     test-file)))

(ert-deftest org-mcp-test-set-tags-refuses-a-before-that-is-no-tag-name ()
  "What a `before' is decides which class refuses it.
Org writes tags from `org-tag-re', so a value outside that set names
a tag no heading could carry: the assertion cannot be satisfied by
any version of the file, and calling it a conflict would send a
client to read a file that has nothing to tell it.  It is refused as
the malformed call it is, in the unmarked validation class, by the
one test that says what a tag is — the test the tags in `after' pass
as well.

A set of real tag names the heading does not carry is the other
case, and it stays the conflict this assertion exists to report:
there is a heading to read again and a set to read off it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags")))
      (pcase-dolist
          (`(,before ,refusal)
           '(("not a tag!" "Invalid tag name: not a tag!")
             (["work" "a-b"] "Invalid tag name: a-b")
             (["work" "personal"]
              "conflict: Tags mismatch: expected 'personal, work', \
found 'urgent, work'")))
        (org-mcp-test--call-tool-refused
         "org-node-set-tags"
         `((link . ,link) (before . ,before) (after . ["personal"]))
         (concat "\\`" (regexp-quote refusal) "\\'")
         test-file)))))

(ert-deftest org-mcp-test-set-tags-token-in-before-keeps-the-digest-refusal ()
  "A token in `before' is refused in the words written for a token.
It is no tag name either, so the general refusal would cover it, and
covering it there would cost a client the one sentence that says
which of the two forms of `before' this tool takes.  The token is
therefore looked for first, and the whole message it gets is pinned
here."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags")))
      (dolist (before '("sha256:3f9c2a1b8e4d7c05" ["work" "sha256:3f9c2a1b8e4d7c05"]))
        (org-mcp-test--call-tool-refused
         "org-node-set-tags"
         `((link . ,link) (before . ,before) (after . ["personal"]))
         (concat
          "\\`"
          (regexp-quote
           "Tags is asserted with the value it holds, not with a digest: \
'sha256:3f9c2a1b8e4d7c05' covers a region and this call changes one field")
          "\\'")
         test-file)))))

(ert-deftest org-mcp-test-set-tags-empty-before-asserts-no-own-tags ()
  "An empty prior set asserts that the heading carries none of its own.
It is an assertion like any other, not a parameter left blank."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-set-tags"
        `((link . ,link) (before . []) (after . ["work" "urgent"])))
       [] ["work" "urgent"] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-set))))

(ert-deftest org-mcp-test-set-tags-empty-before-on-a-tagged-heading ()
  "An empty prior set is refused where the heading carries tags."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (org-mcp-test--call-tool-refused
     "org-node-set-tags"
     `((link
        .
        ,(org-mcp-test--file-link test-file "*Task with Tags"))
       (before . [])
       (after . ["personal"]))
     (concat
      "\\`"
      (regexp-quote
       "conflict: Tags mismatch: expected '(no tags)', \
found 'urgent, work'")
      "\\'")
     test-file)))

(ert-deftest org-mcp-test-set-tags-clears-with-an-empty-after ()
  "An empty set written leaves the heading no tags of its own.
That is the other spelling of clearing, and the one that pays for it
by asserting the whole set it destroys."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-set-tags"
        `((link . ,link) (before . ["work" "urgent"]) (after . [])))
       ["work" "urgent"] [] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-clear))))

(ert-deftest org-mcp-test-set-tags-asserts-own-tags-not-inherited-ones ()
  "The prior set is the heading's own tags, never the set in effect.
The call writes local tags only, so asserting what the heading
inherits would assert values it cannot change and would refuse
because an ancestor was edited."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance t)
          (org-tags-exclude-from-inheritance nil)
          (link (org-mcp-test--file-link test-file "*Tagged Child")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-set-tags"
        `((link . ,link) (before . ["ctag"]) (after . ["ctag" "own"])))
       ["ctag"] ["ctag" "own"] ["filetag" "ptag"])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-child-tag-added))))

(ert-deftest org-mcp-test-set-tags-refuses-an-effective-set-as-before ()
  "Asserting the tags in effect is refused where they differ from the own set.
A client that sent what a read returned under `tags' rather than
under `local_tags' is told which of the two the tool compares."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance t)
          (org-tags-exclude-from-inheritance nil))
      (org-mcp-test--call-tool-refused
       "org-node-set-tags"
       `((link
          .
          ,(org-mcp-test--file-link test-file "*Tagged Child"))
         (before . ["ctag" "ptag" "filetag"])
         (after . ["ctag" "own"]))
       (concat
        "\\`"
        (regexp-quote
         "conflict: Tags mismatch: expected 'ctag, filetag, ptag', \
found 'ctag'")
        "\\'")
       test-file))))

(ert-deftest org-mcp-test-tag-tools-refuse-a-blank-tag-set ()
  "A blank tag set is a parameter left out, on all three tools.
Clients fill a parameter they are not using with a blank, so a blank
that cleared the heading's tags would make a well-behaved client
destroy them.  `[]' is how a call says the empty set."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags")))
      (dolist (blank '("" :json-false nil))
        (dolist (tool '("org-node-add-tags" "org-node-remove-tags"))
          (org-mcp-test--call-tool-refused
           tool
           `((link . ,link) (after . ,blank))
           "\\`Missing required parameter: after\\'"
           test-file))
        (org-mcp-test--call-tool-refused
         "org-node-set-tags"
         `((link . ,link) (before . ["work" "urgent"]) (after . ,blank))
         "\\`Missing required parameter: after\\'"
         test-file)
        (org-mcp-test--call-tool-refused
         "org-node-set-tags"
         `((link . ,link) (before . ,blank) (after . ["personal"]))
         "\\`Missing required parameter: before\\'"
         test-file)))))

(ert-deftest org-mcp-test-tag-tools-refuse-an-omitted-tag-set ()
  "A tag set left out altogether is refused before anything is read.
The parameter is the assertion on org-node-set-tags and the change
itself on the other two, so none of them has a meaning without it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags")))
      (dolist (tool '("org-node-add-tags" "org-node-remove-tags"))
        (org-mcp-test--call-tool-refused
         tool `((link . ,link))
         "\\`Missing required parameter: after\\'" test-file))
      (org-mcp-test--call-tool-refused
       "org-node-set-tags" `((link . ,link) (after . ["personal"]))
       "\\`Missing required parameter: before\\'" test-file))))

(defconst org-mcp-test--tag-members-that-are-no-string
  '((1 "1")
    (t "true")
    (:json-false "false")
    (nil "null")
    (((a . "b")) "an object")
    (["inner"] "an array"))
  "JSON values a tag set may not hold, each with how a refusal names it.
A number, true, false, null, an object and a nested array, as
`json-read-from-string' decodes them.  The second element is the JSON
spelling `org-mcp--json-name' gives the first, written out rather than
computed, so that a message which changes has to be edited here instead
of agreeing with whatever the code prints.  A client reads its own
vocabulary back: what it sent as false is named false, not the
`:json-false' its value decoded to.")

(ert-deftest org-mcp-test-tag-tools-refuse-a-member-that-is-no-string ()
  "A tag set holds strings, and one place says so for every tool.
`org-tag-re' is a test on text, so a member that is not text reaches
it as a wrong type and crosses the boundary as an internal error --
which names no parameter and tells a client nothing it can act on.
The check sits where a tag set is decided, so every parameter that
takes one inherits it: both parameters of `org-node-set-tags', the
`after' of each delta, and the `tags' of `org-node-create'.

A real tag rides beside the bad one in every set, so what is pinned
is a test of each member and not of the parameter as a whole."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags")))
      (pcase-dolist
          (`(,member ,printed)
           org-mcp-test--tag-members-that-are-no-string)
        (let ((set (vector "work" member)))
          (pcase-dolist
              (`(,tool ,params)
               `(("org-node-set-tags"
                  ((link . ,link)
                   (before . ,set)
                   (after . ["personal"])))
                 ("org-node-set-tags"
                  ((link . ,link)
                   (before . ["work" "urgent"])
                   (after . ,set)))
                 ("org-node-add-tags" ((link . ,link) (after . ,set)))
                 ("org-node-remove-tags" ((link . ,link) (after . ,set)))
                 ("org-node-create"
                  ((title . "New Task")
                   (todo . "TODO")
                   (content . "Body.")
                   (tags . ,set)
                   (parent . ,(concat "file:" test-file))))))
            (org-mcp-test--call-tool-refused
             tool params
             (concat
              "\\`"
              (regexp-quote
               (concat "A tag must be a string: " printed))
              "\\'")
             test-file)))))))

(ert-deftest org-mcp-test-tag-tools-refuse-a-set-that-is-no-tag-set ()
  "A tag set arrives as a string or an array, and nothing else does.
A number or a boolean is neither, and is refused as the format it
is.  A JSON object decodes to a list of pairs, so it arrives as a
list like any other by the time a tag set is read, and it is refused
for the pair it holds, which is no tag.  Either way nothing is
written, and the internal error a wrong type would raise is out of
reach.

That both parameters are covered from one place is pinned by
`org-mcp-test-tag-tools-refuse-a-member-that-is-no-string'; what
this test adds is the whole parameter rather than a member of it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-tags))
    (let ((link (org-mcp-test--file-link test-file "*Task with Tags")))
      (pcase-dolist
          (`(,value ,refusal)
           '((5 "Invalid tags format: 5")
             (t "Invalid tags format: t")
             (((a . "b")) "A tag must be a string: an object")))
        (org-mcp-test--call-tool-refused
         "org-node-set-tags"
         `((link . ,link) (before . ,value) (after . ["personal"]))
         (concat "\\`" (regexp-quote refusal) "\\'")
         test-file)
        (org-mcp-test--call-tool-refused
         "org-node-set-tags"
         `((link . ,link) (before . ["work" "urgent"]) (after . ,value))
         (concat "\\`" (regexp-quote refusal) "\\'")
         test-file)))))

(defconst org-mcp-test--calls-taking-a-required-link
  '(("org-node-read" "link" ())
    ("org-node-text" "link" ())
    ("org-node-set-todo" "link" ((before . "TODO") (after . "DONE")))
    ("org-node-set-title" "link" ((before . "a") (after . "b")))
    ("org-node-set-content" "link" ((before . "a") (after . "b")))
    ("org-node-set-properties"
     "link"
     ((before . ((EFFORT . "1:00"))) (after . ((EFFORT . "2:00")))))
    ("org-node-set-scheduled"
     "link"
     ((before . "") (after . "2026-09-27")))
    ("org-node-set-deadline"
     "link"
     ((before . "") (after . "2026-09-27")))
    ("org-node-set-priority" "link" ((before . "") (after . "A")))
    ("org-node-add-tags" "link" ((after . ["urgent"])))
    ("org-node-remove-tags" "link" ((after . ["urgent"])))
    ("org-node-set-tags" "link" ((before . []) (after . ["urgent"])))
    ("org-node-add-note" "link" ((note . "A note.")))
    ("org-node-delete" "link" ((before . "sha256:0000000000000000")))
    ("org-node-archive" "link" ((before . "sha256:0000000000000000")))
    ("org-node-refile"
     "link"
     ((before . "sha256:0000000000000000") (parent . real-link)))
    ("org-node-refile"
     "parent"
     ((link . real-link) (before . "sha256:0000000000000000")))
    ("org-node-create" "parent" ((title . "T") (todo . "TODO")))
    ("org-clock-in" "link" ())
    ("org-clock-out" "link" ())
    ("org-clock-add"
     "link"
     ((start . "2026-09-21T09:00") (end . "2026-09-21T10:00")))
    ("org-clock-delete" "link" ((start . "2026-09-21T09:00"))))
  "Every required link parameter on the surface, with a call around it.
Each entry is the tool, the parameter that names a link, and the rest
of a call that would otherwise be well formed, so that what a refusal
answers is the blank link and nothing else.  The symbol `real-link\='
stands for a link the test file answers to, since a second link that
resolves to nothing would be refused before the blank one is read.
The list is the sweep:
a tool added with a link parameter and left out of it is a tool whose
blank was never checked.")

(ert-deftest org-mcp-test-a-blank-link-names-the-parameter-it-arrived-in ()
  "A required link parameter left blank refuses as the parameter it is.
A blank that reached the parser instead would come back as `Not an Org
link: nil\=' -- the Elisp reader\='s spelling of the client\='s own JSON
null, in a message naming no parameter of the call.  Every link a call
sends is resolved through `org-mcp--link-target\=', which reads it with
`org-mcp--link-given\=' first, so the refusal is the same on every tool
and in every spelling a client fills an unused parameter with."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (let ((real-link
           (org-mcp-test--file-link test-file "*Task with Properties")))
      (pcase-dolist (`(,tool ,name ,rest)
                     org-mcp-test--calls-taking-a-required-link)
        (dolist (blank (list nil :json-false "" "   "))
          (org-mcp-test--call-tool-refused
           tool
           (cons (cons (intern name) blank)
                 (mapcar
                  (lambda (pair)
                    (if (eq (cdr pair) 'real-link)
                        (cons (car pair) real-link)
                      pair))
                  rest))
           (concat
            "\\`Missing required parameter: " (regexp-quote name) "\\'")
           test-file))))))

(ert-deftest org-mcp-test-a-blank-optional-link-still-means-none ()
  "An optional link parameter keeps its meaning for every blank.
`previous_sibling\=' means the new node goes last, and `clock_out\='
means there is no clock the call has to close.  Both are read by
`org-mcp--optional-link-given\=', which answers nil where the required
reader refuses, so a sweep over the required ones cannot take these
with it."
  (dolist (blank (list nil :json-false "" "   "))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-bare-todo))
      (let* ((parent (concat "file:" test-file))
             (result
              (json-read-from-string
               (mcp-server-lib-ert-call-tool
                "org-node-create"
                `((title . "Appended")
                  (todo . "TODO")
                  (parent . ,parent)
                  (previous_sibling . ,blank))))))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'title result) "Appended")))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-clock-in"
               `((link
                  .
                  ,(org-mcp-test--file-link test-file "*Simple Task"))
                 (start_time . "2026-09-21T09:00")
                 (clock_out . ,blank))))))
        (should (equal (alist-get 'clocked_in result) t))
        (org-clock-out nil t)))))

(ert-deftest org-mcp-test-a-refusal-names-a-value-in-json ()
  "A refusal that shows a value shows it in the client\='s own language.
`json-read-from-string\=' makes an alist of an object, nil of null and
`:json-false\=' of false, and a refusal that printed those back handed
the client the spelling of its own value in another language.
`org-mcp--json-name\=' is the one definition of how a JSON value is
named in a message, and this covers every parameter reader that names
one."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (let ((link
           (org-mcp-test--file-link test-file "*Task with Properties")))
      (pcase-dolist
          (`(,tool ,params ,refusal)
           `(("org-node-read"
              ((depth . ((a . "b"))))
              "depth must be a whole number of generations, not: \
an object")
             ("org-node-read"
              ((depth . ["x"]))
              "depth must be a whole number of generations, not: \
an array")
             ("org-node-read"
              ((fields . 7))
              "fields takes an array of field names, or the name of a \
configured list as a string, not: 7")
             ("org-node-read"
              ((computed . 7))
              "computed takes an array of names, or \"all\" or \"none\" \
as a string, not: 7")
             ("org-node-read"
              ((properties . [7]))
              "A property name is a string, not: 7")
             ("org-node-delete"
              ((before . ((a . "b"))))
              ,(concat
                "before must be the digest a read of this node "
                "returned, starting `sha256:': an object"))
             ("org-node-delete"
              ((before . 7))
              ,(concat
                "before must be the digest a read of this node "
                "returned, starting `sha256:': 7"))
             ("org-clock-in"
              ((resolve . ((a . "b"))))
              "resolve must be true or false: an object")))
        (org-mcp-test--call-tool-refused
         tool
         (cons `(link . ,link) params)
         (concat "\\`" (regexp-quote refusal) "\\'")
         test-file)))))

(ert-deftest org-mcp-test-tag-tools-publish-their-parameters-as-required ()
  "The schema says which tag parameters a call must carry.
A client discovers them there and never from the handler, so a
parameter published as optional is a guard that is off whatever the
handler then does with it."
  (org-mcp-test--with-enabled
    (dolist (id '("org-node-add-tags" "org-node-remove-tags"))
      (should
       (equal (org-mcp-test--registered-tool-required id)
              '("link" "after"))))
    (should
     (equal (org-mcp-test--registered-tool-required "org-node-set-tags")
            '("link" "before" "after")))))

(ert-deftest org-mcp-test-tag-tools-write-own-tags-alike-under-inheritance ()
  "The three tools treat a heading's own tags the same either way.
Inheritance decides what a heading has from elsewhere, and the tools
write what it has itself, so the same run over the same child leaves
the same file and reports the same own sets whether inheritance is
on or off.  Only `inherited' differs, which is the one field that is
about the setting."
  (dolist (inheritance '(t nil))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-inherited-tags))
      (let* ((org-use-tag-inheritance inheritance)
             (org-tags-exclude-from-inheritance nil)
             (elsewhere
              (if inheritance
                  ["filetag" "ptag"]
                []))
             (link (org-mcp-test--file-link test-file "*Tagged Child")))
        (org-mcp-test--should-report-tags
         (org-mcp-test--call-tag-tool
          "org-node-add-tags" `((link . ,link) (after . "own")))
         ["ctag"] ["ctag" "own"] elsewhere)
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-child-tag-added)
        (org-mcp-test--should-report-tags
         (org-mcp-test--call-tag-tool
          "org-node-remove-tags" `((link . ,link) (after . "ctag")))
         ["ctag" "own"] ["own"] elsewhere)
        (org-mcp-test--should-report-tags
         (org-mcp-test--call-tag-tool
          "org-node-set-tags"
          `((link . ,link) (before . ["own"]) (after . ["ctag"])))
         ["own"] ["ctag"] elsewhere)
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--pattern-child-tag-restored)))))

(ert-deftest org-mcp-test-set-tags-invalid-name ()
  "Test that invalid tag names are rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (org-mcp-test--call-tool-refused
     "org-node-set-tags"
     `((link . ,(org-mcp-test--file-link test-file "*Simple Task"))
       (before . [])
       (after . "invalid tag!"))
     "\\`Invalid tag name: invalid tag!\\'"
     test-file)))

(ert-deftest org-mcp-test-remove-tags-invalid-name ()
  "A name Org could not have written is refused rather than passed over.
Removing it would change nothing, but the call is a mistake and
saying so is more use than silence."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (org-mcp-test--call-tool-refused
     "org-node-remove-tags"
     `((link . ,(org-mcp-test--file-link test-file "*Simple Task"))
       (after . "invalid tag!"))
     "\\`Invalid tag name: invalid tag!\\'"
     test-file)))

(ert-deftest org-mcp-test-set-tags-free-form-with-alist ()
  "Free-form tags are accepted even when `org-tag-alist' is configured.
Org permits free-form tags, so we only enforce `org-tag-re' here,
not membership in the configured alist."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-tag-alist '("work" "personal"))
          (link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-set-tags"
        `((link . ,link) (before . []) (after . "nonexistent")))
       [] ["nonexistent"] []))))

(ert-deftest org-mcp-test-set-tags-mutex-violation ()
  "Test that mutually exclusive tags are rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((org-tag-alist
           '(:startgroup "work" "personal" :endgroup "urgent")))
      (org-mcp-test--call-tool-refused
       "org-node-set-tags"
       `((link . ,(org-mcp-test--file-link test-file "*Simple Task"))
         (before . [])
         (after . ["work" "personal"]))
       (concat
        "\\`"
        (regexp-quote
         "Tags 'work', 'personal' are mutually exclusive \
(cannot use together)")
        "\\'")
       test-file))))

(ert-deftest org-mcp-test-remove-tags-ignores-mutex-groups ()
  "Two tags of one group can be removed together.
Mutual exclusivity is a rule about what a heading ends up carrying,
and a call that only takes tags away cannot break it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-mutex-tagged))
    (let ((org-tag-alist
           '(:startgroup "work" "personal" :endgroup "urgent"))
          (link (org-mcp-test--file-link test-file "*Task with Tags")))
      (org-mcp-test--should-report-tags
       (org-mcp-test--call-tag-tool
        "org-node-remove-tags"
        `((link . ,link) (after . ["work" "personal"])))
       ["work" "personal"] [] [])
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-tags-clear))))

(ert-deftest org-mcp-test-set-tags-id-link ()
  "Test setting tags via an `id:' link."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((link (concat "id:" org-mcp-test--crud-test-id))
          (result
           (org-mcp-test--call-tag-tool
            "org-node-set-tags"
            `((link . ,link) (before . []) (after . "work")))))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

(ert-deftest org-mcp-test-add-tags-id-link ()
  "Test adding a tag via an `id:' link."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((link (concat "id:" org-mcp-test--crud-test-id))
          (result
           (org-mcp-test--call-tag-tool
            "org-node-add-tags" `((link . ,link) (after . "work")))))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

;;; Tests for org-node-set-priority

(ert-deftest org-mcp-test-set-priority-set ()
  "Test setting priority on a bare task."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (params `((link . ,link)
                     (before . "")
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
                     (before . "B")
                     (after . "C")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-priority" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'before result) "B"))
      (should (equal (alist-get 'after result) "C"))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-priority-change))))

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
                  (before . "")
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
                  (before . "")
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
                    (before . "")
                    (after . "A")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-node-set-priority" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

(ert-deftest org-mcp-test-set-priority-before-mismatch-refuses ()
  "A priority the heading does not carry refuses the call."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (org-mcp-test--call-tool-refused
     "org-node-set-priority"
     `((link . ,(org-mcp-test--file-link test-file "*Priority Task"))
       (before . "A")
       (after . "C"))
     "\\`conflict: Priority mismatch: expected 'A', found 'B'\\'"
     test-file)))

(ert-deftest org-mcp-test-set-priority-non-string-before-is-malformed ()
  "A `before\=' that is no kind of value is a malformed call.
It is refused as validation and not as a conflict: reading the file
again would not help, because nothing about the file is in question."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (org-mcp-test--call-tool-refused
     "org-node-set-priority"
     `((link . ,(org-mcp-test--file-link test-file "*Priority Task"))
       (before . 3)
       (after . "C"))
     "\\`before must be a string, not 3\\'"
     test-file)))

(ert-deftest org-mcp-test-a-refusal-names-json-in-json ()
  "A refusal names what arrived in the client\='s own language.
`json-read-from-string\=' is what turns a call into Lisp, so printing
its result back would answer a client in the spelling of another
language: an object would read as an alist and true as `t\='.  Both
readers of a required text parameter name the value by its JSON
kind instead, and the `after\=' side names null among what it takes,
because there it means something."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (let ((link (org-mcp-test--file-link test-file "*Priority Task")))
      (dolist (case
               '((((a . 1)) . "an object")
                 ([1 2] . "an array")
                 (t . "true")))
        (org-mcp-test--call-tool-refused
         "org-node-set-priority"
         `((link . ,link) (before . ,(car case)) (after . "C"))
         (concat "\\`before must be a string, not "
                 (cdr case)
                 "\\'")
         test-file)
        (org-mcp-test--call-tool-refused
         "org-node-set-priority"
         `((link . ,link) (before . "B") (after . ,(car case)))
         (concat
          "\\`after must be a string, or null to take the value "
          "away, not "
          (cdr case)
          "\\'")
         test-file)))))

(ert-deftest org-mcp-test-set-priority-empty-before-asserts-none ()
  "An empty `before\=' asserts the heading carries no priority."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (org-mcp-test--call-tool-refused
     "org-node-set-priority"
     `((link . ,(org-mcp-test--file-link test-file "*Priority Task"))
       (before . "")
       (after . "C"))
     "\\`conflict: Priority mismatch: expected '', found 'B'\\'"
     test-file)))

;;; Removing the priority through org-node-set-priority

(ert-deftest org-mcp-test-set-priority-null-after-takes-it-off ()
  "A null `after\=' takes the priority away.
`before\=' is the character destroyed and the response reports it,
because the response is the only record the call leaves of what was
there."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (let* ((link (org-mcp-test--file-link test-file "*Priority Task"))
           (params `((link . ,link) (before . "B") (after)))
           (result-text
            (mcp-server-lib-ert-call-tool
             "org-node-set-priority" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'before result) "B"))
      (should (equal (alist-get 'after result) ""))
      (should (equal (alist-get 'link result) link))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-priority-remove))))

(ert-deftest org-mcp-test-set-priority-null-after-refuses-stale-before ()
  "A priority the headline does not carry refuses the removal."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (org-mcp-test--call-tool-refused
     "org-node-set-priority"
     `((link . ,(org-mcp-test--file-link test-file "*Priority Task"))
       (before . "C")
       (after))
     "\\`conflict: Priority mismatch: expected 'C', found 'B'\\'"
     test-file)))

(ert-deftest org-mcp-test-set-priority-null-after-without-a-priority ()
  "A null `after\=' on a headline carrying no priority writes nothing."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((result
           (org-mcp-test--call-tool-leaving-file
            "org-node-set-priority"
            `((link
               .
               ,(org-mcp-test--file-link test-file "*Simple Task"))
              (before . "")
              (after))
            test-file)))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'before result) ""))
      (should (equal (alist-get 'after result) ""))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-bare-todo))))

(ert-deftest org-mcp-test-set-priority-empty-after-is-no-character ()
  "\"\" is no priority character and is refused as one; false is left out.
Null is the one spelling that takes a value away, because null is
JSON's word for no value.  An empty string is a value, and this
field has none — so it reaches the field's own check and is refused
there, naming what the field does accept.  False and [] are what a
client fills a parameter it is not using with, and are refused as
the parameter left out."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (let ((link (org-mcp-test--file-link test-file "*Priority Task")))
      (org-mcp-test--call-tool-refused
       "org-node-set-priority"
       `((link . ,link) (before . "B") (after . ""))
       "\\`Priority must be a single character, got ''\\'"
       test-file)
      (dolist (blank '(:json-false []))
        (org-mcp-test--call-tool-refused
         "org-node-set-priority"
         `((link . ,link) (before . "B") (after . ,blank))
         "\\`Missing required parameter: after\\'"
         test-file)))))

;;; Tests for adding to a body org-node-set-content rewrites

;; Adding to a body is a rewrite of it: `before' asserts the body the
;; addition is planned against, and `after' carries that body with the
;; addition in it.  The tests below are the ones that covered a mode
;; that added without asserting; what they pin — where a body ends,
;; that the text lands before the children, that a node with no body
;; takes its first content — is the same either way, and a repeat of
;; any of these calls is now refused rather than writing twice.

(ert-deftest org-mcp-test-edit-body-add-line-to-existing-body ()
  "A line is added to a body by rewriting the body with it in.
The body it is added to is asserted by its digest, so the call says
what it is adding to and a second one cannot add the line twice."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let* ((link (org-mcp-test--file-link test-file "*Simple Task"))
           (params
            `((link . ,link)
              (before . ,(org-mcp-test--content-digest-of link))
              (after . "Task body text.\nAppended line.")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-content" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (should
       (equal (alist-get 'link result)
              (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-body-line-added))))

(ert-deftest org-mcp-test-edit-body-writes-into-an-entry-with-none ()
  "An entry with no body takes its first content through an empty before.
The empty string is the assertion that there is nothing to overwrite,
so the call that fills an empty node states that it is empty."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-empty-body))
    (let* ((link (org-mcp-test--file-link test-file "*Empty Body Task"))
           (params `((link . ,link)
                     (before . "")
                     (after . "New body content.")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-content" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-body-written-into-empty))))

(ert-deftest org-mcp-test-edit-body-writes-whitespace-after-as-body ()
  "A whitespace-only after is body text, and it replaces the body.
`after' carries what the body is to hold, and whitespace is
something a body can hold, so it is written rather than read as a
parameter the call left out.  `before' still says what it overwrites,
so a body cleared this way was asserted first."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-body-to-edit))
    (let ((link (org-mcp-test--file-link test-file "*Task")))
      (org-mcp-test--call-edit-body-and-check
       test-file link "old text" "   "
       "\\`\\* Task\n *\n\\'" link))))

(ert-deftest org-mcp-test-edit-body-add-before-children ()
  "Content added to a body goes before the child headlines.
The body ends where the first child begins, so a rewrite of it
reaches no further than that, whatever the children hold."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-children))
    (let* ((link (org-mcp-test--file-link test-file "*Parent Task"))
           (params
            `((link . ,link)
              (before . ,(org-mcp-test--content-digest-of link))
              (after . "Parent body.\nAppended text.")))
           (result-text
            (mcp-server-lib-ert-call-tool "org-node-set-content" params))
           (result (json-read-from-string result-text)))
      (should (equal (alist-get 'success result) t))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--pattern-body-added-before-children))))

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
the end of its subtree, whichever heading follows it.  An empty
before asserts there is no body there and puts the text under Parent:
when a child follows at once, when a property drawer comes first, and
when a sibling with a child of its own follows.  Nothing lands in the
next heading's body, and the response links to Parent."
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
    (org-mcp-test--with-temp-org-files
        ((test-file content))
      (let ((link (org-mcp-test--file-link test-file search)))
        (org-mcp-test--call-edit-body-and-check
         test-file link "" "Parent body." expected link)))))

(defconst org-mcp-test--content-body-before-sibling
  "* Parent\nParent body.\n\n* Sibling\nSibling body.\n"
  "Parent with a body and no child, a blank line, then a sibling.")

(defconst org-mcp-test--regex-body-before-sibling-added
  (concat
   "\\`\\* Parent\n"
   "Parent body\\.\n"
   "Appended\\.\n"
   "\n"
   "\\* Sibling\n"
   "Sibling body\\.\n"
   "\\'")
  "Regex matching the whole sibling file once Parent's body gains a line.")

(defconst org-mcp-test--content-body-to-edit "* Task\nold text\n"
  "Task whose body is one line, for replacing that line.")

(defconst org-mcp-test--regex-body-to-edit-replaced
  "\\`\\* Task\nnew text\n\\'"
  "Regex matching the whole file once the body line is replaced.")

(ert-deftest org-mcp-test-edit-body-refuses-an-append-parameter ()
  "A call still sending `append' is refused, and the body is untouched.
The tool publishes no such parameter, and a parameter no tool
publishes is refused by name rather than dropped, so a client written
against a surface that had the mode is told what it sent that no
longer exists.  It is refused whichever way the flag is spelled,
including the ones that used to mean replace, because what is refused
is the parameter and not its value."
  (dolist (append '(t "true" :json-false "false" "" nil "yes"))
    (org-mcp-test--with-temp-org-files
        ((test-file org-mcp-test--content-body-to-edit))
      (org-mcp-test--call-tool-refused
       "org-node-set-content"
       `((link . ,(org-mcp-test--file-link test-file "*Task"))
         (before . "old text")
         (after . "new text")
         (append . ,append))
       "\\`Unexpected parameter: append\\'"
       test-file))))

(ert-deftest org-mcp-test-edit-body-publishes-no-append-parameter ()
  "org-node-set-content's schema names link, before, after and files.
The mode is gone from the surface a client reads, not only from the
paths it reaches, so a client never plans a call around it."
  (org-mcp-test--with-enabled
    (let ((properties
           (org-mcp-test--registered-tool-properties
            "org-node-set-content")))
      (should
       (equal (sort (copy-sequence properties) #'string<)
              '("after" "before" "files" "link")))
      (should
       (equal
        (sort
         (copy-sequence
          (org-mcp-test--registered-tool-required
           "org-node-set-content"))
         #'string<)
        '("after" "before" "link"))))))

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
       org-mcp-test--regex-body-mixed-case-replaced link))))

(ert-deftest org-mcp-test-edit-body-add-line-before-sibling ()
  "Adding to a body followed by a sibling keeps the blank line between.
The text goes on the line after the body's last line, and the blank
line before the sibling stays the only one, so the body's end is
where the sibling's own text begins and not a line earlier."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-body-before-sibling))
    (let ((link (org-mcp-test--file-link test-file "*Parent")))
      (org-mcp-test--call-edit-body-and-check
       test-file link "Parent body." "Parent body.\nAppended."
       org-mcp-test--regex-body-before-sibling-added link))))

(ert-deftest org-mcp-test-edit-body-headline-error ()
  "A body that would add a headline at the same level is refused."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--call-tool-refused
       "org-node-set-content"
       `((link . ,link)
         (before . "Task body text.")
         (after . "* A headline"))
       "\\`Body cannot contain headlines at level 1 or higher\\'"
       test-file))))

(ert-deftest org-mcp-test-edit-body-unbalanced-blocks-error ()
  "A body whose #+BEGIN block is never closed is refused."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (org-mcp-test--call-tool-refused
       "org-node-set-content"
       `((link . ,link)
         (before . "Task body text.")
         (after . "#+BEGIN_SRC\ncode\n"))
       "\\`Body contains unclosed SRC block\\'"
       test-file))))

(ert-deftest org-mcp-test-edit-body-id-link ()
  "org-node-set-content reaches a node through an `id:' link."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-todo-with-test-id
   `(,org-mcp-test--crud-test-id)
   (let* ((link (concat "id:" org-mcp-test--crud-test-id))
          (params `((link . ,link)
                    (before . ,(org-mcp-test--content-digest-of link))
                    (after . "Rewritten.")))
          (result-text
           (mcp-server-lib-ert-call-tool "org-node-set-content" params))
          (result (json-read-from-string result-text)))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'link result) link)))))

;;; A note nobody sent does not cost the change it rode on

;; `note' is optional, and a client fills an optional parameter it is
;; not using with a blank.  Every spelling of one therefore means the
;; same thing — no note — and the state change goes through without
;; one.  Getting this wrong cost more than a refusal: the note was
;; written inside the change the state change was made in, so failing
;; to write it took the state change down with it.

(defconst org-mcp-test--note-blanks
  (list :json-false [] nil "" "   ")
  "Every spelling of a `note' the call is not sending.
JSON false and [] are what a client fills an unused parameter with,
null is JSON's own word for nothing, and a string of whitespace is
prose with nothing in it.")

(defconst org-mcp-test--pattern-task-one-done-unlogged
  (concat
   "\\`\\* DONE Task One\n"
   "CLOSED: \\[[^]]+\\]\n"
   ":LOGBOOK:\n"
   "- CLOSING NOTE \\[[^]]+\\]\n"
   ":END:\n"
   "Task description\\.\n?\\'")
  "Pattern after a DONE that a blank `note' left the prose out of.
`org-log-done' still records the transition; what the blank leaves
out is prose under that entry's heading line.")

(ert-deftest org-mcp-test-set-todo-blank-note-still-moves-the-state ()
  "Every blank `note' moves the TODO state and records no prose.
A blank is the parameter the call did not send, so it asks for
nothing and costs nothing — least of all the state change it was
sent alongside."
  (dolist (blank org-mcp-test--note-blanks)
    (org-mcp-test--with-temp-org-files
        ((test-file "* TODO Task One\nTask description."))
      (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
            (org-log-done 'note)
            (org-log-into-drawer t))
        (let ((result
               (json-read-from-string
                (mcp-server-lib-ert-call-tool
                 "org-node-set-todo"
                 `((link
                    .
                    ,(org-mcp-test--file-link test-file "*Task One"))
                   (before . "TODO")
                   (after . "DONE")
                   (note . ,blank))))))
          (should (equal (alist-get 'success result) t))
          (should (eq (alist-get 'saved result) t))
          (should (equal (alist-get 'before result) "TODO"))
          (should (equal (alist-get 'after result) "DONE")))
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--pattern-task-one-done-unlogged)))))

(ert-deftest org-mcp-test-set-todo-blank-note-moves-the-state-unlogged ()
  "A blank `note' moves the state where no log setting records it either.
With nothing arming an entry there is no entry for prose to go
under, and the blank asked for none, so the transition is the whole
of what the call does."
  (dolist (blank org-mcp-test--note-blanks)
    (org-mcp-test--with-temp-org-files
        ((test-file "* TODO Task One\nTask description."))
      (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
            (org-log-done nil)
            (org-log-into-drawer t))
        (let ((result
               (org-mcp-test--call-update-todo-state
                (org-mcp-test--file-link test-file "*Task One")
                "DONE" "TODO" blank)))
          (should (equal (alist-get 'success result) t))
          (should (equal (alist-get 'after result) "DONE")))
        (org-mcp-test--verify-file-matches
         test-file
         "\\`\\* DONE Task One\nTask description\\.\n?\\'")))))

(ert-deftest org-mcp-test-set-todo-refuses-a-note-that-is-no-text ()
  "A `note' that is neither text nor blank is a malformed call.
It names the parameter the client sent rather than the machinery
behind it, and nothing is written."
  (org-mcp-test--with-temp-org-files
      ((test-file "* TODO Task One\nTask description."))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-log-done 'note)
          (org-log-into-drawer t))
      (dolist (value '(42 t))
        (org-mcp-test--call-tool-refused
         "org-node-set-todo"
         `((link . ,(org-mcp-test--file-link test-file "*Task One"))
           (before . "TODO")
           (after . "DONE")
           (note . ,value))
         "\\`note must be a string, not "
         test-file)))))

(ert-deftest org-mcp-test-add-note-refuses-a-blank-note-legibly ()
  "org-node-add-note refuses a blank `note' as the missing parameter.
Its `note' is what the call is for, so a blank is not a note it does
without — it is the call with nothing in it.  The refusal says which
parameter, and an empty string says instead that the note itself was
empty, which is the same refusal Org would give it."
  (org-mcp-test--with-temp-org-files
      ((test-file "* TODO Task One\nTask description."))
    (let ((link (org-mcp-test--file-link test-file "*Task One")))
      (dolist (blank '(:json-false []))
        (org-mcp-test--call-tool-refused
         "org-node-add-note"
         `((link . ,link) (note . ,blank))
         "\\`Missing required parameter: note\\'"
         test-file))
      (dolist (empty '("" "   "))
        (org-mcp-test--call-tool-refused
         "org-node-add-note"
         `((link . ,link) (note . ,empty))
         "\\`Note cannot be empty or whitespace-only\\'"
         test-file))
      (org-mcp-test--call-tool-refused
       "org-node-add-note"
       `((link . ,link) (note . 42))
       "\\`note must be a string, not "
       test-file))))

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

(ert-deftest org-mcp-test-tags-agree-on-a-child-inside-a-node ()
  "A child expanded inside its parent carries the tags a read gives it.
The third read path is a walk: `depth' expands a child in place, and
that child is built by the same builder from the same
`org-get-tags' call, so what the walk shows and what a read of the
child shows are one answer rather than two that agree by accident."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-inherited-tags))
    (let ((org-use-tag-inheritance t)
          (org-tags-exclude-from-inheritance nil))
      (let* ((parent
              (json-parse-string
               (mcp-server-lib-ert-call-tool
                "org-node-read"
                `((link
                   . ,(org-mcp-test--file-link
                       test-file "*Tagged Parent"))
                  (depth . 1)))
               :object-type 'alist))
             (child (aref (alist-get 'children parent) 0)))
        (should (equal (alist-get 'title child) "Tagged Child"))
        (should
         (equal (alist-get 'tags child) ["filetag" "ptag" "ctag"]))
        (should (equal (alist-get 'local_tags child) ["ctag"]))
        (should
         (equal (alist-get 'tags child)
                (alist-get
                 'tags
                 (org-mcp-test--read-structured
                  test-file "Tagged Child"))))))))

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

(defconst org-mcp-test--regex-links-alpha-body-extended
  (concat
   "\\`"
   (regexp-quote org-mcp-test--content-links-preamble)
   "\\* Alpha\n"
   " *:PROPERTIES:\n"
   " *:CUSTOM_ID: +alpha-slug\n"
   " *:END:\n"
   "Alpha body\\.\n"
   "Alpha extended\\.\n"
   "\\*\\* Review\n"
   "Alpha review\\.\n"
   (regexp-quote org-mcp-test--content-links-beta)
   (regexp-quote org-mcp-test--content-links-gamma)
   "\\'")
  "Regex matching the links file once Alpha's body gains a line.")

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
         `((link . ,(format "file:%s" test-file)) (before . []) (after . "work")))))
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
                     (funcall with-files `((link . ,link) (before . []) (after . "work")))
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
               "org-node-set-tags" `((link . ,link) (before . []) (after . "work"))))))
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
               `((link . ,(format form test-file)) (before . []) (after . "work"))))))
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
               `((link . ,(format form test-file)) (before . []) (after . "work"))))))
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
     "Alpha body."
     "Alpha body.\nAlpha extended."
     org-mcp-test--regex-links-alpha-body-extended
     (org-mcp-test--file-link test-file "#alpha-slug"))))

(ert-deftest org-mcp-test-link-heading-tools ()
  "Every other tool that changes a heading accepts a link."
  (dolist (case
           '(("org-node-set-properties"
              (before . ((FOO)))
              (after . ((FOO . "bar"))))
             ("org-node-set-scheduled" (before . "") (after . "2026-03-27"))
             ("org-node-set-deadline" (before . "") (after . "2026-03-27"))
             ("org-node-set-priority" (before . "") (after . "A"))
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
  "Clock tools accept links: add and delete by ID, in by title, out bracketed."
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
     `((link . ,(format "[[file:%s::*Gamma][Gamma]]" test-file))
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
                       ("org-node-set-tags" (link . ,link) (before . []) (after . "work"))))
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
                 ("org-node-set-tags" (link . ,link) (before . []) (after . "work"))))
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
         "org-node-set-todo"
         `((link . ,link) (before . "TODO") (after . "DONE"))
         "not in allowed list"
         out))
      (org-mcp-test--with-id-tracking
          (list allowed)
          `((,org-mcp-test--content-with-id-id . ,with-id))
        (let ((link (format "id:%s" org-mcp-test--content-with-id-id)))
          (org-mcp-test--call-tool-refused
           "org-node-text" `((link . ,link)) "not in allowed list")
          (org-mcp-test--call-tool-refused
           "org-node-set-todo"
           `((link . ,link) (before . "TODO") (after . "DONE"))
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
                       ("org-node-set-tags" (link . ,link) (before . []) (after . "work"))
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
                       ("org-node-set-tags" (link . ,link) (before . []) (after . "work"))))
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
A line starting with `* ' is a heading even inside a block, as Org
parses it, and is listed.  A line escaped with a comma, as Org writes
one inside a block, is no heading: it stays in the preamble, and a
title link to it is refused by a read and by a write, leaving the file
unchanged."
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
         "org-node-set-tags" `((link . ,escaped) (before . []) (after . "oops"))
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
                 ("org-node-set-tags" (link . ,link) (before . []) (after . "work"))))
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
                   ("org-node-set-tags" (link . ,link) (before . []) (after . "work"))))
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
                                  (before . []) (after . "work"))))
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
                       "org-node-set-tags" `((link . ,link) (before . []) (after . "work"))
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
                        (link . ,address)
                        (before . "TODO")
                        (after . "DONE"))
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
                        (link . ,link) (before . "") (after . "TODO"))
                       ("org-node-set-title"
                        (link . ,link)
                        (before . "Beta")
                        (after . "Beta Renamed"))
                       ("org-node-set-content"
                        (link . ,link)
                        (before . "Beta body.")
                        (after . "Beta body rewritten."))
                       ("org-node-set-properties"
                        (link . ,link)
                        (before . ((EFFORT)))
                        (after . ((EFFORT . "1:00"))))
                       ("org-node-set-scheduled"
                        (link . ,link) (before . "") (after . "2026-03-27"))
                       ("org-node-set-deadline"
                        (link . ,link) (before . "") (after . "2026-03-28"))
                       ("org-node-set-tags" (link . ,link) (before . []) (after . "work"))
                       ("org-node-set-priority"
                        (link . ,link) (before . "") (after . "A"))
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
               (before . "TODO")
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
                               ("org-node-set-tags" (link . ,form) (before . []) (after . "work"))
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
    "org-node-set-properties"
    `((link . ,link)
      (before . ((SEEN)))
      (after . ((SEEN . "yes")))))))

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
                   "org-node-set-tags" `((link . ,gamma) (before . []) (after . "work")))))
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
                `((link . ,(org-mcp-test--file-link test-file "*Beta")) (before . []) (after . "work"))))))
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
            (org-mcp-test--file-link test-file "other-anchor")
            "link")))
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
        (alist-get
         'link (org-mcp-test--call-clock-out gamma "2026-03-23T16:45:00"))
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

;;; Null and false are the parameter left out

(ert-deftest org-mcp-test-null-before-is-a-parameter-left-out ()
  "A blank `before\=' asserts nothing; it is the parameter left out.
Clients fill a parameter they are not using with a blank, so reading
null as \"the field held nothing\" would let such a client vouch for
an emptiness it never saw and go on to destroy what was there.  The
assertion of absence is the empty string, which a call has to type."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-scheduled))
    (let ((link (org-mcp-test--file-link test-file "*Scheduled Task"))
          (missing "\\`Missing required parameter: before\\'"))
      (dolist (blank '(nil :json-false []))
        (org-mcp-test--call-tool-refused
         "org-node-set-todo"
         `((link . ,link) (before . ,blank) (after . "DONE"))
         missing test-file)
        (org-mcp-test--call-tool-refused
         "org-node-set-title"
         `((link . ,link) (before . ,blank) (after . "Renamed"))
         missing test-file)
        (org-mcp-test--call-tool-refused
         "org-node-set-scheduled"
         `((link . ,link) (before . ,blank) (after . "2026-04-15"))
         missing test-file)
        (org-mcp-test--call-tool-refused
         "org-node-set-scheduled"
         `((link . ,link) (before . ,blank) (after))
         missing test-file)
        (org-mcp-test--call-tool-refused
         "org-node-delete"
         `((link . ,link) (before . ,blank))
         missing test-file)))))

(ert-deftest org-mcp-test-null-before-left-out-on-deadline-and-priority ()
  "The same rule on the two fields whose headings live elsewhere."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-deadline))
    (let ((link (org-mcp-test--file-link test-file "*Deadline Task"))
          (missing "\\`Missing required parameter: before\\'"))
      (dolist (blank '(nil :json-false []))
        (org-mcp-test--call-tool-refused
         "org-node-set-deadline"
         `((link . ,link) (before . ,blank) (after . "2026-04-15"))
         missing test-file)
        (org-mcp-test--call-tool-refused
         "org-node-set-deadline"
         `((link . ,link) (before . ,blank) (after))
         missing test-file))))
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-priority))
    (let ((link (org-mcp-test--file-link test-file "*Priority Task"))
          (missing "\\`Missing required parameter: before\\'"))
      (dolist (blank '(nil :json-false []))
        (org-mcp-test--call-tool-refused
         "org-node-set-priority"
         `((link . ,link) (before . ,blank) (after . "A"))
         missing test-file)
        (org-mcp-test--call-tool-refused
         "org-node-set-priority"
         `((link . ,link) (before . ,blank) (after))
         missing test-file)))))

(ert-deftest org-mcp-test-blank-property-map-is-a-parameter-left-out ()
  "A blank property map is the parameter left out, on either side."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (let ((link
           (org-mcp-test--file-link test-file "*Task with Properties")))
      (dolist (blank '(nil :json-false "" []))
        (org-mcp-test--call-tool-refused
         "org-node-set-properties"
         `((link . ,link)
           (before . ,blank)
           (after . ((EFFORT . "2:00"))))
         "\\`Missing required parameter: before\\'"
         test-file)
        (org-mcp-test--call-tool-refused
         "org-node-set-properties"
         `((link . ,link)
           (before . ((EFFORT . "1:00")))
           (after . ,blank))
         "\\`Missing required parameter: after\\'"
         test-file)))))

(ert-deftest org-mcp-test-a-null-before-is-honoured-when-it-is-true ()
  "A null `before\=' on a property the drawer lacks lets the write through.
The refused sibling asserts null against a property that is there.
This is the same assertion where it holds: the drawer carries no
NEWPROP, so the call writes one, which is how a client creates a
property it has read the heading and found nothing for."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-todo-with-props))
    (let* ((link
            (org-mcp-test--file-link test-file "*Task with Properties"))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-set-properties"
              `((link . ,link)
                (before . ((NEWPROP)))
                (after . ((NEWPROP . "written"))))))))
      (should (equal (alist-get 'properties_set result) ["NEWPROP"]))
      (should (equal (alist-get 'properties_deleted result) []))
      (should (equal (alist-get 'before result) '((NEWPROP)))))))

(ert-deftest org-mcp-test-no-field-clearing-tools-are-published ()
  "Removing a field is a setter with an empty `after\=', not a tool.
One tool per field is the surface convention, and a client that
found a second id for the same field would have two spellings of one
change to choose between.  The ids are absent from the schema, and a
call to one is an unknown tool rather than a silent success."
  (org-mcp-test--with-configured-server
      ((test-file org-mcp-test--content-todo-with-scheduled))
      ()
    (dolist (id
             '("org-node-remove-deadline"
               "org-node-remove-priority"
               "org-node-remove-properties"
               "org-node-remove-scheduled"))
      (should-not (member id (org-mcp-test--registered-tool-ids)))
      (org-mcp-test--call-tool-refused
       id
       `((link . ,(org-mcp-test--file-link test-file "*Scheduled Task"))
         (before . "<2026-03-01 Sun>"))
       (concat "\\`Tool not found: " id "\\'")
       test-file))))

(ert-deftest org-mcp-test-blank-after-on-set-title-is-left-out ()
  "A blank new title is the parameter left out, as everywhere else.
A client that fills a parameter it is not using sends null, false or
[], and none of them names a title, so the call says nothing about
what the heading is to be called and nothing is written.  A value
that is no text at all is a malformed call, named as such rather
than reaching Org.  The empty string is text, and it keeps the
refusal a title of no text has always had.

`before' is read the same way, so neither parameter of this tool
answers a client with an internal error."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (dolist (blank '(nil :json-false []))
        (org-mcp-test--call-tool-refused
         "org-node-set-title"
         `((link . ,link) (before . "Simple Task") (after . ,blank))
         "\\`Missing required parameter: after\\'"
         test-file))
      (org-mcp-test--call-tool-refused
       "org-node-set-title"
       `((link . ,link) (before . "Simple Task") (after . 5))
       "\\`after must be a string, not 5\\'"
       test-file)
      (org-mcp-test--call-tool-refused
       "org-node-set-title"
       `((link . ,link) (before . "Simple Task") (after . ""))
       "\\`Headline title cannot be empty or contain only \
whitespace\\'"
       test-file)
      (org-mcp-test--call-tool-refused
       "org-node-set-title"
       `((link . ,link) (before . 5) (after . "Renamed"))
       "\\`before must be a string, not 5\\'"
       test-file))))

(ert-deftest org-mcp-test-blank-before-on-set-content-is-left-out ()
  "A blank body precondition is the parameter left out, body or none.
org-node-set-content asserts a body the way every other write
asserts a field, so a node with nothing in its body answers a blank
the same as a node with something in it: nothing is written either
way, and the refusal names the parameter that never arrived rather
than reporting on a file the call said nothing about.

The empty string is not among the blanks.  It is the value naming a
body with nothing in it, the assertion a client has to type, and it
keeps the refusal it has always had."
  (org-mcp-test--with-set-content-file test-file
    (let ((target (org-mcp-test--set-content-link))
          (bare (org-mcp-test--file-link test-file "*Sibling"))
          (missing "\\`Missing required parameter: before\\'"))
      (dolist (blank '(nil :json-false []))
        (dolist (link (list target bare))
          (org-mcp-test--call-tool-refused
           "org-node-set-content"
           `((link . ,link) (before . ,blank) (after . "Rewritten."))
           missing
           test-file)))
      (org-mcp-test--call-tool-refused
       "org-node-set-content"
       `((link . ,target) (before . "") (after . "Rewritten."))
       "\\`conflict: An empty before asserts the node has no content,"
       test-file))))

(ert-deftest org-mcp-test-blank-after-on-set-content-is-left-out ()
  "A blank body to write is the parameter left out, whatever before says.
`after' carries the text the body is to hold, and a client that
fills a parameter it is not using leaves no text there, so the call
says nothing about what the body should become and nothing is
written.  It reads the same in all three ways a body is asserted -
a substring of it, the \"\" that says it has none, and the digest
over the whole of it - because what is missing is the text to write
and not the assertion.

The empty string is not among the blanks: it is the text naming a
body with nothing in it, and a body is emptied by sending it."
  (org-mcp-test--with-set-content-file test-file
    (let ((target (org-mcp-test--set-content-link))
          (bare (org-mcp-test--file-link test-file "*Sibling"))
          (missing "\\`Missing required parameter: after\\'"))
      (dolist (blank '(nil :json-false []))
        (pcase-dolist (`(,link ,before)
                       `((,target "Second line of the body.")
                         (,target ,(org-mcp-test--content-digest-of target))
                         (,bare "")))
          (org-mcp-test--call-tool-refused
           "org-node-set-content"
           `((link . ,link) (before . ,before) (after . ,blank))
           missing
           test-file))))))

(ert-deftest org-mcp-test-non-string-after-on-set-content-is-malformed ()
  "An `after\=' that is no kind of text is a malformed call.
It is refused as validation and not as a conflict: nothing about the
file is in question, so reading the node again would not help."
  (org-mcp-test--with-set-content-file test-file
    (org-mcp-test--call-tool-refused
     "org-node-set-content"
     `((link . ,(org-mcp-test--set-content-link))
       (before . "Second line of the body.")
       (after . 3))
     "\\`after must be a string, not 3\\'"
     test-file)))

(ert-deftest org-mcp-test-every-body-write-reads-its-before ()
  "Every way to change a body reads `before\=', so none writes unguarded.
The parameter is required, and a blank one refuses the call rather
than reaching a path that has no use for it.  A body is added to by
asserting what it holds and sending it back with the addition in,
which is the same guarded write as any other."
  (org-mcp-test--with-enabled
    (should
     (member
      "before"
      (org-mcp-test--registered-tool-required "org-node-set-content"))))
  (org-mcp-test--with-set-content-file test-file
    (let ((link (org-mcp-test--set-content-link)))
      (dolist (blank '(nil :json-false []))
        (org-mcp-test--call-tool-refused
         "org-node-set-content"
         `((link . ,link) (before . ,blank) (after . "Added line."))
         "\\`Missing required parameter: before\\'"
         test-file))
      (org-mcp-test--call-edit-body-and-check
       test-file
       link
       (org-mcp-test--content-digest-of link)
       (concat
        "First line of the body.\n"
        "Second line of the body.\n"
        "Appended line.")
       org-mcp-test--set-content-line-added
       link))))

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
          `((link . ,link) (before . "TODO") (after . "NOPE"))))
        'validation)))))

(ert-deftest org-mcp-test-clock-out-refuses-without-a-running-clock ()
  "org-clock-out with no clock running is refused as a conflict.
The client believed a clock ran; reading the active clock again is
what puts that right, so the refusal is marked.  The heading `link'
names exists and holds no clock, so it is the belief that a clock
runs, and not the link, that the refusal answers."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-task-content))
    (should
     (equal
      (org-mcp-test--call-tool-expecting-error
       test-file "org-clock-out"
       `((link . ,(org-mcp-test--file-link test-file "*Task One"))))
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
           (before . "TODO")
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
           (before . "TODO")
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
              (org-mcp-test--file-link test-file "*Child") "DONE"
              "TODO")))
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

(ert-deftest org-mcp-test-fields-default-says-the-digests-are-out ()
  "A tool's description says the digests are not in its default.
It is the text a model reads before deciding whether it must ask for
a digest, so a description promising every field is the one that
sends a client into a write with no token to assert with.  The node a
default read returns is checked against the same claim, so the
sentence and the list cannot drift apart."
  (org-mcp-test--with-id-setup test-file org-mcp-test--content-node-shape
      (list org-mcp-test--node-shape-parent-id)
    (dolist (tool '("org-node-read" "org-query"))
      (should
       (string-match-p
        "Defaults to every field below[^.]*digests"
        (org-mcp-test--registered-tool-description tool))))
    (let ((node
           (org-mcp-test--read-fields
            (concat "id:" org-mcp-test--node-shape-parent-id) nil)))
      (should-not (assq 'digest node))
      (should-not (assq 'content_digest node)))))

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
         (before . []) (after . ["later"])))
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
  "\\`\\* TODO \\[#A\\] Ship v3 \\[1/3\\]\nBody\\.\n\\'"
  "Regex matching the cookie-title file after the rename.
The cookie outlives the rename: a read normalized it away, so the
new title cannot carry it, and Org never puts a cookie back.")

(defconst org-mcp-test--content-plain-title
  "* TODO Ship v2\nBody.\n"
  "A heading whose title carries no statistics cookie.")

(defconst org-mcp-test--regex-plain-title-renamed
  "\\`\\* TODO Ship v3\nBody\\.\n\\'"
  "Regex matching the plain-title file after the rename.")

(defconst org-mcp-test--regex-cookie-title-replaced
  "\\`\\* TODO \\[#A\\] Ship v3 \\[2/5\\]\nBody\\.\n\\'"
  "Regex matching the cookie-title file renamed with a new cookie.")

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

(ert-deftest org-mcp-test-rename-keeps-the-statistics-cookie ()
  "A rename carries the heading's statistics cookie over.
A read normalizes the cookie away, so `after' has none to send back,
and writing it verbatim would take the cookie off the heading for
good: Org refreshes a cookie that is there and never adds one, so
the parent's progress display would not come back.  A heading that
carried none gains none, and an `after' naming a cookie of its own
is written as it stands."
  (org-mcp-test--with-temp-org-files
      ((kept org-mcp-test--content-cookie-title)
       (none org-mcp-test--content-plain-title)
       (named org-mcp-test--content-cookie-title))
    (org-mcp-test--call-rename-headline-and-check
     (org-mcp-test--file-link kept "*Ship v2")
     "Ship v2" "Ship v3" kept
     org-mcp-test--regex-cookie-title-renamed)
    (org-mcp-test--call-rename-headline-and-check
     (org-mcp-test--file-link none "*Ship v2")
     "Ship v2" "Ship v3" none
     org-mcp-test--regex-plain-title-renamed)
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-set-title"
             `((link . ,(org-mcp-test--file-link named "*Ship v2"))
               (before . "Ship v2")
               (after . "Ship v3 [2/5]"))))))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'before result) "Ship v2"))
      (should (equal (alist-get 'after result) "Ship v3 [2/5]"))
      (should
       (equal (alist-get 'link result)
              (org-mcp-test--file-link named "*Ship v3")))
      (org-mcp-test--verify-file-matches
       named org-mcp-test--regex-cookie-title-replaced))))

;;; One accessor for every asserted field

(defconst org-mcp-test--content-every-asserted-field
  "* TODO [#B] Every Field :work:
SCHEDULED: <2026-06-20 Sat>--<2026-06-21 Sun> DEADLINE: <2026-07-01 Wed>
:PROPERTIES:
:FOO: nil
:END:
Body line.
"
  "A heading carrying every field a read returns and a write asserts.
Each one is spelled the way a second accessor gets wrong: the
SCHEDULED is a date range, which stops at the first `>' when a
planning line is read as an entry property, and the drawer holds a
property whose text is `nil', which reads as no property at all
unless the text is taken literally.  The title, the TODO keyword,
the priority and the heading's own tags round out the list.")

(defconst org-mcp-test--asserted-fields
  '((title "org-node-set-title" "Renamed")
    (todo "org-node-set-todo" "DONE")
    (priority "org-node-set-priority" "C")
    (scheduled "org-node-set-scheduled" "2026-08-01")
    (deadline "org-node-set-deadline" "2026-08-02")
    (local_tags "org-node-set-tags" ["home"])
    (properties "org-node-set-properties" ((FOO . "bar"))))
  "Every field of a node that a read returns and a write asserts.
Each entry is the key a read returns the field under, the tool whose
`before' asserts it, and an `after' that tool writes.  A field that
gains both a read and an assertion belongs here: the test over this
table is what fails when the two paths are pointed at two accessors
again.

`content' and the two digests are not fields of this kind.  Their
assertion is a substring or a `sha256:' token rather than the value
a read returns, and both paths already take their region from
`org-mcp--body-bounds' and `org-mcp--subtree-bounds'.")

(ert-deftest org-mcp-test-a-read-value-is-an-assertion-that-holds ()
  "Every field a read returns is a `before' its own setter accepts.
One named accessor per field serves the read surface and the
assertion path, so what a read hands back is what an assertion is
compared against.  Two accessors refused a true belief for good: a
date range read whole and truncated on comparison, a property read
as the text `nil' and compared as absence.

Each field is asserted against its own freshly read node, so a write
that realigns the heading cannot make the next assertion hold by
accident."
  (pcase-dolist (`(,field ,tool ,after) org-mcp-test--asserted-fields)
    (ert-info ((symbol-name field) :prefix "field: ")
      (org-mcp-test--with-temp-org-files
          ((test-file org-mcp-test--content-every-asserted-field))
        (let* ((link
                (org-mcp-test--file-link test-file "*Every Field"))
               (node
                (json-read-from-string
                 (mcp-server-lib-ert-call-tool
                  "org-node-read"
                  `((link . ,link) (properties . ["FOO"])))))
               (before (alist-get field node))
               (result
                (json-read-from-string
                 (mcp-server-lib-ert-call-tool
                  tool
                  `((link . ,link)
                    (before . ,before)
                    (after . ,after))))))
          (should (equal (alist-get 'success result) t))
          (should (eq (alist-get 'saved result) t)))))))

(defconst org-mcp-test--content-property-text-nil
  "* TODO Flagged Task
:PROPERTIES:
:FOO: nil
:END:
Body line.
"
  "A heading whose property holds the text org-node-set-properties
writes for JSON false.")

(defconst org-mcp-test--regex-property-text-nil-replaced
  "\\`\\* TODO Simple Task\n:PROPERTIES:\n:FOO: +bar\n:END:\nTask body text\\.\n\\'"
  "Regex matching the bare file once FOO holds bar, not the text nil.
The property arrives there by org-node-set-properties writing JSON
false and is replaced by the same tool, so the file is the one a
round trip through the two calls leaves.")

(defconst org-mcp-test--regex-property-text-nil-removed
  "\\`\\* TODO Flagged Task\nBody line\\.\n\\'"
  "Regex matching the file once FOO is gone, drawer and all.")

(ert-deftest org-mcp-test-a-property-whose-text-is-nil-asserts-as-nil ()
  "The text `nil\=' is asserted as itself and never as an absent property.
The whole way round in one test: org-node-set-properties writes the
text `nil\=' for JSON false, so the value is one this server creates
rather than one the file was seeded with; a read hands it back; and
that value, exactly as the read returned it, is the `before\=' the
next write asserts with.

Asserting the property absent is the stale belief the guard exists
to refuse, and it takes nothing away."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
      (should
       (equal
        (alist-get
         'properties_set
         (json-read-from-string
          (mcp-server-lib-ert-call-tool
           "org-node-set-properties"
           `((link . ,link)
             (before . ((FOO)))
             (after . ((FOO . :json-false)))))))
        ["FOO"]))
      (let ((read-back
             (alist-get
              'properties
              (json-read-from-string
               (mcp-server-lib-ert-call-tool
                "org-node-read"
                `((link . ,link) (properties . ["FOO"])))))))
        (should (equal read-back '((FOO . "nil"))))
        (org-mcp-test--call-tool-refused
         "org-node-set-properties"
         `((link . ,link)
           (before . ((FOO)))
           (after . ((FOO . "bar"))))
         "\\`conflict: Property 'FOO' mismatch: expected (absent), \
found 'nil'\\'"
         test-file)
        (org-mcp-test--call-tool-refused
         "org-node-set-properties"
         `((link . ,link)
           (before . ((FOO)))
           (after . ((FOO))))
         "\\`conflict: Property 'FOO' mismatch: expected (absent), \
found 'nil'\\'"
         test-file)
        (let ((result
               (json-read-from-string
                (mcp-server-lib-ert-call-tool
                 "org-node-set-properties"
                 `((link . ,link)
                   (before . ,read-back)
                   (after . ((FOO . "bar"))))))))
          (should (equal (alist-get 'success result) t))
          (should (eq (alist-get 'saved result) t))
          (should (equal (alist-get 'properties_set result) ["FOO"]))
          (org-mcp-test--verify-file-matches
           test-file
           org-mcp-test--regex-property-text-nil-replaced))))))

(ert-deftest org-mcp-test-removing-a-property-whose-text-is-nil ()
  "A deletion names the text `nil\=' it destroys and records it.
Its `before\=' is the map a read returned, sent back unchanged.  The
response is the only record left once the property is gone, so it
carries that value rather than the empty string a second accessor
reported for it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-property-text-nil))
    (let* ((link (org-mcp-test--file-link test-file "*Flagged Task"))
           (read-back
            (alist-get
             'properties
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-read"
               `((link . ,link) (properties . ["FOO"]))))))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-set-properties"
              `((link . ,link)
                (before . ,read-back)
                (after . ((FOO))))))))
      (should (equal read-back '((FOO . "nil"))))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'properties_deleted result) ["FOO"]))
      (should (equal (alist-get 'before result) read-back))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-property-text-nil-removed))))

(defconst org-mcp-test--content-scheduled-range
  "* TODO Ranged Task
SCHEDULED: <2026-06-20 Sat>--<2026-06-21 Sun>
Body line.
"
  "A heading whose SCHEDULED is a date range.")

(defconst org-mcp-test--content-deadline-range
  "* TODO Ranged Task
DEADLINE: <2026-07-01 Wed>--<2026-07-03 Fri>
Body line.
"
  "A heading whose DEADLINE is a date range.")

(defconst org-mcp-test--regex-range-removed
  "\\`\\* TODO Ranged Task\nBody line\\.\n\\'"
  "Regex matching the ranged file once the planning line is gone.
No half of the range is left behind: Org's own remover matches one
timestamp, so a range would leave `--<…>' on a line of its own where
the client reads it as body text.")

(defconst org-mcp-test--content-both-ranges
  "* TODO Ranged Task
SCHEDULED: <2026-06-20 Sat>--<2026-06-21 Sun> DEADLINE: <2026-07-01 Wed>--<2026-07-03 Fri>
Body line.
"
  "A heading whose SCHEDULED and DEADLINE are both date ranges.")

(defconst org-mcp-test--regex-deadline-range-kept
  "\\`\\* TODO Ranged Task\nDEADLINE: <2026-07-01 Wed>--<2026-07-03 Fri>\nBody line\\.\n\\'"
  "Regex matching the file once the ranged SCHEDULED alone is gone.")

(defun org-mcp-test--planning-read-back (link field)
  "Return FIELD of the node LINK names, as org-node-read returns it.
FIELD is `scheduled\=' or `deadline\='.  A test asserts with what this
returned rather than with a string of its own, so it fails if the
read and the assertion are ever pointed at different accessors."
  (alist-get
   field
   (json-read-from-string
    (mcp-server-lib-ert-call-tool "org-node-read" `((link . ,link))))))

(ert-deftest org-mcp-test-set-scheduled-removes-a-whole-range ()
  "A ranged SCHEDULED is read whole, asserted whole and removed whole.
`before\=' is the string the read returned, not one the test composed,
and the removal leaves no half of the range behind as body text."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-scheduled-range))
    (let* ((link (org-mcp-test--file-link test-file "*Ranged Task"))
           (before
            (org-mcp-test--planning-read-back link 'scheduled))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-set-scheduled"
              `((link . ,link) (before . ,before) (after))))))
      (should (equal before "<2026-06-20 Sat>--<2026-06-21 Sun>"))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'before result) before))
      (should (equal (alist-get 'after result) ""))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-range-removed))))

(ert-deftest org-mcp-test-set-deadline-removes-a-whole-range ()
  "A ranged DEADLINE is read whole, asserted whole and removed whole.
Shaped like the SCHEDULED case, over the other planning keyword."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-deadline-range))
    (let* ((link (org-mcp-test--file-link test-file "*Ranged Task"))
           (before (org-mcp-test--planning-read-back link 'deadline))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-set-deadline"
              `((link . ,link) (before . ,before) (after))))))
      (should (equal before "<2026-07-01 Wed>--<2026-07-03 Fri>"))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'before result) before))
      (should (equal (alist-get 'after result) ""))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-range-removed))))

(ert-deftest org-mcp-test-set-scheduled-removal-leaves-the-deadline ()
  "Removing one ranged planning entry leaves the other where it was.
Both keywords share a line, so a removal that ran past the end of
its own entry would take the neighbour with it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-both-ranges))
    (let* ((link (org-mcp-test--file-link test-file "*Ranged Task"))
           (before
            (org-mcp-test--planning-read-back link 'scheduled))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-set-scheduled"
              `((link . ,link) (before . ,before) (after))))))
      (should (equal (alist-get 'success result) t))
      (should (equal (alist-get 'before result) before))
      (should (equal (alist-get 'after result) ""))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-deadline-range-kept))))

(defconst org-mcp-test--content-accumulating-property
  "* TODO Joined Up
:PROPERTIES:
:FOO: one
:FOO+: two
:END:
Body line.
"
  "A drawer whose second line adds to the property the first writes.
Org joins them, so the node carries one property holding `one two'.")

(defconst org-mcp-test--regex-accumulating-property-removed
  "\\`\\* TODO Joined Up\nBody line\\.\n\\'"
  "Regex matching the joined-up file once the property is gone.")

(ert-deftest org-mcp-test-an-accumulating-property-is-one-property ()
  "A `NAME+' line adds to NAME rather than writing it a second time.
Org joins the two lines into the one value a read returns, so there
is a value to assert and a value to destroy.  It is not the drawer
that writes one name twice, which has neither."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-accumulating-property))
    (let ((link (org-mcp-test--file-link test-file "*Joined Up")))
      (should
       (equal
        (alist-get
         'properties
         (json-read-from-string
          (mcp-server-lib-ert-call-tool
           "org-node-read"
           `((link . ,link) (properties . ["FOO"])))))
        '((FOO . "one two"))))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-set-properties"
               `((link . ,link)
                 (before . ((FOO . "one two")))
                 (after . ((FOO))))))))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'properties_deleted result) ["FOO"]))
        (should (equal (alist-get 'before result) '((FOO . "one two"))))
        (org-mcp-test--verify-file-matches
         test-file
         org-mcp-test--regex-accumulating-property-removed)))))

(defconst org-mcp-test--regex-accumulating-property-set
  (concat
   "\\`\\* TODO Joined Up\n"
   ":PROPERTIES:\n"
   ":FOO: +three\n"
   ":END:\n"
   "Body line\\.\n\\'")
  "Regex matching the joined-up file once the property is set afresh.
The lines the new value supersedes are gone, so the property holds
what the call asked for and nothing else.")

(ert-deftest org-mcp-test-a-set-supersedes-what-a-property-accumulated ()
  "A set writes the whole of what the property holds, accumulators included.
`org-set-property' writes the plain line and leaves a `NAME+' line
standing, which would leave the property reading as the new value and
the old addition together while the response called it set.  The call
asserted the joined value, so the lines that value came from are the
lines it replaces."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-accumulating-property))
    (let* ((link (org-mcp-test--file-link test-file "*Joined Up"))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-set-properties"
              `((link . ,link)
                (before . ((FOO . "one two")))
                (after . ((FOO . "three"))))))))
      (should (equal (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'properties_set result) ["FOO"]))
      (should (equal (alist-get 'properties_deleted result) []))
      (should (equal (alist-get 'before result) '((FOO . "one two"))))
      (should (equal (alist-get 'link result) link))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-accumulating-property-set)
      ;; The value the response reports set is the value a read hands
      ;; back, so the client's next `before' is the one it just sent.
      (should
       (equal
        (alist-get
         'properties
         (json-read-from-string
          (mcp-server-lib-ert-call-tool
           "org-node-read"
           `((link . ,link) (properties . ["FOO"])))))
        '((FOO . "three")))))))

(ert-deftest org-mcp-test-a-set-against-an-accumulator-asserts-the-whole ()
  "Half of a joined value is not the value the property holds.
The `before' a client sends is the value a read handed it, and a
read joins the lines, so asserting one line alone is a conflict and
nothing is written."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-accumulating-property))
    (org-mcp-test--call-tool-refused
     "org-node-set-properties"
     `((link . ,(org-mcp-test--file-link test-file "*Joined Up"))
       (before . ((FOO . "one")))
       (after . ((FOO . "three"))))
     "\\`conflict: Property 'FOO' mismatch: expected 'one', \
found 'one two'\\'"
     test-file)))

(ert-deftest org-mcp-test-an-accumulating-name-is-not-a-property ()
  "A name ending in `+' names no property a call can read or write.
`FOO+' is a line that adds to what FOO holds: no read reports it, so
a `before' for it asserts nothing, and a write under it would change
FOO behind an assertion that never named it.  Both ends refuse it and
the drawer is left alone."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-accumulating-property))
    (let ((link (org-mcp-test--file-link test-file "*Joined Up")))
      (dolist (value '("three" ""))
        (org-mcp-test--call-tool-refused
         "org-node-set-properties"
         `((link . ,link)
           (before . (("FOO+")))
           (after . (("FOO+" . ,value))))
         "\\`Not a property name: FOO\\+\\."
         test-file))
      (org-mcp-test--call-tool-refused
       "org-node-read"
       `((link . ,link) (properties . ["FOO+"]))
       "\\`Not a property name: FOO\\+\\."
       test-file))))

(defconst org-mcp-test--content-duplicate-property
  "* TODO Twice Told
:PROPERTIES:
:FOO: one
:FOO: two
:END:
Body line.
"
  "A drawer writing one property name on two lines.
Malformed Org: Org's own readers disagree about which line the
property is, so the value a call asserts cannot be pinned down.")

(defconst org-mcp-test--content-duplicate-and-accumulator
  "* TODO Twice Told
:PROPERTIES:
:FOO: one
:FOO+: two
:FOO: three
:END:
Body line.
"
  "A drawer writing one name twice with an accumulator between them.
The accumulator is legitimate; the second plain line is what makes
the drawer malformed, and it is still malformed with one there.")

(ert-deftest org-mcp-test-a-property-written-twice-refuses-legibly ()
  "A property written on two drawer lines refuses, naming the property.
Org reads such a drawer two ways — a scan reports the first line, a
lookup the last — so no assertion can be checked against it and no
write can say which line it lands on.  The refusal says what is
wrong with the file rather than reporting a mismatch the client
cannot resolve by reading again.

A `NAME+' line among them changes nothing: it adds to the name
rather than writing it, so it is neither what makes the drawer
malformed nor what excuses it."
  (org-mcp-test--with-temp-org-files
      ((plain org-mcp-test--content-duplicate-property)
       (mixed org-mcp-test--content-duplicate-and-accumulator))
    (dolist (test-file (list plain mixed))
      (let ((link (org-mcp-test--file-link test-file "*Twice Told")))
        (dolist (call
                 `(("org-node-set-properties"
                    ((link . ,link)
                     (before . ((FOO . "one")))
                     (after . ((FOO . "three")))))
                   ("org-node-set-properties"
                    ((link . ,link)
                     (before . ((FOO . "one")))
                     (after . ((FOO)))))))
          (org-mcp-test--call-tool-refused
           (car call) (cadr call)
           "\\`blocked: Property 'FOO' is written twice"
           test-file))))))

;;; Taking a whole node away

;; org-node-delete, org-node-archive and org-node-refile each take a
;; node from where it is, and each one asserts the subtree it is about
;; to move by echoing the `digest' a read handed the client.  These
;; tests call them the way a client calls them: read the node for a
;; token, then send the token back.

(defconst org-mcp-test--verbs-target-id
  "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  "ID of Target in `org-mcp-test--verbs-content'.")

(defconst org-mcp-test--verbs-content
  (concat
   "* TODO Keep :work:\n"
   "Keep body.\n"
   "* TODO Target\n"
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--verbs-target-id "\n"
   ":END:\n"
   ":LOGBOOK:\n"
   "- Note taken on [2026-03-20 Fri 09:00] \\\\\n"
   "  Decided this one.\n"
   ":END:\n"
   "Target body.\n"
   "** Child\n"
   "*** Grandchild\n"
   "* TODO Home\n"
   "Home body.\n"
   "** First child\n"
   "** Second child\n")
  "A file with a node to take away and a node to take it to.
Target carries an ID, a LOGBOOK, a body and two generations of
descendants, so a verb that drops part of a subtree is caught.  Home
has two children, so a move can name a position among them.")

(defconst org-mcp-test--verbs-target-gone
  (concat
   "\\`\\* TODO Keep :work:\n"
   "Keep body\\.\n"
   "\\* TODO Home\n"
   "Home body\\.\n"
   "\\*\\* First child\n"
   "\\*\\* Second child\n"
   "\\'")
  "The complete file after Target has left it.")

(defconst org-mcp-test--verbs-target-drawers
  (concat
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--verbs-target-id "\n"
   ":END:\n"
   ":LOGBOOK:\n"
   "- Note taken on \\[2026-03-20 Fri 09:00\\] \\\\\\\\\n"
   "  Decided this one\\.\n"
   ":END:\n"
   "Target body\\.\n")
  "Regexp matching everything Target carries below its heading line.
It is the same at every level, so a move that drops a drawer or the
LOGBOOK on the way fails wherever the node lands.")

(defconst org-mcp-test--verbs-target-drawers-refile-logged
  (concat
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--verbs-target-id "\n"
   ":END:\n"
   ":LOGBOOK:\n"
   "- Refiled on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\]\n"
   "- Note taken on \\[2026-03-20 Fri 09:00\\] \\\\\\\\\n"
   "  Decided this one\\.\n"
   ":END:\n"
   "Target body\\.\n")
  "`org-mcp-test--verbs-target-drawers' after a logged refile.
The refile entry joins the LOGBOOK the node already carries, above
the note that was in it, where Org puts the newest entry.  It is the
heading line of `org-log-note-headings' and nothing else: no note
body follows it, because no one was asked for one.")

(defconst org-mcp-test--verbs-target-refiled
  (concat
   "\\`\\* TODO Keep :work:\n"
   "Keep body\\.\n"
   "\\* TODO Home\n"
   "Home body\\.\n"
   "\\*\\* First child\n"
   "\\*\\* TODO Target\n"
   org-mcp-test--verbs-target-drawers
   "\\*\\*\\* Child\n"
   "\\*\\*\\*\\* Grandchild\n"
   "\\*\\* Second child\n"
   "\\'")
  "The complete file after Target moves under Home, after First child.
Every generation is one level deeper than it was and the LOGBOOK
travels with the node.")

(defconst org-mcp-test--verbs-target-refiled-logged
  (concat
   "\\`\\* TODO Keep :work:\n"
   "Keep body\\.\n"
   "\\* TODO Home\n"
   "Home body\\.\n"
   "\\*\\* First child\n"
   "\\*\\* TODO Target\n"
   org-mcp-test--verbs-target-drawers-refile-logged
   "\\*\\*\\* Child\n"
   "\\*\\*\\*\\* Grandchild\n"
   "\\*\\* Second child\n"
   "\\'")
  "The same file as `org-mcp-test--verbs-target-refiled', refile logged.
The move is the one that file records; the only difference is the
entry `org-log-refile' asked for.")

(defconst org-mcp-test--verbs-target-last-child
  (concat
   "\\`\\* TODO Keep :work:\n"
   "Keep body\\.\n"
   "\\* TODO Home\n"
   "Home body\\.\n"
   "\\*\\* First child\n"
   "\\*\\* Second child\n"
   "\\*\\* TODO Target\n"
   org-mcp-test--verbs-target-drawers
   "\\*\\*\\* Child\n"
   "\\*\\*\\*\\* Grandchild\n"
   "\\'")
  "The complete file after Target moves under Home naming no sibling.")

(defconst org-mcp-test--verbs-child-archived-in-place
  (concat
   "\\`\\* TODO Keep :work:\n"
   "Keep body\\.\n"
   "\\* TODO Target\n"
   org-mcp-test--verbs-target-drawers
   "\\* TODO Home\n"
   "Home body\\.\n"
   "\\*\\* First child\n"
   "\\*\\* Second child\n"
   "\n"
   "\\* Archived\n"
   "\n"
   "\\*\\* Child\n"
   ":PROPERTIES:\n"
   ":ARCHIVE_TIME: .+\n"
   ":ARCHIVE_FILE: .+\n"
   ":ARCHIVE_OLPATH: Target\n"
   ":ARCHIVE_CATEGORY: .+\n"
   ":END:\n"
   "\\*\\*\\* Grandchild\n"
   "\\'")
  "The complete file after Child is archived to a heading in it.
The time, the origin file and the category are whatever this run
makes of them; the outline path is the one fact the test pins,
because it is what says where the node was.")

(defconst org-mcp-test--verbs-target-at-top
  (concat
   "\\`\\* TODO Target\n"
   org-mcp-test--verbs-target-drawers
   "\\*\\* Child\n"
   "\\*\\*\\* Grandchild\n"
   "\\* TODO Keep :work:\n"
   "Keep body\\.\n"
   "\\* TODO Home\n"
   "Home body\\.\n"
   "\\*\\* First child\n"
   "\\*\\* Second child\n"
   "\\'")
  "The complete file after Target moves to the top level of its file.")

(defconst org-mcp-test--verbs-other-content
  (concat
   "#+TITLE: Projects\n"
   "\n"
   "* TODO Project One\n"
   "Project body.\n"
   "** Existing child\n"
   "* TODO Project Two\n")
  "A second file a node moves into.
It opens with a preamble, so a move to its top level has to land
after the preamble and before every heading, where a node created at
the top level lands.")

(defconst org-mcp-test--verbs-other-with-target
  (concat
   "\\`#\\+TITLE: Projects\n"
   "\n"
   "\\* TODO Project One\n"
   "Project body\\.\n"
   "\\*\\* Existing child\n"
   "\\*\\* TODO Target\n"
   org-mcp-test--verbs-target-drawers
   "\\*\\*\\* Child\n"
   "\\*\\*\\*\\* Grandchild\n"
   "\\* TODO Project Two\n"
   "\\'")
  "The complete second file after Target moves under Project One.")

(defconst org-mcp-test--verbs-other-with-target-logged
  (concat
   "\\`#\\+TITLE: Projects\n"
   "\n"
   "\\* TODO Project One\n"
   "Project body\\.\n"
   "\\*\\* Existing child\n"
   "\\*\\* TODO Target\n"
   org-mcp-test--verbs-target-drawers-refile-logged
   "\\*\\*\\* Child\n"
   "\\*\\*\\*\\* Grandchild\n"
   "\\* TODO Project Two\n"
   "\\'")
  "The complete second file after a logged refile of Target into it.
The entry is in this file, the one the node landed in, and not in
the one it left.")

(defconst org-mcp-test--refile-log-drawer-content
  (concat
   "* TODO Target\n"
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--verbs-target-id "\n"
   ":LOG_INTO_DRAWER: NOTES\n"
   ":END:\n"
   "* TODO Home\n")
  "A node naming its own log drawer, and somewhere to refile it to.")

(defconst org-mcp-test--refile-log-drawer-after
  (concat
   "\\`\\* TODO Home\n"
   "\\*\\* TODO Target\n"
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--verbs-target-id "\n"
   ":LOG_INTO_DRAWER: NOTES\n"
   ":END:\n"
   ":NOTES:\n"
   "- Refiled on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\]\n"
   ":END:\n"
   "\\'")
  "The complete file after Target, which names a log drawer, is refiled.
The entry is in the drawer the node names, not in a LOGBOOK.")

(defconst org-mcp-test--verbs-other-with-target-at-top
  (concat
   "\\`#\\+TITLE: Projects\n"
   "\n"
   "\\* TODO Target\n"
   org-mcp-test--verbs-target-drawers
   "\\*\\* Child\n"
   "\\*\\*\\* Grandchild\n"
   "\\* TODO Project One\n"
   "Project body\\.\n"
   "\\*\\* Existing child\n"
   "\\* TODO Project Two\n"
   "\\'")
  "The complete second file after Target moves to its top level.")

(defun org-mcp-test--verbs-link ()
  "Return the link to Target in `org-mcp-test--verbs-content'."
  (concat "id:" org-mcp-test--verbs-target-id))

(defun org-mcp-test--verbs-digest (&optional link)
  "Return the `digest' a read of LINK returns, as a client reads it.
LINK defaults to Target's."
  (alist-get
   'digest
   (org-mcp-test--read-fields
    (or link (org-mcp-test--verbs-link)) ["digest"])))

(defmacro org-mcp-test--with-verbs-file (file-var &rest body)
  "Bind FILE-VAR to a temp file of `org-mcp-test--verbs-content' for BODY."
  (declare (indent 1) (debug t))
  `(org-mcp-test--with-id-setup ,file-var org-mcp-test--verbs-content
       (list org-mcp-test--verbs-target-id)
     ,@body))

(defmacro org-mcp-test--with-verbs-files (file-var other-var &rest body)
  "Bind FILE-VAR and OTHER-VAR to the two verb fixtures for BODY.
Both are allowed files, and Target's ID is registered in FILE-VAR,
so a move between them is a move between two files a call reaches."
  (declare (indent 2) (debug t))
  `(org-mcp-test--with-temp-org-files
       ((,file-var org-mcp-test--verbs-content)
        (,other-var org-mcp-test--verbs-other-content))
     (org-mcp-test--with-id-tracking
      (list ,file-var ,other-var)
      (list (cons org-mcp-test--verbs-target-id ,file-var))
      ,@body)))

(ert-deftest org-mcp-test-node-delete-takes-the-whole-subtree ()
  "org-node-delete takes the node and every generation under it.
The response carries the link the node had, read while it was still
there, so a client can say which node it lost."
  (org-mcp-test--with-verbs-file test-file
    (let* ((link (org-mcp-test--verbs-link))
           (result
            (mcp-server-lib-ert-call-tool
             "org-node-delete"
             `((link . ,link)
               (before . ,(org-mcp-test--verbs-digest))))))
      (let ((response (json-read-from-string result)))
        (should (eq (alist-get 'success response) t))
        (should (eq (alist-get 'saved response) t))
        (should (equal (alist-get 'link response) link)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--verbs-target-gone))))

(ert-deftest org-mcp-test-node-delete-refuses-a-stale-digest ()
  "A token the node no longer carries refuses the delete, file untouched.
The refusal is a conflict: the call was well formed and the file has
moved on from what it asserted."
  (org-mcp-test--with-verbs-file test-file
    (let ((stale (org-mcp-test--verbs-digest))
          (link (org-mcp-test--verbs-link)))
      (mcp-server-lib-ert-call-tool
       "org-node-set-title"
       `((link . ,link) (before . "Target") (after . "Target renamed")))
      (org-mcp-test--call-tool-refused
       "org-node-delete"
       `((link . ,link) (before . ,stale))
       "\\`conflict: Subtree mismatch: .*nothing was deleted\\'"
       test-file))))

(ert-deftest org-mcp-test-node-verbs-withhold-the-digest-they-found ()
  "The conflict names the token the call sent, never the current one.
The current digest is the one value that would make the same call
succeed, so a refusal carrying it would make resending the call the
cheapest recovery there is — and a caller asserting a digest it never
read asserts nothing.  The refusal says the subtree moved and sends
the caller back to a read instead."
  (org-mcp-test--with-verbs-file test-file
    (let ((stale (org-mcp-test--verbs-digest))
          (link (org-mcp-test--verbs-link)))
      (mcp-server-lib-ert-call-tool
       "org-node-set-title"
       `((link . ,link) (before . "Target") (after . "Target renamed")))
      (let ((fresh (org-mcp-test--verbs-digest link)))
        (should-not (string= stale fresh))
        (dolist (call
                 `(("org-node-delete" . ((link . ,link) (before . ,stale)))
                   ("org-node-archive" . ((link . ,link) (before . ,stale)))
                   ("org-node-refile"
                    .
                    ((link . ,link)
                     (before . ,stale)
                     (parent
                      .
                      ,(org-mcp-test--file-link test-file "*Home"))))))
          (let ((message
                 (org-mcp-test--refusal-message (car call) (cdr call))))
            (should (string-match-p (regexp-quote stale) message))
            (should-not (string-match-p (regexp-quote fresh) message))
            (should (string-match-p "read the node again" message))))))))

(ert-deftest org-mcp-test-node-delete-refuses-after-a-descendant-moves ()
  "A change to a grandchild the client never read makes its token stale.
The read asked for the token alone, no children and no depth, and
the token still covers them: what a verb takes away is the subtree
entire, so that is what it asserts."
  (org-mcp-test--with-verbs-file test-file
    (let ((stale (org-mcp-test--verbs-digest))
          (link (org-mcp-test--verbs-link)))
      (mcp-server-lib-ert-call-tool
       "org-node-set-tags"
       `((link . ,(org-mcp-test--file-link test-file "*Grandchild"))
         (before . []) (after . ["later"])))
      (org-mcp-test--call-tool-refused
       "org-node-delete"
       `((link . ,link) (before . ,stale))
       "\\`conflict: Subtree mismatch:"
       test-file))))

(ert-deftest org-mcp-test-node-verbs-refuse-a-token-of-no-such-form ()
  "A value that is no digest is a malformed call, not a conflict.
A conflict would send the client back to read the same file and
assert with the same value again; naming the call malformed sends it
back for a token."
  (org-mcp-test--with-verbs-file test-file
    (dolist (tool '("org-node-delete" "org-node-archive"))
      (org-mcp-test--call-tool-refused
       tool
       `((link . ,(org-mcp-test--verbs-link))
         (before . "e3b0c44298fc1c14"))
       "\\`before must be the digest"
       test-file))
    (org-mcp-test--call-tool-refused
     "org-node-refile"
     `((link . ,(org-mcp-test--verbs-link))
       (before . "e3b0c44298fc1c14")
       (parent . ,(org-mcp-test--file-link test-file "*Home")))
     "\\`before must be the digest"
     test-file)))

(ert-deftest org-mcp-test-node-verbs-have-no-unguarded-spelling ()
  "None of the three can be called without saying what it acts on.
`before' is a required parameter of each, so a call that omits it
never reaches the file: there is no spelling of these verbs that
destroys something the caller did not name."
  (org-mcp-test--with-verbs-file test-file
    (dolist (tool '("org-node-delete" "org-node-archive"))
      (org-mcp-test--call-tool-refused
       tool
       `((link . ,(org-mcp-test--verbs-link)))
       "before"
       test-file))
    (org-mcp-test--call-tool-refused
     "org-node-refile"
     `((link . ,(org-mcp-test--verbs-link))
       (parent . ,(org-mcp-test--file-link test-file "*Home")))
     "before"
     test-file)))

(ert-deftest org-mcp-test-node-verbs-refuse-a-link-naming-a-file ()
  "A whole file is no node for these verbs to take away.
`org-mcp--goto-heading' refuses the link before anything is read or
written, so the file is as it was."
  (org-mcp-test--with-verbs-file test-file
    (let ((file-link (concat "file:" test-file)))
      (org-mcp-test--call-tool-refused
       "org-node-delete"
       `((link . ,file-link)
         (before . ,(org-mcp-test--verbs-digest file-link)))
       "\\`Link does not point to a heading:"
       test-file))))

(ert-deftest org-mcp-test-node-delete-description-points-at-archive ()
  "The delete tool's description steers a client toward archiving.
A page a model never opens steers nothing, so the pointer is where a
model reads: in the tool's own description."
  (org-mcp-test--with-verbs-file _test-file
    (should
     (string-match-p
      "org-node-archive"
      (org-mcp-test--registered-tool-description "org-node-delete")))))

(ert-deftest org-mcp-test-node-delete-refuses-the-node-the-clock-runs-in ()
  "A node the Emacs session's clock runs in is not deleted.
The CLOCK line would leave the file with the text while Emacs went on
believing a clock runs, and the user's next clock-out would fail with
no line left to close.  The refusal is the unmarked validation class,
not a conflict: the open CLOCK line was in the region the client
read, so its token is fresh and reading again resolves nothing.  The
clock is left running, because a tool that stops the user's clock on
their behalf is worse than one that declines."
  (org-mcp-test--with-verbs-file test-file
    (let ((link (org-mcp-test--verbs-link)))
      (org-mcp-test--call-clock-in link "2026-03-20T09:30:00")
      (org-mcp-test--with-session-clock test-file
        (org-mcp-test--call-tool-refused
         "org-node-delete"
         `((link . ,link)
           (before . ,(org-mcp-test--verbs-digest link)))
         "\\`The clock is running in this node.*org-clock-out"
         test-file)
        (should (org-clock-is-active))))))

(ert-deftest org-mcp-test-node-delete-takes-a-node-the-clock-is-not-in ()
  "A clock running in another node is no reason to refuse the delete.
The guard asks where the clock is, not whether one runs: Keep holds
the open CLOCK line here, so taking Target away strands nothing."
  (org-mcp-test--with-verbs-file test-file
    (org-mcp-test--call-clock-in
     (org-mcp-test--file-link test-file "*Keep") "2026-03-20T09:30:00")
    (org-mcp-test--with-session-clock test-file
      (let ((link (org-mcp-test--verbs-link)))
        (should
         (eq
          (alist-get
           'success
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-node-delete"
             `((link . ,link)
               (before . ,(org-mcp-test--verbs-digest link))))))
          t))
        (should (org-clock-is-active))))))

(ert-deftest org-mcp-test-node-refile-carries-the-subtree-and-a-position ()
  "org-node-refile puts the node under a new parent, after a named sibling.
The LOGBOOK travels with the node, and Org shifts every generation
to the level of the node's new place."
  (org-mcp-test--with-verbs-file test-file
    (let* ((link (org-mcp-test--verbs-link))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-refile"
              `((link . ,link)
                (before . ,(org-mcp-test--verbs-digest))
                (parent . ,(org-mcp-test--file-link test-file "*Home"))
                (previous_sibling
                 .
                 ,(org-mcp-test--file-link
                   test-file "*First child")))))))
      (should (eq (alist-get 'success result) t))
      (should (equal (alist-get 'link result) link))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--verbs-target-refiled))))

(ert-deftest org-mcp-test-node-refile-without-a-sibling-appends ()
  "Naming no sibling puts the node last under its new parent.
`previous_sibling' means on a move what it means on org-node-create,
so a caller that knows one knows the other."
  (org-mcp-test--with-verbs-file test-file
    (mcp-server-lib-ert-call-tool
     "org-node-refile"
     `((link . ,(org-mcp-test--verbs-link))
       (before . ,(org-mcp-test--verbs-digest))
       (parent . ,(org-mcp-test--file-link test-file "*Home"))))
    (org-mcp-test--verify-file-matches
     test-file org-mcp-test--verbs-target-last-child)))

(ert-deftest org-mcp-test-node-refile-to-the-top-level-of-a-file ()
  "A parent naming a whole file moves the node to that file's top level.
The node lands before every heading already there, where a node
created at the top level lands, and Org shifts it to level 1."
  (org-mcp-test--with-verbs-file test-file
    (mcp-server-lib-ert-call-tool
     "org-node-refile"
     `((link . ,(org-mcp-test--verbs-link))
       (before . ,(org-mcp-test--verbs-digest))
       (parent . ,(concat "file:" test-file))))
    (org-mcp-test--verify-file-matches
     test-file org-mcp-test--verbs-target-at-top)))

(ert-deftest org-mcp-test-node-refile-refuses-a-stale-digest ()
  "A stale token refuses the move and leaves the file where it was."
  (org-mcp-test--with-verbs-file test-file
    (let ((stale (org-mcp-test--verbs-digest))
          (link (org-mcp-test--verbs-link)))
      (mcp-server-lib-ert-call-tool
       "org-node-add-note"
       `((link . ,link) (note . "Something happened here.")))
      (org-mcp-test--call-tool-refused
       "org-node-refile"
       `((link . ,link)
         (before . ,stale)
         (parent . ,(org-mcp-test--file-link test-file "*Home")))
       "\\`conflict: Subtree mismatch: .*nothing was refiled\\'"
       test-file))))

(ert-deftest org-mcp-test-node-refile-names-the-file-of-the-id-it-moves ()
  "`files' finds the node to move, and the parent is read from its file.
A parent that is not an `id:' link is not a parameter `files'
applies to, so sending both has to be taken rather than refused."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--verbs-content))
    (org-mcp-test--with-id-tracking (list test-file) nil
      (let ((link (org-mcp-test--verbs-link))
            (files (vector test-file)))
        (mcp-server-lib-ert-call-tool
         "org-node-refile"
         `((link . ,link)
           (before
            .
            ,(alist-get
              'digest
              (json-read-from-string
               (mcp-server-lib-ert-call-tool
                "org-node-read"
                `((link . ,link)
                  (fields . ["digest"])
                  (files . ,files))))))
           (parent . ,(org-mcp-test--file-link test-file "*Home"))
           (files . ,files)))
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--verbs-target-last-child)))))

(ert-deftest org-mcp-test-node-refile-refuses-its-own-descendant ()
  "A node cannot be moved under itself or under one of its children.
The destination is found before anything is cut, so the refusal
costs the file nothing."
  (org-mcp-test--with-verbs-file test-file
    (let ((link (org-mcp-test--verbs-link)))
      (org-mcp-test--call-tool-refused
       "org-node-refile"
       `((link . ,link)
         (before . ,(org-mcp-test--verbs-digest))
         (parent . ,(org-mcp-test--file-link test-file "*Child")))
       "\\`parent .* is the node being refiled, or a node under it\\'"
       test-file))))

(ert-deftest org-mcp-test-node-refile-crosses-files ()
  "A node moves into another file, its whole subtree with it.
This is the filing a GTD workflow is made of: an item leaves the
inbox for a project.  The subtree arrives under its new parent with
its LOGBOOK and its drawers, and it is gone from the file it left."
  (org-mcp-test--with-verbs-files test-file other-file
    (let* ((link (org-mcp-test--verbs-link))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-refile"
              `((link . ,link)
                (before . ,(org-mcp-test--verbs-digest))
                (parent
                 .
                 ,(org-mcp-test--file-link
                   other-file "*Project One")))))))
      (should (eq (alist-get 'success result) t))
      (should (eq (alist-get 'saved result) t))
      (should (equal (alist-get 'link result) link))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--verbs-target-gone)
      (org-mcp-test--verify-file-matches
       other-file org-mcp-test--verbs-other-with-target))))

(ert-deftest org-mcp-test-node-refile-crosses-files-to-a-top-level ()
  "A parent naming another file moves the node to that file's top level.
It lands after the preamble and before every heading there, where a
node created at the top level lands, so `parent' means the same on a
move as on org-node-create wherever the file is."
  (org-mcp-test--with-verbs-files test-file other-file
    (mcp-server-lib-ert-call-tool
     "org-node-refile"
     `((link . ,(org-mcp-test--verbs-link))
       (before . ,(org-mcp-test--verbs-digest))
       (parent . ,(concat "file:" other-file))))
    (org-mcp-test--verify-file-matches
     test-file org-mcp-test--verbs-target-gone)
    (org-mcp-test--verify-file-matches
     other-file org-mcp-test--verbs-other-with-target-at-top)))

(ert-deftest org-mcp-test-node-refile-across-files-keeps-the-id-resolving ()
  "An `id:' link to a node that changed file still finds it.
Org re-registers the IDs in a pasted subtree against the file they
land in, so a client holding the link it moved the node by can read
the node straight back."
  (org-mcp-test--with-verbs-files test-file other-file
    (let ((link (org-mcp-test--verbs-link)))
      (mcp-server-lib-ert-call-tool
       "org-node-refile"
       `((link . ,link)
         (before . ,(org-mcp-test--verbs-digest))
         (parent
          .
          ,(org-mcp-test--file-link other-file "*Project One"))))
      (org-mcp-test--should-resolve-to link "Target"))))

(ert-deftest org-mcp-test-node-refile-across-files-refuses-a-stale-digest ()
  "A stale token refuses a cross-file move and leaves both files alone.
Two files are at stake, and a refusal has to be worth nothing to
either of them."
  (org-mcp-test--with-verbs-files test-file other-file
    (let ((stale (org-mcp-test--verbs-digest))
          (link (org-mcp-test--verbs-link))
          (other-before (org-mcp-test--read-file other-file)))
      (mcp-server-lib-ert-call-tool
       "org-node-set-title"
       `((link . ,link) (before . "Target") (after . "Target renamed")))
      (org-mcp-test--call-tool-refused
       "org-node-refile"
       `((link . ,link)
         (before . ,stale)
         (parent
          .
          ,(org-mcp-test--file-link other-file "*Project One")))
       "\\`conflict: Subtree mismatch: .*nothing was refiled\\'"
       test-file)
      (should
       (string= (org-mcp-test--read-file other-file) other-before)))))

(ert-deftest org-mcp-test-node-refile-refuses-a-parent-out-of-reach ()
  "A move is no way to write a file a call may not reach.
The destination is checked like any file a call names, before
anything is cut, so the node stays where it is."
  (org-mcp-test--with-verbs-files test-file other-file
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--call-tool-refused
       "org-node-refile"
       `((link . ,(org-mcp-test--verbs-link))
         (before . ,(org-mcp-test--verbs-digest))
         (parent
          .
          ,(org-mcp-test--file-link other-file "*Project One")))
       "the referenced file not in allowed list"
       test-file))))

(ert-deftest org-mcp-test-node-refile-refuses-a-parent-heading-not-there ()
  "A parent naming a heading that is not there refuses the move.
The search runs in the file the parent names, and a search that ends
on nothing is refused rather than left to land the node at some
other place in that file.  Neither file is written."
  (org-mcp-test--with-verbs-files test-file other-file
    (let ((other-before (org-mcp-test--read-file other-file)))
      (org-mcp-test--call-tool-refused
       "org-node-refile"
       `((link . ,(org-mcp-test--verbs-link))
         (before . ,(org-mcp-test--verbs-digest))
         (parent
          .
          ,(org-mcp-test--file-link other-file "*No such heading")))
       "\\`Cannot resolve link"
       test-file)
      (should
       (string= (org-mcp-test--read-file other-file) other-before)))))

(ert-deftest org-mcp-test-node-refile-refuses-an-unknown-id-parent ()
  "An `id:' parent Emacs's ID index does not hold refuses the move.
`files' says where to find the node the call moves, never where to
put it, so an `id:' parent is looked for in the index and refused by
name when it is not there.  Neither file is written."
  (org-mcp-test--with-verbs-files test-file other-file
    (let ((other-before (org-mcp-test--read-file other-file)))
      (org-mcp-test--call-tool-refused
       "org-node-refile"
       `((link . ,(org-mcp-test--verbs-link))
         (before . ,(org-mcp-test--verbs-digest))
         (parent . "id:99999999-8888-7777-6666-555555555555")
         (files . ,(vector test-file)))
       "\\`Cannot find ID"
       test-file)
      (should
       (string= (org-mcp-test--read-file other-file) other-before)))))

;; What a refile records.  `org-log-refile' is the user's setting for
;; it, and a refile made here leaves the record a refile made by hand
;; leaves: the same entry, in the same place, governed by the same
;; variables.  These tests are the ones that say an agent's moves are
;; not the invisible ones.

(ert-deftest org-mcp-test-node-refile-logs-the-move-org-logs ()
  "`org-log-refile' set to `time' puts a refile entry in the LOGBOOK.
The entry joins the LOGBOOK the node already carries and travels
with the node, so the record of the move is on the node wherever it
went."
  (org-mcp-test--with-verbs-file test-file
    (let ((org-log-refile 'time)
          (org-log-into-drawer t)
          (link (org-mcp-test--verbs-link)))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-refile"
               `((link . ,link)
                 (before . ,(org-mcp-test--verbs-digest))
                 (parent . ,(org-mcp-test--file-link test-file "*Home"))
                 (previous_sibling
                  .
                  ,(org-mcp-test--file-link
                    test-file "*First child")))))))
        (should (eq (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'link result) link)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--verbs-target-refiled-logged))))

(ert-deftest org-mcp-test-node-refile-logs-a-note-setting-without-asking ()
  "`org-log-refile' set to `note' records the move and waits for no one.
Org's own route to the entry arms `post-command-hook' and opens an
`*Org Note*' buffer for a person to type in, which an MCP call has
nobody to finish.  The call returns, the hook is not armed, no note
buffer is left behind, and the entry written is the one the `time'
setting writes: a heading line with no note body under it."
  (org-mcp-test--with-verbs-file test-file
    (let ((org-log-refile 'note)
          (org-log-into-drawer t)
          (link (org-mcp-test--verbs-link)))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-refile"
               `((link . ,link)
                 (before . ,(org-mcp-test--verbs-digest))
                 (parent . ,(org-mcp-test--file-link test-file "*Home"))
                 (previous_sibling
                  .
                  ,(org-mcp-test--file-link
                    test-file "*First child")))))))
        (should (eq (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'link result) link)))
      (should-not (memq 'org-add-log-note post-command-hook))
      (should-not (get-buffer "*Org Note*"))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--verbs-target-refiled-logged))))

(ert-deftest org-mcp-test-node-refile-logs-nothing-when-unset ()
  "`org-log-refile' unset leaves the node exactly as it travelled.
A user who does not log refiles gets no entry from org-mcp either:
the setting is the whole decision, and the LOGBOOK the node carries
arrives untouched."
  (org-mcp-test--with-verbs-file test-file
    (let ((org-log-refile nil)
          (org-log-into-drawer t)
          (link (org-mcp-test--verbs-link)))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-refile"
               `((link . ,link)
                 (before . ,(org-mcp-test--verbs-digest))
                 (parent . ,(org-mcp-test--file-link test-file "*Home"))
                 (previous_sibling
                  .
                  ,(org-mcp-test--file-link
                    test-file "*First child")))))))
        (should (eq (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'link result) link)))
      (should-not (get-buffer "*Org Note*"))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--verbs-target-refiled))))

(ert-deftest org-mcp-test-node-refile-logs-into-the-drawer-the-node-names ()
  "A node's own `LOG_INTO_DRAWER' says where its refile entry goes.
Placement is Org's, through `org-log-into-drawer', so the per-heading
property outranks the global setting here as everywhere else."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--refile-log-drawer-content
      (list org-mcp-test--verbs-target-id)
    (let ((org-log-refile 'time)
          (org-log-into-drawer nil)
          (link (org-mcp-test--verbs-link)))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-refile"
               `((link . ,link)
                 (before . ,(org-mcp-test--verbs-digest))
                 (parent
                  .
                  ,(org-mcp-test--file-link test-file "*Home")))))))
        (should (eq (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'link result) link)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--refile-log-drawer-after))))

(ert-deftest org-mcp-test-node-refile-logs-in-the-file-it-lands-in ()
  "A cross-file refile writes its entry into the file the node landed in.
That is where `org-refile' writes it and where a reader of the node
will look for it; the file the node left keeps no trace of the
move."
  (org-mcp-test--with-verbs-files test-file other-file
    (let ((org-log-refile 'time)
          (org-log-into-drawer t)
          (link (org-mcp-test--verbs-link)))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-refile"
               `((link . ,link)
                 (before . ,(org-mcp-test--verbs-digest))
                 (parent
                  .
                  ,(org-mcp-test--file-link
                    other-file "*Project One")))))))
        (should (eq (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'link result) link)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--verbs-target-gone)
      (org-mcp-test--verify-file-matches
       other-file org-mcp-test--verbs-other-with-target-logged))))

(ert-deftest org-mcp-test-node-refile-does-not-run-the-insert-hook ()
  "`org-after-refile-insert-hook' does not run, by decision.
It is arbitrary user code, and `org-refile' runs it where org-mcp is
midway through a change group over two files: a hook that edits a
buffer or signals there leaves a call that cannot say what it wrote.
The file is what a refile alone makes of it."
  (org-mcp-test--with-verbs-file test-file
    (let* ((ran nil)
           (org-after-refile-insert-hook
            (list
             (lambda ()
               (setq ran t)
               (insert "hook was here\n"))))
           (link (org-mcp-test--verbs-link)))
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-refile"
               `((link . ,link)
                 (before . ,(org-mcp-test--verbs-digest))
                 (parent . ,(org-mcp-test--file-link test-file "*Home"))
                 (previous_sibling
                  .
                  ,(org-mcp-test--file-link
                    test-file "*First child")))))))
        (should (eq (alist-get 'success result) t))
        (should (eq (alist-get 'saved result) t))
        (should (equal (alist-get 'link result) link)))
      (should-not ran)
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--verbs-target-refiled))))

(ert-deftest org-mcp-test-node-archive-writes-where-the-node-came-from ()
  "org-node-archive moves the node out and records where it was.
Org writes the origin file, the outline path and the TODO state the
node held into it as ARCHIVE_ properties, which is what makes an
archive the one relocation a reader can follow backwards.  The
response names the file it went to."
  (org-mcp-test--with-verbs-file test-file
    (let* ((archive (concat test-file "_archive"))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-archive"
              `((link . ,(org-mcp-test--verbs-link))
                (before . ,(org-mcp-test--verbs-digest)))))))
      (unwind-protect
          (progn
            (should (eq (alist-get 'success result) t))
            (should (eq (alist-get 'saved result) t))
            (should
             (equal
              (alist-get 'archive_file result)
              (abbreviate-file-name archive)))
            (org-mcp-test--verify-file-matches
             test-file org-mcp-test--verbs-target-gone)
            (let ((archived (org-mcp-test--read-file archive)))
              (should (string-match-p "\\* TODO Target" archived))
              (should (string-match-p ":ARCHIVE_FILE:" archived))
              (should (string-match-p ":ARCHIVE_TODO: *TODO" archived))
              (should (string-match-p "\\*\\*\\* Grandchild" archived))
              (should (string-match-p "Decided this one\\." archived))))
        (when (file-exists-p archive)
          (delete-file archive))))))

(ert-deftest org-mcp-test-node-archive-to-a-heading-in-the-same-file ()
  "An archive location naming a heading keeps the node in its own file.
`org-archive-location' decides where an archive goes, and a location
with no file part names a heading in the file the node is in.  The
node archived here has a parent, so the outline path Org writes into
it leads back to where it was — the property a reader follows to put
it back."
  (org-mcp-test--with-verbs-file test-file
    (let* ((org-archive-location "::* Archived")
           (link (org-mcp-test--file-link test-file "*Child"))
           (result
            (json-read-from-string
             (mcp-server-lib-ert-call-tool
              "org-node-archive"
              `((link . ,link)
                (before . ,(org-mcp-test--verbs-digest link)))))))
      (should (eq (alist-get 'saved result) t))
      (should
       (equal
        (alist-get 'archive_file result)
        (abbreviate-file-name test-file)))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--verbs-child-archived-in-place))))

(ert-deftest org-mcp-test-node-archive-leaves-a-dirty-archive-unsaved ()
  "An archive file the user is editing is written but not saved.
Org would save it itself, which would commit the user's own unsaved
edits as a side effect of an MCP call.  org-mcp saves it the way it
saves any buffer it writes to, so the edits stay the user's to
persist and `saved' says the change has not reached disk."
  (org-mcp-test--with-verbs-file test-file
    (let* ((archive (concat test-file "_archive"))
           (buffer nil))
      (unwind-protect
          (progn
            (with-temp-file archive
              (insert "* Already archived\n"))
            (setq buffer (find-file-noselect archive))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "The user was typing here.\n")
              (should (buffer-modified-p)))
            (let ((result
                   (json-read-from-string
                    (mcp-server-lib-ert-call-tool
                     "org-node-archive"
                     `((link . ,(org-mcp-test--verbs-link))
                       (before . ,(org-mcp-test--verbs-digest)))))))
              (should (eq (alist-get 'saved result) :json-false)))
            (org-mcp-test--verify-file-matches
             test-file org-mcp-test--verbs-target-gone)
            (should-not
             (string-match-p
              "Target" (org-mcp-test--read-file archive)))
            (org-mcp-test--verify-buffer-matches
             buffer "\\* TODO Target"))
        (when buffer
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (when (file-exists-p archive)
          (delete-file archive))))))

(ert-deftest org-mcp-test-node-archive-refuses-a-stale-digest ()
  "A stale token refuses the archive, and no archive file is written.
Archiving writes two files, so a refusal has to leave both alone."
  (org-mcp-test--with-verbs-file test-file
    (let ((stale (org-mcp-test--verbs-digest))
          (link (org-mcp-test--verbs-link))
          (archive (concat test-file "_archive")))
      (mcp-server-lib-ert-call-tool
       "org-node-set-todo"
       `((link . ,link) (before . "TODO") (after . "DONE")))
      (org-mcp-test--call-tool-refused
       "org-node-archive"
       `((link . ,link) (before . ,stale))
       "\\`conflict: Subtree mismatch: .*nothing was archived\\'"
       test-file)
      (should-not (file-exists-p archive)))))

;;; The three verbs against a buffer the user is editing

(ert-deftest org-mcp-test-node-delete-through-a-dirty-buffer ()
  "A delete lands in the buffer the user is editing, not around it.
The file is left alone because the buffer holds the user's own
unsaved edit, so the server is asked what the node's file holds now:
the node is gone from its answer, the user's edit is still in it,
and `saved' says the change has not reached disk."
  (org-mcp-test--with-verbs-file test-file
    (org-mcp-test--with-dirty-buffer (buffer on-disk) test-file
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-delete"
               `((link . ,(org-mcp-test--verbs-link))
                 (before . ,(org-mcp-test--verbs-digest)))))))
        (org-mcp-test--verify-served-matches
         test-file
         (org-mcp-test--served-regex
          org-mcp-test--verbs-target-gone))
        (org-mcp-test--assert-unsaved result test-file on-disk buffer)))))

(ert-deftest org-mcp-test-node-delete-refused-leaves-the-buffer-alone ()
  "A refused delete takes nothing out of the buffer either.
The node's title is changed and saved first, so the token is stale
and the file on disk is what the buffer was made from.  After the
refusal the server still serves the node, and the buffer differs
from its file by the user's edit and nothing else."
  (org-mcp-test--with-verbs-file test-file
    (let ((stale (org-mcp-test--verbs-digest))
          (link (org-mcp-test--verbs-link)))
      (mcp-server-lib-ert-call-tool
       "org-node-set-title"
       `((link . ,link) (before . "Target") (after . "Target renamed")))
      (org-mcp-test--with-dirty-buffer (buffer on-disk) test-file
        (org-mcp-test--call-tool-refused
         "org-node-delete"
         `((link . ,link) (before . ,stale))
         "\\`conflict: Subtree mismatch: .*nothing was deleted\\'"
         test-file)
        (let ((served (org-mcp-test--served-text test-file)))
          (should (string-match-p "TODO Target renamed" served))
          (should (string-match-p "Grandchild" served)))
        (org-mcp-test--assert-content-unmoved buffer test-file on-disk)))))

(ert-deftest org-mcp-test-node-archive-through-a-dirty-buffer ()
  "An archive takes the node out of the buffer and writes the archive.
The node's own file is the user's to save, so it keeps the node; the
archive file is org-mcp's own and reaches disk.  The server is asked
what the node's file holds now, and the node is gone from it."
  (org-mcp-test--with-verbs-file test-file
    (let ((archive (concat test-file "_archive")))
      (unwind-protect
          (org-mcp-test--with-dirty-buffer (buffer on-disk) test-file
            (let ((result
                   (json-read-from-string
                    (mcp-server-lib-ert-call-tool
                     "org-node-archive"
                     `((link . ,(org-mcp-test--verbs-link))
                       (before . ,(org-mcp-test--verbs-digest)))))))
              (org-mcp-test--verify-served-matches
               test-file
               (org-mcp-test--served-regex
                org-mcp-test--verbs-target-gone))
              (org-mcp-test--assert-unsaved
               result test-file on-disk buffer)
              ;; The archive file was org-mcp's to save, and it is on
              ;; disk with the node in it.
              (let ((archived (org-mcp-test--read-file archive)))
                (should (string-match-p "\\* TODO Target" archived))
                (should
                 (string-match-p "\\*\\*\\* Grandchild" archived)))))
        (when (file-exists-p archive)
          (delete-file archive))))))

(ert-deftest org-mcp-test-node-archive-refused-leaves-the-buffer-alone ()
  "A refused archive writes neither the buffer nor an archive file."
  (org-mcp-test--with-verbs-file test-file
    (let ((stale (org-mcp-test--verbs-digest))
          (link (org-mcp-test--verbs-link))
          (archive (concat test-file "_archive")))
      (mcp-server-lib-ert-call-tool
       "org-node-set-title"
       `((link . ,link) (before . "Target") (after . "Target renamed")))
      (org-mcp-test--with-dirty-buffer (buffer on-disk) test-file
        (org-mcp-test--call-tool-refused
         "org-node-archive"
         `((link . ,link) (before . ,stale))
         "\\`conflict: Subtree mismatch: .*nothing was archived\\'"
         test-file)
        (should (string-match-p
                 "TODO Target renamed"
                 (org-mcp-test--served-text test-file)))
        (org-mcp-test--assert-content-unmoved buffer test-file on-disk)
        (should-not (file-exists-p archive))))))

(ert-deftest org-mcp-test-node-refile-through-a-dirty-source-buffer ()
  "A refile out of a buffer the user is editing leaves that file alone.
The node reaches the other file, which org-mcp saves, and leaves the
buffer it came from unsaved with the user's edit still in it."
  (org-mcp-test--with-verbs-files test-file other-file
    (org-mcp-test--with-dirty-buffer (buffer on-disk) test-file
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-refile"
               `((link . ,(org-mcp-test--verbs-link))
                 (before . ,(org-mcp-test--verbs-digest))
                 (parent
                  .
                  ,(org-mcp-test--file-link
                    other-file "*Project One")))))))
        (org-mcp-test--verify-served-matches
         test-file
         (org-mcp-test--served-regex org-mcp-test--verbs-target-gone))
        (org-mcp-test--verify-served-matches
         other-file org-mcp-test--verbs-other-with-target)
        (org-mcp-test--assert-unsaved result test-file on-disk buffer)
        (org-mcp-test--verify-file-matches
         other-file org-mcp-test--verbs-other-with-target)))))

(ert-deftest org-mcp-test-node-refile-through-a-dirty-destination ()
  "A refile into a buffer the user is editing leaves that file alone.
The node is written into the destination buffer and stays there
unsaved, while the file it left was org-mcp's to save and reaches
disk.  `saved' answers for both files, so it is false."
  (org-mcp-test--with-verbs-files test-file other-file
    (org-mcp-test--with-dirty-buffer (buffer on-disk) other-file
      (let ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-node-refile"
               `((link . ,(org-mcp-test--verbs-link))
                 (before . ,(org-mcp-test--verbs-digest))
                 (parent
                  .
                  ,(org-mcp-test--file-link
                    other-file "*Project One")))))))
        (org-mcp-test--verify-served-matches
         other-file
         (org-mcp-test--served-regex
          org-mcp-test--verbs-other-with-target))
        (org-mcp-test--assert-unsaved result other-file on-disk buffer)
        (org-mcp-test--verify-file-matches
         test-file org-mcp-test--verbs-target-gone)))))

(ert-deftest org-mcp-test-node-refile-refused-leaves-the-buffer-alone ()
  "A refused refile moves nothing, in either file or either buffer."
  (org-mcp-test--with-verbs-files test-file other-file
    (let ((stale (org-mcp-test--verbs-digest))
          (link (org-mcp-test--verbs-link)))
      (mcp-server-lib-ert-call-tool
       "org-node-set-title"
       `((link . ,link) (before . "Target") (after . "Target renamed")))
      (let ((other-before (org-mcp-test--read-file other-file)))
        (org-mcp-test--with-dirty-buffer (buffer on-disk) test-file
          (org-mcp-test--call-tool-refused
           "org-node-refile"
           `((link . ,link)
             (before . ,stale)
             (parent
              .
              ,(org-mcp-test--file-link other-file "*Project One")))
           "\\`conflict: Subtree mismatch: .*nothing was refiled\\'"
           test-file)
          (should (string-match-p
                   "TODO Target renamed"
                   (org-mcp-test--served-text test-file)))
          (should-not
           (string-match-p
            "Target"
            (org-mcp-test--served-text other-file)))
          (org-mcp-test--assert-content-unmoved
           buffer test-file on-disk)
          (should
           (string=
            (org-mcp-test--read-file other-file) other-before)))))))

(defconst org-mcp-test--verbs-tagged-content
  (concat
   "* TODO Tagged :work:urgent:\n"
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--verbs-target-id "\n"
   ":END:\n"
   "** Descendant :deep:\n"
   "* TODO Home\n"
   "** Child\n")
  "A tagged node with a tagged descendant, and a parent two levels down.
Refiling it under Child shifts both headings by two levels, so a
paste that left the tags where they were is visible.")

(ert-deftest org-mcp-test-node-refile-aligns-the-tags-it-shifts ()
  "A refiled heading's tags are aligned for the level it lands at.
Org shifts the subtree by promoting or demoting every heading in it,
and that is what aligns the tags, so a heading arrives with its tags
at `org-tags-column' rather than at the column its old level put
them.  The descendant is checked too: the shift reaches all of them."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--verbs-tagged-content
      (list org-mcp-test--verbs-target-id)
    (let ((link (concat "id:" org-mcp-test--verbs-target-id)))
      (mcp-server-lib-ert-call-tool
       "org-node-refile"
       `((link . ,link)
         (before . ,(org-mcp-test--verbs-digest link))
         (parent . ,(org-mcp-test--file-link test-file "*Child"))))
      (let ((served (org-mcp-test--served-text test-file)))
        ;; Both headings are two levels deeper than they were.
        (should
         (string-match-p "^\\*\\*\\* TODO Tagged " served))
        (should (string-match-p "^\\*\\*\\*\\* Descendant " served))
        ;; And both carry their tags at the configured column, which
        ;; right-aligns the tag string to end there.
        (dolist (heading '("TODO Tagged" "Descendant"))
          (let ((line
                 (car
                  (seq-filter
                   (lambda (line)
                     (string-match-p (concat "\\* " heading " ") line))
                   (split-string served "\n")))))
            (should line)
            (should (string-suffix-p ":" line))
            (should (= (length line) (abs org-tags-column)))))))))

;;; Every write against a buffer the user is editing

;; org-mcp writes through the buffer the user is editing and leaves
;; the file to that user (docs/adr/0002), so what a client is served
;; after a write is the buffer's content and not the file's.  Each
;; test below dirties the buffer with an edit of the user's own,
;; calls one write endpoint, and asks the server what the file holds:
;; the change is in the answer, the user's edit is still in it, the
;; file on disk has not moved, and `saved' says so.

(defconst org-mcp-test--dirty-created-child-regex
  (concat
   "\\`\\* TODO Simple Task\n"
   "Task body text\\.\n"
   "Typed by hand, not saved\\.\n"
   "\\*\\* TODO New Child\n"
   "\\'")
  "The whole file served after a child is created under the task.
The new heading lands after the body the user was typing into, which
is the last line of its parent's subtree.")

(ert-deftest org-mcp-test-node-create-through-a-dirty-buffer ()
  "A created node lands in the buffer the user is editing."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--content-bare-todo
   "org-node-create"
   (lambda (file)
     `((title . "New Child")
       (todo . "TODO")
       (content . nil)
       (parent . ,(org-mcp-test--file-link file "*Simple Task"))))
   '((title . "New Child"))
   org-mcp-test--dirty-created-child-regex))

(defconst org-mcp-test--dirty-renamed-regex
  (concat
   "\\`\\* TODO Renamed Task\n"
   "Task body text\\.\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "The whole file served after the task is renamed.")

(ert-deftest org-mcp-test-node-set-title-through-a-dirty-buffer ()
  "A renamed title lands in the buffer the user is editing."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--content-bare-todo
   "org-node-set-title"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Simple Task"))
       (before . "Simple Task")
       (after . "Renamed Task")))
   '((before . "Simple Task") (after . "Renamed Task"))
   org-mcp-test--dirty-renamed-regex))

(defconst org-mcp-test--dirty-replaced-body-regex
  (concat
   "\\`\\* TODO Simple Task\n"
   "Body replaced\\.\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "The whole file served after the task's body is replaced.
The user's own line is part of that body and stays where it was, so
a replacement that took the whole body with it shows here.")

(ert-deftest org-mcp-test-node-set-content-through-a-dirty-buffer ()
  "A replaced body lands in the buffer the user is editing."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--content-bare-todo
   "org-node-set-content"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Simple Task"))
       (before . "Task body text.")
       (after . "Body replaced.")))
   nil
   org-mcp-test--dirty-replaced-body-regex))

(defconst org-mcp-test--dirty-property-regex
  (concat
   "\\`\\* TODO Simple Task\n"
   " *:PROPERTIES:\n"
   " *:FIRST: +1\n"
   " *:END:\n"
   "Task body text\\.\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "The whole file served after a property is set on the task.")

(ert-deftest org-mcp-test-node-set-properties-through-a-dirty-buffer ()
  "A property lands in the buffer the user is editing."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--content-bare-todo
   "org-node-set-properties"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Simple Task"))
       (before . ((FIRST)))
       (after . ((FIRST . "1")))))
   '((properties_set . ["FIRST"]))
   org-mcp-test--dirty-property-regex))

(defconst org-mcp-test--content-task-with-every-field
  (concat
   "* TODO [#A] Simple Task :work:urgent:\n"
   "SCHEDULED: <2026-03-27 Fri> DEADLINE: <2026-04-01 Wed>\n"
   ":PROPERTIES:\n"
   ":OWNER:    ada\n"
   ":END:\n"
   "Task body text.\n")
  "A task carrying every field a removal endpoint takes away.
One file stands behind all of them, so each test's regexp says both
what its endpoint took and what it left, and a removal that reached
past its own field fails in a neighbour's test.")

(defconst org-mcp-test--dirty-every-field-drawer
  (concat
   " *:PROPERTIES:\n"
   " *:OWNER: +ada\n"
   " *:END:\n"
   "Task body text\\.\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "What the task keeps below its planning line, and the user's own line.
The removals that leave the PROPERTIES drawer alone all end here.")

(defconst org-mcp-test--dirty-properties-removed-regex
  (concat
   "\\`\\* TODO \\[#A\\] Simple Task[ \t]+:work:urgent:\n"
   "SCHEDULED: <2026-03-27 Fri> DEADLINE: <2026-04-01 Wed>\n"
   "Task body text\\.\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "The whole file served after the task's only property is removed.
The drawer goes with the last property in it.")

(ert-deftest org-mcp-test-node-delete-property-through-a-dirty-buffer ()
  "A removed property leaves the buffer the user is editing."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--content-task-with-every-field
   "org-node-set-properties"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Simple Task"))
       (before . ((OWNER . "ada")))
       (after . ((OWNER)))))
   '((properties_deleted . ["OWNER"]) (before . ((OWNER . "ada"))))
   org-mcp-test--dirty-properties-removed-regex))

(defconst org-mcp-test--dirty-scheduled-regex
  (concat
   "\\`\\* TODO Simple Task\n"
   "SCHEDULED: <2026-03-27 .*>\n"
   "Task body text\\.\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "The whole file served after SCHEDULED is set on the task.")

(ert-deftest org-mcp-test-node-set-scheduled-through-a-dirty-buffer ()
  "A SCHEDULED timestamp lands in the buffer the user is editing."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--content-bare-todo
   "org-node-set-scheduled"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Simple Task"))
       (before . "")
       (after . "2026-03-27")))
   '((before . ""))
   org-mcp-test--dirty-scheduled-regex))

(defconst org-mcp-test--dirty-scheduled-removed-regex
  (concat
   "\\`\\* TODO \\[#A\\] Simple Task[ \t]+:work:urgent:\n"
   "DEADLINE: <2026-04-01 Wed>\n"
   org-mcp-test--dirty-every-field-drawer)
  "The whole file served after SCHEDULED is taken off the task.
The DEADLINE beside it on the planning line stays.")

(ert-deftest org-mcp-test-node-clear-scheduled-through-a-dirty-buffer ()
  "A removed SCHEDULED leaves the buffer the user is editing."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--content-task-with-every-field
   "org-node-set-scheduled"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Simple Task"))
       (before . "<2026-03-27 Fri>")
       (after)))
   '((before . "<2026-03-27 Fri>") (after . ""))
   org-mcp-test--dirty-scheduled-removed-regex))

(defconst org-mcp-test--dirty-deadline-regex
  (concat
   "\\`\\* TODO Simple Task\n"
   "DEADLINE: <2026-04-01 .*>\n"
   "Task body text\\.\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "The whole file served after DEADLINE is set on the task.")

(ert-deftest org-mcp-test-node-set-deadline-through-a-dirty-buffer ()
  "A DEADLINE timestamp lands in the buffer the user is editing."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--content-bare-todo
   "org-node-set-deadline"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Simple Task"))
       (before . "")
       (after . "2026-04-01")))
   '((before . ""))
   org-mcp-test--dirty-deadline-regex))

(defconst org-mcp-test--dirty-deadline-removed-regex
  (concat
   "\\`\\* TODO \\[#A\\] Simple Task[ \t]+:work:urgent:\n"
   "SCHEDULED: <2026-03-27 Fri>\n"
   org-mcp-test--dirty-every-field-drawer)
  "The whole file served after DEADLINE is taken off the task.
The SCHEDULED beside it on the planning line stays.")

(ert-deftest org-mcp-test-node-clear-deadline-through-a-dirty-buffer ()
  "A removed DEADLINE leaves the buffer the user is editing."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--content-task-with-every-field
   "org-node-set-deadline"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Simple Task"))
       (before . "<2026-04-01 Wed>")
       (after)))
   '((before . "<2026-04-01 Wed>") (after . ""))
   org-mcp-test--dirty-deadline-removed-regex))

(defconst org-mcp-test--dirty-tags-regex
  (concat
   "\\`\\* TODO Simple Task[ \t]+:work:urgent:\n"
   "Task body text\\.\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "The whole file served after tags are set on the task.")

(ert-deftest org-mcp-test-node-set-tags-through-a-dirty-buffer ()
  "Tags land in the buffer the user is editing."
  (let ((org-tag-alist '("work" "personal" "urgent")))
    (org-mcp-test--write-through-dirty-buffer
     org-mcp-test--content-bare-todo
     "org-node-set-tags"
     (lambda (file)
       `((link . ,(org-mcp-test--file-link file "*Simple Task"))
         (before . [])
         (after . ["work" "urgent"])))
     '((before . []) (after . ["work" "urgent"]) (inherited . []))
     org-mcp-test--dirty-tags-regex)))

(defconst org-mcp-test--dirty-tag-added-regex
  (concat
   "\\`\\* TODO \\[#A\\] Simple Task[ \t]+:work:urgent:later:\n"
   "SCHEDULED: <2026-03-27 Fri> DEADLINE: <2026-04-01 Wed>\n"
   org-mcp-test--dirty-every-field-drawer)
  "The whole file served after one tag is added to the task.
The two tags it carried are still on it: an add takes nothing away.")

(ert-deftest org-mcp-test-node-add-tags-through-a-dirty-buffer ()
  "An added tag lands in the buffer the user is editing."
  (let ((org-tag-alist '("work" "urgent" "later")))
    (org-mcp-test--write-through-dirty-buffer
     org-mcp-test--content-task-with-every-field
     "org-node-add-tags"
     (lambda (file)
       `((link . ,(org-mcp-test--file-link file "*Simple Task"))
         (after . "later")))
     '((before . ["work" "urgent"])
       (after . ["work" "urgent" "later"])
       (inherited . []))
     org-mcp-test--dirty-tag-added-regex)))

(defconst org-mcp-test--dirty-tag-removed-regex
  (concat
   "\\`\\* TODO \\[#A\\] Simple Task[ \t]+:work:\n"
   "SCHEDULED: <2026-03-27 Fri> DEADLINE: <2026-04-01 Wed>\n"
   org-mcp-test--dirty-every-field-drawer)
  "The whole file served after one of two tags is removed from the task.
The tag the call did not name is still on it.")

(ert-deftest org-mcp-test-node-remove-tags-through-a-dirty-buffer ()
  "A removed tag leaves the buffer the user is editing."
  (let ((org-tag-alist '("work" "urgent" "later")))
    (org-mcp-test--write-through-dirty-buffer
     org-mcp-test--content-task-with-every-field
     "org-node-remove-tags"
     (lambda (file)
       `((link . ,(org-mcp-test--file-link file "*Simple Task"))
         (after . "urgent")))
     '((before . ["work" "urgent"])
       (after . ["work"])
       (inherited . []))
     org-mcp-test--dirty-tag-removed-regex)))

(defconst org-mcp-test--dirty-priority-regex
  (concat
   "\\`\\* TODO \\[#A\\] Simple Task\n"
   "Task body text\\.\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "The whole file served after a priority is set on the task.")

(ert-deftest org-mcp-test-node-set-priority-through-a-dirty-buffer ()
  "A priority lands in the buffer the user is editing."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--content-bare-todo
   "org-node-set-priority"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Simple Task"))
       (before . "")
       (after . "A")))
   '((before . "") (after . "A"))
   org-mcp-test--dirty-priority-regex))

(defconst org-mcp-test--dirty-priority-removed-regex
  (concat
   "\\`\\* TODO Simple Task[ \t]+:work:urgent:\n"
   "SCHEDULED: <2026-03-27 Fri> DEADLINE: <2026-04-01 Wed>\n"
   org-mcp-test--dirty-every-field-drawer)
  "The whole file served after the priority is taken off the task.
The heading keeps its TODO state and its tags; only the cookie goes.")

(ert-deftest org-mcp-test-node-clear-priority-through-a-dirty-buffer ()
  "A removed priority leaves the buffer the user is editing."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--content-task-with-every-field
   "org-node-set-priority"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Simple Task"))
       (before . "A")
       (after)))
   '((before . "A") (after . ""))
   org-mcp-test--dirty-priority-removed-regex))

(defconst org-mcp-test--dirty-note-regex
  (concat
   "\\`\\* TODO Simple Task\n"
   ":LOGBOOK:\n"
   "- Note taken on \\[[-0-9]+ [A-Z][a-z]+ [0-9:]+ *\\] \\\\\\\\\n"
   "  A note of my own\\.\n"
   ":END:\n"
   "Task body text\\.\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "The whole file served after a note is added to the task.")

(ert-deftest org-mcp-test-node-add-note-through-a-dirty-buffer ()
  "A LOGBOOK note lands in the buffer the user is editing."
  (let ((org-log-into-drawer t))
    (org-mcp-test--write-through-dirty-buffer
     org-mcp-test--content-bare-todo
     "org-node-add-note"
     (lambda (file)
       `((link . ,(org-mcp-test--file-link file "*Simple Task"))
         (note . "A note of my own.")))
     nil
     org-mcp-test--dirty-note-regex)))

(defconst org-mcp-test--dirty-clock-delete-regex
  (concat
   "\\`\\* TODO Task One\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-01-02 [A-Za-z]\\{2,3\\} 10:00\\]"
   "--\\[2026-01-02 [A-Za-z]\\{2,3\\} 11:00\\] =>  1:00\n"
   ":END:\n"
   "Typed by hand, not saved\\.\n"
   "\\'")
  "The whole file served after one of two CLOCK entries is deleted.")

(ert-deftest org-mcp-test-clock-delete-through-a-dirty-buffer ()
  "A deleted CLOCK entry leaves the buffer the user is editing."
  (org-mcp-test--write-through-dirty-buffer
   org-mcp-test--clock-delete-multi-initial-content
   "org-clock-delete"
   (lambda (file)
     `((link . ,(org-mcp-test--file-link file "*Task One"))
       (start . "2026-01-01T10:00:00")))
   '((deleted . t))
   org-mcp-test--dirty-clock-delete-regex))

;;; Refusals against a buffer the user is editing

;; A refusal has the same obligation as a write: the file was never
;; going to change, so file bytes prove nothing here.  The damage a
;; refused write could do is in the buffer, on its way to disk at the
;; user's next save, and only the server can be asked about that.

(defun org-mcp-test--dirty-write-refusals (link file)
  "Return a refused call per node write endpoint reaching LINK in FILE.
Each entry is (TOOL PARAMS REFUSAL): a call that is refused after the
endpoint has found the node, so a refusal that damaged the node on
its way out would be visible."
  `(("org-node-set-todo"
     ((link . ,link) (before . "DONE") (after . "TODO"))
     "\\`conflict: State mismatch: ")
    ("org-node-set-title"
     ((link . ,link) (before . "Wrong Title") (after . "Renamed"))
     "\\`conflict: Title mismatch: ")
    ("org-node-set-content"
     ((link . ,link) (before . "no such text") (after . "Replaced."))
     "\\`conflict: Body text not found: ")
    ("org-node-set-properties"
     ((link . ,link)
      (before . ((TODO)))
      (after . ((TODO . "DONE"))))
     "\\`Cannot set special property 'TODO'")
    ("org-node-set-properties"
     ((link . ,link)
      (before . ((OWNER . "ada")))
      (after . ((OWNER))))
     "\\`conflict: Property 'OWNER' mismatch: ")
    ("org-node-set-scheduled"
     ((link . ,link) (before . "") (after . "not-a-date"))
     "\\`Invalid date 'not-a-date' - expected")
    ("org-node-set-scheduled"
     ((link . ,link) (before . "<2026-03-27 Fri>") (after))
     "\\`conflict: SCHEDULED mismatch: ")
    ("org-node-set-deadline"
     ((link . ,link) (before . "") (after . "not-a-date"))
     "\\`Invalid date 'not-a-date' - expected")
    ("org-node-set-deadline"
     ((link . ,link) (before . "<2026-04-01 Wed>") (after))
     "\\`conflict: DEADLINE mismatch: ")
    ("org-node-set-tags"
     ((link . ,link) (before . []) (after . "invalid tag!"))
     "\\`Invalid tag name: invalid tag!")
    ("org-node-add-tags"
     ((link . ,link) (after . "invalid tag!"))
     "\\`Invalid tag name: invalid tag!")
    ("org-node-remove-tags"
     ((link . ,link) (after . "invalid tag!"))
     "\\`Invalid tag name: invalid tag!")
    ("org-node-set-priority"
     ((link . ,link) (before . "") (after . "Z"))
     "\\`Priority 'Z' out of range ")
    ("org-node-set-priority"
     ((link . ,link) (before . "A") (after))
     "\\`conflict: Priority mismatch: ")
    ("org-node-add-note"
     ((link . ,link) (note . "   "))
     "\\`Note cannot be empty or whitespace-only")
    ("org-node-create"
     ((title . "Has\nNewline")
      (todo . "TODO")
      (content . nil)
      (parent . ,(concat "file:" file)))
     "\\`Headline title cannot contain newlines")))

(ert-deftest org-mcp-test-write-refused-leaves-the-dirty-buffer-alone ()
  "A refused write moves nothing in the buffer the user is editing.
Every node write endpoint is refused in turn over the same buffer,
and after each one the server still serves the file's own text and
the user's edit, and nothing else."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-bare-todo))
    (org-mcp-test--with-dirty-buffer (buffer on-disk) test-file
      (let ((link (org-mcp-test--file-link test-file "*Simple Task")))
        (pcase-dolist
            (`(,tool ,params ,message)
             (org-mcp-test--dirty-write-refusals link test-file))
          (org-mcp-test--call-tool-refused
           tool params message test-file)
          (org-mcp-test--assert-content-unmoved
           buffer test-file on-disk))))))

(ert-deftest org-mcp-test-clock-refused-leaves-the-dirty-buffer-alone ()
  "A refused clock call moves nothing in the buffer the user is editing."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--clock-delete-multi-initial-content))
    (org-mcp-test--with-dirty-buffer (buffer on-disk) test-file
      (let ((link (org-mcp-test--file-link test-file "*Task One")))
        (org-mcp-test--call-tool-refused
         "org-clock-add"
         `((link . ,link)
           (start . "2026-01-05T11:00:00")
           (end . "2026-01-05T10:00:00"))
         "\\`End time .* is before start time " test-file)
        (org-mcp-test--assert-content-unmoved buffer test-file on-disk)
        (org-mcp-test--call-tool-refused
         "org-clock-delete"
         `((link . ,link) (start . "2026-01-03T09:00:00"))
         "\\`No clock entry starting at " test-file)
        (org-mcp-test--assert-content-unmoved buffer test-file on-disk)
        (org-mcp-test--call-tool-refused
         "org-clock-out"
         `((link . ,link) (end_time . "2026-01-05T11:00:00"))
         "\\`conflict: No active clock to stop" test-file)
        (org-mcp-test--assert-content-unmoved buffer test-file on-disk)))))

(ert-deftest org-mcp-test-clock-in-refused-leaves-the-dirty-buffer-alone ()
  "A clock-in refused for a running clock leaves the buffer alone.
The clock runs in another file, so the refusal comes after the target
is resolved, and the buffer the user is editing keeps its own text."
  (org-mcp-test--with-temp-org-files
      ((running org-mcp-test--clock-task-with-open-clock)
       (test-file org-mcp-test--clock-task-content))
    (org-mcp-test--with-session-clock running
      (org-mcp-test--with-dirty-buffer (buffer on-disk) test-file
        (org-mcp-test--call-tool-refused
         "org-clock-in"
         `((link . ,(org-mcp-test--file-link test-file "*Task One"))
           (start_time . "2026-01-01T11:00:00"))
         "\\`conflict: A clock is running on " test-file)
        (org-mcp-test--assert-content-unmoved
         buffer test-file on-disk)))))
;;; A destination folded in the buffer

;; Emacs folds a file with `#+STARTUP: overview' as it visits it, so
;; every heading below the top level is invisible, which is the state
;; a user's own buffer is usually in.  A placement that asks Org for
;; the next *visible* heading then leaves the node wherever the fold
;; happens to end, and the call reports success with a link that
;; resolves to it.  These tests pin the placement against the text of
;; the file, which is what the client is promised.

(defconst org-mcp-test--folded-target-id
  "fedcba98-7654-3210-fedc-ba9876543210"
  "ID of Target in `org-mcp-test--folded-source'.")

(defconst org-mcp-test--folded-destination
  (concat
   "#+STARTUP: overview\n"
   "* Outer\n"
   "** Parent\n"
   "Parent body.\n"
   "\n"
   "** Follower\n"
   "Follower body.\n"
   "* Elsewhere\n")
  "A file whose Parent is folded by the time a call reaches it.
Parent and Follower are both under a level-1 heading, so `overview'
hides them and leaves Elsewhere visible.  The blank line Org leaves
before a new entry ends Parent's subtree, so a placement stops on it
rather than on a heading, and the next heading Org can see from there
is Elsewhere: a placement that follows the fold puts the node at the
end of Follower instead of at the end of Parent.")

(defconst org-mcp-test--folded-target
  (concat
   "* TODO Target\n"
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--folded-target-id "\n"
   ":END:\n"
   "Target body.\n")
  "The node a refile moves under the folded Parent.")

(defconst org-mcp-test--folded-source
  (concat org-mcp-test--folded-destination org-mcp-test--folded-target)
  "The folded destination with the node to move already in it.")

(defconst org-mcp-test--folded-target-under-parent
  (concat
   "\\`#\\+STARTUP: overview\n"
   "\\* Outer\n"
   "\\*\\* Parent\n"
   "Parent body\\.\n"
   "\n"
   "\\*\\*\\* TODO Target\n"
   ":PROPERTIES:\n"
   ":ID:       " org-mcp-test--folded-target-id "\n"
   ":END:\n"
   "Target body\\.\n"
   "\\*\\* Follower\n"
   "Follower body\\.\n"
   "\\* Elsewhere\n"
   "\\'")
  "The complete destination file once Target is Parent's last child.
Target stands between Parent's body and Follower, one level deeper
than Parent, so a node that went to the end of Follower's subtree or
that came back a level fails here.")

(ert-deftest org-mcp-test-node-refile-under-a-folded-parent ()
  "A refile puts the node under the parent it names, folded or not.
The parent's subtree ends on a blank line, and the heading after it
is folded, so the placement has to read the file's text rather than
what is visible in it."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--folded-source
      (list org-mcp-test--folded-target-id)
    (let ((link (concat "id:" org-mcp-test--folded-target-id)))
      (mcp-server-lib-ert-call-tool
       "org-node-refile"
       `((link . ,link)
         (before . ,(org-mcp-test--verbs-digest link))
         (parent . ,(org-mcp-test--file-link test-file "*Parent"))))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--folded-target-under-parent))))

(ert-deftest org-mcp-test-node-refile-into-a-folded-file ()
  "A refile into another file reads that file's text, not its folds.
The destination file is folded as Emacs visits it for the move, which
is the first time anything has opened it."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--folded-target)
       (other-file org-mcp-test--folded-destination))
    (org-mcp-test--with-id-tracking
     (list test-file other-file)
     (list (cons org-mcp-test--folded-target-id test-file))
     (let ((link (concat "id:" org-mcp-test--folded-target-id)))
       (mcp-server-lib-ert-call-tool
        "org-node-refile"
        `((link . ,link)
          (before . ,(org-mcp-test--verbs-digest link))
          (parent . ,(org-mcp-test--file-link other-file "*Parent"))))
       (org-mcp-test--verify-file-matches
        other-file org-mcp-test--folded-target-under-parent)
       (should (string= (org-mcp-test--read-file test-file) ""))))))

(defconst org-mcp-test--folded-made-under-parent
  (concat
   "\\`#\\+STARTUP: overview\n"
   "\\* Outer\n"
   "\\*\\* Parent\n"
   "Parent body\\.\n"
   "\\*\\*\\* TODO Made\n"
   "\\*\\* Follower\n"
   "Follower body\\.\n"
   "\\* Elsewhere\n"
   "\\'")
  "The complete file once org-node-create has added Made under Parent.
Made stands between Parent's body and Follower, where the same call
against the same file unfolded puts it: `org-insert-heading' spends
the blank line that ended Parent's subtree on the new entry either
way.")

(ert-deftest org-mcp-test-node-create-under-a-folded-parent ()
  "org-node-create adds the node under the parent it names, folded or not.
This one predates the epic: the heading goes in through
`org-insert-heading', which relocates to a visible heading unless it
is told that an invisible one is where the caller means."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--folded-destination))
    (org-mcp-test--add-todo-and-check
     "Made" "TODO" nil nil
     (org-mcp-test--file-link test-file "*Parent")
     nil
     (file-name-nondirectory test-file)
     test-file
     org-mcp-test--folded-made-under-parent)))

(provide 'org-mcp-test)
;;; org-mcp-test.el ends here
