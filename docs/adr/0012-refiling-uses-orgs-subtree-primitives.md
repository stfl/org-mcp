# Refiling uses Org's subtree primitives, not `org-refile`

org-records-mcp relocates a node with `org-cut-subtree` and `org-paste-subtree`,
resolving the destination with the same functions `org-node-create` uses, so
`parent` and `previous_sibling` mean one thing across both tools. We chose this
over calling `org-refile`, which derives its insertion point from the target
heading and `org-reverse-note-order` and therefore cannot name a position among
siblings, and which appends at end-of-file for a whole-file destination where
org-records-mcp puts a node before the first heading. `org-refile` also calls
`bookmark-set` on the name in `org-bookmark-names-plist`, and with
`bookmark-save-flag` at its default a refile would write the user's bookmarks
file — a write outside the allowed files, from a server whose posture is that it
touches those files and nothing else. `org-refile` itself relocates with
`org-copy-subtree`, `org-paste-subtree` and a `delete-region`, so this is the
same machinery one level down, at the level where a position is expressible.
The cost accepted is that `org-after-refile-insert-hook` does not run.
