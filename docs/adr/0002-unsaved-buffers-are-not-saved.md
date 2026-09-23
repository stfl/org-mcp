# org-records-mcp does not save a buffer that holds unsaved edits

When a buffer visiting the target file already holds the user's unsaved edits,
org-records-mcp applies its change to that buffer, leaves saving to the user, and
reports `saved: false`. Saving the buffer would also write the user's
in-progress edits to disk as a side effect of an unrelated call, so always
saving was rejected. Users who want every change on disk promptly enable
`auto-save-visited-mode`.
