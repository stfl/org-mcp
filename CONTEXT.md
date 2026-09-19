# org-mcp

org-mcp gives MCP clients access to Emacs Org-mode: reading, querying and
editing Org files through the running Emacs session. This fork shapes it around
an agile-GTD workflow.

## Scope

**Allowed files**:
The Org files a call reaches when it names no other file. By default these are
the agenda files.
_Avoid_: pre-defined files, whitelist, agenda files (when meaning the scope)

**Scope override**:
A single call reaching an Org file outside the allowed files by naming that
file. It lasts for that call only; nothing carries over to the next call.
_Avoid_: file override, overwriting the scope, target file scope

**Override policy**:
The server-side setting that decides whether scope overrides are refused,
permitted for any Org file, or permitted only under override roots. Refusal is
the default.
_Avoid_: override flag

**Override root**:
A directory under which the override policy permits scope overrides.

**GTD query**:
A query whose meaning comes from the GTD workflow itself, such as the inbox,
next actions or the backlog. It always runs over the allowed files and never
takes a scope override.
_Avoid_: agenda query, GTD endpoint

## Headings

**Indexed file**:
An Org file whose IDs Emacs records in its ID index, so an ID in it resolves
without naming the file.

**ID**:
A UUID identifying a heading, or a whole file when it sits in the file's top
property drawer, across all indexed files. Only headings and files that are
indexed carry one.
_Avoid_: slug ID, org-id

**Custom ID**:
A slug identifying a heading within its own file. Headings in files outside the
index carry one instead of an ID, and a link to one always names its file.
_Avoid_: CUSTOM_ID slug, anchor

**Link**:
A native Org link, the text Org resolves inside `[[…]]`, naming a heading or a
file. It is the only way a call names a heading, and the form every write
returns.
_Avoid_: URI, resource URI, headline path

## Writes

**Unsaved change**:
An edit org-mcp applied to an Emacs buffer that already held the user's own
unsaved edits. It exists in that buffer but not on disk until the user saves.
_Avoid_: staged change, dirty write
