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

## Nodes

**Node**:
A file or a heading: a title, its metadata, a body and its children, addressed
by a link. One shape serves both, and serves a child and a query result alike,
so a client learns one vocabulary to walk an outline.
_Avoid_: headline, heading (when meaning the structure rather than the Org
syntax), item, entry, match

**Representation**:
How much of a node a response carries: the fields a call asks for. A call
names them as a list, or by the name of a list configured in advance. Every
representation is the same node, never a different type, so a reference — a
node carrying its link alone — grows into a full one by asking for more.
_Avoid_: stub, lightweight child, projection (when addressed to a user)

**Property**:
A value from a node's own Org property drawer. It is part of the file, it
survives a round trip, and a call names the properties it wants.
_Avoid_: field, attribute

**Computed field**:
A value a configured function produces for a node when it is read, such as the
rank a workflow ranks its items by. It is this server's answer at this moment,
belongs to no drawer, and is never written back.
_Avoid_: virtual property, extra property, derived property

**Digest**:
An opaque token standing for a node's exact content, its descendants included.
A call that destroys or relocates a whole node sends back the digest it read,
saying what it expects to act on; a node's own fields say that for a call that
changes one field.
_Avoid_: hash, fingerprint, checksum, etag

**View**:
A named query a workflow defines: one question asked of the outline, such as
next actions or stuck projects. It is the atom an agenda command and a call
are both composed from, so the two answer alike.
_Avoid_: block, agenda, saved query, stored query, report

**Filter**:
A named restriction a view is asked under, such as one person's work or one
project. A call names a filter rather than writing a query, so it can only ask
what the configuration already knows.
_Avoid_: tag filter, scope (which is the files a call reaches)

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

**Own tags**:
The tags written on a heading itself, as against the tags in effect on it,
which include the ones it inherits. A write reaches a heading's own tags only,
and a tag assertion is over them. A read returns them as `local_tags`.
_Avoid_: direct tags, private tags, explicit tags

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
