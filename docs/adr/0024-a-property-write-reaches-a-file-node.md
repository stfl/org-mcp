# A property write reaches a file node, and no other write does

`org-node-set-properties` takes a link naming a whole file and writes that
file's own property drawer, the one above its `#+` settings and before its
first heading. It asserts there what it asserts on a heading: the state each
named property was in, one assertion per property, per
`0013-a-digest-guards-a-subtree-a-value-guards-a-field.md`. A file has no
subtree for a digest to cover, and this write needs none — it replaces one
field at a time — so a file node brings no vocabulary of its own and the three
states of `0018-a-property-map-spells-three-states.md` carry it whole.

Every other write tool refuses that link. A TODO keyword, a priority cookie, a
planning line, a heading's own tags and a LOGBOOK note are written on a
headline and a file has none; `org-node-set-title` renames a headline, where a
file's title is a `#+TITLE:` keyword; `org-node-set-content` replaces a body,
where a file's body is the preamble those keywords live in; and the whole-node
verbs take a node away or move it, which for a file would mean deleting or
moving the file. `org-node-create` and `org-node-refile` still take a file link
as their `parent`, where it names that file's top level.

We chose this over letting the answer fall out of which tools happen to call
`org-mcp--goto-heading`, which is how the split arose: the read surface treated
a file as a node and the write surface refused one, so a client could read a
file's `ID` and had no way to set it. We chose it over widening every tool at
once, which would have made `org-node-set-title` write `#+TITLE:` — a keyword
rather than a headline, a different element type reached by different
accessors, and a decision of its own. The dividing line is Org's: the property
drawer is the one construct Org keeps on a heading and at the top of a file
with the same syntax and the same accessors, so it is the one a write reaches
through a single contract.
