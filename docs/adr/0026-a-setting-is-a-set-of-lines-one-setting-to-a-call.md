# A setting is a set of lines, and a call writes one setting

`org-file-settings` reads a file's in-buffer settings and `org-file-set-setting`
writes one of them. Both name a file, or a heading in it, since the settings are
file-wide. The read comes with the write and not after it: `before` is required
on every destroying write, and a client cannot assert a value it has no way to
obtain. Only `#+TITLE:` was reachable before, through a file node's `title`,
which answers with the file's own name when the file writes no `#+TITLE:` — so
that field cannot tell a file that sets none from one that sets its own name.
The read answers `[]` for the first and `["notes.org"]` for the second.

The value of a setting is the set of lines the file writes it on, not a string.
Org keeps every line it finds: two `#+TODO:` lines are two sequences, two
`#+FILETAGS:` lines are both in effect, and even `#+ARCHIVE:` and `#+CATEGORY:`,
where Org reads the first line and ignores the rest, can stand on several lines
that a write has to account for. So the guard asserts the whole set, as the tag
replacement does in
`0015-a-tag-delta-needs-no-guard-a-replacement-asserts-everything.md`: the call
takes away every line it does not list, and its blast radius is the whole set.
Unlike a tag set the order is asserted too, because Org reads it — two
`#+TITLE:` lines join in the order they stand, and the first `#+TODO:` sequence
is the one a heading with no keyword enters first.

One call writes one setting. We chose this over a map of settings, the shape
`org-node-set-properties` takes, because a drawer's lines are all one kind of
thing while these are not: `#+TODO:` reaches every heading in the file and
`#+TITLE:` reaches nothing, so a call carrying several would refuse them all for
one setting's sake, or report a part of itself done.

The settings in scope are `#+TITLE:`, `#+TODO:`, `#+ARCHIVE:`, `#+CATEGORY:`,
`#+FILETAGS:` and `#+STARTUP:`: the ones that say what the file is and how Org
treats it, which a client setting a file up has to write, and whose effect on
what is already written org-records-mcp can account for. We chose an allowlist over
taking any `#+` line, because two kinds must not be reachable here. A setting
that names another file — `#+SETUPFILE:`, `#+INCLUDE:` — would change what this
file means by editing what a file the call never named says, and the allowed
files never saw that file. And `#+PROPERTY:` sets properties file-wide: it
belongs to the property surface, where
`0018-a-property-map-spells-three-states.md` already spells what a property line
can be, and one construct with two writers would have two vocabularies for one
assertion. `#+CATEGORY:` is in although Org files its value in
`org-keyword-properties` beside the `#+PROPERTY:` ones, because it names one
thing rather than an arbitrary key.

These settings are not properties, which
`0024-a-property-write-reaches-a-file-node.md` left open when it kept
`org-node-set-title` off a file's `#+TITLE:`: a keyword is a different element
type reached by different accessors, so it is a tool of its own rather than a
widening of that one. `org-node-set-title` still renames headlines only.
