# An array parameter is also taken as its JSON text

Every parameter documented as taking an array — `fields`, `properties`,
`computed`, `files` and `tags` — also takes the JSON text of that array, and
reads it back as the array before any other check runs. A value whose first
non-blank character is `[` and which is not a JSON array is refused, naming the
parameter.

The advertised schema types every parameter as `string`. `mcp-server-lib`
builds the schema from the handler's argument list and gives each argument the
type `string`; its tool spec carries `:id`, `:description`, `:title` and
`:read-only` and nothing that could say otherwise. A client that validates its
arguments against the advertised schema therefore cannot send an array at all:
it sends the array as its own JSON text, and without this the text arrives as
one value — one field-list name, one tag, one path spelled with brackets.

Correcting the advertised schema is the better fix and is not ours to make: it
belongs in `mcp-server-lib`, which would have to carry a per-parameter type.
Reaching into that package's registry after registration to rewrite the schema
was rejected — it is private state, and a server that edits its library's
bookkeeping breaks on the release that reorganises it.

A leading `[` is unambiguous for all five parameters: a file path is absolute,
a field, property or computed name is an identifier, and `org-tag-re` forbids
`[` in a tag. So the text form costs nothing a caller could have meant, and
every string form each parameter already offers — a single tag, a single path,
`all`, `none`, the name of a configured field list — goes on meaning what it
meant. When `mcp-server-lib` grows a per-parameter type, the schema is
corrected and this decoding stays as the compatible path for clients already
sending text.
