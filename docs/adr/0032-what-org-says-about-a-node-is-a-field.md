# What Org says about a node is a field

A node's ancestors and whether it is blocked are node fields, `breadcrumbs` and
`blocked`, not computed fields.

`breadcrumbs` lists the headings above a node, outermost first, each an object
of the `title`, `link` and `level` a read of that ancestor answers with. The
file is not a crumb, since `file` and `link` already name it. `blocked` is
`org-entry-blocked-p`: `org-blocker-hook` asked about a change to done, so it
counts `org-enforce-todo-dependencies` and whatever blocker the user's Emacs
adds, org-edna's included, without org-records-mcp depending on any of them.
Each is left out where it has no value (ADR 0005): `breadcrumbs` on a top-level
heading and a file, `blocked` on a heading with a done keyword or none and on a
file. A read carries both unasked; a match list carries them when the call or
`org-records-mcp-list-fields` names them.

Fields are what Org says about the node, and computed fields are the
workflow's answers (ADR 0006); both of these are Org's. We chose them as fields
over a computed field in each workflow that wants one, where every workflow
writes the same walk or the same hook call, and a crumb's title or link can
drift from what a read of that ancestor returns. We chose `breadcrumbs` over a
single outline-path string, which a client cannot follow without parsing it
back into links.
