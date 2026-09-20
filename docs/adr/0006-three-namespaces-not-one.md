# A node's fields, its Org drawer and its computed values are three namespaces

A call names drawer properties and computed values in parameters of their own,
and each arrives under its own key — `properties` and `computed` — beside the
node's fields. We chose this over naming them inside `fields`, where a drawer
key called `TITLE` would collide with the node field `title`, and over returning
one merged map, which is convenient and a correctness bug: a client cannot see
which values the file holds and which are this server's answer at read time, so
the next write puts our opinion into the user's file. The same decision rules
out a setting that merges a workflow's answers into the node's own keys, which
is what `org-mcp-ql-extra-properties` did on the query path alone.
