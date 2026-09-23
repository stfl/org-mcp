# A match list is an overview, a read is the whole node

`org-query` and `org-view` answer with an overview. A match carries the fields
`org-records-mcp-list-fields` names — out of the box `title`, `todo`,
`priority`, `tags`, `scheduled`, `deadline` and `link` — no Org drawer, and the
computed fields `org-records-mcp-list-computed-fields` names, which out of the
box is none. `org-node-read` and the `org://{link}` resource answer with the
whole node: every field but the two digests, the whole drawer and every
computed field. A call names `fields`, `properties` or `computed` to replace
either default.

The two calls answer different questions. A list is read to decide which
matches to look at, and it costs context per match, so each line carries what
triage needs and the link the rest is one read away by. A read is made once a
node is chosen, so it carries everything; a client that reads a node and finds
the drawer missing has to know the property names before it can ask for them,
which is the thing it read the node to learn.

The list's columns are two settings rather than constants because the
question a list answers is the workflow's. org-records-mcp names the overview
it knows every workflow needs and configures no computed field, so it names
none to carry; a workflow package adds to both lists the names its own lists
sort or group by. A name in either setting is checked at the call as a name
the call sent would be.

We chose this over a match list that carries the whole drawer and every
computed field, which spends context on every match for values a triage pass
rarely reads, and over a read that carries neither unasked, which makes the
call that returns the whole node return less than the whole of it. The inverse
is no special case: a call wanting more of a list names it, and a call wanting
less of a read sends `"none"` or names its fields.
