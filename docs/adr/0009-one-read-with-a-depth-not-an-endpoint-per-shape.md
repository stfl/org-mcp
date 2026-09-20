# One read with a depth, not an endpoint per shape

A node is read by `org-node-read` at whatever `depth` the caller asks for, and
no second endpoint exists for a particular shape of that answer. We chose this
over keeping a dedicated outline tool — a cheap fixed two-level read of a file
that needed no `depth` and cost one call — and, behind that, over the habit of
answering a common question with a tool of its own, which is what grew five
vocabularies for one node. The price accepted is that a client wanting the old
answer sends `depth: 1` and a field list, and pays the node ceiling for a file
whose top two levels are large.
