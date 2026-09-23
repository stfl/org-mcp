# An oversized read is refused, never trimmed

A read whose walk would carry more nodes than `org-records-mcp-read-max-nodes` is
refused, and the refusal names the node the walk stopped at and the two remedies
— a shallower `depth`, or a read of that node on its own. The ceiling counts the
nodes the response carries rather than the levels it descends, because a level
bound says nothing about cost: one level of a nine-hundred-heading file is worse
than five levels of a small one. We chose refusing over trimming to fit, because
a caller handed a silently shortened subtree reads it exactly as it reads a
complete one and has no way to find out otherwise.
