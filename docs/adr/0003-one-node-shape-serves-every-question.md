# One node shape serves every question

org-records-mcp answers with one node wherever it answers with a node at all: a file is
a node at level 0, a heading is a node, a child inside a node is a node, and a
query match is a node. One function builds all four, the collection of children
is called `children` everywhere, and a client learns one vocabulary to walk a
whole outline. We chose this over a shape per question — an outline entry, a
match, a lightweight child reference — which is what the surface had, and which
gave the same fact a different key depending on which call returned it.
