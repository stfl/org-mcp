# A digest covers a buffer region, not a tool's output

Both digests are taken over a region of the file, and `org-node-text` is defined
as returning the subtree region minus one trailing newline it drops for its
reader: the read depends on the region, never the region on the read. We chose
this over hashing what the verbatim read returns, which is the obvious
implementation because that string already exists and needs no second definition
of a subtree's extent; doing it that way would make a trimming decision taken
for display into the boundary a destructive write is guarded by. The price is a
visible asymmetry — two nodes carrying identical `content` and different
`content_digest`s — which reads like a bug, and "fixing" it by hashing `content`
is exactly the reversal this record exists to stop.
