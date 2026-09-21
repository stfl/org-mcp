# Every body write asserts what it overwrites

`org-node-set-content` is the only way to change a node's body, and it reads
`before` on every call: a substring of the body, or the body's `content_digest`.
Adding a line to a long body therefore costs sending that body back, guarded.

A mode that added to the end of the body without asserting anything was rejected
with that cost in mind. It was the only write on the surface that was neither
idempotent nor guarded, so a client whose request timed out after the server had
written — or that simply retried — added the same text twice and was told both
times that it had succeeded. Duplication is not destruction, which is what the
guards were specified against, but it is the same silent wrong result.

A guarded append, `before` carrying the body's digest and `after` the text to
add, was rejected with it. It would have closed the retry hazard and kept the
bandwidth, at the price of a second spelling for one operation and of a
precondition on a call whose justification was that it needed none.

Clients written against the mode are refused by name — `Unexpected parameter:
append` — rather than silently given a replacement, so a call built for the old
surface fails loudly instead of writing something its author did not describe.
