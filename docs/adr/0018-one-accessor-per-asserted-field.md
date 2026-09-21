# One accessor per asserted field

A field a write asserts and a read returns is read through one named accessor
that both paths call: the heading fields through
`org-mcp--heading-metadata-at-point`, which a field record names by its plist
key rather than carrying a reader of its own, and the drawer through
`org-mcp--drawer-at-point`. So a value a read handed a client is a value an
assertion accepts, and the guard cannot be pointed at a second reader without
deleting the one seam that exists.

We chose this over repairing each field where it was caught. Two readers are
not merely redundant, because Org's own two disagree in both directions on the
same field: `org-entry-get` stops a planning timestamp at the first `>`, so a
date range read whole could never be asserted at all, and reports `:FOO: nil`
as no property, so the empty string was an accepted assertion that then
destroyed a value it never named. A guard that refuses a true belief and admits
a stale one is worse than no guard, because it reads as protection. Local
repairs would have left the remaining fields agreeing by coincidence, which is
the state the three defects came out of.

The cost is that a value is fetched through the whole heading's metadata rather
than through the one reader for that field. An assertion runs once per write,
so it buys the invariant for a parse the read path was making anyway.

A field record's `:remove` stays its own, and takes the Org function whose
removal covers the whole field: `org-add-planning-info`, not `org-schedule`
with a `(4)` prefix, which matches a single timestamp and so leaves half a date
range on the line.

`content` and the two digests are not fields of this kind. What they assert is
a substring or an opaque `sha256:` token rather than the value a read returns,
and both paths already resolve to one region through `org-mcp--body-bounds` and
`org-mcp--subtree-bounds`.
