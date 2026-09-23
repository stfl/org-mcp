# A file's drawer is made where Org reads one, not where Org would write one

A property write creates the drawer a file lacks, as it creates the drawer a
heading lacks. Org puts a file's at the top of the buffer — above `#+TITLE:`,
`#+TODO:` and every other setting, after any leading comment line — and that
placement is not cosmetic. Org reads a file-level drawer only there, so a
drawer written under a `#+` line is read as no drawer at all and the property
could never be found again.

Where Org's placement rule and Org's reading rule disagree, the reading rule
decides. A file whose first line is a heading has no region before that
heading, and `org-set-property` at `point-min` writes that *heading's* drawer,
because every Org property accessor starts from
`org-back-to-heading-or-point-min`. org-records-mcp writes the two lines
`org-insert-property-drawer` would have written above the heading instead, and
reads a file node's drawer under the same rule, so such a file reports no
properties of its own until a write gives it some.

A call that only takes properties away makes nothing, as `org-entry-delete`
makes nothing: its assertions were all of absence, and all of them held.
Making one anyway would leave the same bytes, because `org-entry-delete`
removes a drawer it has emptied — but it would write and save the file for a
call that changed nothing, and it would rest the outcome on Org cleaning up
after a drawer org-records-mcp had no reason to make.

We chose this over calling `org-set-property` at `point-min` and letting Org
place the drawer, which sets a property on the first heading while the response
reports the file's link and the file's `before` — a write landing on a node the
call never named, and, for a removal, taking a line out of that heading. We
chose it over refusing a file that opens on a heading, which is what Org's own
commands amount to: that refusal has no remedy through the server, since no
tool writes above a first heading, and the read already answers that the file's
drawer is empty rather than that it is impossible.
