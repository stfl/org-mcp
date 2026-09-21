# A blank parameter is one the call does not send

A blank value — JSON `null`, `false`, `""`, `[]` or `{}` — means that the call
does not send that parameter, and nothing more: an optional parameter takes its
default, a required one is refused as a parameter left out, and no blank
anywhere removes a value. Removal is a named tool instead —
`org-node-remove-scheduled`, `-deadline`, `-priority` and `-properties`, each
with a required `before` asserting the value it destroys, and
`org-node-remove-tags`, which names the tags it takes and so destroys nothing
unseen. We chose this over the sentinel that read an empty `after` as "clear
this field", which made every destructible field reachable by accident: a
client fills a parameter it is not using with a blank, so the same value that
means "I am not sending this" also wiped a deadline, a priority or a property,
without the call ever naming a removal or vouching for what it removed. Two
empty values are kept as values rather than blanks, each in the one place its
field's vocabulary needs it — `""` is how a `before` over a field holding text
asserts that there was nothing, and `[]` is the empty tag set — and both are
legible only because those parameters are required, which is what lets an empty
value mean something instead of meaning absence.
