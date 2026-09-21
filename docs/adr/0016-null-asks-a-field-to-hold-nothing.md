# Null asks a field to hold nothing; an empty string is a value

Each field spells its own emptiness, and that principle stands. What it does
not license is reading `""` as every field's spelling for it.

In an `after`, JSON `null` is the ask for the field to hold nothing — it is
JSON's own word for no value — and an empty string is a value, accepted exactly
where the field has one. A body can be empty and a property line can stand
present and empty, so `""` is meaningful there; a TODO keyword, a SCHEDULED, a
DEADLINE and a priority are each a value or an absence with nothing in between,
so on those `""` names no state the field can hold and is refused with the
message that says what the field does take and the null that takes a value away.
Every other blank — `false`, `[]`, `{}` — is a parameter the client filled but
did not send, and is refused as one, so nothing is destroyed by a parameter
nobody meant to fill. We chose this over spelling every removal `after: ""`,
which we shipped first: that made an *invalid input* into a command, and it only
looked uniform because one spelling was applied across fields whose value sets
differ. We chose it over a named removal tool per field, which we also shipped
first and reversed, because `before` is required and already says what a removal
destroys, so four extra ids bought nothing on a surface whose convention is one
tool per field.

A `before` keeps `""` for *there was no value*, on every field, and that
asymmetry with `after` is deliberate rather than tolerated. An assertion names a
**state the field was in**, and its states are the field's values plus the empty
one; a value to write names a **value the field will hold**, and a field with no
empty value has none to name. That distinction carries the decision by itself,
and it is the same one the read side already makes: a node omits a key for a
field it has no value for, and never sends `null`, per
`0005-a-node-omits-what-it-has-no-value-for`. So one condition is spelled three
ways across a round trip — absent in a read, `""` in a `before`, `null` in an
`after` — and each says exactly one thing where it stands. A `before` is also
the guard, so a blank there can never assert anything — but that says only that
a meaningful blank in `after` is safe, which was as true of the spelling this
replaced, so it decides nothing between them. The tag set is the same rule seen
from the other side — it has an empty value, spells it `[]`, and a tag delta
stays a separate tool for the reason in
`0015-a-tag-delta-needs-no-guard-a-replacement-asserts-everything`.
