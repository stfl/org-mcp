# A key left out of a map asserts that the field holds nothing

`before_planning` names a planning field the headline carries a timestamp for
and leaves out one it carries none for, so `{"scheduled": "<2026-06-20 Sat
+1w>"}` asserts a SCHEDULED and asserts that there is no DEADLINE. A headline
that does carry a DEADLINE refuses that call. `""` and `null` inside the map are
refused, because omission already says what they would say and one state keeps
one spelling, per `0016-null-asks-a-field-to-hold-nothing.md`.

We chose this over the spelling every scalar `before` uses, where `""` names the
state of a field holding nothing. A scalar parameter has no way to say *nothing*
except by naming a value for it; a map does, by not holding the key. Spending a
second spelling on a state the container can already express would have made
every call name an empty DEADLINE for the thousands of headings that never had
one — a cost paid on each call to say something the map says by staying quiet.

The parameter is optional and the server requires it of a headline whose state
change would move a planning value. That is not the unguarded optional
precondition `0014-a-precondition-is-required-not-optional.md` refuses: the
refusal fires in exactly the case where something would move, so the guard is
off nowhere it would have caught anything, and only a repeat moves a planning
value at all. Publishing it required would tax every ordinary transition for a
guard that cannot fire on one.

## Where this does not reach

**Only inside a container the call already sent.** Omission is an assertion when
it is distinguishable from forgetting. A key left out of a map the client built
is a positive act; a parameter left out at top level reads the same whether the
client meant it or never thought about it, which is why a top-level `before`
stays required. The parameter's own absence here means "asserting nothing", and
that is safe only because of the refusal above — not because absence is
expressive at top level.

**Not `org-node-set-properties`.** Key omission in a property map already
carries a different, load-bearing meaning: *this call does not touch this
property*. `org-records-mcp--asserted-property-values` holds `before` and `after` to a
strict bijection, refusing by name a property `after` writes that `before` omits
and a property `before` names that `after` leaves alone, so an absent key there
can only ever mean "not part of this call". Giving it a second meaning would
make one signal answer two questions. The state omission would duplicate is
taken as well: `null` already spells "no such line" under
`0018-a-property-map-spells-three-states.md`. Nor do properties want the
ergonomics this was built for — every property a call touches was chosen by the
client for that call, so there is no equivalent of the headlines that would need
an empty DEADLINE spelled out.

**Not `org-clock-in`'s `clock_out`**, which is optional at top level and whose
blank already means no clock is running. That is a different mechanism rather
than a variant of this one: it is checked against what `org-records-mcp--clock-find-active`
observes at the moment of the call, an independently observable fact, so a
client that forgot the parameter gets the same refusal as one that lied about
it. Nothing there rests on the client having meant its silence.
