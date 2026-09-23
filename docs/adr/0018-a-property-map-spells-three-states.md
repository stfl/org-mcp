# A property map spells three states, where a field spells two

A property map value is `null` for a drawer line that is not there, `""` for a
line carrying no value, and any other string for the text the line holds. Both
sides of a write take the same three, so a `before` asserts the state a read
returned and an `after` names the state it wants. We chose this over the
two-state vocabulary the other fields take, where a value is either there or it
is not, because a drawer line has a third state between those two: `org-entry-properties`
returns `("FOO" . "")` for a line written `:FOO:` and nothing at all for a
property the drawer lacks, and org-records-mcp's read path already shows the
difference — an absent property is omitted, per
`0005-a-node-omits-what-it-has-no-value-for.md`. With two spellings the write
side could not reach a state the read side could show, so a line carrying
nothing could be neither created nor preserved, and a call asserting `""`
destroyed it while reporting that it had changed nothing.

The cost accepted is that `null` destroys inside this map, where everywhere
else it is the parameter a call left out. What makes that safe here is that a
map's keys are the call's own statement of what it means to touch, which an
unused optional parameter is not, and that
`0014-a-precondition-is-required-not-optional.md` still applies: `before` names
every property `after` writes, so a deletion asserts what it destroys.
