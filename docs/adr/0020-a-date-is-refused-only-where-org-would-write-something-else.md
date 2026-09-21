# A date is refused only where Org would write something else

A date parameter is validated against one question: would Org put in the file
the date the call named? A value Org reads and writes back unchanged is
written, whatever it looks like; a value Org reads and then writes as some
other date is refused. Nothing else is asked, so the check has no opinion of
its own to drift from Org's.

That settles the cases the surface has. An inactive timestamp goes into a
planning line active and a date range goes in as its first half alone, so both
are refused. So is text Org's parser reads past: it reads a timestamp's parts
and keeps none of what else stands between the brackets, so
`<2026-03-27 Fri 09:00 +1w typo>` would reach the file as
`<2026-03-27 Fri 09:00 +1w>` — the repeater the call asked for and none of the
word it got wrong. What Org read is asked of the string the call sent, a word at
a time: a word whose absence leaves the rendering as it was is a word Org read
nothing from, and a word Org did read cannot go without the rendering going with
it. Each word is asked in its own right, because a word Org reads past stands
anywhere between the brackets — a repeater typed wrong stands before the
repeater it was meant to be. The parsed element cannot answer the question at
all, because the words Org dropped are in no property of it, and
reading Org's timestamp grammar a second time here would give the check an
opinion of its own, which this record denies it.

This refusal is asked after every other one, because its message names the
timestamp Org would have written and a client sends what a message names. Asked
earlier it would name what the next refusal rejects: for `<2026-02-30 Fri typo>`
the second of March, which is exactly the rolled date the surface refuses.

The day name is read past and written rather than refused, for the reason the
rule is what it is: Org writes the day the date falls on, so a call that
misspells the day name still gets the date it named and loses nothing by it. A
span whose hours run backwards — `2026-03-27 10:00-09:00` — and a first-only
warning delay standing alone — `2026-03-27 --3d`, which carries the doubled
hyphen a range is joined by — each reach the file exactly as sent, a read hands
them straight back, and a `before` built from that read matches, so both are
written.

One loss the rule does not reach is refused all the same. Org's planning writer
takes a first-only warning delay off a timestamp that also carries a repeater,
so `<2026-03-27 Fri +1w --3d>` would reach the file as `<2026-03-27 Fri +1w>`.
The date the call named is the date the field would hold, so the question above
passes it; what goes is a warning. A warning the call asked for, absent from the
file and answered with a success, is the silent half-write the date range is
refused for, and it costs a client the same whether the part that went missing
was a date or a warning. The pairing is refused, and the refusal names the two
timestamps that go in its place: the repeater by itself, and the `-3d` that
warns before every repeat.

That makes org-mcp stricter than Org here, which is the cost. Org accepts the
pairing, and a person typing it in Emacs gets the shortened timestamp and no
complaint. We pay it because the two readers are not alike: a person watches the
line they typed collapse in the buffer in front of them, while a client is told
`success: true` and learns nothing unless it reads `after` back and compares. A
faithful mapping is to Org's semantics, not to Org's silence.

The check asks the parsed element, not the string, because the element is where
the evidence survives: the parse keeps the delay and the loss happens later,
inside the planning writer, so what Org renders for the value still carries it.
`:warning-type` is `first` and `:repeater-type` is non-nil — one condition, and
narrow, because either part alone is written as sent.

We chose this over refusing what reads as a mistake. Backwards hours are one,
but Org accepts them, the agenda shows them, and a person editing the file in
Emacs can write one. `after` takes the raw string a read returns, so a rule
about how a date *reads* would refuse a value a read of an ordinary Org file
produced, and org-mcp would be the one place in the round trip that cannot carry
what Org itself holds. The date range is the one break in that round trip, and
it is there because the write would lose half of the value silently — which is
the cost a refusal has to buy off before it is worth making.

We chose it over accepting whatever Org's parser reads, which is where the
surface started. That parser is deliberately tolerant, and a string it reads is
not a string Org's planning writer carries: `<2026-03-27 Fri 09:00>--<2026-03-27
Fri 10:00>` parses, and the file gets nine o'clock and nothing else.
