# A date is refused only where Org would write something else

A date parameter is validated against one question: would Org put in the file
exactly what the call sent? A value Org reads and writes back unchanged is
written, whatever it looks like; a value Org reads and writes shorter, or as
another date, is refused. Nothing else is asked, so the check has no opinion of
its own to drift from Org's.

That settles the cases the surface has. An inactive timestamp goes into a
planning line active and a date range goes in as its first half alone, so both
are refused. A span whose hours run backwards — `2026-03-27 10:00-09:00` —
reaches the file exactly as sent, a read hands it straight back, and a `before`
built from that read matches it, so it is written.

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
