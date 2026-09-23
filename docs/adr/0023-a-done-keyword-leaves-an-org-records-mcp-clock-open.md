# A done keyword leaves an org-records-mcp clock open

`org-clock-in` writes the CLOCK line itself and starts no clock in the Emacs
session, so `org-clock-out-when-done` finds nothing to act on: `org-node-set-todo`
reaching a done keyword leaves the line open and reports no `clock`. org-records-mcp
does not close it in Org's place. `org-clock-out` is the call that closes a
clock — it takes the `end_time` and the `note` the close is recorded with, and
reports the start, end and duration back — and the missing `clock` field is what
tells a client to make it.

We chose this over closing the line inside the done transition. The condition
under which a done keyword stops a clock is `org-clock-out-if-current`'s, and
closing the line here would mean restating it: `org-clock-out-when-done`, and
whatever else that function comes to consult, read and applied by org-records-mcp rather
than by Org. A restatement is a second answer to a question Org already answers,
and it parts company with `C-c C-t` on the same heading as soon as the two fall
out of step. org-records-mcp reports what Org decided; it does not decide in Org's place.

We chose it over reaching for `org-records-mcp--clock-resolve-dangling`, which deletes an
open CLOCK line rather than closing it, and so would throw away the time a client
had just finished recording.

And we chose it over registering the clock with Org so that
`org-clock-out-when-done` could see it. That would start the mode-line and idle
timers, set `org-clock-marker` and `org-clock-hd-marker`, push to
`org-clock-history`, and put `org-resolve-clocks` — which prompts — in the path
of a server with no terminal to answer it.

The cost is that a client clocking a task through org-records-mcp and then finishing it
gets no `clock` field and leaves a CLOCK line open, where the same task clocked
in Emacs is closed and reported. `docs/clocking.org` states it.
