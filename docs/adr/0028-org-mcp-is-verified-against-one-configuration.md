# org-mcp is verified against one configuration

org-mcp behaves as a regular Org session, and it does that by not deciding
anything Org already decides. A write goes through `org-todo`,
`org-archive-subtree`, `org-clock-clock-out`, `org-schedule`, `org-deadline` and
`org-refile`, so the settings governing logging, blocking, drawers and done
transitions are read by Org at the moment it acts. `org-log-done`,
`org-log-repeat`, `org-log-into-drawer`, `org-clock-out-when-done`,
`org-enforce-todo-dependencies` and `org-archive-mark-done` appear in
`org-mcp.el` only in docstrings: nothing reads them, because nothing has to.
That is the claim, and delegation is what makes it cheap to keep.

A claim nothing exercises is a wish, so this record fixes what backs it. A
setting is **verified** when the suite runs the same assertion under two or more
of its values and asserts a different outcome for each — `org-log-into-drawer`
at `t` and at `nil`, `org-use-tag-inheritance` at `t`, `nil`, a list and a
regexp. A setting bound once to make a fixture parse is not verified by being
bound: pinning `org-priority-highest` to Org's own default keeps an assertion
deterministic and says nothing about a session that moves the range.
`docs/verified-settings.org` carries the list, is written for the user asking
whether org-mcp fits their Org, and is the page a parked ticket points at.

Settings outside that list are **untested, not unsupported**. Org reads them
itself, so its behaviour applies and most of them will simply work; what org-mcp
does not have is evidence. Saying so is the honest position, and it is also the
useful one, because it tells a reader which of their settings puts them ahead of
the suite. We chose this over claiming support for Org's configuration surface
as a whole, which nothing could check and which the first counter-example would
falsify. We chose it over refusing to run under a setting the suite does not
cover, which would break sessions that work today to defend a claim nobody made.
And we chose it over enumerating every Org variable with a verdict beside it:
the settings org-mcp never reads have no verdict to give that is not Org's, and
a table asserting one for each would assert exactly what this record says org-mcp
does not know.

org-mcp answers for the Emacs it runs in, and for one Emacs. Org keeps element
caches in a store shared by every Emacs using the same `org-persist-directory`,
and a second Emacs exiting can delete a file the first is reading back. org-mcp
does not defend against that: the boundary is the process, and a peer editing
Org's shared state from outside it is Org's own concern. The test suite gives
its run a private store, which is isolation of the tests and not a claim about
the server.

The consequence for the tracker is that a defect reachable only under an
unverified setting is **recorded and left open**, never built and never closed
as `wontfix`. Building it would ship code no test covers, under a setting no
session runs, to fix a report nobody has made from use; closing it would throw
away a measurement someone paid for. So `#56` (a clock-out swallowing an Org
veto, under `org-clock-out-switch-to-state` with a blocker installed), `#83` (a
done transition rewriting tags, under `org-todo-state-tags-triggers`) and `#93`
(a buffer left half-set-up when a peer Emacs deletes a cache file) stay open with
their measurements attached, and the settings page names each so a reader meets
it before the defect does.
