# An empty string is a value, not a blank

A blank parameter — JSON `null`, `false`, `[]` or `{}` — is one the call does
not send: an optional parameter takes its default and a required one is refused
as a parameter left out. An empty string is not among them on a required
parameter that carries text. It is that field's own vocabulary for *nothing*,
so `before: ""` asserts the field held no value and `after: ""` takes the value
away, on the one tool that owns the field. We chose this over a named removal
tool per field, which we built first and reversed: a required parameter never
means "not sent", which is the same reasoning that lets `""` be an assertion in
`before`, and once `before` is required the call already says what it destroys,
so `{before: "<2026-09-20 Sun>", after: ""}` was never unguarded. What remained
was one meaning for blank with no exceptions, and that did not pay for four
extra ids on a published surface where the convention is one tool per field.
Each field spells its own emptiness: `""` where the field holds text, `[]` for
the tag set that `org-node-set-tags` replaces. Adding and removing tags stay
separate tools regardless, because a delta names what it changes and destroys
nothing unseen, which is a different shape from emptying a field and is
recorded in `0015-a-tag-delta-needs-no-guard-a-replacement-asserts-everything`.
