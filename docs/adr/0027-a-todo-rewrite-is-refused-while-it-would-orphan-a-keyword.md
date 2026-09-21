# A #+TODO: rewrite is refused while it would orphan a keyword

Writing a file's `#+TODO:` settings is refused while any heading in that file
carries a keyword the new sequences would no longer name. The refusal names each
such keyword and counts the headings carrying it. It is the unmarked validation
class: the file is as the client believed it to be, so reading it again resolves
nothing, and Org vetoed nothing — what has to change is the call.

Org does not refuse such a rewrite and does not mark the headings. A keyword its
sequences no longer name is simply the first word of the heading's title:
`* WAIT Hear back` under a sequence without `WAIT` is a heading with no keyword
titled `WAIT Hear back`, which is what `org-heading-components` and
`org-element` both report. Nothing in the file records that it was ever a state,
and the headings were retitled by a call that named none of them.

We chose the refusal over writing the line and reporting how many headings it
orphaned. A report arrives after the fact, and the fact is not recoverable from
the file: putting the keywords back restores the states, but only for a client
that still holds the old sequence and knows which titles to look at. A guarded
write whose consequence cannot be undone from what it returns is not guarded.

We chose it over a parameter that waives the check, because the waiver would be
the interesting path and nothing about the call says which one a client meant.

The refusal has a remedy inside the server, which is what makes it a refusal and
not a dead end. A keyword is retired in three calls: write the new sequences
beside the old ones, move the headings with `org-node-set-todo` while both sets
of keywords are valid, then write the sequences again without the old one. A
keyword no heading carries is dropped silently, so a workflow can be trimmed of
states nothing is in.

The check asks what Org will reach after the write, not what the call wrote: the
file's own `#+TODO:` lines are taken out of what `org-collect-keywords` returns
and the call's lines put in their place, so a `#+SETUPFILE:`, a `#+SEQ_TODO:`
and a `#+TYP_TODO:` all still count, and a file left naming no sequence falls
back to the global `org-todo-keywords` as Org falls back to it. The sequences are
assembled by the function that assembles them for `org-config-todo`, so the two
cannot disagree about what a file's workflow is.

`#+ARCHIVE:` and `#+FILETAGS:` reach past their own lines too and are written
without a refusal. Neither turns something already in the file into something
else: an archive setting redirects the next archive, and a file tag changes which
tags are in effect while no heading's own tags move. The `#+TODO:` refusal is
about a write that rewrites headings, not about a write with consequences.
