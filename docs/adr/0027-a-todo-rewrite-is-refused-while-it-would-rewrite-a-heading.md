# A #+TODO: rewrite is refused while it would rewrite a heading

Writing a file's `#+TODO:` settings is refused while any heading in that file
would read differently afterwards. The refusal names the keywords that move and
counts the headings each takes with it. It is the unmarked validation class: the
file is as the client believed it to be, so reading it again resolves nothing,
and Org vetoed nothing — what has to change is the call.

Org takes a heading's first word for its keyword when the sequences name that
word, and for the start of its title when they do not. That is one rule and it
cuts both ways. Under sequences that stop naming `WAIT`, `* WAIT ship it` is a
heading with no keyword titled `WAIT ship it`; under sequences that start naming
it, `* WAIT for the parts` is a WAIT heading titled `for the parts`. Measured:
`org-heading-components` and `org-element` both report the moved title, and
`org-todo-keywords-1` moves with the settings. Each direction rewrites headings
the call named none of, each changes their `::*title` links, and each leaves
nothing in the file saying what they were. So the guard covers both.

We chose this over guarding the direction that takes a keyword away and letting
the other through. Every reason for guarding at all — nothing in the file
records what the heading was, the headings were rewritten by a call that named
none of them — is a statement about the rule rather than about a direction.
Recoverability does not separate them either: in both, the heading bytes never
move, writing the settings back restores the reading exactly, and the client
holds the old set because the response echoes `before`. A guard is directional
only where its reason is, and this one's is not.

We chose the refusal over writing the line and reporting how many headings it
moved. A report arrives after the fact, and the fact is not recoverable from the
file: putting the keywords back restores the states, but only for a client that
still holds the old sequences and knows which titles to look at. A guarded write
whose consequence cannot be undone from what it returns is not guarded. We chose
it over a parameter that waives the check, because the waiver would be the
interesting path and nothing about the call says which one a client meant.

The refusal has a remedy inside the server, which is what makes it a refusal and
not a dead end, and there is one per direction. A keyword on its way out is
retired in three calls: write the new sequences beside the old ones, move the
headings with `org-node-set-todo` while both sets are valid, then write the
sequences again without the old one. A title about to become a keyword is moved
out of the way with `org-node-set-title` first. A keyword no heading carries
passes in either direction without a word, so a workflow can be trimmed of
states nothing is in and extended with states no title begins with.

Which headings move is Org's reading and not a reading of their titles. A
keyword stands in the slot before the priority cookie, so `* [#A] WAIT behind a
cookie` keeps `WAIT` in its title whatever the sequences name; and the match is
on the whole word, so sequences naming `WAIT` say nothing about `* WAITING`.
Both were measured, and both are why the check asks Org rather than deciding
from the first word of a title: the write is made, `org-set-regexps-and-options`
reads the settings again, the headings are read a second time, and the two
readings are compared. Deriving the answer from the keyword sets instead would
be a second headline grammar beside Org's, and it would refuse those two
headings. When the comparison refuses, the change group puts the text back and
the caller puts Org's reading of it back with it, so a refused call leaves the
buffer holding the workflow its file still names.

`#+ARCHIVE:` and `#+FILETAGS:` reach past their own lines too and are written
without a refusal. Neither turns something already in the file into something
else: an archive setting redirects the next archive, and a file tag changes which
tags are in effect while no heading's own tags move. This refusal is about a
write that rewrites headings, not about a write with consequences.
