# A TODO configuration answer is about a file

`org-config-todo` takes an optional `link` and answers for the file that link
names. The answer is the keyword set Org reached in that file, which is the set
a write to a heading in it is validated against. Sent no link, it answers from
the global `org-todo-keywords`. Both paths return the same shape — the raw
`TODO(t!)` forms, the `|` where the sequence puts it, and an `isFinal` per
keyword — so a client reads one response format whichever question it asked.

An Org file may define its own workflow in `#+TODO:`, and the writes are
validated against that file's `org-todo-keywords-1`. A discovery tool that can
only report the global configuration therefore names states the file refuses and
omits the states it accepts, and leaves the refusal as a client's only route to
the truth. A tool of its own for the per-file question was rejected: it is the
same question about a different subject, and two tools would drift into two
answer shapes.

A link naming a heading answers for that heading's file rather than being
refused. These settings are file-wide, so the heading adds nothing to the
question, and a client that holds a heading's link should not have to take it
apart to ask. The heading itself is never looked up.

The link is resolved the way every other link on the surface is, `files`
included, so an `id:` link into a file outside Emacs's ID index is reachable
here as it is from a read or a write. A `files` sent without a `link` is refused
rather than ignored: the answer would be the global one while the call named a
file, which is the confusion this tool's `link` exists to end.

A file naming no sequence of its own is answered with the global sequences,
because that is the set Org gives it. Answering with nothing would describe an
empty workflow, which is not what such a file has.

The per-file answer is built by reading the file's own `#+TODO:`, `#+SEQ_TODO:`
and `#+TYP_TODO:` settings through `org-collect-keywords` and assembling them
the way `org-set-regexps-and-options` does, so a `#+SETUPFILE:` is followed and
the sequences are ordered as Org orders them. Rebuilding the answer from the
buffer-local variables Org derives instead was rejected: `org-todo-keywords-1`
drops each keyword's fast-access key and logging directives, and
`org-todo-key-alist` carries a key for every keyword, including the ones
`org-assign-fast-keys` invents for a sequence that named none — so that route
reports a key the file never wrote.
