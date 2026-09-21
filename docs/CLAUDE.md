# docs/

The user-facing reference. Every `.org` page here is addressed to a human
reading about org-mcp, not to an agent changing it, and that decides what may go
in and which way the links run.

Two Markdown subdirectories sit outside that rule and are described at the
bottom of this file: `adr/`, for why the interface is shaped as it is, and
`agents/`, the configuration the engineering skills read. Neither is linked
from a user-facing page, and `just lint` does not touch them.

## Routing

One page per question a reader arrives with. A new fact goes to the page that
owns the question, never to a second page that repeats it:

| Page | Owns |
|---|---|
| `installation.org` | installing the package and the shims, registering a client, the dependency versions, what the `initialize` handshake reports |
| `file-access.org` | the allowed files, `org-mcp-file-scope-override`, the `files` parameter, directory searches, finding an ID's file |
| `links.org` | the link forms a call takes, what is refused, the link every response carries |
| `reading.org` | the `org://{link}` resource, `org-node-read`, `org-node-text`, and the five configuration and discovery tools |
| `writing.org` | what a write does to buffers and files, and the nineteen write tools |
| `queries.org` | `org-query`, `org-view`, and the views, filters and settings that define them |
| `clocking.org` | the seven clock tools and `org-mcp-clock-continuous-threshold` |

The README is a primer, not a shorter copy of these pages: a fact earns a place
there only by changing what org-mcp is or what it costs to run — the tool count,
the quickstart, a limit. `CONTRIBUTING.org` states the same admission test for
contributors, and `CONTRIBUTING.org` itself owns everything about changing the
code.

## Links run outward

A page here links to another page here, to `../README.org` or to
`../CONTRIBUTING.org`. It never links to a `CLAUDE.md`, an `AGENTS.md` or
anything under `.claude/`: those are addressed to a different reader, they
assume context the user does not have, and an inbound link freezes their
headings as anchors. When a page seems to need a fact that only lives in an
agent file, the fact is filed wrong — give it a home here.

```sh
grep -rnE '\[\[file:[^]]*((CLAUDE|AGENTS)\.md|\.claude/)' ../README.org ../CONTRIBUTING.org .
```

## Headings are link targets

Cross-page links carry the heading text: `[[file:links.org::*Addressing headings
with links][Addressing headings with links]]`. Renaming a heading breaks every
inbound link silently — org-lint checks that the *file* exists, not the search
part. Grep for the old heading across `../README.org`, `../CONTRIBUTING.org` and
`docs/` before renaming one.

`just lint` org-lints these pages, `README.org` and `CONTRIBUTING.org`, each from
its own directory, so a relative link resolves the way a reader follows it. A
link to a file that does not exist fails the commit. The file list is the
`org-lint` script in `Eask`: `README.org`, `CONTRIBUTING.org`, and every `.org`
file under `docs/` at any depth, so a new page is linted without being named,
wherever it sits.

## Org markup, not Markdown

These are Org files: `*bold*`, `/emphasis/`, `=verbatim=` for code and
identifiers, `#+begin_src json` blocks for examples. `**bold**` is a Markdown
habit that renders as literal asterisks — and at the start of a line it becomes
a second-level heading.

## docs/adr/

Numbered Markdown records of decisions that shaped the interface, one decision
per file: what org-mcp does, and what that was chosen over. They are short and
written in the present tense, and nothing links to them from the user-facing
pages — they are for whoever asks why the interface is shaped this way. A
decision that survives a design discussion belongs here; the discussion itself
belongs in `.omc/plans/`, which is not committed.

## docs/agents/

The issue tracker, the triage label vocabulary and the domain-doc rules the
engineering skills read, one Markdown file each. They are addressed to an agent,
so nothing in `README.org`, `CONTRIBUTING.org` or the `.org` pages here links to
them; the root `CLAUDE.md` does, under "Agent skills". The skills resolve these
paths themselves, so a file here is renamed only alongside the skill that reads
it.
