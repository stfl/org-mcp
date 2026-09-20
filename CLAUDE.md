# CLAUDE.md

Guidance for agents changing this repository. It builds on the user's global
guidelines at `~/.claude/CLAUDE.md`.

org-mcp is a thin MCP adapter on top of Org-mode. Its value is a faithful,
stable mapping between MCP primitives and Org's existing semantics, so the
default answer to "how do I parse/navigate/clock this?" is an Org function that
already does it. `CONTRIBUTING.org` holds that rule with the table of APIs to
reach for, the devshell, the checks and the test conventions. Read it before
touching `org-mcp.el` or `org-mcp-test.el`.

## Commands

The command lines below are here because an agent working through the shell
never loads a page. Why they are what they are belongs to `CONTRIBUTING.org`.
They run inside the Nix devshell:

```sh
nix develop --command just check    # fmt + lint + test, the pre-commit gate
nix develop --command just fmt      # elisp-autofmt and shfmt
nix develop --command git commit …  # the hook runs `just check`, which needs the shell
```

A failing lint stage leaves `.lint-output.txt`; the test run always leaves
`.test-output.txt`. Read those instead of re-running the stage verbosely.

## What a change is obliged to keep true

- **A change to anything a client or an Emacs user can observe updates its
  documentation page in the same commit.** Tools, resource templates and
  `defcustom`s are the obvious ones; a public variable, what the `initialize`
  handshake reports, and a refusal message a client acts on count too.
  `CONTRIBUTING.org`, "What a change owes the documentation", maps each to its
  page. The README changes only when what org-mcp *is* or what it costs to run
  changes.
- **Human-facing documents never link into this file** or into any `AGENTS.md`,
  and never into `.claude/`. `README.org`, `CONTRIBUTING.org` and `docs/*.org`
  link to each other; this file links out to them. A fact a human needs belongs
  in one of theirs.
- **`just lint` org-lints `README.org`, `CONTRIBUTING.org` and `docs/*.org`.** A
  link to a file that does not exist fails the commit, so a renamed or deleted
  page has to be repaired in the same change.
- **The directory files below are part of the code.** Changing what one of them
  describes obliges reconciling it before the work is finished; a directory file
  that documents a removed behaviour teaches the next agent something false.

## Directory files

Each of these loads when a file in that directory is read or edited with the
file tools. Reading the same path with `cat` loads none of them, so anything an
agent must not miss is in this file instead.

| File | Covers |
|---|---|
| `scripts/CLAUDE.md` | the quiet-output contract, the formatter's Emacs pin, what a new script owes the Justfile |
| `docs/CLAUDE.md` | which page owns which facts, Org markup, headings as link targets, the ADRs |
| `.github/workflows/CLAUDE.md` | pinning and zizmor suppressions, the matrix the README's support claim comes from |

`AGENTS.md` beside each one is a symlink to it, for tools that read that name.
`CLAUDE.md` is always the real file: a missing symlink costs another tool and
never costs Claude Code.

## Agent skills

The three files below are agent-facing configuration the engineering skills
read. They are the one thing under `docs/` not addressed to a user, and no
human-facing page links to them.

### Issue tracker

GitHub issues on `stfl/org-mcp`, driven through `gh`, with an umbrella issue per
body of work and its children in dependency order. See
`docs/agents/issue-tracker.md`.

### Triage labels

The five canonical roles, each label string equal to its name. A ticket with an
open blocker carries none of them. See `docs/agents/triage-labels.md`.

### Domain docs

Single-context: `CONTEXT.md` at the root, `docs/adr/` beside it. See
`docs/agents/domain.md`.

## Plans and specs

Tickets live on the tracker. Design reports and decision records live in
`.omc/plans/`, one Markdown file per topic; `.gitignore` excludes `.omc/`, so
they stay on this machine. Tracked files and issue bodies carry their
conclusions, never links to them — a reader outside this machine cannot follow
one, so a `.omc/plans/…` path is stripped before anything is published.

## Ending a session

Work ends committed, never stashed: the stash is shared with every worktree of
this repository and other sessions pop it.

A session ends with the branch pushed: `git pull --rebase && git push`, then
`git status` to confirm the branch tracks its remote. `feat/native-links-and-file-scope`
is published and tracks `origin`; `main` is where it is headed, and merging it
there is Stefan's decision, not a session's.

Several agents work this repository at once, each in its own worktree under
`.claude/worktrees/`. Before editing after a resume, check `git status
--short --branch` and the worktree you are in; before merging, ask the sessions
named in `.omc/plans/orchestration-*.md` what they have in flight.
