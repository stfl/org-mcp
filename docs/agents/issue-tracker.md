# Issue tracker: GitHub

Issues and specs for this repo live as GitHub issues on `stfl/org-mcp`. Use the
`gh` CLI for all operations.

Infer the repo from `git remote -v`; `gh` does this automatically when run
inside a clone. `origin` is `stfl/org-mcp`, the fork this work happens on;
`upstream` is `laurynas-biveinis/org-mcp` and has its own tracker. A ticket
about this fork's roadmap goes to `origin`. Only a defect that reproduces
against upstream belongs on `upstream`, and that is a decision Stefan makes,
never a session.

## Conventions

- **Create an issue**: `gh issue create --title "..." --body-file <path>`. A
  body longer than a line goes in a file; a heredoc through `--body` mangles
  backticks.
- **Read an issue**: `gh issue view <number> --comments`.
- **List issues**: `gh issue list --state open --json number,title,body,labels,comments --jq '[.[] | {number, title, body, labels: [.labels[].name], comments: [.comments[].body]}]'`
  with `--label` and `--state` filters as needed.
- **Comment**: `gh issue comment <number> --body "..."`
- **Apply / remove labels**: `gh issue edit <number> --add-label "..."` /
  `--remove-label "..."`
- **Close**: `gh issue close <number> --comment "..."`

## Umbrella issues

A body of work larger than one ticket is an umbrella issue holding the
publishable spec, with a `## Tickets` checklist naming its children in
dependency order. Each child carries a `Part of #<umbrella>` line and a
`**Blocked by:** #<n>, #<n>` line. A child with no open blocker is startable.

Two umbrellas are open:

| Issue | Covers |
|---|---|
| #1 | Restructure the API surface around one node — native Org links replace the invented address formats |
| #2 | Write safety — a write says what it believed it was changing |

Adding a ticket to a set means creating the issue, adding a line to the
umbrella's checklist, and naming its blockers in its own body. The three places
are the whole mechanism; there is no sub-issue API in use here.

A ticket closes in the step that merges its branch: `gh issue close <n>
--comment` naming the merge commit, and its line in the umbrella's checklist
ticked in the same pass. The merge commit's message carries `Closes #<n>` too,
so GitHub closes a ticket whose manual close was missed once the commit reaches
`main`. A branch named for its ticket — `fix/91-id-locations-leak` — keeps the
number in front of whoever merges it.

## Blocking

A blocked ticket states its blockers as `**Blocked by:** #<n>` at the top of the
body and carries no triage label — the five canonical roles have no "blocked",
and an unlabelled issue with an open blocker is the honest representation. It
gains `ready-for-agent` when its blockers close.

## Pull requests as a triage surface

**PRs as a request surface: no.** _(Set to `yes` if this repo treats external
PRs as feature requests; `/triage` reads this flag.)_

When set to `yes`, PRs run through the same labels and states as issues, using
the `gh pr` equivalents:

- **Read a PR**: `gh pr view <number> --comments` and `gh pr diff <number>`.
- **List external PRs for triage**: `gh pr list --state open --json number,title,body,labels,author,authorAssociation,comments`
  then keep only `authorAssociation` of `CONTRIBUTOR`,
  `FIRST_TIME_CONTRIBUTOR` or `NONE`.
- **Comment / label / close**: `gh pr comment`, `gh pr edit --add-label` /
  `--remove-label`, `gh pr close`.

GitHub shares one number space across issues and PRs, so a bare `#42` may be
either: resolve with `gh pr view 42` and fall back to `gh issue view 42`.

## When a skill says "publish to the issue tracker"

Create a GitHub issue on `stfl/org-mcp`.

## When a skill says "fetch the relevant ticket"

Run `gh issue view <number> --comments`.

## What stays out of the tracker

The design reports and decision records behind these tickets live in
`.omc/plans/`, which `.gitignore` excludes. An issue body must stand on its own:
a reader on GitHub cannot follow a path into this machine. Strip every
`.omc/plans/...` reference before publishing, and carry the conclusion instead
of the link.
