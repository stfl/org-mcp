# Issue tracker: GitHub

Issues and specs for this repo live as GitHub issues on `stfl/org-records-mcp`. Use the
`gh` CLI for all operations.

Infer the repo from `git remote -v`; `gh` does this automatically when run
inside a clone. `origin` is `stfl/org-records-mcp`, where this project lives;
`upstream` is `laurynas-biveinis/org-mcp`, the project it grew out of, with its
own tracker. A ticket about org-records-mcp goes to `origin`. Only a defect that reproduces
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
publishable spec. Its children are its **sub-issues**, linked in dependency
order. GitHub lists them under the umbrella with a completed-of-total count, and
shows each child's parent, so the sub-issue list is the one record of what a set
contains and how far it has got. Each child names its blockers as a
`**Blocked by:** #<n>, #<n>` line in its own body; a child with no open blocker
is startable.

One umbrella is open: #2, write safety — a write says what it believed it was
changing.

Adding a ticket to a set means creating the issue, linking it as a sub-issue,
and naming its blockers in its body. The link takes the issue's database `id`,
not its number, and appends to the list, so link a set in dependency order:

```sh
id=$(gh api repos/stfl/org-records-mcp/issues/<n> --jq .id)
gh api -X POST repos/stfl/org-records-mcp/issues/<umbrella>/sub_issues -F sub_issue_id="$id"
gh api repos/stfl/org-records-mcp/issues/<umbrella>/sub_issues --paginate --jq '.[] | "#\(.number) \(.state)"'
```

The umbrella's body carries the spec. Its `## Tickets` section says the children
are its sub-issues and holds only what a list cannot: which tickets are one
decision, what the work found, what was filed alongside and left out of the set.

A ticket closes in the step that merges its branch: `gh issue close <n>
--comment` naming the merge commit, and the umbrella's count follows. The merge
commit's message carries `Closes #<n>` too, so GitHub closes a ticket whose
manual close was missed once the commit reaches `main`. A branch named for its
ticket — `fix/91-id-locations-leak` — keeps the number in front of whoever
merges it.

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

Create a GitHub issue on `stfl/org-records-mcp`.

## When a skill says "fetch the relevant ticket"

Run `gh issue view <number> --comments`.

## What stays out of the tracker

The design reports and decision records behind these tickets live in
`.omc/plans/`, which `.gitignore` excludes. An issue body must stand on its own:
a reader on GitHub cannot follow a path into this machine. Strip every
`.omc/plans/...` reference before publishing, and carry the conclusion instead
of the link.
