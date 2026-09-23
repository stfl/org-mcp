# Domain docs

How the engineering skills consume this repo's domain documentation when
exploring the codebase. This repo is single-context: one `CONTEXT.md` at the
root and one `docs/adr/`.

## Before exploring, read these

- **`CONTEXT.md`** at the repo root — the vocabulary: what a node, a link, a
  file set and a view mean here.
- **`docs/adr/`** — the numbered decision records. Read the ones that touch the
  area about to be worked in.

If a file is missing, proceed silently. Don't flag its absence and don't suggest
creating it upfront. `/domain-modeling` creates them when a term or a decision
actually gets resolved.

## File structure

```
/
├── CONTEXT.md
├── docs/adr/
│   ├── 0001-scope-override-lasts-one-call.md
│   └── 0002-unsaved-buffers-are-not-saved.md
├── org-records-mcp.el
└── org-records-mcp-test.el
```

There is no `CONTEXT-MAP.md`: a single Emacs package is one context.

## Use the glossary's vocabulary

When output names a domain concept — an issue title, a refactor proposal, a
hypothesis, a test name — use the term as `CONTEXT.md` defines it. Don't drift
to a synonym the glossary avoids.

A concept missing from the glossary is a signal: either the language is being
invented and should be reconsidered, or there is a real gap worth noting for
`/domain-modeling`.

## Flag ADR conflicts

Output that contradicts an existing ADR surfaces the conflict rather than
silently overriding it:

> _Contradicts ADR-0001 (scope override lasts one call), but worth reopening
> because…_

## Where an ADR comes from

A decision that survives a design discussion becomes an ADR; the discussion
itself stays in `.omc/plans/` and is not committed. `docs/CLAUDE.md` owns the
form an ADR takes.
