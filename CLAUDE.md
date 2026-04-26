# CLAUDE.md

This file provides guidance to you, Claude Code, when working with code in this
repository. These guidelines build on the common user's guidelines at
~/.claude/CLAUDE.md locally or
<https://raw.githubusercontent.com/laurynas-biveinis/dotfiles/refs/heads/master/ai/.claude/CLAUDE.md>
online.

## Git Commits

Always run `git commit` inside the Nix devshell:

```sh
nix develop --command git commit …
```

The pre-commit hooks run `just lint` and `just test`, which require the tools
provided by the devshell (eask, shellcheck, zizmor, etc.).

## Formatting

Run `just fmt` inside the Nix devshell to auto-format Elisp and shell code:

```sh
nix develop --command just fmt
```

This runs `elisp-autofmt` on `.el` files and `shfmt` on shell scripts.

## Project Overview

This repository is for org-mcp, which is an integration between Emacs Org-mode
and the Model Context Protocol (MCP).

org-mcp is a thin MCP adapter on top of Org-mode. Its value is a faithful,
stable mapping between MCP primitives and Org's existing semantics. Do NOT
reinvent functionality that Org already provides — see "Prefer Org APIs Over
Manual Parsing" below for the core principle that governs all changes to
`org-mcp.el` and `org-mcp-test.el`.

User-facing documentation is in README.org.

In the ERT tests, always use defconst constants for before and after Org file
images.

To verify the changed Org content, use a single regular expression, matching
the complete Org file.

When adding, removing, or changing MCP tools, resource templates, or custom
variables (`defcustom`) in `org-mcp.el`, update `README.org` accordingly:

- New tools → add a `***` subsection under the appropriate `**` section in
  "Available MCP Tools"
- New `defcustom` → document it in a "Configuring …" section and mention it in
  the Doom Emacs example if relevant
- Removed or renamed tools/variables → remove or update their documentation

## Prefer Org APIs Over Manual Parsing

org-mcp is a thin MCP adapter on top of Org-mode. Its value is a faithful,
stable mapping between MCP primitives and Org's existing semantics. Do NOT
reinvent functionality that Org already provides.

Before adding or changing any parsing, navigation, clock, drawer, tag, or
TODO-state logic, audit Org's public API first. Prefer:

- `org-find-olp`, `org-get-outline-path` over manual level-regex walks
- `org-map-entries`, `org-map-tree`, `org-element-map` over level regex
- `org-end-of-meta-data` over hand-rolled drawer skipping
- `org-heading-components`, `org-element-at-point` over chained
  `org-entry-get`
- `org-insert-subheading` over manual heading-asterisks insertion
- `org-clock-in`/`-out`, `org-find-open-clocks`, `org-clock-resolve` over
  CLOCK regex scanners
- `org-remove-empty-drawer-at` over custom drawer deletion
- `org-time-string-to-time`, `org-duration-from-minutes` over custom parsers
- `org-add-log-setup` + `org-store-log-note` over manual LOGBOOK formatting
- `org-todo-keywords-1`, `org-done-keywords` over destructuring
  `org-todo-keywords`
- `org-tag-alist-to-groups`, `org-tag-re` over custom tag parsing/regex
- `org-element-parse-buffer` over regex-based block/drawer detection

If you believe an Org API is genuinely missing or unsuitable, document the
reason in a comment adjacent to the workaround so future agents (and humans)
can re-evaluate when Org evolves.

When touching existing code that duplicates Org functionality, prefer
replacing it with the Org API rather than extending the duplication.


<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:ca08a54f -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

## Session Completion

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd dolt push
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
<!-- END BEADS INTEGRATION -->
