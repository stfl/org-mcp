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

## Project Overview

This repository is for org-mcp, which is an integration between Emacs Org-mode
and the Model Context Protocol (MCP).

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
