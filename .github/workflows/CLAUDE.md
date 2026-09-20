# .github/workflows/

Three workflows: `elisp-test.yml` (the test matrix), `claude.yml` and
`claude-code-review.yml` (the Claude Code actions).

## zizmor names its files

`just lint` runs zizmor over the three workflows, and the `zizmor` recipe in the
`Justfile` lists them by path — nothing globs. A fourth workflow is unlinted
until it is added there.

## Pinning and suppressions

Actions are pinned by version tag, `actions/checkout@v6`, not by commit SHA.
zizmor's `unpinned-uses` rule disagrees, so each `uses:` line carries the
suppression on the line itself:

```yaml
- uses: actions/checkout@v6 # zizmor: ignore[unpinned-uses]
```

A new step without that comment fails `just lint`, and the failure text names
`unpinned-uses` without saying that the convention here is the tag plus the
comment.

Workflows start from `permissions: {}` at the top level, and each job grants
only what it needs. `actions/checkout` runs with `persist-credentials: false`.

## The matrix is a published claim

`elisp-test.yml` runs the suite on `ubuntu-latest` and `macos-latest` against
Emacs 31.1 and 30.2. `README.org` ("Requirements") and `docs/installation.org`
state that support in words, and `Eask` and the `Package-Requires` header in
`org-mcp.el` carry the minimum version. Changing the matrix obliges changing
whichever of those the change makes untrue; a support claim nobody tests is the
badge problem in prose.
