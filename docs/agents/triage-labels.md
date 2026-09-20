# Triage labels

The skills speak in terms of five canonical triage roles. This file maps those
roles to the label strings on `stfl/org-mcp`.

| Role in the skills | Label in this tracker | Meaning |
|---|---|---|
| `needs-triage` | `needs-triage` | Maintainer needs to evaluate this issue |
| `needs-info` | `needs-info` | Waiting on the reporter for more information |
| `ready-for-agent` | `ready-for-agent` | Fully specified, ready for an AFK agent |
| `ready-for-human` | `ready-for-human` | Requires human implementation |
| `wontfix` | `wontfix` | Will not be actioned |

When a skill names a role — "apply the AFK-ready triage label" — use the string
in the middle column.

A ticket with an open blocker carries none of these. The roles describe
readiness to be picked up, and a blocked ticket is not; its
`**Blocked by:** #<n>` line says why. It gains `ready-for-agent` when its
blockers close.

`bug`, `documentation`, `enhancement` and the rest of GitHub's defaults are
untouched by the skills and free to use alongside a triage label.
