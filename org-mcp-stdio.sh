#!/usr/bin/env bash
# org-mcp-stdio.sh — convenience wrapper around emacs-mcp-stdio.sh that
# injects the org-mcp server-id and init/stop function names so callers
# (org-cli, MCP client configs, ad-hoc scripts) don't have to repeat the
# boilerplate.
#
# Resolution order for the underlying emacs-mcp-stdio.sh:
#   1. <script_dir>/emacs-mcp-stdio.sh — co-located install (Nix layout
#      installs both shims into ~/.config/emacs/, so this is the fast path).
#   2. emacs-mcp-stdio.sh in $PATH.
#
# Extra arguments are forwarded after the org-mcp defaults, so callers can
# still pass --socket=..., a custom --server-id=... override, etc.

set -eu -o pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
shim="$script_dir/emacs-mcp-stdio.sh"
if [ ! -x "$shim" ]; then
	if ! shim=$(command -v emacs-mcp-stdio.sh 2>/dev/null); then
		echo "org-mcp-stdio.sh: cannot locate emacs-mcp-stdio.sh in '$script_dir' or \$PATH" >&2
		exit 1
	fi
fi

exec "$shim" \
	--server-id=org-mcp \
	--init-function=org-mcp-enable \
	--stop-function=org-mcp-disable \
	"$@"
