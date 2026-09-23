# A scope override lasts one call

org-records-mcp reaches its allowed files by default. A call reaches any other Org file
by naming it, either as the file of a link or as an entry in a `files`
parameter, and a server-side override policy decides whether that is refused,
permitted for any Org file, or permitted only under listed roots. Nothing
carries over to the next call, and GTD queries never take an override. We chose
this over a tool that sets a session-wide scope, which lets one call's scope
leak into every later call, and over a `file` parameter on every tool, which
would repeat what the link already names.
