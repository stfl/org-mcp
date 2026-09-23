# A title is what Org's normalizer says it is

A title org-records-mcp returns is compared and resolved the way Org resolves a link
search, through `org-link--normalize-string`, so the title a read hands back is
exactly the search a later call can send. A test pins that dependency, because
the function is private and no public one does the same job. We chose this over
a local regexp stripping the pieces we knew about — TODO keyword, priority,
tags, statistics cookie — which is how the surface came to return a title its
own resolver could not find, and over byte-exact comparison, which refuses a
heading Org itself would match.
