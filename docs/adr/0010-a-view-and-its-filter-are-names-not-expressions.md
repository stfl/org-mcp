# A view and its filter are names, not expressions

A view and a filter are each a name from a list the workflow configures, and an
unknown name is refused with the valid names in the message. We chose this over
a query expression supplied beside the view: a closed vocabulary can be refused
usefully and an open one cannot, which is the answer to an agent inventing
predicates that do not exist, and `org-query` already is the escape hatch for a
caller that means to write a query. We also chose it over `org-agenda-custom-commands`
as the registry, which stores compositions rather than questions, so one key
there cannot name one query.
