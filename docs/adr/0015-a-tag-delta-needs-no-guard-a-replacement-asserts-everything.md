# A tag delta needs no guard; a tag replacement asserts everything

Adding and removing tags name the tags they change and take no precondition,
while replacing the set requires the entire prior set of the heading's own tags.
The asymmetry follows from what each call destroys: a delta touches only the
tags it lists, so a tag the client never read survives it and there is nothing
unseen to assert; a replacement takes away every tag it does not list, so its
blast radius is the whole set and the assertion has to cover the whole set. What
is asserted is the heading's own tags and never the set in effect on it, because
the write reaches local tags only — asserting inherited values would refuse a
call because an ancestor was edited, and would assert values the call could not
have changed. We chose this over one rule for all three, in either direction: a
precondition on the deltas would make two clients adding different tags collide
over a set neither is replacing, and dropping it from the replacement would let
a client destroy tags it had never seen.
