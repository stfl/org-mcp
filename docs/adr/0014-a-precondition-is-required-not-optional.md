# A precondition is required, not optional

A write that replaces a value takes its `before` as a required parameter, so the
published schema carries the obligation and a call that omits it is refused
before it reaches the file. Required is also what lets `""` mean something: a
required parameter never meets the blank-parameter rule, so an empty `before`
asserts that the field held nothing — an assertion an optional parameter cannot
express, because there it is indistinguishable from a call that sent nothing at
all. We chose this over an optional parameter, which is an off guard when the
caller is a model nobody told to fill it and which silently accepts a client
still sending a parameter name the tool has since renamed, and over a defcustom
enforcing it, which would make the published schema differ between
installations so that no client could discover which regime it is talking to.
