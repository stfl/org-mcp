# A digest guards a subtree; a value guards a field

A verb that acts on a whole node asserts with a digest of its subtree; a setter
that replaces one field asserts with the value that field held. The two
assertions have different radii because the two changes do. A digest is the only
assertion that can cover descendants the client never read, which is what a
delete needs; a field's own previous value is legible to a client, survives being
read and echoed, and says precisely what that write believed. We chose this over
one assertion for everything — a digest on field setters would make a caller
re-read a whole subtree to change a TODO keyword, and a field value on a delete
would guard the heading while saying nothing about the children going with it.
