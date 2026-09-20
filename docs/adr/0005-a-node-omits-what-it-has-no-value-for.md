# A node omits what it has no value for, and the request disambiguates

A node carries the fields the call asked for, minus any it has no value for, and
never sends `null`. Once a call names the fields it wants, an absent key means
both *not requested* and *requested but empty*, and the caller tells the two
apart because it holds the list it sent. We chose this over emitting `null` for
a field that was asked for and is empty, which spends a key per empty field per
match on a long result, and reverses the never-null invariant on every node on
every endpoint, to disambiguate something the caller can already disambiguate.
