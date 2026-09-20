# The whole-node verbs are guarded from birth

Delete, archive and refile were added in one change, each registered with its
assertion already required, so no commit in the history contains an unguarded
one. Each takes the subtree digest and no new-value counterpart, because the
verb is the change. We chose this over adding the verbs first and the guard
after, which a tracker cannot hold across two tickets: there is always a window
between them, and the window is exactly where an agent acting on a stale read
destroys a subtree nobody can recover.
