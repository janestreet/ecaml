August 2016

Most Emacs C functions functions take a value of type `emacs_env*`,
the active Emacs environment.  To avoid threading the active
environment through lots of code, an in particular through OCaml code
that needs to call Emacs, the implementation instead maintains an
invariant that any OCaml code that can call into Emacs runs within an
Emacs callback.  We then use global state, [active_env], to hold the
active Emacs environment.  This global state is set at the start of
each callback from Emacs to OCaml and restored at the end.

Locking
-------
All callbacks from Emacs to OCaml run holding the Async lock.  This is
enforced by `Function.make_function_internal`, which registers the
`dispatch_function` callback, the only way to call from Emacs to OCaml.

The only way an Async cycle runs is via the callback mechanism.
Hence, by the same reasoning, Async jobs are guaranteed to have a
valid active environment.

Emacs values as OCaml values
----------------------------
Emacs values are embedded in an OCaml custom block.  We attach a
finalizer to the custom block that, when the OCaml value is finalized,
frees the Emacs values.  We use an Async finalizer to ensure that the
freeing code runs as an Async job, and hence has an active Emacs
environment.
