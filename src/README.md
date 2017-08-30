June 2017

Most Emacs C functions take a value of type `emacs_env*`, the active
Emacs environment. To avoid threading the active environment through
lots of code, and in particular through OCaml code that needs to call
Emacs, the implementation instead maintains an invariant that any
OCaml code that can call into Emacs runs within an Emacs callback. We
then use global state, `active_env`, to hold the active Emacs
environment. This global state is set at the start of each callback
from Emacs to OCaml and restored at the end.

Emacs values as OCaml values
----------------------------

Emacs values are embedded in an OCaml custom block.  We store a
finalizer in the custom block that, when the OCaml value is finalized,
records the Emacs value to be freed at the next callback from Emacs to
OCaml.
