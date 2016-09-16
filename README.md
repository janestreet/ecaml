Emacs plugin in OCaml
=====================

Ecaml is a library for writing Emacs plugins in OCaml. It uses Emacs
25 support for plugins to load native OCaml code into Emacs.

Building a plugin
-----------------

### Using Jane Street jenga rules

Simply add `(only_shared_object true)` to the `executables` stanza in
your jbuild. For instance:

```scheme
(executables
 ((names (my_plugin))
  (libraries (ecaml))
  (only_shared_object true)))
```

Then build `my_plugin.so` with jenga. This has been tested in opam
using [jane-build](https://github.com/janestreet/jane-build).

### Using other build systems

You need to use the `-output-complete-obj` option of ocamlopt to
produce a self-contained shared object file. For instance to build the
plugin in the `example/` directory:

```shell
$ ocamlfind ocamlopt -linkpkg -package ecaml -thread -output-complete-obj \
    -runtime-variant _pic -pp ppx-jane example_plugin.ml -o example_plugin.so
```

Links
-----

As of Emacs version 25, Emacs supports plugins.  Here is the API:

  http://git.savannah.gnu.org/cgit/emacs.git/tree/src/emacs-module.h?id=e18ee60b02d08b2f075903005798d3d6064dc013

Here's a tutorial:

  http://diobla.info/blog-archive/modules-tut.html

Here's an example plugin:

  http://git.savannah.gnu.org/cgit/emacs.git/tree/modules/mod-test/mod-test.c?id=e18ee60b02d08b2f075903005798d3d6064dc013

Licensing
---------

Note that Emacs modules must be GPL compatible, so you must make sure
that your work based on Ecaml is released under a GPL compatible
license.

Ecaml itself is released under the Apache 2.0 license which is GPL
compatible.
