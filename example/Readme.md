# Readme

## To build this file, run

```
dune build hello_world.so
```

## To test, run

```   
emacs -Q -L _build/default --batch --eval "(progn (require 'hello_world) (say-hello \"emacs\"))"
```
