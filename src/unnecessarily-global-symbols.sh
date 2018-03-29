#!/bin/bash

# N.B. This check does not cover symbols in [Q.A] or [Q.K].

declare -a files
for symbol in $(sed -rn 's/^let ([^ ]+) +=.*$/\1/p' q.ml) ; do
    files=( $(grep -l "Q.${symbol}\b" -- *.ml || true) )
    case "${#files[@]}" in
        0)
            echo -e "Q.$symbol is not used in any files."
            ;;
        1)
            echo -e "${files[0]} is the only call site of [Q.$symbol]"
            ;;
    esac
done | sort
