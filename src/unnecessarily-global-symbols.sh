#!/bin/bash

# N.B. This check does not cover symbols in [Q.A] or [Q.K].
retval=0
declare -a files
while read -r symbol ; do
    files=( $(grep -l "Q.${symbol}\b" -- *.ml || true) )
    loc=$(grep -Hn " $symbol " q.ml | sed -rn 's/^([^:]+):([^:]+):.*$/File "\1", line \2, characters 0-0:/p')
    case "${#files[@]}" in
        0)
            retval=1
            echo -e "$loc\n[Q.$symbol] is not used in any files."
            ;;
        1)
            retval=1
            echo -e "$loc\n[Q.$symbol] is used in only one file: ${files[0]}"
            ;;
    esac
done < <(sed -rn 's/^let ([^ ]+) +=.*$/\1/p' q.ml)
exit "$retval"
