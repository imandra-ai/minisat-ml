#!/bin/sh

FILE=$1
shift

if grep 'cnf.gz' <<< "$FILE" > /dev/null ; then
  exec gunzip -c "$FILE" | z3 -in -dimacs $@
else
  exec z3 "$FILE" $@
fi
