#!/usr/bin/env bash

perf record --call-graph=dwarf $@

perf script \
  | stackcollapse-perf --kernel \
  | sed 's/caml//g' \
  | flamegraph > perf.svg

