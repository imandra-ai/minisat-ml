#!/bin/sh

J="17"
TEST_OPTS="-j $J --progress"
DATE="`date +%FT%H:%M`"

mkdir -p snapshots
logitest run --meta=`git rev-parse HEAD` \
  --summary snapshots/bench-satrace06-$DATE.txt \
  --csv snapshots/bench-satrace06-$DATE.csv \
  -c benchs/conf.toml --profile=bench-satrace-06 $TEST_OPTS


