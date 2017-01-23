#!/bin/bash
set -e

stack install doctest
stack install fswatcher

clear
./run_tests.sh "$@" &
PID="$!"
trap "./scripts/kill_recursively.sh $PID" EXIT

fswatcher --path src/Main.hs echo "restart" | while read X; do
  ./scripts/kill_recursively.sh "$PID"
  
  clear
  ./run_tests.sh "$@" &
  PID="$!"
  trap "./scripts/kill_recursively.sh $PID" EXIT
done
