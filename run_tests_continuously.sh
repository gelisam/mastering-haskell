#!/bin/bash
set -e

stack install fswatcher

clear
./run_tests.sh "$@" &
PID="$!"
trap "./scripts/kill_recursively.sh $PID" EXIT

fswatcher --path src echo "restart" | while read X; do
  ./scripts/kill_recursively.sh "$PID"
  
  ./run_tests.sh "$@" &
  PID="$!"
  trap "./scripts/kill_recursively.sh $PID" EXIT
done
