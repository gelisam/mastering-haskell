#!/bin/bash
set -e

stack install fswatcher

clear
./run_slave3.sh "$@" &
PID="$!"
trap "./scripts/kill_recursively.sh $PID" EXIT

fswatcher --path ~/.local/bin echo "restart" | while read X; do
  ./scripts/kill_recursively.sh "$PID"
  
  clear
  ./run_slave3.sh "$@" &
  PID="$!"
  trap "./scripts/kill_recursively.sh $PID" EXIT
done
