#!/bin/bash
set -e

clear
stack install

time ./scripts/run_tests_and_hang.sh "$@" &
PID="$!"
trap "./scripts/kill_recursively.sh $PID" EXIT


# wait for the child process to terminate
wait
