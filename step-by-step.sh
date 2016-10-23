#!/bin/bash
set -e

clear
for x in `seq 10`; do
  echo
done
echo "** START **"

(
  echo ':set stop :cmd return ":list\n:step"'
  echo ':step main'
) | ghci src/Main.hs &
PID="$!"
sleep 5
kill "$PID"
echo "** KILLED **"
