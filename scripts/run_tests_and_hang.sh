#!/bin/bash
set -e

(
  echo ':set stop :cmd return ":list\n:step"'
  echo ':step main'
) | ghci src/Main.hs &

# wait for the child process to terminate
wait
