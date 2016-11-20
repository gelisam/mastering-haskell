#!/bin/bash
set -e

(
  echo "line 1"
  echo "line 2"
  echo "line 3"
) | stack exec course &

# wait for the child process to terminate
wait
