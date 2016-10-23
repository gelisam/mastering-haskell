#!/bin/bash
set -e

course slave  localhost 1235 &

# wait for the child process to terminate
wait
