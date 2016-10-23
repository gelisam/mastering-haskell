#!/bin/bash
set -e

course slave  localhost 1234 &

# wait for the child process to terminate
wait
