#!/bin/bash
set -e

course slave  localhost 1236 &

# wait for the child process to terminate
wait
