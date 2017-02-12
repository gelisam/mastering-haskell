#!/bin/bash
set -e

course master localhost 1237 &

# wait for the child process to terminate
wait
