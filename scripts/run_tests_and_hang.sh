#!/bin/bash
set -e

stack exec -- doctest src/Main.hs &

# wait for the child process to terminate
wait
