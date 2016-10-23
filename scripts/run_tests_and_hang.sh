#!/bin/bash
set -e

stack exec course &

# wait for the child process to terminate
wait
