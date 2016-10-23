#!/bin/bash
set -e

unsafePerformIO &

# wait for the child process to terminate
wait
