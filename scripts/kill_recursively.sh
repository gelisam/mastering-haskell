#!/bin/bash
set -e

function kill_children {
  local PARENT_PID="$1"
  local CHILD_PID
  for CHILD_PID in `pgrep -P "$PARENT_PID"`; do
    kill_entire_family "$CHILD_PID"
  done
  echo kill_children $PARENT_PID done
}

function kill_entire_family {
  local PARENT_PID="$1"
  kill_children "$PARENT_PID"
  kill "$PARENT_PID" 2> /dev/null || true
  echo kill_entire_family $PARENT_PID done
}


kill_entire_family "$1"
echo done
