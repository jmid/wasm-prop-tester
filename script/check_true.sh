#!/bin/bash

if [ "$#" -lt 1 ] || ! [ -f "$1" ]; then
  echo "Usage: $0 FILE" >&2
  exit 1
fi

if [[ $(< "$1") == "true" ]]
then
#    echo "file is true"
    exit 0
else
#    echo "file is not true"
    exit 1
fi
