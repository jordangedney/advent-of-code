#!/bin/bash

if [ "$1" == "build" ]; then
    find . | grep .hs | entr sh -c 'stack build; pkill advent2020-exe;'
    exit 0
fi
if [ "$1" == "run" ]; then
    while true; do stack exec advent2020-exe; sleep 10;  done
    exit 0
fi

echo "Must specify either 'build' or 'run'"
