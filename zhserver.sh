#!/usr/bin/sh

# Find absolute path to directory where this script is located.
HERE=$(dirname $(readlink -f "$0"))

export LD_LIBRARY_PATH=$HERE/lib 

cd $HERE

echo "Dir = "$(pwd)

./zhserver.bin $@

