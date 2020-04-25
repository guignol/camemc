#!/bin/bash

#dune exec camer one
dune exec camer one two three > tmp.s
gcc -static -o tmp tmp.s
./tmp
echo "$?"