#!/bin/bash

#dune exec camer one
DIR="_compile"
mkdir ${DIR}
dune exec camer one two three > ${DIR}/tmp.s
gcc -static -o ${DIR}/tmp ${DIR}/tmp.s
${DIR}/tmp
echo "$?"