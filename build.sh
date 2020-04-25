#!/bin/bash

#dune exec camer one
DIR="_compile"
mkdir -p ${DIR}
dune exec camer one two three four > ${DIR}/tmp.s
gcc -static -o ${DIR}/tmp ${DIR}/tmp.s
${DIR}/tmp
echo "$?"