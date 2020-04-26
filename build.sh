#!/bin/bash

#dune exec camer one
DIR="_compile"
mkdir -p ${DIR}
dune exec camer 1 + 2 + 4 > ${DIR}/tmp.s
gcc -static -o ${DIR}/tmp ${DIR}/tmp.s
${DIR}/tmp
echo "$?"