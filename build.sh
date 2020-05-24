#!/bin/bash

try() {
#	assert "$1" "int main() { return $2; }"
	assert "$1" "$2"
}

assert() {
	expected="$1"
	input="$2"

  DIR="_compile"
  mkdir -p ${DIR}
  dune exec camer "--" "$input" > ${DIR}/tmp.s
  gcc -static -o ${DIR}/tmp ${DIR}/tmp.s
  ${DIR}/tmp
	actual="$?"

	if [ "$actual" = "$expected" ]; then
		echo "[$actual] $input"
	else
		echo "[$expected expected, but got $actual] $input"
		exit 1
	fi
}

try 10 '-1 + 2 + (14 - 3) - 1 * 2'

#try 0 "return 0;"
#try "42" "return 42;"
try 42 "40+2;"
try 0 '100-100;'
try 10 '100-100+10;'
try 111 '100 + 10 +1;'
try 100 '100 * 10 / 10;'
try 101 '100 + 10 / 10;'
try 0 '10 * -1 + 10;'
try 90 '100 + -1 * 10;'

try 0 '0 == 1;'
try 1 '42 == 42;'
try 1 '0 != 1;'
try 0 '42 != 42;'
try 1 '42 != 42 + 1;'
try 1 '2 + 42 != 42;'
try 1 '(2 + 42) != 42;'

try 1 '0 < 1;'
try 0 '1 < 1;'
try 0 '2 < 1;'
try 1 '0 <= 1;'
try 1 '1 <= 1;'
try 0 '2 <= 1;'

try 1 '1 > 0;'
try 0 '1 > 1;'
try 0 '1 > 2;'
try 1 '1 >= 0;'
try 1 '1 >= 1;'
try 0 '1 >= 2;'

echo OK