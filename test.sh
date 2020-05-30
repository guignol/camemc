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
	dune exec camer "--" "$input" >${DIR}/tmp.s
	gcc -static -o ${DIR}/tmp ${DIR}/tmp.s
	${DIR}/tmp
	actual="$?"

	if [ "$actual" = "$expected" ]; then
		echo "[$actual] $input"
	else
		echo "[$expected expected, but got $actual] $input"
		echo errrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrror
		exit 1
	fi
}

# try 99 'int a; a = 0; int i; for (i = 0; i < 10; i = i + 1) a = a + 1; return 99;'
# try 10 'int a; a = 0; int i; for (i = 0; i < 10; i = i + 1) a = a + 1; return a;'
try 10 'a = 0; while (a < 10) a = a + 1; return a;'

try 37 'a = 1983; b = 2020; if ((b - a) == 37) return 37; else return 36;'
try 12 'a = 13; if (a == 0) return 3; else return 12;'
try 12 'if (0) return 3; else return 12;'
try 3 'if (10) return 3; return 12;'

try 25 'a_3 = 12; _loc = 3; return a_3 * _loc - 11;'
try 25 'a_3 = 12; _loc = 3; return a_3 * _loc - 11; 24;'

try 5 'aaa = 3; aaa + 2;'
try 5 'aaa = 3; b29 = 2; b29 + aaa;'
try 5 'O0 = 3; O0 = 2; O0 + 3;'

try 5 'a = 3; a + 2;'
try 3 'a = 3; a;'
# try 5 'a = 3; z = 2; return a + z;'

try 5 '1 + 2 + 3 + 4; 5;'
try 10 '1 + 2 + 3 + 4;'
try 10 '-1 + 2 + (14 - 3) - 1 * 2;'

#try 0 "return 0;"
#try 42 "return 42;"
try 42 "42;"
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
