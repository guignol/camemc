#!/bin/bash

try() {
	assert "$1" "int main() { $2 }"
}

DIR="_compile"

assert() {
	expected="$1"
	input="$2"

	mkdir -p ${DIR}
	dune exec camer "--" "$input" >${DIR}/tmp.s
	if [ "$?" = "0" ]; then
		gcc -static -o ${DIR}/tmp ${DIR}/tmp.s ${DIR}/foo.s
		# DIR="_compile"; gcc -static -o ${DIR}/tmp ${DIR}/tmp.s ${DIR}/foo.s && ${DIR}/tmp; echo $?
		${DIR}/tmp
		actual="$?"
	else
		# actual="failed"
		actual="$?"
	fi

	if [ "$actual" = "$expected" ]; then
		echo "[$actual] $input"
	else
		echo "[$expected expected, but got $actual]"
		echo "$input"
		echo errrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrror
		exit 1
	fi
}

run_test() {
	filename="$1"
	mkdir -p ${DIR}

	# コンパイル
	dune exec camer "--" "-f" "$filename" >${DIR}/tmp.s
	if [ "$?" = 1 ]; then
		echo "**********************"
		echo " error on compile"
		echo "**********************"
		exit 1
	fi

	# リンク
	gcc -static -o ${DIR}/tmp ${DIR}/tmp.s
	# gcc -static -o ${DIR}/tmp ${DIR}/tmp.s ${DIR}/foo.s
	if [ "$?" = 1 ]; then
		echo "**********************"
		echo " error on link"
		echo "**********************"
		exit 1
	fi

	# 実行
	${DIR}/tmp
	# DIR="_compile"; gcc -static -o ${DIR}/tmp ${DIR}/tmp.s && ${DIR}/tmp; echo $?
}

# テストコード
TEST_CODE="$(pwd)/test.c"
run_test $TEST_CODE
# exit $?
result=$?

if [ $result != 0 ]; then
	echo "**********************"
	echo " error on test: $result"
	echo "**********************"
	exit 1
fi

#################################################

try 4 'int x; return sizeof(x);'
try 8 'int *y; return sizeof(y);'
try 4 'int x; return sizeof(x + 3);'
try 8 'int *y; return sizeof(y + 3);'
try 4 'int *y; return sizeof(*y);'
# sizeofに渡す式は何でもよい
try 4 'return sizeof(1);'
# sizeofの結果は現在int型なのでsizeof(int)と同じ
try 4 'return sizeof(sizeof(1));'

try 10 'int a; a = 0; int b; b = 1; int i; for (i = 0; i < 100; i = i + 1) { a = a + 1; b = a + 1; if (a == 10) return a; } return b - 1;'
try 11 'int a; a = 3; int b; b = 2; int c; c = 6; return a + b + c; '
try 11 'int a; int b; int c; { a = 3; b = 2; c = 6; return a + b + c; }'

# try 13 'printf("moji: %i", 13); return 13;'
# try 13 'int a; a = 13;  printf("moji: %i", 13); return a;'

try 12 'int b; b = 1; int a; a = foo() + b; return a;'
try 11 'return foo();'
try 12 'int a; return a = foo() + 1;'
try 12 'int a; return a = bar(11) + 1;'
try 12 'int a; int b; b = 11; return a = bar(b) + 1;'
try 12 'int a; return a = hoge(11, 1);'
try 12 'int a; int b; a = 1; b = 11; return hoge(b, a);'
try 12 'return hoge(11, 1);'

try 3 '{ int aa; int b; aa = 3; { b = 2; } return aa; }'
try 10 'int a; int b; int i; a = 0; b = 1; for (i = 0; i < 10; i = i + 1) { a = a + 1; b = a + 1; } return b - 1;'
try 10 'int a; int b; int i; a = 0; b = 1; for (i = 0; i < 10; i = i + 1) { a = a + 1; b = a + 1; } return a;'
try 10 'int value; int i; value = 0; for (i = 0; i < 10; i = i + 1) { value = value + 1; } return value;'

try 3 '{ int a; return a = 3; }'
try 2 '{ int a; int b; a = 3; return b = 2; }'
try 6 '{ int a; int b; int c; a = 3; b = 2; return c = 6; }'
try 6 '{ int a; int b; int c; a = 3; b = 2; return c = 6; return a + b + c; }'

try 99 'int a; a = 0; int i; for (i = 0; i < 10; i = i + 1) a = a + 1; return 99;'
try 10 'int a; a = 0; int i; for (i = 0; i < 10; i = i + 1) a = a + 1; return a;'
try 10 'int a; a = 0; while (a < 10) a = a + 1; return a;'

try 37 'int a; int b; a = 1983; b = 2020; if ((b - a) == 37) return 37; else return 36;'
try 12 'int a; a = 13; if (a == 0) return 3; else return 12;'
try 12 'if (0) return 3; else return 12;'

try 25 'int a_3; int _loc; a_3 = 12; _loc = 3; return a_3 * _loc - 11;'
try 25 'int a_3; int _loc; a_3 = 12; _loc = 3; return a_3 * _loc - 11; 24;'

try 5 'int aaa; aaa = 3; aaa + 2;'
try 5 'int b29; int aaa; aaa = 3; b29 = 2; b29 + aaa;'
try 5 'int O0; O0 = 3; O0 = 2; O0 + 3;'

try 5 'int a; a = 3; a + 2;'
try 3 'int a; a = 3; a;'
try 3 'return 3;'
try 5 'int a; int z; a = 3; z = 2; return a + z;'

try 5 '1 + 2 + 3 + 4; return 5;'
try 10 'return 1 + 2 + 3 + 4;'
try 10 'return -1 + 2 + (14 - 3) - 1 * 2;'

# try 0 '1 + aaa'
try 0 "return 0;"
try 42 "return 42;"
try 42 "return 40+2;"
try 0 'return 100-100;'
try 10 'return 100-100+10;'
try 111 'return 100 + 10 +1;'
try 100 'return 100 * 10 / 10;'
try 101 'return 100 + 10 / 10;'
try 0 'return 10 * -1 + 10;'
try 90 'return 100 + -1 * 10;'

try 0 'return 0 == 1;'
try 1 'return 42 == 42;'
try 1 'return 0 != 1;'
try 0 'return 42 != 42;'
try 1 'return 42 != 42 + 1;'
try 1 'return 2 + 42 != 42;'
try 1 'return (2 + 42) != 42;'

try 1 'return 0 < 1;'
try 0 'return 1 < 1;'
try 0 'return 2 < 1;'
try 1 'return 0 <= 1;'
try 1 'return 1 <= 1;'
try 0 'return 2 <= 1;'

try 1 'return 1 > 0;'
try 0 'return 1 > 1;'
try 0 'return 1 > 2;'
try 1 'return 1 >= 0;'
try 1 'return 1 >= 1;'
try 0 'return 1 >= 2;'

echo OK
