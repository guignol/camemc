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
		# gcc -static -o ${DIR}/tmp ${DIR}/tmp.s
		${DIR}/tmp
		actual="$?"
	else
		actual="failed"
	fi

	if [ "$actual" = "$expected" ]; then
		echo "[$actual] $input"
	else
		echo "[$expected expected, but got $actual] $input"
		echo errrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrror
		exit 1
	fi
}

gcc -o ${DIR}/foo.s -S foo.c

# assert 4 "$(
#   cat <<END
# int main() {
#   int x;
#   int *y;
#   x = 3;
#   y = &x;
#   *y = 4;
#   return x;
# }
# END
# )"

assert 3 "$(
  cat <<END
int main() {
	int x;
	x = 3;
	return *(&x);
}
END
)"

try 10 'int a; a = 0; int b; b = 1; int i; for (i = 0; i < 100; i = i + 1) { a = a + 1; b = a + 1; if (a == 10) return a; } return b - 1;'
try 11 'int a; a = 3; int b; b = 2; int c; c = 6; return a + b + c; '
try 11 'int a; int b; int c; { a = 3; b = 2; c = 6; return a + b + c; }'

assert 1 "$(
	cat <<END
int fibonacci(int n) {
	if (n == 0)	{
		return 1;
	} else if (n == 1) {
		return 1;
	}
	return fibonacci(n - 1) + fibonacci(n - 2);
}

int main() {
  int i;
	for (i = 0; i < 10; i = i + 1) 	{
		hoge(i, fibonacci(i));
	}
	return 1;
}
END
)"

assert 13 'int add(int a, int b) { hoge(a, b); return a + b; } int main() { return add(1, 12); }'
assert 13 'int add(int a, int b) { return a + b; } int main() { return add(1, 12); }'
assert 13 'int salut() { int a; int b; a = 1; b = 12; return 13; } int main() { return salut(); } '
assert 13 'int salut() { return 13; } int main() { return salut(); } '
assert 13 'int main() { return bar(13); }'

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
