
int foo()
{
	return 11;
}

int bar(int a)
{
	return a;
}

int hoge(int a, int b)
{
	return a + b;
}

// alloc_array_4(&p, 1, 2, 4, 8);
int alloc_array_4(int **p, int a, int b, int c, int d) {
    int array[4] = {a, b, c, d};
    *p = array;
    return **p;
}