
// int printf();

// void exit();

// void exit(int status);

// void exit(int status);

// void exit();

// int hoge(int, int);

// int hoge(int x, int y);

int hoge(int x, int y) {
    printf("--------------hoge: %i, %i\n", x, y);
    return x + y;
}

// int bar();

int bar(int v) {
    printf("--------------bar: %i\n", v);
    return v;
}

// int foo(void);

// int foo();

// int foo(void);

int foo() {
    return bar(11);
}

// int fprintf();

// extern void *stderr; // 使われているかどうかは 2>/dev/null で確認
int *stderr;

int count;

// void assert(char *, int expected, int);

int assert(char *name, int expected, int actual) {
    count = count + 1;
    if (expected != actual) {
        fprintf(stderr, "%d: \"%s\"\n", count, name);
        fprintf(stderr, "=> %d expected but got %d\n", expected, actual);
        exit(1);
    } else {
        printf("%d: \"%s\"\n", count, name);
        return 0;
    }
}

/////////////////////////////////////////////////

// char *moji_global = "moji_global_before\n";

// void init_global() {
//     moji_global = "moji_global_after\n";
// }

// // 4
// int string_global() {
//     printf(moji_global);
//     init_global();
//     printf(moji_global);
//     return 4;
// }

// 9
int string_literal_japanese() {
    printf("日本語ですね\n");
    return 9;
}

// 3
int string_literal_ascii_1() {
    char *moji;
    moji = "moji\ndesu\nne\n";
    printf(moji);
    return 3;
}


/////////////////////////////////////////////////

// 3
int char_calculate_array_1() {
    char x[3];
    x[0] = -1;
    x[1] = 2;
    int y;
    y = 4;
    return x[0] + y;
}

// 2
// int char_calculate_array_2() {
//     char x['b'] = {'a', 'b', 'c'};
//     'a'[x] = 'd';
//     return 'f' - x['a'];
// }

/////////////////////////////////////////////////

// int *a = 55; // 暗黙のキャストだけどエラーにならないパターン
int *a;
int *b;
int aa;

// 5
int global_variable_1() {
    int a; // 型の異なるローカル変数
    a = 3;
    b = &a;
    *b = 2 + a;
    return a;
}

// 9
int global_variable_2() {
    aa = 1;
    b = &aa;
    *b = 8 + aa;
    return aa;
}

// 3
int init() {
    aa = 6;
}

int global_variable_3() {
    int b;
    init();
    b = aa;
    int aa;
    aa = 3;
    return b - aa;
}

// 3
int global_variable_4() {
    aa = 3;
    int b;
    b = 6;
    return b - aa;
}

char global_c_1;
char global_c_2;

// 2
// int global_variable_5() {
//     global_c_1 = 'a';
//     global_c_2 = 'c';
//     return global_c_2 - global_c_1;
// }

/////////////////////////////////////////////////

// 12 
int array_and_pointer_6() {
    int a[2][3];
    int b[5];
    b[4] = 1;
    (a[0])[b[4]] = 12;
    return (a[0])[1];
}

// 12 
int array_and_pointer_7() {
    int a[2][3][4];
    int one;
    one = 1;
    a[0][one][2] = 7;
    one[a][2][3] = 5;
    return a[0][one][2] + a[one][2][3];
}

// 12 
int array_and_pointer_8() {
    int a[2][3];
    a[0][0] = 11;
    a[0][1] = 12;
    a[0][2] = 13;
    a[1][0] = 21;
    a[1][1] = 22;
    a[1][2] = 23;
    return a[0][1];
}

// 8 
int array_and_pointer_9() {
    int a[2];
    int b;
    b = sizeof(a);
    return b;
}

/////////////////////////////////////////////////

// 24
int array_and_pointer_10() {
    int a[2][3];
    int b;
    b = sizeof(a);
    return b;
}

// 3
int array_and_pointer_11() {
    int a[2];
    int *b;
    int c;
    c = 3;
    b = &c;
    a[0] = *b;
    b = &a;
    // &a = &c; // TODO これはできない
    return *b;
}

// 3
int array_and_pointer_12() {
    int a;
    int *b;
    int **c;
    int ***d;
    a = 1;
    b = &a;
    c = &b;
    d = &c;
    a = ***d + 2;
    return a;
}

// 3
int array_and_pointer_13() {
    // ポインタへのポインタの配列
    int **p[4];
    int a;
    int *b;
    int **c;
    a = 1;
    b = &a;
    c = &b;
    p[0] = c;
    a = *(*(p[0])) + 2;
    return **p[0];  // 3
}

// 3
int array_and_pointer_14() {
    int a[2];
    0[a] = 1;
    1[a] = 2;
    int *p;
    p = a;
    int maji;
    maji = 0[p]; // ポインタにも[]使える
    return maji + 1[a];  // 3
}

// 3
// int array_and_pointer_15() {
//     int a[5] = {1, 2, 3, 4, 5};
//     int maji;
//     maji = a[1 + 3];
//     return maji - a[1];  // 3
// }

// 3
int array_and_pointer_16() {
    int a[2];
    a[0] = 1;
    a[1] = 2;
    int *p;
    p = a;
    int maji;
    maji = p[0]; // ポインタにも[]使える
    return maji + a[1];  // 3
}

// 3
int array_and_pointer_17() {
    int *a[2];
    int b;
    int *c;
    int *d;
    b = 3;
    c = &b;
    *a = c;
    d = *a;
    return *d;
}

// 3
int array_and_pointer_18() {
    int a[2];
    *a = 3;
    return *a;
}

// 3
int array_and_pointer_19() {
    int a[2];
    *a = 1;
    *(a + 1) = 2;
    int *p;
    p = a;
    return *p + *(p + 1);  // 3
}

/////////////////////////////////////////////////

// 4
int sizeof_1() {
    int x;
    return sizeof(x);
}

// 8
int sizeof_2() {
    int *y;
    return sizeof(y);
}

// 4
int sizeof_3() {
    int x;
    return sizeof(x + 3);
}

// 8
int sizeof_4() {
    int *y;
    return sizeof(y + 3);
}

// 4
int sizeof_5() {
    int *y;
    return sizeof(*y);
}

// 4
int sizeof_6() {
    // sizeofに渡す式は何でもよい
    return sizeof(1);
}

// 4
int sizeof_7() {
    // このコンパイラでは
    // sizeofの結果はint型なのでsizeof(int)と同じ
    // 実際のCでは
    // sizeofの結果はsize_t型（8バイト）になる
    return sizeof(sizeof(1));
}

// 引数なしの宣言と、非ポインタのcharを含む定義および宣言は互換なし
//char chaa();
//char chaa(char c);

char chaa(char c) {
    return c + 1;
}

// 1
int sizeof_8() {
    char c;
    c = sizeof(c);
    int i;
    i = chaa(c);
    return i - c;
}

// 136
// int sizeof_9() {
//     // 4
//     int i = sizeof 1;
//     // 4
//     int a = sizeof i;
//     // 8 (pointer)
//     int b = sizeof(int (*)[4][5][6]);
//     // 120 = 4 * 5 * 6 * 1 (char)
//     int c = sizeof(char[4][5][6]);
//     return i + a + b + c;
// }

/////////////////////////////////////////////////

// 3
// int pointer_and_calculate_1() {
// //    int array_4[4] = {0, 1, 2, 3, 4};
//     int array_4[4] = {0, 1, 2, 3,};

//     int *p;
//     int *q;
//     p = array_4;

//     // foo(*p);
//     q = p + 3;
//     return q - p;
// }

// 2
// int pointer_and_calculate_2() {
//     int array_4[5] = {0, 1, 2, 3,};
//     array_4[0] = 0;
//     array_4[1] = 1;
//     array_4[2] = 2;
//     array_4[3] = 3;

//     int *p;
//     p = array_4;

//     bar(*p);
//     p = p + 1;
//     p = 1 + p;
//     return *p;
// }

// 3
// int pointer_and_calculate_3() {
//     int x;
//     int i;
//     int y;
//     x = 3;
//     y = 5;
//     // ポインタ演算では8bytesずつ動くので、
//     // 4bytesずつ並んでいるintの2つ分スタックポインタを動かす
//     i = *(&y + 2); // ポインタ演算？（関数フレームの実装に依存）
//     return i;
// }

// 4
int pointer_and_calculate_4() {
    int x;
    int *y;
    x = 3;
    y = &x;
    *y = 4;
    return x;
}

// 3
int pointer_and_calculate_5() {
    int x;
    x = 3;
    return *(&x);
}

/////////////////////////////////////////////////

int fibonacci(int n) {
    if (n == 0) {
        return 1;
    } else if (n == 1) {
        return 1;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

int add_1(int a, int b) {
    hoge(a, b);
    return a + b;
}

int add_2(int a, int b) { return a + b; }

int salut_1() {
    int a;
    int b;
    a = 1;
    b = 12;
    return 13;
}

int salut_2() { return 13; }

// 13
int function_1() { return add_1(1, 12); }

// 13
int function_2() { return add_2(1, 12); }

// 13
int function_3() { return salut_1(); }

// 13
int function_4() { return salut_2(); }

// 13
int function_5() { return bar(13); }

// 37
int function_6() {
    int a;
    int b;
    a = 1983;
    b = 2020;
    if ((b - a) == 37) return 37; else return 36;
}

// 12
int function_7() {
    int a;
    a = 13;
    if (a == 0) return 3; else return 12;
}

// 12
int function_8() { if (0) return 3; else return 12; }

// 1
int function_9() {
    int i;
    for (i = 0; i < 10; i = i + 1) {
        hoge(i, fibonacci(i));
    }
    return 1;
}

/////////////////////////////////////////////////

// 13
int function_10() {
    printf("moji: %i\n", 13);
    return 13;
}

// 13
int function_11() {
    int a;
    a = 13;
    printf("moji: %i\n", 13);
    return a;
}

// 12
int function_12() {
    int b;
    b = 1;
    int a;
    a = foo() + b;
    return a;
}

// 11
int function_13() {
    return foo();
}

// 12
int function_14() {
    int a;
    return a = foo() + 1;
}

// 12
int function_15() {
    int a;
    return a = bar(11) + 1;
}

// 12
int function_16() {
    int a;
    int b;
    b = 11;
    return a = bar(b) + 1;
}

// 12
int function_17() {
    int a;
    return a = hoge(11, 1);
}

// 12
int function_18() {
    int a;
    int b;
    a = 1;
    b = 11;
    return hoge(b, a);
}

// 12
int function_19() {
    return hoge(11, 1);
}

/////////////////////////////////////////////////

int make_arg() {
    return 1;
}

int two_args(int a, int b) {
    return a + b;
}

// 2
int function_20() {
    return two_args(make_arg(), make_arg());
}

/////////////////////////////////////////////////

// 10
int block_1() {
    int a;
    a = 0;
    int b;
    b = 1;
    int i;
    for (i = 0; i < 100; i = i + 1) {
        a = a + 1;
        b = a + 1;
        if (a == 10) return a;
    }
    return b - 1;
}

// 11
int block_2() {
    int a;
    a = 3;
    int b;
    b = 2;
    int c;
    c = 6;
    return a + b + c;
}

// 11
int block_3() {
    int a;
    int b;
    int c;
    {
        a = 3;
        b = 2;
        c = 6;
        return a + b + c;
    }
}

// 3
int block_4() {
    {
        int aa;
        int b;
        aa = 3;
        printf("%d", aa);
        { b = 2; }
        return aa;
    }
}

// 10
int block_5() {
    int a;
    int b;
    int i;
    a = 0;
    b = 1;
    for (i = 0; i < 10; i = i + 1) {
        a = a + 1;
        b = a + 1;
    }
    return b - 1;
}

// 10
int block_6() {
    int a;
    int b;
    int i;
    a = 0;
    b = 1;
    for (i = 0; i < 10; i = i + 1) {
        a = a + 1;
        b = a + 1;
    }
    return a;
}

// 10
int block_7() {
    int value;
    int i;
    value = 0;
    for (i = 0; i < 10; i = i + 1) { value = value + 1; }
    return value;
}

// 6
int block_8() {
    {
        int a;
        int b;
        int c;
        a = 3;
        b = 2;
        return c = 6;
    }
}

// 6
int block_9() {
    {
        int a;
        int b;
        int c;
        a = 3;
        b = 2;
        return c = 6;
        return a + b + c;
    }
}

/////////////////////////////////////////////////

// int assert_others(void);

int assert_others() {
    assert("", 99, ({
        int a;
        a = 0;
        int i;
        for (i = 0; i < 10; i = i + 1) a = a + 1;
        99;
    }));
    // assert("", 10, ({
    //     int a;
    //     a = 0;
    //     int i;
    //     for (i = 0; i < 10; i = i + 1) a = a + 1;
    //     a;
    // }));
    // assert("", 10, ({
    //     int a;
    //     a = 0;
    //     while (a < 10) a = a + 1;
    //     a;
    // }));

    // assert("", 25, ({
    //     int a_3;
    //     int _loc;
    //     a_3 = 12;
    //     _loc = 3;
    //     a_3 * _loc - 11;
    // }));
    // assert("", 5, ({
    //     int aaa;
    //     aaa = 3;
    //     aaa + 2;
    // }));
    // assert("", 5, ({
    //     int b29;
    //     int aaa;
    //     aaa = 3;
    //     b29 = 2;
    //     b29 + aaa;
    // }));
    // assert("", 5, ({
    //     int O0;
    //     O0 = 3;
    //     O0 = 2;
    //     O0 + 3;
    // }));

    // assert("", 5, ({
    //     int a;
    //     a = 3;
    //     a + 2;
    // }));
    // assert("", 3, ({
    //     int a;
    //     a = 3;
    //     a;
    // }));
    // assert("", 5, ({
    //     int a;
    //     int z;
    //     a = 3;
    //     z = 2;
    //     a + z;
    // }));

    assert("", 0, ({ 0; }));
    assert("", 42, ({ 42; }));
    assert("", 42, ({ 40 + 2; }));
    assert("", 0, ({ 100 - 100; }));
    assert("", 10, ({ 100 - 100 + 10; }));
    assert("", 111, ({ 100 + 10 + 1; }));
    assert("", 100, ({ 100 * 10 / 10; }));
    assert("", 101, ({ 100 + 10 / 10; }));
    assert("", 0, ({ 10 * -1 + 10; }));
    assert("", 90, ({ 100 + -1 * 10; }));

    assert("", 0, ({ 0 == 1; }));
    assert("", 1, ({ 42 == 42; }));
    assert("", 1, ({ 0 != 1; }));
    assert("", 0, ({ 42 != 42; }));
    assert("", 1, ({ 42 != 42 + 1; }));
    assert("", 1, ({ 2 + 42 != 42; }));
    assert("", 1, ({ (2 + 42) != 42; }));

    assert("", 1, ({ 0 < 1; }));
    assert("", 0, ({ 1 < 1; }));
    assert("", 0, ({ 2 < 1; }));
    assert("", 1, ({ 0 <= 1; }));
    assert("", 1, ({ 1 <= 1; }));
    assert("", 0, ({ 2 <= 1; }));

    assert("", 1, ({ 1 > 0; }));
    assert("", 0, ({ 1 > 1; }));
    assert("", 0, ({ 1 > 2; }));
    assert("", 1, ({ 1 >= 0; }));
    assert("", 1, ({ 1 >= 1; }));
    assert("", 0, ({ 1 >= 2; }));

    // assert("modulo", 3, ({ 7 % 4; }));
    // assert("ternary_1", 7, ({
    //     int a = 3;
    //     7 % 4 == a ? 7 : 5;
    // }));
    // assert("ternary_2", 55, ({ 7 ? '7' : '5'; }));
    // assert("ternary_3", 8, ({
    //     int a = 3;
    //     int *b = a == 3 ? 0 : &a;
    //     b ? 11 : sizeof(b);
    // }));
    // assert("ternary_4", 0, ({
    //     int a = -1;
    //     (a < 0) ? 0 : 1;
    // }));
}

/////////////////////////////////////////////////

int main() {
    // assert("goto_1", 2, goto_1());

    // assert("enum_switch_1", 11, enum_switch_1());
    // assert("enum_switch_2", 9, enum_switch_2());
    // assert("enum_switch_3", 10, enum_switch_3());
    // assert("enum_switch_4", 29, enum_switch_4());
    // assert("enum_switch_5", 12, enum_switch_5());
    // assert("enum_switch_6", 3, enum_switch_6());

    // assert("bool_invert_1", 11, bool_invert_1());
    // assert("bool_invert_2", 11, bool_invert_2());
    // assert("bool_invert_3", 3, bool_invert_3());

    // assert("use_struct_1", 13, use_struct_1());
    // assert("use_struct_2", 7, use_struct_2());
    // assert("use_struct_3", 55, use_struct_3());
    // assert("use_struct_4", 11, use_struct_4());
    // assert("use_struct_5", 5, use_struct_5());
    // assert("use_struct_6", 49, use_struct_6());
    // assert("use_struct_7", 3, use_struct_7());
    // assert("use_struct_8", 19, use_struct_8());

    // assert("scope_for_1", 11, scope_for_1());
    // assert("scope_for_2", 12, scope_for_2());
    // assert("scope_for_3", 45, scope_for_3());
    // assert("scoped_1", 1, scoped_1());
    // assert("scoped_2", 3, scoped_2());

    // assert("loop_1", 9, loop_1());
    // assert("loop_2", 55, loop_2());
    // assert("loop_3", 10, loop_3());
    // assert("loop_4", 3, loop_4());

    // TODO ポインタを返す関数のテストを追加
    // assert("string_return", 8, string_return());

    // assert("string_global", 4, string_global());
    assert("string_literal_japanese", 9, string_literal_japanese());
    assert("string_literal_ascii_1", 3, string_literal_ascii_1());
    // assert("string_literal_ascii_2", 4, string_literal_ascii_2());
    // assert("string_literal_ascii_3", 2, string_literal_ascii_3());

    // assert("string_literal_char_array_1", 110, string_literal_char_array_1());
    // assert("string_literal_char_array_2", 0, string_literal_char_array_2());
    // assert("string_literal_char_array_3", 106, string_literal_char_array_3());
    // assert("string_literal_char_array_4", 5, string_literal_char_array_4());
    // assert("string_literal_char_array_5", 21, string_literal_char_array_5());
    // assert("string_literal_char_array_6", 4, string_literal_char_array_6());

    // assert("char_literal_1", 5, char_literal_1());
    // assert("char_literal_2", 6, char_literal_2());
    // assert("char_literal_3", 7, char_literal_3());
    // assert("char_literal_4", 9, char_literal_4());

    // assert("char_array_and_pointer_1", 13, char_array_and_pointer_1());
    // assert("char_array_and_pointer_2", 8, char_array_and_pointer_2());
    // assert("char_array_and_pointer_3", 24, char_array_and_pointer_3());
    // assert("char_array_and_pointer_4", 4, char_array_and_pointer_4());
    // assert("char_array_and_pointer_5", 3, char_array_and_pointer_5());
    // assert("char_array_and_pointer_6", 5, char_array_and_pointer_6());
    // assert("char_array_and_pointer_7", 8, char_array_and_pointer_7());
    // assert("char_array_and_pointer_8", 33, char_array_and_pointer_8());
    // assert("char_array_and_pointer_9", 21, char_array_and_pointer_9());

    assert("char_calculate_array_1", 3, char_calculate_array_1());
    // assert("char_calculate_array_2", 2, char_calculate_array_2());

    assert("global_variable_1", 5, global_variable_1());
    assert("global_variable_2", 9, global_variable_2());
    assert("global_variable_3", 3, global_variable_3());
    assert("global_variable_4", 3, global_variable_4());
    // assert("global_variable_5", 2, global_variable_5());
    // assert("global_variable_6", 7, global_variable_6());
    // assert("global_variable_7", 4, global_variable_7());
    // assert("global_variable_8", 17, global_variable_8());
    // assert("global_variable_9", 7, global_variable_9());

    // assert("global_variable_10", 11, global_variable_10());
    // assert("global_variable_11", 33, global_variable_11());
    // assert("global_variable_12", 1, global_variable_12());
    // assert("global_variable_13", 1, global_variable_13());
    // assert("global_variable_14", 12, global_variable_14());
    // assert("global_variable_15", 13, global_variable_15());
    // assert("global_variable_16", 16, global_variable_16());
    // assert("global_variable_17", 2, global_variable_17());
    // assert("global_variable_18", 3, global_variable_18());
    // assert("global_variable_19", 4, global_variable_19());

    // assert("global_variable_20", 10, global_variable_20());
    // assert("global_variable_21", 16, global_variable_21());
    // assert("global_variable_22", 39, global_variable_22());
    // assert("global_variable_23", 9, global_variable_23());
    // assert("global_variable_24", 8, global_variable_24());
    // assert("global_variable_25", 9, global_variable_25());

    // assert("array_initialize_1", 0, array_initialize_1());
    // assert("array_initialize_2", 2, array_initialize_2());
    // assert("array_initialize_3", 4, array_initialize_3());

    // assert("array_and_pointer_1", 8, array_and_pointer_1());
    // assert("array_and_pointer_2", 8, array_and_pointer_2());
    // assert("array_and_pointer_3", 8, array_and_pointer_3());
    // assert("array_and_pointer_4", 8, array_and_pointer_4());
    // assert("array_and_pointer_5", 12, array_and_pointer_5());
    assert("array_and_pointer_6", 12, array_and_pointer_6());
    assert("array_and_pointer_7", 12, array_and_pointer_7());
    assert("array_and_pointer_8", 12, array_and_pointer_8());
    assert("array_and_pointer_9", 8, array_and_pointer_9());

    assert("array_and_pointer_10", 24, array_and_pointer_10());
    assert("array_and_pointer_11", 3, array_and_pointer_11());
    assert("array_and_pointer_12", 3, array_and_pointer_12());
    assert("array_and_pointer_13", 3, array_and_pointer_13());
    assert("array_and_pointer_14", 3, array_and_pointer_14());
    // assert("array_and_pointer_15", 3, array_and_pointer_15());
    assert("array_and_pointer_16", 3, array_and_pointer_16());
    assert("array_and_pointer_17", 3, array_and_pointer_17());
    assert("array_and_pointer_18", 3, array_and_pointer_18());
    assert("array_and_pointer_19", 3, array_and_pointer_19());

    assert("sizeof_1", 4, sizeof_1());
    assert("sizeof_2", 8, sizeof_2());
    assert("sizeof_3", 4, sizeof_3());
    assert("sizeof_4", 8, sizeof_4());
    assert("sizeof_5", 4, sizeof_5());
    assert("sizeof_6", 4, sizeof_6());
    assert("sizeof_7", 4, sizeof_7()); // ★
    assert("sizeof_8", 1, sizeof_8());
    // assert("sizeof_9", 136, sizeof_9());

    // assert("pointer_and_calculate_1", 3, pointer_and_calculate_1());
    // assert("pointer_and_calculate_2", 2, pointer_and_calculate_2());
    // assert("pointer_and_calculate_3", 3, pointer_and_calculate_3()); // ★
    assert("pointer_and_calculate_4", 4, pointer_and_calculate_4());
    assert("pointer_and_calculate_5", 3, pointer_and_calculate_5());

    assert("function_1", 13, function_1());
    assert("function_2", 13, function_2());
    assert("function_3", 13, function_3());
    assert("function_4", 13, function_4());
    assert("function_5", 13, function_5());
    assert("function_6", 37, function_6());
    assert("function_7", 12, function_7());
    assert("function_8", 12, function_8());
    assert("function_9", 1, function_9());

    assert("function_10", 13, function_10());
    assert("function_11", 13, function_11());
    assert("function_12", 12, function_12());
    assert("function_13", 11, function_13());
    assert("function_14", 12, function_14());
    assert("function_15", 12, function_15());
    assert("function_16", 12, function_16());
    assert("function_17", 12, function_17());
    assert("function_18", 12, function_18());
    assert("function_19", 12, function_19());

    assert("function_20", 2, function_20());

    assert("block_1", 10, block_1());
    assert("block_2", 11, block_2());
    assert("block_3", 11, block_3());
    assert("block_4", 3, block_4());
    assert("block_5", 10, block_5());
    assert("block_6", 10, block_6());
    assert("block_7", 10, block_7());
    assert("block_8", 6, block_8());
    assert("block_9", 6, block_9());

    assert_others();

    printf("=========== ============= ============\n");

    return 0;
}