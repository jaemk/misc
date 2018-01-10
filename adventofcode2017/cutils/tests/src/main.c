#include <stdio.h>
#include <stdlib.h>
#include "../../utils.h"

#define ASSERT(desc, ty, expr, op, expected, expln) \
do { \
    printf("|     |--- TEST: %s ...", desc); \
    ty __val = expr; \
    ty __exp = expected; \
    if (__val op __exp) { } else { \
        fprintf(stderr, "Error: "); \
        fprintf(stderr, expln, __exp, __val); \
        fprintf(stderr, "\n"); \
        abort(); \
    } \
    printf(" OK\n"); \
} while(0)


void string_tests() {
    printf("String tests:\n");

    printf("| --- New string:\n");
    String s = string_new();
    ASSERT("new capacity", size_t, s.cap, ==, 0, "expected: %lu, got: %lu");

    printf("| --- Push a char:\n");
    string_push_char(&s, 'a');
    ASSERT("capacity",  size_t, s.cap, ==, 16, "expected: %lu, got: %lu");
    ASSERT("len",       size_t, s.len, ==, 1, "expected: %lu, got: %lu");
    ASSERT("first",     char, string_index(&s, 0), ==, 'a', "expected: %c, got: %c");

    printf("| --- Push a char:\n");
    string_push_char(&s, 'b');
    ASSERT("capacity",  size_t, s.cap, ==, 16, "expected: %lu, got: %lu");
    ASSERT("len",       size_t, s.len, ==, 2, "expected: %lu, got %lu");
    ASSERT("second",    char, string_index(&s, 1), ==, 'b', "expected: %c, got: %c");

    printf("| --- Push a str:\n");
    string_push_cstr(&s, "1234567891234567");
    ASSERT("capacity",  size_t, s.cap, ==, 32, "expected: %lu, got: %lu");
    ASSERT("len",       size_t, s.len, ==, 18, "expected: %lu, got: %lu");
    ASSERT("5th",       char, string_index(&s, 4), ==, '3', "expected: %c, got: %c");
    Str str1 = string_as_str(&s);
    ASSERT("as str, 5th",   char, str_index(&str1, 4), ==, '3', "expected: %c, got: %c");

    string_drop_inner(&s);


    printf("| --- Copy from cstr:\n");
    String s2 = string_copy_from_cstr("\n \t3 spaces around \t\n");
    ASSERT("len",       size_t, s2.len, ==, 21, "expected: %lu, got: %lu");
    ASSERT("capacity",  size_t, s2.len, ==, 21, "expected: %lu, got: %lu");

    printf("| --- Push a char:\n");
    string_push_char(&s2, 'J');
    ASSERT("len",       size_t, s2.len, ==, 22, "expected: %lu, got: %lu");
    ASSERT("capacity",  size_t, s2.cap, ==, 42, "expected: %lu, got: %lu");
    string_drop_inner(&s2);

    printf("| --- From file:\n");
    printf("| --- Trimmed:\n");
    String s3 = read_file("input.txt");
    Str str3 = string_trim_whitespace(&s3);
    ASSERT("first char",    char, str_index(&str3, 0), ==, 's', "expected: %c, got: %c");
    ASSERT("last char",     char, str_index(&str3, str3.len-1), ==, 'g', "expected: %c, got %c");
    string_drop_inner(&s3);

    printf("\n");
}


int main() {
    printf("c-utils tests...\n");
    string_tests();
    return 0;
}

