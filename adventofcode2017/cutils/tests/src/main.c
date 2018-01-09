#include <stdio.h>
#include <stdlib.h>
#include "../../utils.h"

#define ASSERT(desc, cond, str) \
do { \
    printf("|     |--- TEST: %s ...", desc); \
    if (cond) { } else { \
        fprintf(stderr, "Error: "); \
        fprintf(stderr, str); \
        fprintf(stderr, "\n"); \
        abort(); \
    } \
    printf(" OK\n"); \
} while(0)


void string_tests() {
    printf("String tests:\n");

    printf("| --- New string:\n");
    String s = string_new();
    ASSERT("new capacity", s.cap == 0, "Capacity should be 0");

    printf("| --- Push a char:\n");
    string_push_char(&s, 'a');
    ASSERT("capacity",  s.cap == 16, "Capacity should be 16");
    ASSERT("len",       s.len == 1, "Length should be 1");
    ASSERT("first",     string_index(&s, 0) == 'a', "index 0 should be 'a'");

    printf("| --- Push a char:\n");
    string_push_char(&s, 'b');
    ASSERT("capacity",  s.cap == 16, "Capacity should be 16");
    ASSERT("len",       s.len == 2, "Length should be 1");
    ASSERT("second",     string_index(&s, 1) == 'b', "index 1 should be 'b'");

    printf("| --- Push a str:\n");
    string_push_cstr(&s, "1234567891234567", 16);
    ASSERT("capacity",  s.cap == 32, "Capacity should be 32");
    ASSERT("len",       s.len == 18, "Length should be 18");
    ASSERT("5th",       string_index(&s, 4) == '3', "index 4 should be '3'");


    printf("| --- Copy from cstr:\n");
    String s2 = string_copy_from_cstr("\n \t3 spaces around \t\n");
    ASSERT("len",       s2.len == 21, "Length should be 21");
    ASSERT("capacity",  s2.len == 21, "Capacity should be 21");

    printf("| --- Push a char:\n");
    string_push_char(&s2, 'J');
    ASSERT("len",       s2.len == 22, "Length should be 22");
    ASSERT("capacity",  s2.cap == 42, "Capacity should be 42");

    printf("\n");
}


int main() {
    printf("c-utils tests...\n");
    string_tests();
    return 0;
}

