#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
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


void vec_tests() {
    printf("Vec tests:\n");

    printf("| --- New vector (vec of char):\n");
    Vec v1 = vec_new(sizeof(char));
    ASSERT("new capacity",  size_t, v1.cap, ==, 0, "expected: %lu, got: %lu");

    printf("| --- Vec push/resize/index:\n");
    vec_push(&v1, "j");
    ASSERT("new capacity",  size_t, v1.cap, ==, 16, "expected: %lu, got: %lu");
    ASSERT("first char",    char, *(char*)vec_index_ref(&v1, 0), ==, 'j', "expected: %c, got: %c");

    printf("| --- Vec push more force resize:\n");
    char* more_chars_16 = "1234567891234567";
    while (*more_chars_16) {
        vec_push(&v1, more_chars_16);
        more_chars_16++;
    }
    ASSERT("new capacity",  size_t, v1.cap, ==, 32, "expected: %lu, got: %lu");
    ASSERT("new len",       size_t, v1.len, ==, 17, "expected: %lu, got: %lu");
    ASSERT("first char",    char, *(char*)vec_index_ref(&v1, 0), ==, 'j', "expected: %c, got: %c");
    ASSERT("last char",     char, *(char*)vec_index_ref(&v1, v1.len-1), ==, '7', "expected: %c, got: %c");
    vec_drop_inner(&v1);

    printf("| --- New with capacity (vec of Str*):\n");
    Vec v2 = vec_with_capacity(sizeof(Str*), 40);
    Str words[] = {
        str_from_cstr("one"), str_from_cstr("two"),
        str_from_cstr("three!"), str_from_cstr("four"),
    };
    for (size_t i = 0; i < 4; i++) {
        Str* str_ptr = words+i;
        vec_push(&v2, &str_ptr);
    }
    ASSERT("capacity",  size_t, v2.cap, ==, 40, "expected: %lu, got: %lu");
    ASSERT("length",    size_t, v2.len, ==, 4, "expected: %lu, got: %lu");
    Str* first_str = *(Str**)vec_index_ref(&v2, 0);
    ASSERT("size of first str", size_t, first_str->len, ==, 3, "expected %lu, got: %lu");
    ASSERT("first char of first str", char, str_index(first_str, 0), ==, 'o', "expected: %c, got: %c");
    Str* third_str = *(Str**)vec_index_ref(&v2, 2);
    ASSERT("size of third str", size_t, third_str->len, ==, 6, "expected %lu, got: %lu");
    ASSERT("last char of third str", char, str_index(third_str, third_str->len-1), ==, '!', "expected: %c, got: %c");
    Str extra_str = str_from_cstr("bagel");
    Str* extra_str_ptr = &extra_str;
    vec_push(&v2, &extra_str_ptr);
    Str* extra_str_ref = *(Str**)vec_index_ref(&v2, v2.len-1);
    assert(extra_str_ref == extra_str_ptr);
    ASSERT("Str* addresses match", size_t, (size_t)extra_str_ref, ==, (size_t)extra_str_ptr, "expected: %lu, got: %lu");
    ASSERT("last char of last str", char, str_index(extra_str_ref, extra_str_ref->len-1), ==, 'l', "expected: %c, got: %c");
    vec_drop_inner(&v2);

    printf("| --- Vec mutate through ref (vec of String*):\n");
    String s = string_copy_from_cstr("sandwich");
    String* s_ptr = &s;
    Vec v3 = vec_new(sizeof(String*));
    vec_push(&v3, &s_ptr);
    String* s_ref = *(String**)vec_index_ref(&v3, 0);
    ASSERT("String* addresses match", size_t, (size_t)s_ref, ==, (size_t)s_ptr, "expected: %lu, got: %lu");
    string_push_cstr(s_ref, " is good");
    ASSERT("length", size_t, s_ptr->len, ==, 16, "expected: %lu, got: %lu");
    ASSERT("last char", char, string_index(s_ptr, s_ptr->len-1), ==, 'd', "expected: %c, got: %c");
    vec_drop_inner(&v3);
    string_drop_inner(s_ptr);

    printf("| --- Vec owned items (vec of String):\n");
    Vec v4 = vec_new(sizeof(String));
    String s2 = string_copy_from_cstr("meatloaf");
    vec_push(&v4, &s2);
    String* s2_ref = (String*)vec_index_ref(&v4, 0);
    ASSERT("String* addresses differ", size_t, (size_t)s2_ref, !=, (size_t)&s2, "expected: %lu, got: %lu");
    String s3 = string_copy_from_cstr("mashed potatoes");
    vec_push(&v4, &s3);
    // drop all string backing data before dropping the vec backing data
    vec_drop_inner_each(&v4, string_drop_inner);
}


int main() {
    printf("c-utils tests...\n");
    string_tests();
    vec_tests();
    return 0;
}

