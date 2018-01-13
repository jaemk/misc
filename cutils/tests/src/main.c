#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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


void print_vec_of_strs(Vec* v) {
    size_t len = vec_len(v);
    printf("[");
    for (size_t i = 0; i < len; i++) {
        Str* s = (Str*)vec_index_ref(v, i);
        String string = str_to_owned_string(s);
        printf("\"%s\"", string_as_cstr(&string));
        if (i < len - 1) {
            printf(", ");
        }
        string_drop_inner(&string);
    }
    printf("]\n");
}


void test_new_string() {
    printf("| --- New string:\n");
    String s = string_new();
    ASSERT("new capacity", size_t, string_cap(&s), ==, 0, "expected: %lu, got: %lu");

    printf("| --- Push a char:\n");
    string_push_char(&s, 'a');
    ASSERT("capacity",  size_t, string_cap(&s), ==, 16, "expected: %lu, got: %lu");
    ASSERT("len",       size_t, string_len(&s), ==, 1, "expected: %lu, got: %lu");
    ASSERT("first",     char, string_index(&s, 0), ==, 'a', "expected: %c, got: %c");

    printf("| --- Push a char:\n");
    string_push_char(&s, 'b');
    ASSERT("capacity",  size_t, string_cap(&s), ==, 16, "expected: %lu, got: %lu");
    ASSERT("len",       size_t, string_len(&s), ==, 2, "expected: %lu, got %lu");
    ASSERT("second",    char, string_index(&s, 1), ==, 'b', "expected: %c, got: %c");

    printf("| --- Push a str:\n");
    string_push_cstr(&s, "1234567891234567");
    ASSERT("capacity",  size_t, string_cap(&s), ==, 32, "expected: %lu, got: %lu");
    ASSERT("len",       size_t, string_len(&s), ==, 18, "expected: %lu, got: %lu");
    ASSERT("5th",       char, string_index(&s, 4), ==, '3', "expected: %c, got: %c");
    Str str1 = string_as_str(&s);
    ASSERT("as str, 5th",   char, str_index(&str1, 4), ==, '3', "expected: %c, got: %c");

    String s2 = string_copy_from_cstr("ab1234567891234567");
    ASSERT("content equal", uint8_t, string_eq(&s, &s2), ==, 0, "expected: %d, got: %d");

    string_drop_inner(&s);
    string_drop_inner(&s2);
}

void test_string_from_cstr() {
    printf("| --- Copy from cstr:\n");
    String s = string_copy_from_cstr("\n \t3 spaces around \t\n");
    ASSERT("len",       size_t, string_len(&s), ==, 21, "expected: %lu, got: %lu");
    ASSERT("capacity",  size_t, string_len(&s), ==, 21, "expected: %lu, got: %lu");

    printf("| --- Push a char:\n");
    string_push_char(&s, 'J');
    ASSERT("len",       size_t, string_len(&s), ==, 22, "expected: %lu, got: %lu");
    ASSERT("capacity",  size_t, string_cap(&s), ==, 42, "expected: %lu, got: %lu");
    string_drop_inner(&s);
}

void test_string_from_file_str_trim() {
    printf("| --- From file:\n");
    printf("| --- Trimmed:\n");
    String s = read_file("input.txt");
    Str str = string_trim_whitespace(&s);
    ASSERT("first char",    char, str_index(&str, 0), ==, 's', "expected: %c, got: %c");
    ASSERT("last char",     char, str_index(&str, str_len(&str)-1), ==, 'g', "expected: %c, got %c");
    string_drop_inner(&s);
}

void test_str_split_whitespace() {
    printf("| --- String split whitespace:\n");
    String s = string_copy_from_cstr("1  a\n bcdef   \tg \t 3");
    Vec fragments = string_split_whitespace(&s);
    ASSERT("num fragments", size_t, vec_len(&fragments), ==, 5, "expected: %lu, got: %lu");
    printf("| ------ Compare fragments:\n");
    const char* expected_frags[] = {"1", "a", "bcdef", "g", "3"};
    size_t frag_sizes[] = {1, 1, 5, 1, 1};
    for (size_t i = 0; i < 5; i++) {
        Str* frag = (Str*)vec_index_ref(&fragments, i);
        size_t frag_size = frag_sizes[i];
        ASSERT("--- fragment size", size_t, str_len(frag), ==, frag_size, "expected: %lu, got: %lu");
        const char* frag_ptr = str_as_ptr(frag);
        const char* expected_frag = expected_frags[i];
        ASSERT("--- fragment content", int, strncmp(frag_ptr, expected_frag, frag_size), ==, 0, "expected: %d, got: %d");
    }
    vec_drop_inner(&fragments);
    string_drop_inner(&s);
}

void test_str_split_lines() {
    printf("| --- String split lines: ");
    String s = string_copy_from_cstr("\n  a\n bcdefg \n");
    Vec lines = string_split_lines(&s);
    print_vec_of_strs(&lines);
    ASSERT("num lines", size_t, vec_len(&lines), ==, 4, "expected: %lu, got: %lu");
    printf("| ------ Compare lines:\n");
    const char* expected_lines[] = {"", "  a", " bcdefg ", ""};
    size_t line_sizes[] = {0, 3, 8, 0};
    for (size_t i = 0; i < 4; i++) {
        Str* line = (Str*)vec_index_ref(&lines, i);
        size_t line_size = line_sizes[i];
        ASSERT("--- line size", size_t, str_len(line), ==, line_size, "expected: %lu, got: %lu");
        const char* line_ptr = str_as_ptr(line);
        const char* expected_line = expected_lines[i];
        ASSERT("--- line content", int, strncmp(line_ptr, expected_line, line_size), ==, 0, "expected: %d, got: %d");
    }
    vec_drop_inner(&lines);
    string_drop_inner(&s);
}

void string_tests() {
    printf("\nString tests:\n");
    test_new_string();
    test_string_from_cstr();
    test_string_from_file_str_trim();
    test_str_split_whitespace();
    test_str_split_lines();
}


void vec_tests() {
    printf("\nVec tests:\n");

    printf("| --- New vector (vec of char):\n");
    Vec v1 = vec_new(sizeof(char));
    ASSERT("new capacity",  size_t, vec_cap(&v1), ==, 0, "expected: %lu, got: %lu");

    printf("| --- Vec push/resize/index:\n");
    vec_push(&v1, "j");
    ASSERT("new capacity",  size_t, vec_cap(&v1), ==, 16, "expected: %lu, got: %lu");
    ASSERT("first char",    char, *(char*)vec_index_ref(&v1, 0), ==, 'j', "expected: %c, got: %c");

    printf("| --- Vec push more force resize:\n");
    char* more_chars_16 = "1234567891234567";
    while (*more_chars_16) {
        vec_push(&v1, more_chars_16);
        more_chars_16++;
    }
    ASSERT("new capacity",  size_t, vec_cap(&v1), ==, 32, "expected: %lu, got: %lu");
    ASSERT("new len",       size_t, vec_len(&v1), ==, 17, "expected: %lu, got: %lu");
    ASSERT("first char",    char, *(char*)vec_index_ref(&v1, 0), ==, 'j', "expected: %c, got: %c");
    ASSERT("last char",     char, *(char*)vec_index_ref(&v1, vec_len(&v1)-1), ==, '7', "expected: %c, got: %c");
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
    ASSERT("capacity",  size_t, vec_cap(&v2), ==, 40, "expected: %lu, got: %lu");
    ASSERT("length",    size_t, vec_len(&v2), ==, 4, "expected: %lu, got: %lu");
    Str* first_str = *(Str**)vec_index_ref(&v2, 0);
    ASSERT("size of first str", size_t, str_len(first_str), ==, 3, "expected %lu, got: %lu");
    ASSERT("first char of first str", char, str_index(first_str, 0), ==, 'o', "expected: %c, got: %c");
    Str* third_str = *(Str**)vec_index_ref(&v2, 2);
    ASSERT("size of third str", size_t, str_len(third_str), ==, 6, "expected %lu, got: %lu");
    ASSERT("last char of third str", char, str_index(third_str, str_len(third_str)-1), ==, '!', "expected: %c, got: %c");
    Str extra_str = str_from_cstr("bagel");
    Str* extra_str_ptr = &extra_str;
    vec_push(&v2, &extra_str_ptr);
    Str* extra_str_ref = *(Str**)vec_index_ref(&v2, vec_len(&v2)-1);
    assert(extra_str_ref == extra_str_ptr);
    ASSERT("Str* addresses match", uintptr_t, (uintptr_t)extra_str_ref, ==, (uintptr_t)extra_str_ptr, "expected: %lu, got: %lu");
    ASSERT("last char of last str", char, str_index(extra_str_ref, str_len(extra_str_ref)-1), ==, 'l', "expected: %c, got: %c");
    vec_drop_inner(&v2);

    printf("| --- Vec mutate through ref (vec of String*):\n");
    String s = string_copy_from_cstr("sandwich");
    String* s_ptr = &s;
    Vec v3 = vec_new(sizeof(String*));
    vec_push(&v3, &s_ptr);
    String* s_ref = *(String**)vec_index_ref(&v3, 0);
    ASSERT("String* addresses match", uintptr_t, (uintptr_t)s_ref, ==, (uintptr_t)s_ptr, "expected: %lu, got: %lu");
    string_push_cstr(s_ref, " is good");
    ASSERT("length", size_t, string_len(s_ptr), ==, 16, "expected: %lu, got: %lu");
    ASSERT("last char", char, string_index(s_ptr, string_len(s_ptr)-1), ==, 'd', "expected: %c, got: %c");
    vec_drop_inner(&v3);
    string_drop_inner(s_ptr);

    printf("| --- Vec owned items (vec of String):\n");
    Vec v4 = vec_new(sizeof(String));
    String s2 = string_copy_from_cstr("meatloaf");
    vec_push(&v4, &s2);
    String* s2_ref = (String*)vec_index_ref(&v4, 0);
    ASSERT("String* addresses differ", uintptr_t, (uintptr_t)s2_ref, !=, (uintptr_t)&s2, "expected: %lu, got: %lu");
    String s3 = string_copy_from_cstr("mashed potatoes");
    vec_push(&v4, &s3);
    // drop all string backing data before dropping the vec backing data
    vec_drop_inner_each(&v4, string_drop_inner);

    printf("| --- Vec clearing (vec of String):\n");
    printf("| ----- Pusing Strings:\n");
    const char* in_strings[] = {"one", "two", "three", "four"};
    Vec v5 = vec_with_capacity(sizeof(String), 4);
    for (size_t i = 0; i < 4; i++) {
        String s_ = string_copy_from_cstr(in_strings[i]);
        vec_push(&v5, &s_);
    }
    ASSERT("length", size_t, vec_len(&v5), ==, 4, "expected: %lu, got: %lu");
    ASSERT("cap", size_t, vec_cap(&v5), ==, 4, "expected: %lu, got: %lu");
    printf("| ----- Clearing vec:\n");
    vec_clear(&v5, string_drop_inner);
    ASSERT("length", size_t, vec_len(&v5), ==, 0, "expected: %lu, got: %lu");
    ASSERT("cap", size_t, vec_cap(&v5), ==, 4, "expected: %lu, got: %lu");
    vec_drop_inner(&v5);
}


int main() {
    printf("c-utils tests...\n");
    string_tests();
    vec_tests();
    return 0;
}

