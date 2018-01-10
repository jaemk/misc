#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "../../../cutils/utils.h"


size_t part1(char* data) {
    Str str_ = str_from_cstr(data);
    Str str = str_trim_whitespace(&str_);
    size_t len = str.len;

    size_t sum = 0;
    if (str_index(&str, 0) == str_index(&str, len-1))
        sum += str_index(&str, 0) - '0';

    for (size_t i = 0; i < len - 1; i++) {
        char a = str_index(&str, i);
        if (a == str_index(&str, i+1))
            sum += a - '0';
    }
    return sum;
}


size_t step_index(size_t curr, size_t step, size_t limit) {
    size_t next = curr + step;
    while (next >= limit)
        next -= limit;
    return next;
}


size_t part2(char* data) {
    Str str_ = str_from_cstr(data);
    Str str = str_trim_whitespace(&str_);
    size_t len = str.len;
    size_t half = len / 2;
    size_t sum = 0;
    for (size_t i = 0; i < len; i++) {
        size_t next = step_index(i, half, len);
        char a = str_index(&str, i);
        if (a == str_index(&str, next))
            sum += a - '0';
    }
    return sum;
}


void test_examples() {
    printf("|---- Part1... ");
    char* p1_cases[] = {"1122", "1111", "1234", "91212129"};
    size_t p1_expected[] = {3, 4, 0, 9};
    size_t p1_len = sizeof(p1_cases) / sizeof(p1_cases[0]);
    for (size_t i = 0; i < p1_len; i++) {
        size_t ans = part1(p1_cases[i]);
        if (ans != p1_expected[i]) {
            fprintf(stderr, "Part1: Input: %s, Expected %lu, got %lu\n",
                    p1_cases[i], p1_expected[i], ans);
            abort();
        }
    }
    printf("OK\n");

    printf("|---- Part2... ");
    char* p2_cases[] = {"1212", "1221", "123425", "123123", "12131415"};
    size_t p2_expected[] = {6, 0, 4, 12, 4};
    size_t p2_len = sizeof(p2_cases) / sizeof(p2_cases[0]);
    for (size_t i = 0; i < p2_len; i++) {
        size_t ans = part2(p2_cases[i]);
        if (ans != p2_expected[i]) {
            fprintf(stderr, "Part2: Input: %s, Expected %lu, got %lu\n",
                    p2_cases[i], p2_expected[i], ans);
            abort();
        }
    }
    printf("OK\n");
}


int main() {
    printf("\n");
    printf("Running examples...\n");
    test_examples();
    printf("\n");

    printf("Running main input...\n");
    String input = read_file("../input.txt");
    printf("day1-p1: %lu\n", part1(string_as_cstr(&input)));
    printf("day1-p2: %lu\n", part2(string_as_cstr(&input)));
    printf("\n");

    string_drop_inner(&input);
    return 0;
}

