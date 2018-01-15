#include <stdio.h>
#include <limits.h>
#include "cutils/utils.h"


long int part1_check_row(Str* row) {
    Vec parts = str_split_whitespace(row);
    long int min = LONG_MAX;
    long int max = 0;
    for (size_t i = 0; i < vec_len(&parts); i++) {
        Str* part_str = vec_index_ref(&parts, i);
        String part = string_copy_from_str(part_str);
        char* end;
        long int val = strtol(string_as_cstr(&part), &end, 10);
        string_drop(&part);
        if (*end) {
            fprintf(stderr, "Failed parsing at: %s\n", end);
            abort();
        }
        if (val < min) {
            min = val;
        }
        if (val > max) {
            max = val;
        }
    }
    vec_drop(&parts);
    return max - min;
}

long int part1(Str* input) {
    Str str = str_trim_whitespace(input);
    Vec lines = str_split_lines(&str);
    long int sum = 0;
    for (size_t i = 0; i < vec_len(&lines); i++) {
        Str* line = vec_index_ref(&lines, i);
        long int row_sum = part1_check_row(line);
        sum += row_sum;
    }
    vec_drop(&lines);
    return sum;
}

long int part2_check_row(Str* row) {
    Vec parts_str = str_split_whitespace(row);
    size_t len = vec_len(&parts_str);
    Vec parts = vec_with_capacity(sizeof(long int), len);
    for (size_t i = 0; i < len; i++) {
        Str* part_str = vec_index_ref(&parts_str, i);
        String part = string_copy_from_str(part_str);
        char* end;
        long int val = strtol(string_as_cstr(&part), &end, 10);
        if (*end) {
            fprintf(stderr, "Failed parsing at: %s\n", end);
            abort();
        }
        vec_push(&parts, &val);
        string_drop(&part);
    }
    long int result = -1;
    for (size_t i = 0; i < len; i++) {
        if (result > -1)
            break;
        long int* a = vec_index_ref(&parts, i);
        for (size_t j = 0; j < len; j++) {
            if (i == j)
                continue;
            long int* b = vec_index_ref(&parts, j);
            if (*a % *b == 0) {
                result = *a / *b;
                break;
            }
        }
    }
    vec_drop(&parts_str);
    vec_drop(&parts);
    return result;
}

size_t part2(Str* input) {
    Str str = str_trim_whitespace(input);
    Vec lines = str_split_lines(&str);
    long int sum = 0;
    for (size_t i = 0; i < vec_len(&lines); i++) {
        Str* line = vec_index_ref(&lines, i);
        long int row_sum = part2_check_row(line);
        sum += row_sum;
    }
    vec_drop(&lines);
    return sum;
}


void test_part1() {
    printf("|---- Part1... ");
    const char* rows[] = {
        "5 1 9 5",
        "7 5 3",
        "2 4 6 8",
    };
    size_t expected[] = {8, 4, 6};
    for (size_t i = 0; i < 3; i++) {
        Str input = str_from_cstr(rows[i]);
        size_t result = part1_check_row(&input);
        if (result != expected[i]) {
            fprintf(stderr, "Expected: %lu, got: %lu", expected[i], result);
            abort();
        }
    }
    printf("OK\n");
}

void test_part2() {
    printf("|---- Part2... ");
    const char* rows[] = {
        "5 9 2 8",
        "9 4 7 3",
        "3 8 6 5",
    };
    size_t expected[] = {4, 3, 2};
    for (size_t i = 0; i < 3; i++) {
        Str input = str_from_cstr(rows[i]);
        size_t result = part2_check_row(&input);
        if (result != expected[i]) {
            fprintf(stderr, "Expected: %lu, got: %lu", expected[i], result);
            abort();
        }
    }
    printf("OK\n");
}

void test_examples() {
    test_part1();
    test_part2();
}

int main() {
    printf("\n");
    printf("Running examples...\n");
    test_examples();
    printf("\n");

    printf("Running main input...\n");
    String input = read_file("../input.txt");
    Str str = string_as_str(&input);
    printf("day2-p1: %lu\n", part1(&str));
    printf("day2-p2: %lu\n", part2(&str));
    printf("\n");

    string_drop(&input);
    return 0;
}

