#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>


char* read_input(const char* path) {
    FILE* f = fopen(path, "r");
    if (f == NULL) {
        perror("Error opening file");
        return NULL;
    }
    fseek(f, 0, SEEK_END);
    size_t len = ftell(f);
    rewind(f);
    char* content = malloc((len + 1) * sizeof(char));
    fread(content, sizeof(char), len, f);
    fclose(f);
    return content;
}


typedef struct {
    size_t start, end;
    char* data;
} Trim;

Trim* trim_new(char* str) {
    size_t start = 0;
    while (isspace(str[start]))
        start += 1;

    size_t end = strlen(str) - 1;
    while (isspace(str[end]))
        end -= 1;

    Trim* tr = malloc(sizeof(Trim));
    if (tr == NULL)
        return NULL;

    tr->start = start;
    tr->end = end;
    tr->data = str;
    return tr;
}

size_t trim_size(Trim* tr) {
    return 1 + tr->end - tr->start;
}

char trim_get(Trim* tr, size_t i) {
    return tr->data[i];
}


size_t part1(Trim* input) {
    size_t len = trim_size(input);
    size_t sum = 0;
    if (trim_get(input, 0) == trim_get(input, len-1))
        sum += trim_get(input, 0) - '0';

    for (size_t i = 0; i < len - 1; i++) {
        char a = trim_get(input, i);
        if (a == trim_get(input, i+1))
            sum += a - '0';
    }
    return sum;
}


size_t step_index(size_t curr, size_t step, size_t limit) {
    size_t next = curr + step;
    while (next > limit)
        next -= limit;
    return next;
}


size_t part2(Trim* input) {
    size_t len = trim_size(input);
    size_t half = len / 2;
    size_t sum = 0;
    for (size_t i = 0; i < len; i++) {
        size_t next = step_index(i, half, len);
        char a = trim_get(input, i);
        if (a == trim_get(input, next))
            sum += a - '0';
    }
    return sum;
}


int main() {
    char* data = read_input("../input.txt");
    if (data == NULL)
        return -1;

    Trim* input = trim_new(data);
    if (input == NULL)
        return -1;

    printf("day1-p1: %lu\n", part1(input));
    printf("day1-p2: %lu\n", part2(input));

    free(input);
    return 0;
}

