#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "utils.h"


char* read_file(const char* path) {
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


String string_new() {
    String s = { .data=NULL, .len=0, .cap=0 };
    return s;
}

String string_with_capactiy(size_t cap) {
    char* data = malloc((cap + 1) * sizeof(char));
    if (data == NULL) {
        fprintf(stderr, "String alloc failure\n");
        abort();
    }
    String s = { .data=data, .len=0, .cap=cap };
    return s;
}

String string_from_cstr(char* cstr) {
    size_t len = strlen(cstr);
    String s = { .data=cstr, .len=len, .cap=len };
    return s;
}

String string_copy_from_cstr(char* cstr) {
    size_t len = strlen(cstr);
    char* data = malloc((len + 1) * sizeof(char));
    if (data == NULL) {
        fprintf(stderr, "String alloc failure\n");
        abort();
    }
    memset(data, '\0', len + 1);
    strncpy(data, cstr, len);
    String s = { .data=data, .len=len, .cap=len };
    return s;
}

void string_resize(String* s, size_t new_cap) {
    if (new_cap == 0)
        new_cap = 16;

    size_t n_trailing = (new_cap + 1) - s->len;
    char* data = realloc(s->data, (new_cap + 1) * sizeof(char));
    if (data == NULL) {
        fprintf(stderr, "String resize failure\n");
        abort();
    }
    memset(data + s->len, '\0', n_trailing);
    s->data = data;
    s->cap = new_cap;
}

void string_push_char(String* s, char c) {
    size_t avail = s->cap - s->len;
    if (avail == 0) {
        string_resize(s, 2 * s->cap);
    }
    s->data[s->len] = c;
    s->len += 1;
}

void string_push_str(String* s, Str* str) {
    string_push_cstr(s, str_as_cstr(str), str->len);
}

void string_push_cstr(String* s, char* cstr, size_t str_len) {
    size_t avail = s->cap - s->len;
    if (str_len > avail) {
        size_t new_cap = s->cap * 2;
        if (str_len > (new_cap - s->len)) {
            new_cap += str_len - (new_cap - s->len);
        }
        string_resize(s, new_cap);
    }
    while (*cstr) {
        s->data[s->len] = *cstr;
        s->len += 1;
        cstr += 1;
    }
}

char string_index(String* s, size_t index) {
    return s->data[index];
}

Str string_as_str(String* s) {
    Str str = { .source=s, .start=0, .len=s->len };
    return str;
}

Str string_trim_whitespace(String* s) {
    size_t start = 0;
    size_t end = s->len;
    while (start < end && isspace(string_index(s, start)))
        start += 1;

    size_t len;
    if (start >= end) {
        len = 0;
    } else {
        if (end > 0)
            end -= 1;
        while (isspace(string_index(s, end)))
            end -= 1;
        len = end - start + 1;
    }

    Str str = { .source=s, .start=start, .len=len };
    return str;
}

char* string_as_cstr(String* s) {
    return s->data;
}

void string_drop_inner(String* s) {
    free(s->data);
}


char str_index(Str* str, size_t ind) {
    return str->source->data[ind+str->start];
}

char* str_as_cstr(Str* str) {
    return str->source->data + str->start;
}


/* CharSlice* trim_whitespace_str(char* str) { */
/*     size_t start = 0; */
/*     size_t end = strlen(str) - 1; */
/*     while (start < end - 1 && isspace(str[start])) */
/*         start += 1; */

/*     size_t size; */
/*     if (start == end) { */
/*         size = 0; */
/*     } else { */
/*         while (isspace(str[end])) */
/*             end -= 1; */
/*         size = 1 + end - start; */
/*     } */

/*     CharSlice* slice = malloc(sizeof(CharSlice)); */
/*     if (slice == NULL) */
/*         return NULL; */

/*     slice->data = str; */
/*     slice->start = start; */
/*     slice->size = size; */
/*     return slice; */
/* } */

/* char slice_get(CharSlice* slice, size_t i) { */
/*     return slice->data[i]; */
/* } */

