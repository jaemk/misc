#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "utils.h"


/* ----------- String ------------- */


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
    memset(data, '\0', cap + 1);
    String s = { .data=data, .len=0, .cap=cap };
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
    string_push_cstr_bound(s, str->data+str->__start, str->len);
}

void string_push_cstr(String* s, char* cstr) {
    size_t len = strlen(cstr);
    string_push_cstr_bound(s, cstr, len);
}

void string_push_cstr_bound(String* s, char* cstr, size_t str_len) {
    size_t avail = s->cap - s->len;
    if (str_len > avail) {
        size_t new_cap = s->cap * 2;
        if (str_len > (new_cap - s->len)) {
            new_cap += str_len - (new_cap - s->len);
        }
        string_resize(s, new_cap);
    }
    size_t count = 0;
    while (*cstr && count < str_len) {
        s->data[s->len] = *cstr;
        s->len++;
        count++;
        cstr++;
    }
}

char string_index(String* s, size_t index) {
    return s->data[index];
}

Str string_as_str(String* s) {
    Str str = { .data=s->data, .__start=0, .len=s->len };
    return str;
}

Str string_trim_whitespace(String* s) {
    Str str = string_as_str(s);
    return str_trim_whitespace(&str);
}

char* string_as_cstr(String* s) {
    return s->data;
}

void string_drop_inner(String* s) {
    if (s->data != NULL)
        free(s->data);
}


/* ----------- Str -------------- */


Str str_from_cstr(char* cstr) {
    size_t len = strlen(cstr);
    Str s = { .data=cstr, .__start=0, .len=len };
    return s;
}

Str str_trim_whitespace(Str* s) {
    size_t start = 0;
    size_t end = s->len;
    while (start < end && isspace(str_index(s, start)))
        start++;

    size_t len;
    if (start >= end) {
        len = 0;
    } else {
        if (end > 0)
            end--;
        while (isspace(str_index(s, end)))
            end--;
        len = end - start + 1;
    }

    Str str = { .data=s->data, .__start=start, .len=len };
    return str;
}

String str_to_owned_string(Str* str) {
    String s = string_with_capactiy(str->len);
    string_push_str(&s, str);
    return s;
}

char str_index(Str* str, size_t ind) {
    return str->data[ind+str->__start];
}



String read_file(const char* path) {
    FILE* f = fopen(path, "r");
    if (f == NULL) {
        perror("Error opening file");
        abort();
    }
    fseek(f, 0, SEEK_END);
    size_t len = ftell(f);
    rewind(f);
    char* content = malloc((len + 1) * sizeof(char));
    fread(content, sizeof(char), len, f);
    content[len] = '\0';
    fclose(f);
    String s = { .data=content, .len=len, .cap=len };
    return s;
}

