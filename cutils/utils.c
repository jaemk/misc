#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include "utils.h"


/* Increase capacity by a factor of 2, unless it's already really big,
 * then increase by a constant value
 */
size_t __inc_cap(size_t current) {
    if (current > 8192) {
        return current + 8192;
    } else {
        return current * 2;
    }
}


/* ----------- String ------------- */


String string_new() {
    String s = { .__data=NULL, .__len=0, .__cap=0 };
    return s;
}

String string_with_capactiy(size_t cap) {
    char* data = malloc((cap + 1) * sizeof(char));
    if (data == NULL) {
        fprintf(stderr, "String alloc failure\n");
        abort();
    }
    memset(data, '\0', cap + 1);
    String s = { .__data=data, .__len=0, .__cap=cap };
    return s;
}

String string_copy_from_cstr(const char* cstr) {
    size_t len = strlen(cstr);
    char* data = malloc((len + 1) * sizeof(char));
    if (data == NULL) {
        fprintf(stderr, "String alloc failure\n");
        abort();
    }
    memset(data, '\0', len + 1);
    strncpy(data, cstr, len);
    String s = { .__data=data, .__len=len, .__cap=len };
    return s;
}

size_t string_len(String* s) {
    return s->__len;
}

size_t string_cap(String* s) {
    return s->__cap;
}

void string_resize(String* s, size_t new_cap) {
    if (new_cap == 0)
        new_cap = 16;

    size_t n_trailing = (new_cap + 1) - s->__len;
    char* data = realloc(s->__data, (new_cap + 1) * sizeof(char));
    if (data == NULL) {
        fprintf(stderr, "String resize failure\n");
        abort();
    }
    memset(data + s->__len, '\0', n_trailing);
    s->__data = data;
    s->__cap = new_cap;
}

void string_push_char(String* s, char c) {
    size_t avail = s->__cap - s->__len;
    if (avail == 0) {
        size_t new_cap = __inc_cap(s->__cap);
        string_resize(s, new_cap);
    }
    s->__data[s->__len] = c;
    s->__len += 1;
}

void string_push_str(String* s, Str* str) {
    string_push_cstr_bound(s, str->__data, str->__len);
}

void string_push_cstr(String* s, const char* cstr) {
    size_t len = strlen(cstr);
    string_push_cstr_bound(s, cstr, len);
}

void string_push_cstr_bound(String* s, const char* cstr, size_t str_len) {
    size_t avail = s->__cap - s->__len;
    if (str_len > avail) {
        size_t new_cap = __inc_cap(s->__cap);
        if (str_len > (new_cap - s->__len)) {
            new_cap += str_len - (new_cap - s->__len);
        }
        string_resize(s, new_cap);
    }
    size_t count = 0;
    while (*cstr && count < str_len) {
        s->__data[s->__len] = *cstr;
        s->__len++;
        count++;
        cstr++;
    }
}

char string_index(String* s, size_t index) {
    if (index >= s->__len) {
        fprintf(stderr, "Out of bounds: strlen: %lu, index: %lu", s->__len, index);
        abort();
    }
    return s->__data[index];
}

char* string_index_ref(String* s, size_t index) {
    if (index >= s->__len) {
        fprintf(stderr, "Out of bounds: strlen: %lu, index: %lu", s->__len, index);
        abort();
    }
    return s->__data + index;
}

uint8_t string_eq(String* s1, String* s2) {
    if (s1 == s2)
        return 0;

    size_t len = string_len(s1);
    if (len != string_len(s2))
        return 1;

    for (size_t i = 0; i < len; i++) {
        if (string_index(s1, i) != string_index(s2, i))
            return 1;
    }
    return 0;
}

Str string_as_str(String* s) {
    Str str = { .__data=s->__data, .__len=s->__len };
    return str;
}

Str string_trim_whitespace(String* s) {
    Str str = string_as_str(s);
    return str_trim_whitespace(&str);
}

Vec string_split_whitespace(String* s) {
    Str str = string_as_str(s);
    return str_split_whitespace(&str);
}

Vec string_split_lines(String* s) {
    Str str = string_as_str(s);
    return str_split_lines(&str);
}

char* string_as_cstr(String* s) {
    return s->__data;
}

void string_clear(String* s) {
    if (s->__data == NULL)
        return;
    memset(s->__data, '\0', s->__len);
    s->__len = 0;
}

void string_drop_inner(void* string_ptr) {
    String* s = (String*)string_ptr;
    if (s->__data == NULL)
        return;
    free(s->__data);
    s->__len = 0;
    s->__cap = 0;
}


/* ----------- Str -------------- */


Str str_from_cstr(const char* cstr) {
    size_t len = strlen(cstr);
    Str s = { .__data=cstr, .__len=len };
    return s;
}


Str str_from_ptr_len(const char* ptr, size_t len) {
    Str s = { .__data=ptr, .__len=len };
    return s;
}

Str str_trim_whitespace(Str* s) {
    size_t start = 0;
    size_t end = s->__len;
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

    Str str = { .__data=(s->__data + start), .__len=len };
    return str;
}

size_t str_len(Str* str) {
    return str->__len;
}

Vec str_split_lines(Str* str) {
    const char* ptr = str->__data;
    size_t len = str->__len;
    size_t start = 0;
    size_t end = 0;
    Vec v = vec_new(sizeof(Str));
    while (start <= len) {
        end = start;
        while (end <= len && *(ptr + end) != '\n') {
            end++;
        }
        if (end >= len) {
            end = len;
        }
        Str str = str_from_ptr_len((ptr + start), (end - start));
        vec_push(&v, &str);
        start = end + 1;
    }
    return v;
}

Vec str_split_whitespace(Str* str) {
    const char* ptr = str->__data;
    size_t len = str->__len;
    size_t start = 0;
    size_t end = 0;
    Vec v = vec_new(sizeof(Str));
    while (start < len) {
        while (start < len && isspace(*(ptr + start))) {
            start++;
        }
        if (start >= len) {
            break;
        }
        end = start;
        while (end < len && !isspace(*(ptr + end))) {
            end++;
        }
        Str str = str_from_ptr_len((ptr + start), (end - start));
        vec_push(&v, &str);
        start = end + 1;
    }
    return v;
}

const char* str_as_ptr(Str* str) {
    return str->__data;
}

String str_to_owned_string(Str* str) {
    String s = string_with_capactiy(str->__len);
    string_push_str(&s, str);
    return s;
}

char str_index(Str* str, size_t ind) {
    if (ind >= str->__len) {
        fprintf(stderr, "Out of bounds: strlen: %lu, index: %lu", str->__len, ind);
        abort();
    }
    return str->__data[ind];
}

const char* str_index_ref(Str* str, size_t ind) {
    if (ind >= str->__len) {
        fprintf(stderr, "Out of bounds: strlen: %lu, index: %lu", str->__len, ind);
        abort();
    }
    return str->__data + ind;
}

uint8_t str_eq(Str* str1, Str* str2) {
    if (str1 == str2)
        return 0;

    size_t len = str_len(str1);
    if (len != str_len(str2))
        return 1;

    for (size_t i = 0; i < len; i++) {
        if (str_index(str1, i) != str_index(str2, i))
            return 1;
    }
    return 0;
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
    String s = { .__data=content, .__len=len, .__cap=len };
    return s;
}


/* ----------- Vec ------------- */


Vec vec_new(size_t item_size) {
    Vec v = { .__data=NULL, .__item_size=item_size, .__len=0, .__cap=0 };
    return v;
}

Vec vec_with_capacity(size_t item_size, size_t cap) {
    void* data = malloc(cap * item_size);
    if (data == NULL) {
        fprintf(stderr, "Vec alloc failure\n");
        abort();
    }
    Vec v = { .__data=data, .__item_size=item_size, .__len=0, .__cap=cap };
    return v;
}

size_t vec_len(Vec* v) {
    return v->__len;
}

size_t vec_cap(Vec* v) {
    return v->__cap;
}

void vec_resize(Vec* v, size_t new_cap) {
    if (new_cap == 0)
        new_cap = 16;

    void* data = realloc(v->__data, new_cap * v->__item_size);
    if (data == NULL) {
        fprintf(stderr, "Vec resize failure\n");
        abort();
    }
    v->__data = data;
    v->__cap = new_cap;
}

void vec_push(Vec* v, void* obj) {
    size_t avail = v->__cap - v->__len;
    if (avail == 0) {
        size_t new_cap = __inc_cap(v->__cap);
        vec_resize(v, new_cap);
    }
    char* offset = (char*)v->__data + (v->__len * v->__item_size);
    memcpy((void*)offset, obj, v->__item_size);
    v->__len++;
}

void* vec_index_ref(Vec* v, size_t ind) {
    if (ind >= v->__len) {
        fprintf(stderr, "Out of bounds: veclen: %lu, index: %lu", v->__len, ind);
        abort();
    }
    char* offset = (char*)v->__data + (ind * v->__item_size);
    return (void*)offset;
}

void vec_iter_ref(Vec* v, mapFn func) {
    size_t len = vec_len(v);
    for (size_t i = 0; i < len; i++) {
        void* ref = vec_index_ref(v, i);
        func(ref);
    }
}

void vec_clear(Vec* v, mapFn drop) {
    if (v->__data == NULL)
        return;
    vec_iter_ref(v, drop);
    v->__len = 0;
}

void vec_drop_inner(void* vec_ptr) {
    Vec* v = (Vec*)vec_ptr;
    if (v->__data == NULL)
        return;
    free(v->__data);
    v->__len = 0;
    v->__cap = 0;
}

void vec_drop_inner_each(Vec* v, mapFn drop) {
    if (v->__data == NULL)
        return;
    vec_iter_ref(v, drop);
    free(v->__data);
    v->__len = 0;
    v->__cap = 0;
}

