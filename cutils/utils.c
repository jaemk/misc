#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "utils.h"


/* ----------- String ------------- */


String string_new() {
    String s = { .__data=NULL, .len=0, .cap=0 };
    return s;
}

String string_with_capactiy(size_t cap) {
    char* data = malloc((cap + 1) * sizeof(char));
    if (data == NULL) {
        fprintf(stderr, "String alloc failure\n");
        abort();
    }
    memset(data, '\0', cap + 1);
    String s = { .__data=data, .len=0, .cap=cap };
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
    String s = { .__data=data, .len=len, .cap=len };
    return s;
}

void string_resize(String* s, size_t new_cap) {
    if (new_cap == 0)
        new_cap = 16;

    size_t n_trailing = (new_cap + 1) - s->len;
    char* data = realloc(s->__data, (new_cap + 1) * sizeof(char));
    if (data == NULL) {
        fprintf(stderr, "String resize failure\n");
        abort();
    }
    memset(data + s->len, '\0', n_trailing);
    s->__data = data;
    s->cap = new_cap;
}

size_t __inc_cap(size_t current) {
    if (current > 8192) {
        return current + 8192;
    } else {
        return current * 2;
    }
}

void string_push_char(String* s, char c) {
    size_t avail = s->cap - s->len;
    if (avail == 0) {
        size_t new_cap = __inc_cap(s->cap);
        string_resize(s, new_cap);
    }
    s->__data[s->len] = c;
    s->len += 1;
}

void string_push_str(String* s, Str* str) {
    string_push_cstr_bound(s, str->__data+str->__start, str->len);
}

void string_push_cstr(String* s, char* cstr) {
    size_t len = strlen(cstr);
    string_push_cstr_bound(s, cstr, len);
}

void string_push_cstr_bound(String* s, char* cstr, size_t str_len) {
    size_t avail = s->cap - s->len;
    if (str_len > avail) {
        size_t new_cap = __inc_cap(s->cap);
        if (str_len > (new_cap - s->len)) {
            new_cap += str_len - (new_cap - s->len);
        }
        string_resize(s, new_cap);
    }
    size_t count = 0;
    while (*cstr && count < str_len) {
        s->__data[s->len] = *cstr;
        s->len++;
        count++;
        cstr++;
    }
}

char string_index(String* s, size_t index) {
    if (index >= s->len) {
        fprintf(stderr, "Out of bounds: strlen: %lu, index: %lu", s->len, index);
        abort();
    }
    return s->__data[index];
}

char* string_index_ref(String* s, size_t index) {
    if (index >= s->len) {
        fprintf(stderr, "Out of bounds: strlen: %lu, index: %lu", s->len, index);
        abort();
    }
    return s->__data + index;
}

Str string_as_str(String* s) {
    Str str = { .__data=s->__data, .__start=0, .len=s->len };
    return str;
}

Str string_trim_whitespace(String* s) {
    Str str = string_as_str(s);
    return str_trim_whitespace(&str);
}

char* string_as_cstr(String* s) {
    return s->__data;
}

void string_drop_inner(void* string_ptr) {
    String* s = (String*)string_ptr;
    if (s->__data != NULL)
        free(s->__data);
}


/* ----------- Str -------------- */


Str str_from_cstr(char* cstr) {
    size_t len = strlen(cstr);
    Str s = { .__data=cstr, .__start=0, .len=len };
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

    Str str = { .__data=s->__data, .__start=start, .len=len };
    return str;
}

/* void str_split_whitespace(Vec* v, Str* str) { */
/*     char* ptr = str->__data + str->__start */
/*     return vec_with_capacity(sizeof(Str*), str->len); */
/* } */

String str_to_owned_string(Str* str) {
    String s = string_with_capactiy(str->len);
    string_push_str(&s, str);
    return s;
}

char str_index(Str* str, size_t ind) {
    if (ind >= str->len) {
        fprintf(stderr, "Out of bounds: strlen: %lu, index: %lu", str->len, ind);
        abort();
    }
    return str->__data[ind+str->__start];
}

char* str_index_ref(Str* str, size_t ind) {
    if (ind >= str->len) {
        fprintf(stderr, "Out of bounds: strlen: %lu, index: %lu", str->len, ind);
        abort();
    }
    return str->__data + ind + str->__start;
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
    String s = { .__data=content, .len=len, .cap=len };
    return s;
}


/* ----------- Vec ------------- */


Vec vec_new(size_t item_size) {
    Vec v = { .__data=NULL, .__item_size=item_size, .len=0, .cap=0 };
    return v;
}

Vec vec_with_capacity(size_t item_size, size_t cap) {
    void* data = malloc(cap * item_size);
    if (data == NULL) {
        fprintf(stderr, "Vec alloc failure\n");
        abort();
    }
    Vec v = { .__data=data, .__item_size=item_size, .len=0, .cap=cap };
    return v;
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
    v->cap = new_cap;
}

void vec_push(Vec* v, void* obj) {
    size_t avail = v->cap - v->len;
    if (avail == 0) {
        size_t new_cap = __inc_cap(v->cap);
        vec_resize(v, new_cap);
    }
    char* offset = (char*)v->__data + (v->len * v->__item_size);
    memcpy((void*)offset, obj, v->__item_size);
    v->len++;
}

void* vec_index_ref(Vec* v, size_t ind) {
    if (ind >= v->len) {
        fprintf(stderr, "Out of bounds: veclen: %lu, index: %lu", v->len, ind);
        abort();
    }
    char* offset = (char*)v->__data + (ind * v->__item_size);
    return (void*)offset;
}

void vec_drop_inner(void* vec_ptr) {
    Vec* v = (Vec*)vec_ptr;
    if (v->__data != NULL)
        free(v->__data);
}

void vec_drop_inner_each(Vec* v, dropFn drop) {
    if (v->__data == NULL)
        return;
    for (size_t i = 0; i < v->len; i++) {
        char* offset = (char*)v->__data + (i * v->__item_size);
        drop((void*)offset);
    }
    free(v->__data);
}

