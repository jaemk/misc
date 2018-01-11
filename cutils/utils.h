#ifndef ADVENT_UTILS_H
#define ADVENT_UTILS_H

#include <stdlib.h>


/* String
 *
 * Growable string, owns its data
 */
typedef struct {
    char* __data;
    size_t len, cap;
} String;


/* Str
 * Borrowed slice of a string
 */
typedef struct {
    char* __data;
    size_t __start, len;
} Str;


/* Vec
 * Owned array of generic data of `__item_size`
 */
typedef struct {
    void* __data;
    size_t __item_size, len, cap;
} Vec;


/* Slice
 * Borrowed array of generic data with `__item_size`
 */
typedef struct {
    void* __data;
    size_t __item_size, len;
} Slice;


/* -------------------------- */
/* ---- String functions ---- */
/* -------------------------- */
/* Construct a new empty String */
String string_new();

/* Construct a new empty String with the given capacity */
String string_with_capacity(size_t cap);

/* Construct a new String, copying the contents from the given `char*` */
String string_copy_from_cstr(char* cstr);

/* Reads contents of a file into a string,
 * returning ownership of the data.
 */
String read_file(const char* path);

/* Resize the given String with the new size */
void string_resize(String* s, size_t new_cap);

/* Push a char on the end of the String, resizing if necessary */
void string_push_char(String* s, char c);

/* Push bytes copied from a `Str` onto the end of the String, resizing if necessary */
void string_push_str(String* s, Str* str);

/* Push as most `str_len` bytes copied from a `char*` onto the end of the String, resizing if necessary.
 * `str_len` bytes is expected to be the size of the cstr not including the trailing nul byte.
 */
void string_push_cstr_bound(String* s, char* cstr, size_t str_len);

/* Same as `string_push_cstr_bound` except all of the char* up to the nul byte will be pushed */
void string_push_cstr(String* s, char* cstr);

/* Index into a String */
char string_index(String* s, size_t ind);

/* Return a pointer into a String at the given index*/
char* string_index_ref(String* s, size_t ind);

/* Convert String to a Str */
Str string_as_str(String* s);

/* Trim surrounding whitespace from a String, retuning a borrowed Str view */
Str string_trim_whitespace(String* s);

/* Convert a String to a `char*` */
char* string_as_cstr(String* s);

/* free the inner data held by a String */
void string_drop_inner(void* string_ptr);


/* -------------------------- */
/* ----- Str functions ------ */
/* -------------------------- */
/* Construct a new Str from a char* without copying any data */
Str str_from_cstr(char* cstr);

/* Trim surrounding whitespace returning another borrowed Str */
Str str_trim_whitespace(Str* s);

/* Split str by whitespace into a `Vec` of `Str*` */
void str_split_whitespace(Vec* v, Str* s);

/* Convert to an owned String, copying internal data to the new String */
String str_to_owned_string(Str* str);

/* Index into a St */
char str_index(Str* str, size_t ind);

/* Return a pointer into a Str at the given index*/
char* str_index_ref(Str* s, size_t ind);


/* -------------------------- */
/* ----- Vec functions ------ */
/* -------------------------- */
/* Construct a new empty `Vec` */
Vec vec_new(size_t item_size);

/* Construct a new empty `Vec` with the given capacity */
Vec vec_with_capacity(size_t item_size, size_t cap);

/* Resize the given `Vec` with the new capacity */
void vec_resize(Vec* v, size_t new_cap);

/* Push an object of size `Vec->__item_size` onto the given `Vec`, resizing if necessary.
 * Note, the provided `obj` pointer is expected to point to something that is
 * the same size as `Vec->__item_size`. The bytes behind the `obj` pointer will
 * be `memcpy`d onto the beck of the `Vec`.
 */
void vec_push(Vec* v, void* obj);

/* Return a pointer to an item at the given index
 *
 * Example:
 * ```
 * Str str_ = str_from_cstr("stringy string");
 * Str* str = &str_;
 * Vec v = vec_new(sizeof(Str*));
 * vec_push(&v, &str);
 * Str* str_ref = *(Str**)vec_index_ref(&v, 0);
 * ```
 */
void* vec_index_ref(Vec* v, size_t ind);

/* Free the inner data held by a `Vec` */
void vec_drop_inner(void* vec_ptr);

typedef void (*dropFn)(void*);

/* Free the inner data held by a `Vec` after applying
 * the given `drop` function to each element
 */
void vec_drop_inner_each(Vec* v, dropFn drop);


#endif

