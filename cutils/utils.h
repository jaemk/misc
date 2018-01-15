#ifndef CUTILS_H
#define CUTILS_H

#include <stdlib.h>
#include <stdint.h>


/* String
 *
 * Growable string, owns its data
 */
typedef struct {
    char* __data;
    size_t __len, __cap;
} String;


/* Str
 * Borrowed slice of a string
 */
typedef struct {
    const char* __data;
    size_t __len;
} Str;


/* Vec
 * Owned array of generic data of `__item_size`
 */
typedef struct {
    void* __data;
    size_t __item_size, __len, __cap;
} Vec;


/* Slice
 * Borrowed array of generic data with `__item_size`
 */
typedef struct {
    void* __data;
    size_t __item_size, __len;
} Slice;


/* Function used to modify elements in a container
 * Used by containers, like `Vec`, as a "drop function" to allow
 * cleaning up elements, which may be or contain owned pointers
 * to other memory, before dropping the container's backing data.
 */
typedef void (*mapFn)(void*);


/* Function to compare equality of two elements, returning
 * a non-zero value if elements are unequal.
 * Used by containers, like `Vec`, as a way to compare
 * equality of two `Vec`s of arbitrary elements.
 */
typedef uint8_t (*cmpEq)(void*, void*);


/* -------------------------- */
/* ---- String functions ---- */
/* -------------------------- */
/* Construct a new empty String */
String string_new();

/* Construct a new empty String with the given capacity */
String string_with_capacity(size_t cap);

/* Construct a new String, copying the contents from the given `char*` */
String string_copy_from_cstr(const char* cstr);

/* Reads contents of a file into a string,
 * returning ownership of the data.
 */
String read_file(const char* path);

/* Return current `String` length */
size_t string_len(String* s);

/* Return current `String` capacity */
size_t string_cap(String* s);

/* Resize the given String with the new size */
void string_resize(String* s, size_t new_cap);

/* Push a char on the end of the String, resizing if necessary */
void string_push_char(String* s, char c);

/* Push bytes copied from a `Str` onto the end of the String, resizing if necessary */
void string_push_str(String* s, Str* str);

/* Push at most `str_len` bytes copied from a `char*` onto the end of the String, resizing if necessary.
 * `str_len` bytes is expected to be the size of the cstr not including the trailing null byte.
 */
void string_push_cstr_bound(String* s, const char* cstr, size_t str_len);

/* Same as `string_push_cstr_bound` except all of the char* up to the null byte will be pushed */
void string_push_cstr(String* s, const char* cstr);

/* Index into a String */
char string_index(String* s, size_t ind);

/* Return a pointer into a String at the given index*/
char* string_index_ref(String* s, size_t ind);

/* Compare two `String`s for equality, returning a non-zero value
 * when `String`s are unequal
 */
uint8_t string_eq(String* s1, String* s2);

/* Convert String to a Str */
Str string_as_str(String* s);

/* Trim surrounding whitespace from a String, retuning a borrowed Str view */
Str string_trim_whitespace(String* s);

/* Split String by line breaks into a `Vec` of `Str*` */
Vec string_split_lines(String* s);

/* Split String by whitespace into a `Vec` of `Str*` */
Vec string_split_whitespace(String* s);

/* Split specified `String` by the specified (`char*`)
 * substring into a `Vec` of `Str`
 */
Vec string_split_by_cstr(String* s, const char* cstr_pattern);

/* Split specified `String` by the specified (`Str`)
 * substring into a `Vec` of `Str`
 */
Vec string_split_by_str(String* s, Str* pattern);

/* Convert a String to a `char*` */
char* string_as_cstr(String* s);

/* Zero out the contents of the current string and set
 * the length of the `String` to zero. This will not affect the current
 * capacity of the `String`. Subsequent writes will overwrite any existing data.
 */
void string_clear(String* s);

/* free the inner data held by a String */
void string_drop(void* string_ptr);


/* -------------------------- */
/* ----- Str functions ------ */
/* -------------------------- */
/* Construct a new Str from a char* without copying any data */
Str str_from_cstr(const char* cstr);

/* Construct a new Str from a char* and length without copying any data.
 * Allows constructing a `Str` from an arbitrary slice of chars that may
 * not be null-terminated.
 */
Str str_from_ptr_len(const char* cstr, size_t len);

/* Return current `Str` length */
size_t str_len(Str* s);

/* Trim surrounding whitespace returning another borrowed Str */
Str str_trim_whitespace(Str* s);

/* Split Str by line breaks into a `Vec` of `Str` */
Vec str_split_lines(Str* s);

/* Split str by whitespace into a `Vec` of `Str` */
Vec str_split_whitespace(Str* s);

/* Split specified `Str` by the specified (`char*`)
 * substring into a `Vec` of `Str`
 */
Vec str_split_by_cstr(Str* s, const char* cstr_pattern);

/* Split specified `Str` by the specified (`Str`)
 * substring into a `Vec` of `Str`
 */
Vec str_split_by_str(Str* s, Str* pattern);

/* Return a pointer to the inner `Str` data
 * Note, the returned char* is not guaranteed to be
 * a valid null-terminated c-string since the `Str`
 * could be a slice inside of some larger `String`.
 *
 * To get a null-terminated c-string from a `Str`, a copy
 * should be made and converted:
 * ```
 * Str* str = ...;
 * String s = str_to_owned_string(str);
 * char* inner = string_as_cstr(&s);
 * ```
 */
const char* str_as_ptr(Str* s);

/* Convert to an owned String, copying internal data to the new String */
String str_to_owned_string(Str* str);

/* Index into a St */
char str_index(Str* str, size_t ind);

/* Return a pointer into a Str at the given index*/
const char* str_index_ref(Str* s, size_t ind);

/* Compare two `Str`s for equality, returning a non-zero value
 * when `Str`s are unequal
 */
uint8_t str_eq(Str* str1, Str* str2);


/* -------------------------- */
/* ----- Vec functions ------ */
/* -------------------------- */
/* Construct a new empty `Vec` */
Vec vec_new(size_t item_size);

/* Construct a new empty `Vec` with the given capacity */
Vec vec_with_capacity(size_t item_size, size_t cap);

/* Return current `Vec` length */
size_t vec_len(Vec* v);

/* Return current `Vec` capacity */
size_t vec_cap(Vec* v);

/* Resize the given `Vec` with the new capacity */
void vec_resize(Vec* v, size_t new_cap);

/* Push an object of size `Vec->__item_size` onto the given `Vec`, resizing if necessary.
 * Note, the provided `obj` pointer is expected to point to something that is
 * the same size as `Vec->__item_size`. The bytes behind the `obj` pointer will
 * be `memcpy`d onto the back of the `Vec`.
 */
void vec_push(Vec* v, void* obj);

/* Insert an object of size `Vec->__item_size` into the given `Vec`, resizing if necessary.
 * Note, the provided `obj` pointer is expected to point to something that is
 * the same size as `Vec->__item_size`. The bytes behind the `obj` pointer will
 * be `memcpy`d into the location in the `Vec`. All trailing elements will be shifted
 * `__item_size` bytes to the right. Inserting on the end (index = len) behaves like a `push`.
 */
void vec_insert(Vec* v, void* obj, size_t index);

/* Remove an object of size `Vec->__item_size` from the given `Vec`.
 * As elements are removed the `Vec` capacity will remain unchanged.
 */
void vec_remove(Vec* v, size_t index);

/* Remove an object of size `Vec->__item_size` from the given `Vec` after
 * applying the `drop` function to the element's pointer.
 * As elements are removed the `Vec` capacity will remain unchanged.
 */
void vec_remove_with(Vec* v, size_t index, mapFn drop);

/* Return a pointer to an item at the given index
 *
 * Example:
 * ```
 * Str str_ = str_from_cstr("stringy string");
 * Str* str = &str_;
 * Vec v = vec_new(sizeof(Str*));
 * vec_push(&v, &str);
 * Str* str_ref = *(Str**)vec_index_ref(&v, 0);
 * assert(str_ref == str);
 * ```
 */
void* vec_index_ref(Vec* v, size_t ind);

/* Apply the `func` function to each element in the `Vec`.
 */
void vec_iter_ref(Vec* v, mapFn func);

/* Compare two `Vec`s for equality, returning a non-zero value
 * when `Vec`s are unequal. Uses `cmp_func` to check elements
 * for equality.
 */
uint8_t vec_eq(Vec* v1, Vec* v2, cmpEq cmp_func);

/* Apply the `drop` function to each element and then set
 * the length of the `Vec` to zero. This will not affect the current
 * capacity of the `Vec`. Subsequent writes will overwrite any existing data.
 */
void vec_clear(Vec* v, mapFn drop);

/* Free the inner data held by a `Vec` after applying
 * the given `drop` function to each element
 */
void vec_drop_with(Vec* v, mapFn drop);

/* Free the inner data held by a `Vec`
 * Use `vec_drop_with` if the elements need to be cleaned
 * up before being cleared out, e.g. a vec of pointers.
 */
void vec_drop(void* vec_ptr);


#endif

