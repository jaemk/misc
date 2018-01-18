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


/* Function that produces a 64bit hash value from a known pointer
 */
typedef uint64_t (*hashFn)(void*);

/* HashMap
 * Generic hashmap container
 * Requires user to provide `hashFn` (hash-key),
 * `cmpEq` (cmp-item), and `mapFn` (drop-key & drop-item) functions.
 * that operate on the type of object stored.
 */
typedef struct {
    Vec __buckets;
    size_t __key_size, __item_size, __len, __cap;
    double __load_factor;
    hashFn __hash;
    cmpEq __cmp;
    mapFn __drop_key;
    mapFn __drop_item;
} HashMap;

/* HashMapKV
 * Holds pointers to an associated key & value
 */
typedef struct {
    size_t hash_key;
    void* key;
    void* value;
} HashMapKV;

/* HashMapIter
 * Iterator over key & value references of a HashMap.
 */
typedef struct {
    HashMap* __map;
    size_t __count;
    size_t __bucket_ind;
    size_t __bucket_inner_ind;
} HashMapIter;



void utils_noop();


/* -------------------------- */
/* ---- String functions ---- */
/* -------------------------- */
/* Construct a new empty String */
String string_new();

/* Construct a new empty String with the given capacity */
String string_with_capacity(size_t cap);

/* Construct a new String, copying the contents from the given `String*` */
String string_copy(String* s);

/* Construct a new String, copying the contents from the given `Str*` */
String string_copy_from_str(Str* str);

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

/* Resize the given String with the new size.
 * The new capacity is expected to be greater than the current.
 * If the new capacity is smaller, trailing data will be dropped.
 */
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

/* Return a pointer into a String at the given index.
 * Note, references may be invalidated when the container is resized.
 */
char* string_index_ref(String* s, size_t ind);

/* Compare two `String`s for equality, returning a non-zero value
 * when `String`s are unequal
 */
uint8_t string_eq(void* s1, void* s2);

/* Calculate the hash of a `String` and its contents */
uint64_t string_hash(void* s);

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
uint8_t str_eq(void* str1, void* str2);

/* Calculate the hash of a `Str` and its contents */
uint64_t str_hash(void* str);


/* -------------------------- */
/* ----- Vec functions ------ */
/* -------------------------- */
/* Construct a new empty `Vec` */
Vec vec_new(size_t item_size);

/* Construct a new empty `Vec` with the given capacity */
Vec vec_with_capacity(size_t item_size, size_t cap);

/* Construct a new `Vec`, a bitwise copy of `src` */
Vec vec_copy(Vec* src);

/* Return current `Vec` length */
size_t vec_len(Vec* v);

/* Return current `Vec` capacity */
size_t vec_cap(Vec* v);

/* Calculate the hash of a `Vec` by combining the hash codes produced
 * by applying `hash_func` to each element.
 * https://stackoverflow.com/questions/1646807/quick-and-simple-hash-code-combinations
 */
uint64_t vec_hash_with(void* v, hashFn hash_func);

/* Resize the given `Vec` with the new capacity.
 * The new capacity is expected to be greater than the current.
 * If the new capacity is smaller, trailing data will be dropped
 * which may result in leaked memory if dropped elements are/hold
 * pointers that need to be cleaned up.
 */
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
 * Note, references may be invalidated when the container is resized.
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
uint8_t vec_eq(void* v1, void* v2, cmpEq cmp_func);

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


/* -------------------------- */
/* ----- Hash functions ----- */
/* -------------------------- */
/* Apply the fnv-1 64bit hash function to an arbitrary set of bytes */
uint64_t fnv_64(void* ptr, size_t num_bytes);

/* Construct a new HashMap with zero capacity */
HashMap hashmap_new(size_t key_size, size_t item_size, hashFn hash_func, cmpEq cmp_func, mapFn drop_key, mapFn drop_item);

/* Construct a new HashMap with the given capacity */
HashMap hashmap_with_capacity(size_t key_size, size_t item_size, size_t capacity,
                              hashFn hash_func, cmpEq cmp_func, mapFn drop_key, mapFn drop_item);

/* Construct a new HashMap with the given properties */
HashMap hashmap_with_props(size_t key_size, size_t item_size, size_t capacity, double load_factor,
                           hashFn hash_func, cmpEq cmp_func, mapFn drop_key, mapFn drop_item);

void hashmap_drop(HashMap* hashmap);

/* Return current `HashMap` length */
size_t hashmap_len(HashMap* hashmap);

/* Return current `HashMap` capacity */
size_t hashmap_cap(HashMap* hashmap);

/* Resize the given `HashMap` with the new capacity.
 * The new capacity is expected to be greater than the current.
 * If the new capacity is smaller, trailing data will be dropped
 * which may result in leaked memory if dropped elements are/hold
 * pointers that need to be cleaned up.
 */
void hashmap_resize();

/* Insert a key, value pair, replacing any existing matching key.
 * The data behind both the `key` and `value` pointers will be bitwise copied.
 */
void hashmap_insert(HashMap* hashmap, void* key, void* value);

void hashmap_insert_with_hash(HashMap* map, void* key, void* value, size_t hash);

void* hashmap_get_ref(HashMap* hashmap, void* key);

HashMapIter hashmap_iter(HashMap* map);

uint8_t hashmap_iter_done(HashMapIter* iter);

HashMapKV* hashmap_iter_next(HashMapIter* iter);

#endif

