#ifndef ADVENT_UTILS_H
#define ADVENT_UTILS_H

#include <stdlib.h>


/* Reads contents of a file into a string,
 * returning ownership of the data.
 */
char* read_file(const char* path);


/* String
 *
 * Growable string.
 * Owns its data unless constructed with `string_from_cstr`.
 */
typedef struct {
    char* data;
    size_t len, cap;
} String;


/* Str
 * Borrowed slice of a string
 */
typedef struct {
    String* source;
    size_t start, len;
} Str;

/* -------------------------- */
/* ---- String functions ---- */
/* -------------------------- */
/* Construct a new empty String */
String string_new();

/* Construct a new empty String with the given capacity */
String string_with_capacity(size_t cap);

/* Construct a new String from a `char*` without copying any data.
 * In this case, data may not be owned by the returned String */
String string_from_cstr(char* cstr);

/* Construct a new String, copying the contents from the given `char*` */
String string_copy_from_cstr(char* cstr);

/* Resize the given String with the new size */
void string_resize(String* s, size_t new_cap);

/* Push a char on the end of the String, resizing if necessary */
void string_push_char(String* s, char c);

/* Push bytes copied from a `Str` onto the end of the String, resizing if necessary */
void string_push_str(String* s, Str* str);

/* Push bytes copied from a `char*` onto the end of the String, resizing if necessary */
void string_push_cstr(String* s, char* cstr, size_t str_len);

/* Index into a String */
char string_index(String* s, size_t ind);

/* Convert String to a Str */
Str string_as_str(String* s);

/* Trim surrounding whitespace from a String, retuning a borrowed Str view */
Str string_trim_whitespace(String* s);

/* Convert a String to a `char*` */
char* string_as_cstr(String* s);

/* free the inner data held by a String */
void string_drop_inner(String* s);


/* -------------------------- */
/* ----- Str functions ------ */
/* -------------------------- */
char str_index(Str* str, size_t ind);
char* str_as_cstr(Str* str);


#endif

