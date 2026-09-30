#ifndef _TEXT_H_
#define _TEXT_H_

#include <Rinternals.h>
#include "onigmo.h"

#define ORE_ENCODING_NAME_MAX_LEN   64

typedef enum {
    VECTOR_SOURCE,
    FILE_SOURCE,
    CONNECTION_SOURCE
} source_t;

typedef struct {
    char            name[ORE_ENCODING_NAME_MAX_LEN];
    OnigEncoding    onig_enc;
    cetype_t        r_enc;
    Rboolean        convert;
    Rboolean        assumed;
} encoding_t;

typedef struct {
    SEXP            object;
    size_t          length;
    source_t        source;
    void          * handle;
    encoding_t    * encoding;
} text_t;

typedef struct {
    const char    * start;
    const char    * end;
    encoding_t    * encoding;
    Rboolean        incomplete;
} text_element_t;

int ore_strnicmp (const char *str1, const char *str2, size_t num);

UChar * ore_step (OnigEncoding enc, const UChar *p, const UChar *end, size_t n);

char * ore_realloc (const void *ptr, const size_t new_len, const size_t old_len, const int element_size);

encoding_t * ore_encoding (const char *name, OnigEncoding onig_enc, cetype_t *r_enc);

encoding_t * ore_string_encoding (SEXP string);

Rboolean ore_consistent_encodings (encoding_t *text_encoding, OnigEncoding regex_enc);

void * ore_iconv_handle (encoding_t *encoding);

const char * ore_iconv (void *iconv_handle, const char *old, const size_t old_len, size_t *new_len);

void ore_iconv_done (void *iconv_handle);

text_t * ore_text (SEXP text_);

text_element_t * ore_text_element (text_t *text, const size_t index, const Rboolean incremental, text_element_t *previous);

SEXP ore_text_element_to_rchar (text_element_t *element);

SEXP ore_string_to_rchar (const char *string, encoding_t *encoding);

SEXP ore_bytes_to_rchar (const char *bytes, const size_t length, encoding_t *encoding);

SEXP ore_convert_bytes (void *iconv_handle, const char *bytes, const size_t length, const cetype_t r_enc);

void ore_text_done (text_t *text);

#endif
