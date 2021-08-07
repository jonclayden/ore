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

char * ore_realloc (const void *ptr, const size_t new_len, const size_t old_len, const int element_size);

encoding_t * ore_encoding (const char *name, OnigEncoding onig_enc, cetype_t *r_enc);

Rboolean ore_consistent_encodings (OnigEncoding first, OnigEncoding second);

const char * ore_iconv (void *iconv_handle, const char *old);

text_t * ore_text (SEXP text_);

text_element_t * ore_text_element (text_t *text, const size_t index, const Rboolean incremental, text_element_t *previous);

SEXP ore_text_element_to_rchar (text_element_t *element);

SEXP ore_string_to_rchar (const char *string, encoding_t *encoding);

void ore_text_done (text_t *text);

#endif
