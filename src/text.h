#ifndef _TEXT_H_
#define _TEXT_H_

#include "onigmo.h"

typedef enum {
    VECTOR_SOURCE,
    FILE_SOURCE,
    CONNECTION_SOURCE
} source_t;

typedef struct {
    SEXP            object;
    size_t          length;
    source_t        source;
    void          * handle;
    OnigEncoding    encoding;
    const char    * encoding_name;
} text_t;

typedef struct {
    const char    * start;
    const char    * end;
    OnigEncoding    encoding;
    Rboolean        incomplete;
} text_element_t;

int ore_strnicmp (const char *str1, const char *str2, size_t num);

char * ore_realloc (const void *ptr, const size_t new_len, const size_t old_len, const int element_size);

OnigEncoding ore_r_to_onig_enc (cetype_t encoding);

cetype_t ore_onig_to_r_enc (OnigEncoding encoding);

OnigEncoding ore_name_to_onig_enc (const char *enc);

const char * ore_iconv (void *iconv_handle, const char *old);

text_t * ore_text (SEXP text_);

text_element_t * ore_text_element (text_t *text, const size_t index);

SEXP ore_text_element_to_rchar (text_element_t *element, const char *old_enc_name);

SEXP ore_string_to_rchar (const char *string, cetype_t encoding, const char *old_enc_name);

void ore_text_done (text_t *text);

#endif
