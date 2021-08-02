#ifndef _COMPILE_H_
#define _COMPILE_H_

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
    size_t       (* read)(void *, void *, size_t, int);
    OnigEncoding    encoding;
} text_t;

int ore_strnicmp (const char *str1, const char *str2, size_t num);

void ore_regex_finaliser (SEXP regex_ptr);

int ore_store_name (const UChar *name, const UChar *name_end, int n_groups, int *group_numbers, regex_t *regex, void *arg);

OnigEncoding ore_r_to_onig_enc (cetype_t encoding);

OnigEncoding ore_name_to_onig_enc (const char *enc);

text_t * ore_text (SEXP text_);

regex_t * ore_compile (const char *pattern, const char *options, OnigEncoding encoding, const char *syntax_name);

regex_t * ore_retrieve (SEXP regex_, OnigEncoding encoding);

char * ore_build_pattern (SEXP pattern_);

SEXP ore_build (SEXP pattern_, SEXP options_, SEXP encoding_name_, SEXP syntax_name_);

#endif
