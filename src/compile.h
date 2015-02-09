#ifndef _COMPILE_H_
#define _COMPILE_H_

#include "oniguruma.h"

void ore_regex_finaliser (SEXP regex_ptr);

int ore_store_name (const UChar *name, const UChar *name_end, int n_groups, int *group_numbers, regex_t *regex, void *arg);

regex_t * ore_compile (const char *pattern, const char *options, cetype_t encoding);

regex_t * ore_retrieve (SEXP regex_, SEXP text_);

SEXP ore_build (SEXP pattern_, SEXP options_, SEXP encoding_name_);

#endif
