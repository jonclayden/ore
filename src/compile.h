#ifndef _COMPILE_H_
#define _COMPILE_H_

#include "onigmo.h"
#include "text.h"

void ore_regex_finaliser (SEXP regex_ptr);

int ore_store_name (const UChar *name, const UChar *name_end, int n_groups, int *group_numbers, regex_t *regex, void *arg);

regex_t * ore_compile (const char *pattern, const char *options, encoding_t *encoding, const char *syntax_name);

regex_t * ore_retrieve (SEXP regex_, encoding_t *encoding);

char * ore_build_pattern (SEXP pattern_);

SEXP ore_build (SEXP pattern_, SEXP options_, SEXP encoding_name_, SEXP syntax_name_);

#endif
