#ifndef _COMPILE_H_
#define _COMPILE_H_

#include "onigmo.h"

int ore_strnicmp (const char *str1, const char *str2, size_t num);

void ore_regex_finaliser (SEXP regex_ptr);

int ore_store_name (const UChar *name, const UChar *name_end, int n_groups, int *group_numbers, regex_t *regex, void *arg);

OnigEncoding ore_r_to_onig_enc (cetype_t encoding);

OnigEncoding ore_name_to_onig_enc (const char *enc);

regex_t * ore_compile (const char *pattern, const char *options, OnigEncoding encoding, const char *syntax_name);

regex_t * ore_retrieve (SEXP regex_, SEXP text_, const Rboolean using_file);

char * ore_build_pattern (SEXP pattern_);

SEXP ore_build (SEXP pattern_, SEXP options_, SEXP encoding_name_, SEXP syntax_name_);

#endif
