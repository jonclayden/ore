#ifndef _COMPILE_H_
#define _COMPILE_H_

#include "onigmo.h"
#include "text.h"

regex_t * ore_compile (const char *pattern, const char *options, encoding_t *encoding, const char *syntax_name);

regex_t * ore_retrieve (SEXP regex_, encoding_t *encoding);

Rboolean ore_group_name_vector (SEXP vec, regex_t *regex);

SEXP ore_build (SEXP pattern_, SEXP options_, SEXP encoding_name_, SEXP syntax_name_);

#endif
