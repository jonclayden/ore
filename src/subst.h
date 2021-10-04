#ifndef _SUBST_H_
#define _SUBST_H_

typedef struct {
    int     n;
    int   * offsets;
    int   * lengths;
    int   * group_numbers;
} backref_info_t;

char * ore_substitute (const char *text, const int n_matches, const int *offsets, const int *lengths, const char **replacements);

SEXP ore_substitute_substrings (SEXP text_, SEXP n_matches_, SEXP offsets_, SEXP lengths_, SEXP replacements_);

backref_info_t * ore_find_backrefs (const char *replacement, SEXP group_names);

SEXP ore_substitute_all (SEXP regex_, SEXP replacement_, SEXP text_, SEXP all_, SEXP environment, SEXP function_args);

SEXP ore_replace_all (SEXP regex_, SEXP replacement_, SEXP text_, SEXP all_, SEXP simplify_, SEXP environment, SEXP function_args);

SEXP ore_switch_all (SEXP text_, SEXP mappings_, SEXP options_, SEXP encoding_name_);

#endif
