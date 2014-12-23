#ifndef _MAIN_H_
#define _MAIN_H_

typedef struct {
    int capacity;
    int n_regions;
    int n_matches;
    int *offsets;
    int *byte_offsets;
    int *lengths;
    int *byte_lengths;
    char **matches;
} rawmatch_t;

SEXP ore_init ();

SEXP ore_done ();

void ore_regex_finaliser (SEXP regex_ptr);

int ore_store_name (const UChar *name, const UChar *name_end, int n_groups, int *group_numbers, regex_t *regex, void *arg);

regex_t * ore_compile (const char *pattern, const char *options, cetype_t encoding);

SEXP ore_build (SEXP pattern_, SEXP options_, SEXP encoding_name_);

rawmatch_t * ore_rawmatch_alloc (const int n_regions);

char * ore_realloc (const void *ptr, const size_t new_len, const size_t old_len, const int element_size);

void ore_rawmatch_extend (rawmatch_t *match);

void ore_rawmatch_store_string (rawmatch_t *match, const size_t loc, const char *string, const int length);

rawmatch_t * ore_search (regex_t *regex, const char *text, const Rboolean all, const size_t start);

void ore_int_vector (SEXP vec, const int *data, const int n_regions, const int n_matches, const int increment);

void ore_char_vector (SEXP vec, const char **data, const int n_regions, const int n_matches, const cetype_t encoding);

void ore_int_matrix (SEXP mat, const int *data, const int n_regions, const int n_matches, const SEXP col_names, const int increment);

void ore_char_matrix (SEXP mat, const char **data, const int n_regions, const int n_matches, const SEXP col_names, const cetype_t encoding);

SEXP ore_search_all (SEXP regex_ptr, SEXP text_, SEXP all_, SEXP start_, SEXP simplify_, SEXP group_names);

SEXP ore_split (SEXP regex_ptr, SEXP text_, SEXP start_, SEXP simplify_);

SEXP ore_substitute (SEXP text_, SEXP n_matches_, SEXP offsets_, SEXP lengths_, SEXP replacements_);

#endif
