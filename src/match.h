#ifndef _MATCH_H_
#define _MATCH_H_

#include "onigmo.h"
#include "compile.h"

typedef struct {
    int     capacity;
    int     n_regions;
    int     n_matches;
    int   * offsets;
    int   * byte_offsets;
    int   * lengths;
    int   * byte_lengths;
    char ** matches;
} rawmatch_t;

typedef struct {
    char     * start;
    char     * end;
    Rboolean   more;
} file_contents_t;

rawmatch_t * ore_rawmatch_alloc (const int n_regions);

char * ore_realloc (const void *ptr, const size_t new_len, const size_t old_len, const int element_size);

void ore_rawmatch_extend (rawmatch_t *match);

void ore_rawmatch_store_string (rawmatch_t *match, const size_t loc, const char *string, const int length);

rawmatch_t * ore_search (regex_t *regex, const char *text, const char *text_end, const Rboolean all, const size_t start);

void ore_int_vector (SEXP vec, const int *data, const int n_regions, const int n_matches, const int increment);

void ore_char_vector (SEXP vec, const char **data, const int n_regions, const int n_matches, cetype_t encoding, const char *old_enc_name);

void ore_int_matrix (SEXP mat, const int *data, const int n_regions, const int n_matches, const SEXP col_names, const int increment);

void ore_char_matrix (SEXP mat, const char **data, const int n_regions, const int n_matches, const SEXP col_names, cetype_t encoding, const char *old_enc_name);

file_contents_t * ore_read_file (text_t *text, const int buffer_size_level);

SEXP ore_search_all (SEXP regex_, SEXP text_, SEXP all_, SEXP start_, SEXP simplify_, SEXP incremental_);

#endif
