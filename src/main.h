#ifndef _MAIN_H_
#define _MAIN_H_

SEXP ore_init ();

SEXP ore_done ();

void ore_regex_finaliser (SEXP regex_ptr);

int ore_store_name (const UChar *name, const UChar *name_end, int n_groups, int *group_numbers, regex_t *regex, void *arg);

SEXP ore_compile (SEXP pattern_, SEXP options_, SEXP encoding_);

SEXP ore_search (SEXP regex_ptr, SEXP text_, SEXP all_, SEXP start_);

SEXP ore_split (SEXP text_, SEXP n_matches_, SEXP offsets_, SEXP lengths_);

SEXP ore_substitute (SEXP text_, SEXP n_matches_, SEXP offsets_, SEXP lengths_, SEXP replacements_);

#endif
