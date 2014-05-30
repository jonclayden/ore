#ifndef _MAIN_H_
#define _MAIN_H_


SEXP chariot_init ();

SEXP chariot_done ();

void chariot_regex_finaliser (SEXP regex_ptr);

int chariot_store_name (const UChar *name, const UChar *name_end, int n_groups, int *group_numbers, regex_t *regex, void *arg);

SEXP chariot_compile (SEXP pattern_, SEXP options_);

SEXP chariot_search (SEXP regex_ptr, SEXP text_, SEXP start_);

#endif
