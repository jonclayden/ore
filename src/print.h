#ifndef _PRINT_H_
#define _PRINT_H_

SEXP ore_get_list_element (SEXP list, const char *name);

SEXP ore_print_match (SEXP match, SEXP context_, SEXP width_, SEXP max_lines_);

#endif
