#ifndef _PRINT_H_
#define _PRINT_H_

typedef struct {
    Rboolean use_colour;
    int width;
    
    Rboolean in_match;
    int loc;
    
    char *match;
    char *match_start;
    char *context;
    char *context_start;
    char *number;
} printstate_t;

SEXP ore_get_list_element (SEXP list, const char *name);

SEXP ore_print_match (SEXP match, SEXP context_, SEXP width_, SEXP max_lines_, SEXP use_colour_);

#endif
