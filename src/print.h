#ifndef _PRINT_H_
#define _PRINT_H_

typedef struct {
    Rboolean use_colour;
    int width;
    int max_lines;
    int lines_done;
    int n_matches;
    
    Rboolean in_match;
    int loc;
    int current_match;
    char current_match_string[12];
    char *current_match_loc;
    
    char *match;
    char *match_start;
    char *context;
    char *context_start;
    char *number;
    char *number_start;
} printstate_t;

SEXP ore_get_list_element (SEXP list, const char *name);

printstate_t * ore_alloc_printstate (const int context, const int width, const int max_lines, const Rboolean use_colour, const int n_matches, const int max_enc_len);

Rboolean ore_more_lines (printstate_t *state);

void ore_print_line (printstate_t *state);

void ore_switch_state (printstate_t *state, Rboolean match);

void ore_do_push_byte (printstate_t *state, const char byte, const int width);

void ore_push_byte (printstate_t *state, const char byte, const int width);

UChar * ore_push_chars (printstate_t *state, UChar *ptr, int n, OnigEncoding encoding);

SEXP ore_print_match (SEXP match, SEXP context_, SEXP width_, SEXP max_lines_, SEXP use_colour_);

#endif
