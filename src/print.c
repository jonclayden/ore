#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <string.h>

#include "compile.h"
#include "match.h"
#include "print.h"

extern UChar * onigenc_step (OnigEncoding enc, const UChar *p, const UChar *end, int n);
extern UChar * onigenc_step_back (OnigEncoding enc, const UChar *start, const UChar *s, int n);

SEXP ore_get_list_element (SEXP list, const char *name)
{
    SEXP element = R_NilValue;
    SEXP names = getAttrib(list, R_NamesSymbol);
    
    for (int i=0; i<length(names); i++)
    {
    	if (strcmp(CHAR(STRING_ELT(names,i)), name) == 0)
        {
    	   element = VECTOR_ELT(list, i);
    	   break;
       }
    }
    
    return element;
}

printstate_t * ore_alloc_printstate (const int context, const int width, const Rboolean use_colour, const int max_enc_len)
{
    printstate_t *state = (printstate_t *) R_alloc(1, sizeof(printstate_t));
    
    state->use_colour = use_colour;
    state->width = width - 9;
    
    state->in_match = FALSE;
    state->loc = 0;
    
    state->match = R_alloc(max_enc_len+9, width);
    state->match_start = state->match;
    
    if (use_colour)
        state->context = NULL;
    else
        state->context = R_alloc(max_enc_len+9, width);
    state->context_start = state->context;
    
    state->number = NULL;
    
    return state;
}

void ore_print_line (printstate_t *state)
{
    if (state->loc == 0)
        return;
    
    if (!state->use_colour)
    {
        *state->context = '\0';
        Rprintf("context: %s\n", state->context_start);
    }
    else if (state->in_match)
    {
        strncpy(state->match, "\x1b[0m", 4);
        state->match += 4;
    }
    *state->match = '\0';
    Rprintf("  match: %s\n\n", state->match_start);
    
    state->match = state->match_start;
    state->context = state->context_start;
    state->in_match = FALSE;
    state->loc = 0;
}

void ore_do_push_char (printstate_t *state, const char character, Rboolean match)
{
    if (match || state->use_colour)
    {
        *(state->match++) = character;
        if (!state->use_colour)
            *(state->context++) = ' ';
    }
    else
    {
        *(state->context++) = character;
        if (!state->use_colour)
            *(state->match++) = ' ';
    }
}

void ore_push_char (printstate_t *state, const char character, Rboolean match)
{
    int width;
    switch (character)
    {
        case '\t':
        case '\n':
        width = 2;
        break;
        
        default:
        width = 1;
    }
    
    if (state->loc + width >= state->width)
        ore_print_line(state);
    
    if (state->use_colour && match && !state->in_match)
    {
        strncpy(state->match, "\x1b[36m", 5);
        state->match += 5;
        state->in_match = TRUE;
    }
    else if (state->use_colour && !match && state->in_match)
    {
        strncpy(state->match, "\x1b[0m", 4);
        state->match += 4;
        state->in_match = FALSE;
    }
    
    switch (character)
    {
        case '\t':
        ore_do_push_char(state, '\\', match);
        ore_do_push_char(state, 't', match);
        break;
        
        case '\n':
        ore_do_push_char(state, '\\', match);
        ore_do_push_char(state, 'n', match);
        break;
        
        default:
        ore_do_push_char(state, character, match);
    }
    
    state->loc += width;
}

SEXP ore_print_match (SEXP match, SEXP context_, SEXP width_, SEXP max_lines_, SEXP use_colour_)
{
    const int context = asInteger(context_);
    const int width = asInteger(width_);
    const int max_lines = asInteger(max_lines_);
    const Rboolean use_colour = (asLogical(use_colour_) == TRUE);
    
    const int n_matches = asInteger(ore_get_list_element(match, "nMatches"));
    SEXP text_ = ore_get_list_element(match, "text");
    const UChar *text = (const UChar *) CHAR(STRING_ELT(text_, 0));
    OnigEncoding encoding = ore_r_to_onig_enc(getCharCE(STRING_ELT(text_, 0)));
    const int *offsets = (const int *) INTEGER(ore_get_list_element(match, "offsets"));
    const int *byte_offsets = (const int *) INTEGER(ore_get_list_element(match, "byteOffsets"));
    const int *lengths = (const int *) INTEGER(ore_get_list_element(match, "lengths"));
    const int *byte_lengths = (const int *) INTEGER(ore_get_list_element(match, "byteLengths"));
    
    printstate_t *state = ore_alloc_printstate(context, width, use_colour, encoding->max_enc_len);
    
    size_t start = 0;
    size_t start_byte = 0;
    int width_remaining = width - 9;
    for (int i=0; i<n_matches; i++)
    {
        if (offsets[i] - 1 - start > context)
        {
            start = offsets[i] - 1 - context;
            UChar *ptr = onigenc_step_back(encoding, text, text+byte_offsets[i]-1, context);
            ptrdiff_t byte_len = text + byte_offsets[i] - 1 - ptr;
            for (int j=0; j<3; j++)
                ore_push_char(state, '.', FALSE);
            for (int j=0; j<byte_len; j++)
                ore_push_char(state, ptr[j], FALSE);
        }
        else if (offsets[i] - 1 > start)
        {
            UChar *ptr = onigenc_step_back(encoding, text, text+byte_offsets[i]-1, offsets[i]-1-start);
            ptrdiff_t byte_len = text + byte_offsets[i] - 1 - ptr;
            for (int j=0; j<byte_len; j++)
                ore_push_char(state, ptr[j], FALSE);
        }
        
        for (int j=0; j<byte_lengths[i]; j++)
            ore_push_char(state, *(text+byte_offsets[i]-1+j), TRUE);
        
        start = offsets[i] - 1 + lengths[i];
    }
    
    int remaining = onigenc_strlen_null(encoding, text+byte_offsets[n_matches-1]-1+byte_lengths[n_matches-1]);
    int to_print = (remaining < context ? remaining : context);
    for (int j=0; j<to_print; j++)
        ore_push_char(state, *(text+byte_offsets[n_matches-1]-1+byte_lengths[n_matches-1]+j), FALSE);
    if (remaining > context)
    {
        for (int j=0; j<3; j++)
            ore_push_char(state, '.', FALSE);
    }
    
    ore_print_line(state);
    
    return R_NilValue;
}
