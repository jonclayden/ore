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

void ore_do_push_byte (printstate_t *state, const char byte, Rboolean match, Rboolean zero_width)
{
    if (match || state->use_colour)
    {
        *(state->match++) = byte;
        if (!state->use_colour && !zero_width)
            *(state->context++) = ' ';
    }
    else
    {
        *(state->context++) = byte;
        if (!state->use_colour && !zero_width)
            *(state->match++) = ' ';
    }
}

void ore_push_byte (printstate_t *state, const char byte, int width, Rboolean match)
{
    if (width < 0)
    {
        switch (byte)
        {
            case '\t':
            case '\n':
            width = 2;
            break;
        
            default:
            width = 1;
        }
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
    
    switch (byte)
    {
        case '\t':
        ore_do_push_byte(state, '\\', match, FALSE);
        ore_do_push_byte(state, 't', match, FALSE);
        break;
        
        case '\n':
        ore_do_push_byte(state, '\\', match, FALSE);
        ore_do_push_byte(state, 'n', match, FALSE);
        break;
        
        default:
        ore_do_push_byte(state, byte, match, width==0);
    }
    
    state->loc += width;
}

UChar * ore_push_chars (printstate_t *state, UChar *ptr, int n, OnigEncoding encoding, Rboolean match)
{
    for (int i=0; i<n; i++)
    {
        int char_len = encoding->mbc_enc_len(ptr);
        ore_push_byte(state, *(ptr++), -1, match);
        for (int k=1; k<char_len; k++)
            ore_push_byte(state, *(ptr++), 0, match);
    }
    
    return ptr;
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
    size_t text_len = onigenc_strlen_null(encoding, text);
    
    const int *offsets_ = (const int *) INTEGER(ore_get_list_element(match, "offsets"));
    const int *byte_offsets_ = (const int *) INTEGER(ore_get_list_element(match, "byteOffsets"));
    int *offsets = (int *) R_alloc(n_matches, sizeof(int));
    int *byte_offsets = (int *) R_alloc(n_matches, sizeof(int));
    for (int i=0; i<n_matches; i++)
    {
        offsets[i] = offsets_[i] - 1;
        byte_offsets[i] = byte_offsets_[i] - 1;
    }
    
    const int *lengths = (const int *) INTEGER(ore_get_list_element(match, "lengths"));
    const int *byte_lengths = (const int *) INTEGER(ore_get_list_element(match, "byteLengths"));
    
    printstate_t *state = ore_alloc_printstate(context, width, use_colour, encoding->max_enc_len);
    
    size_t start = 0;
    Rboolean reached_end = FALSE;
    for (int i=0; i<n_matches; i++)
    {
        int precontext_len = 0, postcontext_len = 0;
        UChar *ptr;
        
        if (offsets[i] - start > context)
        {
            precontext_len = context;
            ptr = onigenc_step_back(encoding, text, text+byte_offsets[i], precontext_len);
            for (int j=0; j<3; j++)
                ore_push_byte(state, '.', 1, FALSE);
        }
        else if (offsets[i] > start)
        {
            precontext_len = offsets[i] - start;
            ptr = onigenc_step_back(encoding, text, text+byte_offsets[i], precontext_len);
        }
        else
            ptr = (UChar *) text + byte_offsets[i];
        
        ptr = ore_push_chars(state, ptr, precontext_len, encoding, FALSE);
        ptr = ore_push_chars(state, ptr, lengths[i], encoding, TRUE);
        
        start = offsets[i] + lengths[i];
        
        if (i == n_matches - 1)
        {
            if (text_len - start <= context)
            {
                postcontext_len = text_len - start;
                reached_end = TRUE;
            }
            else
                postcontext_len = context;
        }
        else if (offsets[i+1] - start > context)
        {
            if (offsets[i+1] - start - context <= context)
                postcontext_len = offsets[i+1] - start - context;
            else
                postcontext_len = context;
        }
        
        ptr = ore_push_chars(state, ptr, postcontext_len, encoding, FALSE);
        
        start += postcontext_len;
    }
    
    if (!reached_end)
    {
        for (int j=0; j<3; j++)
            ore_push_byte(state, '.', 1, FALSE);
    }
    
    ore_print_line(state);
    
    return R_NilValue;
}
