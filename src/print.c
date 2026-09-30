#include <string.h>
#include <wchar.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "compile.h"
#include "text.h"
#include "match.h"
#include "print.h"
#include "wcwidth.h"

// Space kept free at the end of each line buffer, for colour escape codes and the terminating nul
#define ORE_PRINT_MARGIN    16

typedef struct {
    Rboolean    use_colour;
    int         width;
    int         max_lines;
    int         lines_done;
    int         n_matches;
    
    Rboolean    in_match;
    Rboolean    pending;
    unsigned    loc;
    unsigned    current_match;
    char        current_match_string[12];
    char       *current_match_loc;
    
    char       *match;
    char       *match_start;
    size_t      match_size;
    char       *context;
    char       *context_start;
    size_t      context_size;
    char       *number;
    char       *number_start;
} printstate_t;

// Extract an element of an R list by name
static SEXP ore_get_list_element (SEXP list, const char *name)
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

// Create a printstate_t object, which keeps track of various things
static printstate_t * ore_printstate (const int context, const int width, const int max_lines, const Rboolean use_colour, const int n_matches, const int max_enc_len)
{
    printstate_t *state = (printstate_t *) R_alloc(1, sizeof(printstate_t));
    
    // Store simple values directly
    state->use_colour = use_colour;
    state->max_lines = max_lines;
    state->n_matches = n_matches;
    
    // If context or number lines will be needed, we remove part of the width for the initial labels
    if (use_colour && n_matches == 1)
        state->width = width;
    else
        state->width = width - 9;
    
    // Initialisations
    state->in_match = FALSE;
    state->pending = FALSE;
    state->loc = 0;
    state->current_match = 0;
    state->lines_done = 0;
    
    // If we're using colour we need to allocate enough space for the nine control bytes either side of each match
    // Zero-width characters use space without taking up width, so lines may also be ended early if a buffer fills up (see ore_have_space)
    if (use_colour)
    {
        state->match_size = (max_enc_len+9)*width + 2*ORE_PRINT_MARGIN;
        state->match = R_alloc(state->match_size, 1);
        state->context_size = 0;
        state->context = NULL;
    }
    else
    {
        state->match_size = state->context_size = max_enc_len*width + 2*ORE_PRINT_MARGIN;
        state->match = R_alloc(state->match_size, 1);
        state->context = R_alloc(state->context_size, 1);
    }
    
    // If there is more than one match, allocate memory for the number line
    if (n_matches == 1)
        state->number = NULL;
    else
        state->number = R_alloc(width + ORE_PRINT_MARGIN, 1);
    
    // Pointers to the start of each line
    state->match_start = state->match;
    state->context_start = state->context;
    state->number_start = state->number;
    
    return state;
}

// Can more lines be printed, or should we stop?
static Rboolean ore_more_lines (printstate_t *state)
{
    return (state->max_lines == 0 || state->lines_done < state->max_lines);
}

// Check whether the buffers have room for a character of the specified number of bytes, plus the margin
static Rboolean ore_have_space (const printstate_t *state, const size_t bytes)
{
    const size_t needed = bytes + ORE_PRINT_MARGIN;
    if ((size_t) (state->match - state->match_start) + needed > state->match_size)
        return FALSE;
    if (state->context != NULL && (size_t) (state->context - state->context_start) + needed > state->context_size)
        return FALSE;
    return TRUE;
}

// This function actually prints a line of buffered text, with annotations, to the terminal
static void ore_print_line (printstate_t *state)
{
    // Forget it if nothing has been buffered since the last line
    if (!state->pending)
        return;
    
    // Print the line, unless we've already printed as many lines as are allowed, in which case its content is dropped
    if (ore_more_lines(state))
    {
        // Switch off colour printing temporarily if we're in the middle of a match
        if (state->use_colour && state->in_match)
        {
            memcpy(state->match, "\x1b[0m", 4);
            state->match += 4;
        }
        *state->match = '\0';
        
        // Print out the match string, alone or with a label
        if (state->use_colour && state->n_matches == 1)
            Rprintf("%s\n", state->match_start);
        else
            Rprintf("  match: %s\n", state->match_start);
        
        // Print the context, if it's a separate line (i.e. if we're not using colour)
        if (!state->use_colour)
        {
            *state->context = '\0';
            Rprintf("context: %s\n", state->context_start);
        }
        
        // Print numbers if there is more than one match
        if (state->n_matches > 1)
        {
            *state->number = '\0';
            Rprintf(" number: %s\n", state->number_start);
        }
        
        Rprintf("\n");
        
        // Keep count of lines done
        state->lines_done++;
    }
    
    // Reset
    state->match = state->match_start;
    state->context = state->context_start;
    state->number = state->number_start;
    state->loc = 0;
    state->pending = FALSE;
    
    // Turn colour back on if we were mid-match
    if (state->use_colour && state->in_match)
    {
        memcpy(state->match, "\x1b[36m", 5);
        state->match += 5;
    }
}

// Add a byte to the match (or context), updating other lines appropriately
static void ore_do_push_byte (printstate_t *state, const char byte, const int width)
{
    state->pending = TRUE;
    
    if (state->in_match || state->use_colour)
    {
        *(state->match++) = byte;
        if (!state->use_colour && width > 0)
        {
            for (int i=0; i<width; i++)
                *(state->context++) = ' ';
        }
        if (state->n_matches > 1 && width > 0)
        {
            if (state->in_match)
            {
                for (int i=0; i<width; i++)
                {
                    // Print the next digit of the number, or '=' if we've finished them
                    if (*state->current_match_loc == '\0')
                        *(state->number++) = '=';
                    else
                        *(state->number++) = *(state->current_match_loc++);
                }
            }
            else
            {
                for (int i=0; i<width; i++)
                    *(state->number++) = ' ';
            }
        }
    }
    else
    {
        *(state->context++) = byte;
        if (!state->use_colour && width > 0)
        {
            for (int i=0; i<width; i++)
                *(state->match++) = ' ';
        }
        if (state->n_matches > 1 && width > 0)
        {
            for (int i=0; i<width; i++)
                *(state->number++) = ' ';
        }
    }
}

// Switch from inside to outside match state, or vice versa
static void ore_switch_state (printstate_t *state, Rboolean match)
{
    // Colour escape codes take space, so many empty matches in a row could otherwise fill the buffer
    if (!ore_have_space(state, 0))
        ore_print_line(state);
    
    if (match && !state->in_match)
    {
        // Append the colour escape code, if appropriate
        if (state->use_colour)
        {
            memcpy(state->match, "\x1b[36m", 5);
            state->match += 5;
        }
        
        // Find current match number and convert to string
        state->current_match++;
        snprintf(state->current_match_string, 6, "%u", state->current_match % 100000);
        state->current_match_loc = state->current_match_string;
        
        state->in_match = TRUE;
    }
    else if (!match && state->in_match)
    {
        // Switch back to normal colour, if appropriate
        if (state->use_colour)
        {
            memcpy(state->match, "\x1b[0m", 4);
            state->match += 4;
        }
        
        state->in_match = FALSE;
    }
}

// Push a byte to the buffers, starting a new line if there isn't space for it
static void ore_push_byte (printstate_t *state, const char byte, const int width)
{
    // Print the line and reset the buffers if there isn't space
    if (state->loc + width >= state->width)
        ore_print_line(state);
    
    // Do the actual push(es)
    switch (byte)
    {
        case '\t':
        ore_do_push_byte(state, '\\', 1);
        ore_do_push_byte(state, 't', 1);
        break;
        
        case '\n':
        ore_do_push_byte(state, '\\', 1);
        ore_do_push_byte(state, 'n', 1);
        break;
        
        default:
        ore_do_push_byte(state, byte, width);
    }
    
    // Keep track of the number of characters printed
    state->loc += width;
}

// Push a fixed number of (possibly multibyte) characters to the buffers
static UChar * ore_push_chars (printstate_t *state, UChar *ptr, int n, OnigEncoding encoding)
{
    for (int i=0; i<n; i++)
    {
        int char_len = ONIGENC_MBC_ENC_LEN(encoding, ptr, ptr+encoding->max_enc_len);
        int width;
        
        // Start a new line if the buffers are full, which can happen before the line's width is used up if there are zero-width characters
        // The extra two bytes allow for tabs and newlines, which are printed as escapes
        if (!ore_have_space(state, char_len + 2))
            ore_print_line(state);
        
        wchar_t wc;
        mbtowc(&wc, (const char *) ptr, char_len);
        width = mk_wcwidth(wc);
        
        // Tab and newline characters are expanded into their escaped versions to avoid spurious space in the result
        if (*ptr == '\t' || *ptr == '\n')
            width = 2;
        
        ore_push_byte(state, *(ptr++), width);
        for (int k=1; k<char_len; k++)
            ore_push_byte(state, *(ptr++), 0);
    }
    
    return ptr;
}

// R interface function for printing an "orematch" object
SEXP ore_print_match (SEXP match, SEXP context_, SEXP width_, SEXP max_lines_, SEXP use_colour_)
{
    // Extract scalar values from R types
    const int context = asInteger(context_);
    const int width = asInteger(width_);
    const int max_lines = asInteger(max_lines_);
    const Rboolean use_colour = (asLogical(use_colour_) == TRUE);
    
    // Find the number of matches
    const int n_matches = asInteger(ore_get_list_element(match, "nMatches"));
    
    // Extract the text, and work out its character length and encoding
    // NB: There is only one string in the object, since each searched string produces a new "orematch" object
    SEXP text_ = ore_get_list_element(match, "text");
    const UChar *text = (const UChar *) CHAR(STRING_ELT(text_, 0));
    cetype_t r_encoding = getCharCE(STRING_ELT(text_, 0));
    encoding_t *encoding = ore_encoding(NULL, NULL, &r_encoding);
    const UChar *end = text + strlen(CHAR(STRING_ELT(text_, 0)));
    size_t text_len = onigenc_strlen_null(encoding->onig_enc, text);
    
    // Retrieve offsets and convert to C convention by subtracting 1
    const int *offsets_ = (const int *) INTEGER(ore_get_list_element(match, "offsets"));
    int *offsets = (int *) R_alloc(n_matches, sizeof(int));
    for (int i=0; i<n_matches; i++)
        offsets[i] = offsets_[i] - 1;
    
    // Retrieve match lengths
    const int *lengths = (const int *) INTEGER(ore_get_list_element(match, "lengths"));
    
    // Create the print state object
    printstate_t *state = ore_printstate(context, width, max_lines, use_colour, n_matches, encoding->onig_enc->max_enc_len);
    
    // Print precontext, matched text, and postcontext for each match
    size_t start = 0;
    Rboolean reached_end = FALSE;
    for (int i=0; i<n_matches; i++)
    {
        int precontext_len = 0, postcontext_len = 0;
        UChar *ptr;
        
        if (offsets[i] - start > context)
        {
            // There is more precontext than we want, so truncate (with an ellipsis)
            precontext_len = context;
            for (int j=0; j<3; j++)
                ore_push_byte(state, '.', 1);
        }
        else
            precontext_len = offsets[i] - start;
        
        ptr = ore_step(encoding->onig_enc, text, end, offsets[i] - precontext_len);
        
        // Push precontext, switch to match mode, print matched text, and then switch back
        ptr = ore_push_chars(state, ptr, precontext_len, encoding->onig_enc);
        ore_switch_state(state, TRUE);
        ptr = ore_push_chars(state, ptr, lengths[i], encoding->onig_enc);
        ore_switch_state(state, FALSE);
        
        // Update starting position for next loop
        start = offsets[i] + lengths[i];
        
        if (i == n_matches - 1)
        {
            // Last match: postcontext is the rest of the text
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
            // If the gap to the next match is more than double the context width, truncate it
            if (offsets[i+1] - start - context <= context)
                postcontext_len = offsets[i+1] - start - context;
            else
                postcontext_len = context;
        }
        
        // Push the postcontext
        ptr = ore_push_chars(state, ptr, postcontext_len, encoding->onig_enc);
        
        // Update the start position to the end of the postcontext
        start += postcontext_len;
        
        // Check if we've reached the line limit
        if (!ore_more_lines(state))
        {
            reached_end = TRUE;
            break;
        }
    }
    
    // If we didn't reach the end of the original text, add a final ellipsis
    if (!reached_end)
    {
        for (int j=0; j<3; j++)
            ore_push_byte(state, '.', 1);
    }
    
    // Flush the buffers
    ore_print_line(state);
    
    return R_NilValue;
}
