#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <string.h>

#include "compile.h"
#include "match.h"
#include "split.h"

// Split the strings provided at matches to the regex
SEXP ore_split (SEXP regex_, SEXP text_, SEXP start_, SEXP simplify_)
{
    if (isNull(regex_))
        error("The specified regex object is not valid");
    
    // Convert R objects to C types
    regex_t *regex = (regex_t *) ore_retrieve(regex_, text_);
    const Rboolean simplify = asLogical(simplify_) == TRUE;
    int *start = INTEGER(start_);
    
    // Obtain the lengths of the text and start vectors (the latter will be recycled if necessary)
    const int text_len = length(text_);
    const int start_len = length(start_);
    
    // Check for sensible input
    if (text_len < 1)
        error("The text vector is empty");
    if (start_len < 1)
        error("The vector of starting positions is empty");
    
    SEXP results;
    PROTECT(results = NEW_LIST(text_len));
    
    // Step through each string to be searched
    for (int i=0; i<text_len; i++)
    {
        const cetype_t encoding = getCharCE(STRING_ELT(text_, i));
        if ((encoding == CE_UTF8 && regex->enc == ONIG_ENCODING_ISO_8859_1) || (encoding == CE_LATIN1 && regex->enc == ONIG_ENCODING_UTF8))
        {
            warning("Encoding of text element %d does not match the regex", i+1);
            SET_ELEMENT(results, i, ScalarString(STRING_ELT(text_,i)));
            continue;
        }
        
        const char *text = CHAR(STRING_ELT(text_, i));
        
        // Do the match
        rawmatch_t *raw_match = ore_search(regex, text, TRUE, (size_t) start[i % start_len] - 1);
        
        // If there's no match the return value is the original string
        if (raw_match == NULL)
            SET_ELEMENT(results, i, ScalarString(STRING_ELT(text_,i)));
        else
        {
            // Create a vector long enough to hold the pieces
            SEXP result;
            PROTECT(result = NEW_CHARACTER(raw_match->n_matches + 1));
    
            int start = 0;
            char *fragment;
            size_t current_length;
            for (int j=0; j<raw_match->n_matches; j++)
            {
                const size_t loc = j * raw_match->n_regions;
                
                // Work out the length of the piece and allocate memory for it
                current_length = raw_match->byte_offsets[loc] - start;
                fragment = R_alloc(current_length+1, 1);
        
                // Copy text in, and insert the string into the return value
                if (current_length > 0)
                    strncpy(fragment, text+start, current_length);
                *(fragment + current_length) = '\0';
                SET_STRING_ELT(result, j, mkCharCE(fragment,encoding));
                start += current_length + raw_match->byte_lengths[loc];
            }
    
            // Likewise for the last piece
            current_length = strlen(text) - start;
            fragment = R_alloc(current_length+1, 1);
            if (current_length > 0)
                strncpy(fragment, text+start, current_length);
            *(fragment + current_length) = '\0';
            SET_STRING_ELT(result, raw_match->n_matches, mkCharCE(fragment,encoding));
            
            SET_ELEMENT(results, i, result);
            UNPROTECT(1);
        }
    }
    
    UNPROTECT(1);
    
    // Return just the first (and only) element of the full list, if requested
    if (simplify && text_len == 1)
        return VECTOR_ELT(results, 0);
    else
        return results;
}
