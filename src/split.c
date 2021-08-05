#include <string.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "compile.h"
#include "text.h"
#include "match.h"
#include "split.h"

// Split the strings provided at matches to the regex
SEXP ore_split (SEXP regex_, SEXP text_, SEXP start_, SEXP simplify_)
{
    if (isNull(regex_))
        error("The specified regex object is not valid");
    
    // Convert R objects to C types
    text_t *text = ore_text(text_);
    regex_t *regex = ore_retrieve(regex_, text->encoding);
    const Rboolean simplify = asLogical(simplify_) == TRUE;
    int *start = INTEGER(start_);
    
    // Obtain the length of the start vector (which will be recycled if necessary)
    const int start_len = length(start_);
    
    // Check for sensible input
    if (start_len < 1)
        error("The vector of starting positions is empty");
    
    SEXP results = PROTECT(NEW_LIST(text->length));
    
    // Step through each string to be searched
    for (int i=0; i<text->length; i++)
    {
        text_element_t *text_element = ore_text_element(text, i);
        if (!ore_consistent_encodings(text_element->encoding, regex->enc))
        {
            warning("Encoding of text element %d does not match the regex", i+1);
            SET_ELEMENT(results, i, ScalarString(ore_text_element_to_rchar(text_element, text->encoding_name)));
            continue;
        }
        
        // Do the match
        rawmatch_t *raw_match = ore_search(regex, text_element->start, text_element->end, TRUE, (size_t) start[i % start_len] - 1);
        
        // If there's no match the return value is the original string
        if (raw_match == NULL)
            SET_ELEMENT(results, i, ScalarString(ore_text_element_to_rchar(text_element, text->encoding_name)));
        else
        {
            // Create a vector long enough to hold the pieces
            SEXP result = PROTECT(NEW_CHARACTER(raw_match->n_matches + 1));
            const cetype_t r_encoding = ore_onig_to_r_enc(text_element->encoding);
            
            char *fragment;
            ptrdiff_t offset = 0;
            size_t current_length;
            for (int j=0; j<raw_match->n_matches; j++)
            {
                const size_t loc = j * raw_match->n_regions;
                
                // Work out the length of the piece and allocate memory for it
                current_length = raw_match->byte_offsets[loc] - offset;
                fragment = R_alloc(current_length+1, 1);
                
                // Copy text in, and insert the string into the return value
                if (current_length > 0)
                    strncpy(fragment, text_element->start+offset, current_length);
                *(fragment + current_length) = '\0';
                SET_STRING_ELT(result, j, ore_string_to_rchar(fragment, r_encoding, text->encoding_name));
                offset += current_length + raw_match->byte_lengths[loc];
            }
            
            // Likewise for the last piece
            current_length = strlen(text_element->start) - offset;
            fragment = R_alloc(current_length+1, 1);
            if (current_length > 0)
                strncpy(fragment, text_element->start+offset, current_length);
            *(fragment + current_length) = '\0';
            SET_STRING_ELT(result, raw_match->n_matches, ore_string_to_rchar(fragment, r_encoding, text->encoding_name));
            
            SET_ELEMENT(results, i, result);
            UNPROTECT(1);
        }
    }
    
    ore_text_done(text);
    
    UNPROTECT(1);
    
    // Return just the first (and only) element of the full list, if requested
    if (simplify && text->length == 1)
        return VECTOR_ELT(results, 0);
    else
        return results;
}
