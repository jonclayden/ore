#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

// #include <string.h>

#include "compile.h"
#include "match.h"
#include "delim.h"

SEXP ore_get_fragment (const char *text, const rawmatch_t *begin_match, const rawmatch_t *end_match, int *begin_loc, int *end_loc, const Rboolean nested)
{
    if (begin_loc >= begin_match->n_matches || end_loc >= end_match->n_matches)
        return R_NilValue;
    
    while (begin_match->byte_offsets[begin_loc] > end_match->byte_offsets[end_loc])
    {
        end_loc++;
        if (end_loc >= end_match->n_matches)
            return R_NilValue;
    }
    
    const Rboolean more_beginnings = (begin_loc < begin_match->n_matches - 1);
    if (!more_beginnings || )
}

// Split the strings provided at matches to the regex
SEXP ore_delim (SEXP begin_regex_, SEXP end_regex_, SEXP text_, SEXP all_, SEXP start_, SEXP simplify_, SEXP nested_)
{
    if (isNull(begin_regex_))
        error("The specified start regex object is not valid");
    if (isNull(end_regex_))
        error("The specified end regex object is not valid");
    
    // Convert R objects to C types
    regex_t *begin_regex = (regex_t *) ore_retrieve(begin_regex_, text_);
    regex_t *end_regex = (regex_t *) ore_retrieve(end_regex_, text_);
    const Rboolean all = asLogical(all_) == TRUE;
    const Rboolean simplify = asLogical(simplify_) == TRUE;
    const Rboolean nested = asLogical(nested_) == TRUE;
    int *start = INTEGER(start_);
    
    // Obtain the lengths of the text and start vectors (the latter will be recycled if necessary)
    const int text_len = length(text_);
    const int start_len = length(start_);
    
    // Check for sensible input
    if (text_len < 1)
        error("The text vector is empty");
    if (start_len < 1)
        error("The vector of starting positions is empty");
    if (begin_regex->enc != end_regex->enc)
        error("Encodings of the two regexes do not match");
    
    SEXP results;
    PROTECT(results = NEW_LIST(text_len));
    
    // Step through each string to be searched
    for (int i=0; i<text_len; i++)
    {
        const cetype_t encoding = getCharCE(STRING_ELT(text_, i));
        if ((encoding == CE_UTF8 && begin_regex->enc == ONIG_ENCODING_ISO_8859_1) || (encoding == CE_LATIN1 && begin_regex->enc == ONIG_ENCODING_UTF8))
        {
            warning("Encoding of text element %d does not match the regex", i+1);
            SET_ELEMENT(results, i, ScalarString(STRING_ELT(text_,i)));
            continue;
        }
        
        const char *text = CHAR(STRING_ELT(text_, i));
        
        // Do the match
        rawmatch_t *begin_match = ore_search(begin_regex, text, all, (size_t) start[i % start_len] - 1);
        rawmatch_t *end_match = ore_search(end_regex, text, all, (size_t) start[i % start_len] - 1);
        
        // If there's no match to one or other pattern, the return value is NULL
        if (begin_match == NULL || end_match == NULL)
            SET_ELEMENT(results, i, R_NilValue);
        else
        {
            SEXP result = ore_get_fragment(text, begin_match, end_match, 0, 0, nested);
            SET_ELEMENT(results, i, result);
        }
    }
    
    UNPROTECT(1);
    
    // Return just the first (and only) element of the full list, if requested
    if (simplify && text_len == 1)
        return VECTOR_ELT(results, 0);
    else
        return results;
}
