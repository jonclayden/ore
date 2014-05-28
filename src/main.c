#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "oniguruma.h"
#include "main.h"

SEXP onig_search_R (SEXP pattern_, SEXP text_)
{
    int return_value;
    regex_t *regex;
    OnigErrorInfo einfo;
    OnigRegion *region;
    SEXP offset, offsets, lengths, list;
    
    const char *pattern = CHAR(STRING_ELT(pattern_, 0));
    const size_t pattern_len = strlen(pattern);
    const char *text = CHAR(STRING_ELT(text_, 0));
    const size_t text_len = strlen(text);
    
    return_value = onig_new(&regex, (UChar *) pattern, (UChar *) pattern+pattern_len, ONIG_OPTION_DEFAULT, ONIG_ENCODING_ASCII, ONIG_SYNTAX_DEFAULT, &einfo);
    if (return_value != ONIG_NORMAL)
    {
        char message[ONIG_MAX_ERROR_MESSAGE_LEN];
        onig_error_code_to_str(message, return_value, &einfo);
        error("Oniguruma: %s\n", message);
    }
    
    region = onig_region_new();
    
    return_value = onig_search(regex, (UChar *) text, (UChar *) text+text_len, (UChar *) text, (UChar *) text+text_len, region, ONIG_OPTION_NONE);
    
    PROTECT(offset = NEW_INTEGER(1));
    
    if (return_value == ONIG_MISMATCH)
    {
        PROTECT(offsets = NEW_INTEGER(1));
        PROTECT(lengths = NEW_INTEGER(1));
        
        *INTEGER(offset) = NA_INTEGER;
        *INTEGER(offsets) = NA_INTEGER;
        *INTEGER(lengths) = NA_INTEGER;
    }
    else if (return_value >= 0)
    {
        PROTECT(offsets = NEW_INTEGER(region->num_regs));
        PROTECT(lengths = NEW_INTEGER(region->num_regs));
        
        *INTEGER(offset) = return_value + 1;
        for (int i=0; i<region->num_regs; i++)
        {
            INTEGER(offsets)[i] = region->beg[i] + 1;
            INTEGER(lengths)[i] = region->end[i] - region->beg[i];
        }
    }
    else
    {
        char message[ONIG_MAX_ERROR_MESSAGE_LEN];
        onig_error_code_to_str(message, return_value);
        error("Oniguruma: %s\n", message);
    }
    
    PROTECT(list = NEW_LIST(3));
    SET_ELEMENT(list, 0, offset);
    SET_ELEMENT(list, 1, offsets);
    SET_ELEMENT(list, 2, lengths);
    
    onig_region_free(region, 1);
    onig_free(regex);
    onig_end();
    
    UNPROTECT(4);
    
    return list;
}
