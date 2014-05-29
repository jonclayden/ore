#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "oniguruma.h"
#include "main.h"

SEXP chariot_compile (SEXP pattern_, SEXP options_)
{
    int return_value;
    OnigErrorInfo einfo;
    regex_t *regex;
    
    const char *pattern = CHAR(STRING_ELT(pattern_, 0));
    const char *options = CHAR(STRING_ELT(options_, 0));
    
    OnigOptionType onig_options = ONIG_OPTION_NONE;
    char *option_pointer = options;
    while (*option_pointer)
    {
        switch (*option_pointer)
        {
            case 'm':
            onig_options = onig_options | ONIG_OPTION_MULTILINE;
            break;
            
            case 'i':
            onig_options = onig_options | ONIG_OPTION_IGNORECASE;
            break;
        }
        
        option_pointer++;
    }
    
    return_value = onig_new(&regex, (UChar *) pattern, (UChar *) pattern+strlen(pattern), onig_options, ONIG_ENCODING_ASCII, ONIG_SYNTAX_DEFAULT, &einfo);
    if (return_value != ONIG_NORMAL)
    {
        char message[ONIG_MAX_ERROR_MESSAGE_LEN];
        onig_error_code_to_str(message, return_value, &einfo);
        error("Oniguruma compile: %s\n", message);
    }
    
    if (onig_number_of_names(regex) > 0)
    {
        // int onig_foreach_name(regex_t* reg, int (*func)(const UChar*, const UChar*, int,int*,regex_t*,void*), void* arg)
    }
}

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
