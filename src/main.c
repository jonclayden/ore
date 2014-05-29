#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "oniguruma.h"
#include "main.h"

SEXP chariot_init ()
{
    onig_init();
    return R_NilValue;
}

SEXP chariot_done ()
{
    onig_end();
    return R_NilValue;
}

void chariot_regex_finaliser (SEXP regex_ptr)
{
    regex_t *regex = (regex_t *) R_ExternalPtrAddr(regex_ptr);
    onig_free(regex);
    R_ClearExternalPtr(regex_ptr);
}

int chariot_store_name (const UChar *name, const UChar *name_end, int n_groups, int *group_numbers, regex_t *regex, void *arg)
{
    SEXP name_vector = (SEXP) arg;
    for (int i=0; i<n_groups; i++)
        SET_STRING_ELT(name_vector, group_numbers[i]-1, mkChar(name));
    
    return 0;
}

SEXP chariot_compile (SEXP pattern_, SEXP options_)
{
    int return_value, n_groups;
    OnigErrorInfo einfo;
    regex_t *regex;
    SEXP list, names, regex_ptr;
    
    const char *pattern = CHAR(STRING_ELT(pattern_, 0));
    const char *options = CHAR(STRING_ELT(options_, 0));
    
    OnigOptionType onig_options = ONIG_OPTION_NONE;
    char *option_pointer = (char *) options;
    while (*option_pointer)
    {
        switch (*option_pointer)
        {
            case 'm':
            onig_options |= ONIG_OPTION_MULTILINE;
            break;
            
            case 'i':
            onig_options |= ONIG_OPTION_IGNORECASE;
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
    
    n_groups = onig_number_of_captures(regex);
    PROTECT(list = NEW_LIST(n_groups>0 ? 2 : 1));
    
    PROTECT(regex_ptr = R_MakeExternalPtr(regex, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(regex_ptr, &chariot_regex_finaliser, FALSE);
    SET_ELEMENT(list, 0, regex_ptr);
    
    if (n_groups > 0)
    {
        PROTECT(names = NEW_CHARACTER(n_groups));
        for (int i=0; i<n_groups; i++)
            SET_STRING_ELT(names, i, NA_STRING);
        return_value = onig_foreach_name(regex, &chariot_store_name, names);
        SET_ELEMENT(list, 1, names);
        UNPROTECT(1);
    }
    
    UNPROTECT(2);
    return list;
}

SEXP chariot_search (SEXP regex_ptr, SEXP text_)
{
    int return_value;
    SEXP offset, offsets, lengths, list;
    
    regex_t *regex = (regex_t *) R_ExternalPtrAddr(regex_ptr);
    const char *text = CHAR(STRING_ELT(text_, 0));
    const size_t text_len = strlen(text);
    OnigRegion *region = onig_region_new();
    
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
        error("Oniguruma search: %s\n", message);
    }
    
    PROTECT(list = NEW_LIST(3));
    SET_ELEMENT(list, 0, offset);
    SET_ELEMENT(list, 1, offsets);
    SET_ELEMENT(list, 2, lengths);
    
    onig_region_free(region, 1);
    
    UNPROTECT(4);
    
    return list;
}
