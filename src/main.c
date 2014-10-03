#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <string.h>

#include "oniguruma.h"
#include "main.h"

#define MAX_MATCHES     128

#define ENCODING_ASCII  0
#define ENCODING_UTF8   1
#define ENCODING_LATIN1 2

SEXP ore_init ()
{
    onig_init();
    return R_NilValue;
}

SEXP ore_done ()
{
    onig_end();
    return R_NilValue;
}

void ore_regex_finaliser (SEXP regex_ptr)
{
    regex_t *regex = (regex_t *) R_ExternalPtrAddr(regex_ptr);
    onig_free(regex);
    R_ClearExternalPtr(regex_ptr);
}

int ore_store_name (const UChar *name, const UChar *name_end, int n_groups, int *group_numbers, regex_t *regex, void *arg)
{
    SEXP name_vector = (SEXP) arg;
    for (int i=0; i<n_groups; i++)
        SET_STRING_ELT(name_vector, group_numbers[i]-1, mkChar(name));
    
    return 0;
}

SEXP ore_compile (SEXP pattern_, SEXP options_, SEXP encoding_)
{
    int return_value, n_groups;
    OnigErrorInfo einfo;
    regex_t *regex;
    SEXP list, names, regex_ptr;
    
    const char *pattern = CHAR(STRING_ELT(pattern_, 0));
    const char *options = CHAR(STRING_ELT(options_, 0));
    
    OnigEncoding onig_encoding;
    const int encoding = asInteger(encoding_);
    switch (encoding)
    {
        case ENCODING_UTF8:
        onig_encoding = ONIG_ENCODING_UTF8;
        break;
        
        case ENCODING_LATIN1:
        onig_encoding = ONIG_ENCODING_ISO_8859_1;
        break;
        
        default:
        onig_encoding = ONIG_ENCODING_ASCII;
        break;
    }
    
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
    
    return_value = onig_new(&regex, (UChar *) pattern, (UChar *) pattern+strlen(pattern), onig_options, onig_encoding, ONIG_SYNTAX_DEFAULT, &einfo);
    if (return_value != ONIG_NORMAL)
    {
        char message[ONIG_MAX_ERROR_MESSAGE_LEN];
        onig_error_code_to_str(message, return_value, &einfo);
        error("Oniguruma compile: %s\n", message);
    }
    
    n_groups = onig_number_of_captures(regex);
    PROTECT(list = NEW_LIST(n_groups>0 ? 2 : 1));
    
    PROTECT(regex_ptr = R_MakeExternalPtr(regex, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(regex_ptr, &ore_regex_finaliser, FALSE);
    SET_ELEMENT(list, 0, regex_ptr);
    
    if (n_groups > 0)
    {
        PROTECT(names = NEW_CHARACTER(n_groups));
        for (int i=0; i<n_groups; i++)
            SET_STRING_ELT(names, i, mkChar(""));
        return_value = onig_foreach_name(regex, &ore_store_name, names);
        SET_ELEMENT(list, 1, names);
        UNPROTECT(1);
    }
    
    UNPROTECT(2);
    return list;
}

SEXP ore_search (SEXP regex_ptr, SEXP text_, SEXP all_, SEXP start_)
{
    int return_value, length;
    SEXP n_matches, offsets, lengths, bytes, matches, list;
    
    regex_t *regex = (regex_t *) R_ExternalPtrAddr(regex_ptr);
    const char *text = CHAR(STRING_ELT(text_, 0));
    const size_t text_len = strlen(text);
    const Rboolean all = asLogical(all_);
    OnigRegion *region = onig_region_new();
    
    list = R_NilValue;
    size_t start = (size_t) asInteger(start_) - 1;
    size_t max_matches = all ? MAX_MATCHES : 1;
    int match_number = 0;
    Rboolean vars_created = FALSE;
    
    while (TRUE)
    {
        return_value = onig_search(regex, (UChar *) text, (UChar *) text+text_len, (UChar *) text+start, (UChar *) text+text_len, region, ONIG_OPTION_NONE);
    
        if (return_value == ONIG_MISMATCH)
            break;
        else if (return_value >= 0)
        {
            if (!vars_created)
            {
                PROTECT(list = NEW_LIST(5));
                PROTECT(n_matches = NEW_INTEGER(1));
                PROTECT(offsets = NEW_INTEGER(max_matches * region->num_regs));
                PROTECT(lengths = NEW_INTEGER(max_matches * region->num_regs));
                PROTECT(bytes = NEW_INTEGER(max_matches * region->num_regs));
                PROTECT(matches = NEW_CHARACTER(max_matches * region->num_regs));
                vars_created = TRUE;
            }
            
            for (int i=0; i<region->num_regs; i++)
            {
                length = region->end[i] - region->beg[i];
                INTEGER(offsets)[match_number * region->num_regs + i] = region->beg[i] + 1;
                INTEGER(lengths)[match_number * region->num_regs + i] = onigenc_strlen(regex->enc, text+region->beg[i], text+region->end[i]);
                INTEGER(bytes)[match_number * region->num_regs + i] = length;
                
                if (length == 0)
                    SET_STRING_ELT(matches, match_number * region->num_regs + i, NA_STRING);
                else
                {
                    char *match_ptr = R_alloc(length+1, 1);
                    strncpy(match_ptr, text+region->beg[i], length);
                    *(match_ptr + length) = '\0';
                    SET_STRING_ELT(matches, match_number * region->num_regs + i, mkChar(match_ptr));
                }
            }
            
            start = region->end[0];
            match_number++;
        }
        else
        {
            char message[ONIG_MAX_ERROR_MESSAGE_LEN];
            onig_error_code_to_str(message, return_value);
            error("Oniguruma search: %s\n", message);
        }
        
        onig_region_free(region, 0);
        
        if (!all || match_number == max_matches)
            break;
    }
    
    if (!isNull(list))
    {
        *INTEGER(n_matches) = match_number;
        SET_ELEMENT(list, 0, n_matches);
        SET_ELEMENT(list, 1, offsets);
        SET_ELEMENT(list, 2, lengths);
        SET_ELEMENT(list, 3, bytes);
        SET_ELEMENT(list, 4, matches);
        UNPROTECT(6);
    }
    
    onig_region_free(region, 1);
    
    return list;
}

SEXP ore_split (SEXP text_, SEXP n_matches_, SEXP offsets_, SEXP lengths_)
{
    SEXP result;
    
    const char *text = CHAR(STRING_ELT(text_, 0));
    const int n_matches = asInteger(n_matches_);
    const int *offsets = INTEGER(offsets_);
    const int *lengths = INTEGER(lengths_);
    
    PROTECT(result = NEW_CHARACTER(n_matches + 1));
    
    int start = 0;
    char *fragment;
    size_t current_length;
    for (int i=0; i<n_matches; i++)
    {
        current_length = offsets[i] - 1 - start;
        fragment = R_alloc(current_length+1, 1);
        if (current_length > 0)
            strncpy(fragment, text+start, current_length);
        *(fragment + current_length) = '\0';
        SET_STRING_ELT(result, i, mkChar(fragment));
        start += current_length + lengths[i];
    }
    
    current_length = strlen(text) - start;
    fragment = R_alloc(current_length+1, 1);
    if (current_length > 0)
        strncpy(fragment, text+start, current_length);
    *(fragment + current_length) = '\0';
    SET_STRING_ELT(result, n_matches, mkChar(fragment));
    
    UNPROTECT(1);
    return result;
}

SEXP ore_substitute (SEXP text_, SEXP n_matches_, SEXP offsets_, SEXP lengths_, SEXP replacements_)
{
    SEXP result;
    
    const char *text = CHAR(STRING_ELT(text_, 0));
    const int n_matches = asInteger(n_matches_);
    const int *offsets = INTEGER(offsets_);
    const int *lengths = INTEGER(lengths_);
    
    int *rep_lengths = (int *) R_alloc(n_matches, sizeof(int));
    size_t orig_len = strlen(text);
    size_t string_len = orig_len;
    for (int i=0; i<n_matches; i++)
    {
        rep_lengths[i] = strlen(CHAR(STRING_ELT(replacements_, i)));
        string_len += rep_lengths[i] - lengths[i];
    }
    
    int start = 0;
    char *replacement = R_alloc(string_len+1, 1);
    char *repl_ptr = replacement;
    for (int i=0; i<n_matches; i++)
    {
        strncpy(repl_ptr, text+start, offsets[i]-1-start);
        repl_ptr += offsets[i] - 1 - start;
        strncpy(repl_ptr, CHAR(STRING_ELT(replacements_,i)), rep_lengths[i]);
        repl_ptr += rep_lengths[i];
        start = offsets[i] - 1 + lengths[i];
    }
    
    if (start < orig_len)
        strncpy(repl_ptr, text+start, orig_len-start);
    *(replacement + string_len) = '\0';
    
    PROTECT(result = NEW_CHARACTER(1));
    SET_STRING_ELT(result, 0, mkChar(replacement));
    UNPROTECT(1);
    
    return result;
}
