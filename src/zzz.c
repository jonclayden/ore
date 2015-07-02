#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <string.h>

#include "oniguruma.h"
#include "zzz.h"

extern regex_t *group_number_regex;
extern regex_t *group_name_regex;

// R wrapper function for onig_init(); called when the package is loaded
SEXP ore_init ()
{
    onig_init();
    
    int return_value;
    OnigErrorInfo einfo;
    
    // Create the group number regex, for substitutions
    const char group_number_pattern[10] = "\\\\([1-9])";
    return_value = onig_new(&group_number_regex, (UChar *) group_number_pattern, (UChar *) group_number_pattern+strlen(group_number_pattern), ONIG_OPTION_NONE, ONIG_ENCODING_ASCII, ONIG_SYNTAX_RUBY, &einfo);
    if (return_value != ONIG_NORMAL)
    {
        char message[ONIG_MAX_ERROR_MESSAGE_LEN];
        onig_error_code_to_str((UChar *) message, return_value, &einfo);
        error("Oniguruma compile: %s\n", message);
    }
    
    // Create the group name regex, for substitutions
    const char group_name_pattern[13] = "\\\\k\\<(\\w+)\\>";
    return_value = onig_new(&group_name_regex, (UChar *) group_name_pattern, (UChar *) group_name_pattern+strlen(group_name_pattern), ONIG_OPTION_NONE, ONIG_ENCODING_ASCII, ONIG_SYNTAX_RUBY, &einfo);
    if (return_value != ONIG_NORMAL)
    {
        char message[ONIG_MAX_ERROR_MESSAGE_LEN];
        onig_error_code_to_str((UChar *) message, return_value, &einfo);
        error("Oniguruma compile: %s\n", message);
    }
    
    return R_NilValue;
}

// R wrapper function for onig_end(); called when the package is unloaded
SEXP ore_done ()
{
    onig_free(group_number_regex);
    onig_free(group_name_regex);
    
    onig_end();
    
    return R_NilValue;
}