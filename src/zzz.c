#include <string.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "onigmo.h"
#include "zzz.h"

extern regex_t *group_number_regex;
extern regex_t *group_name_regex;
extern OnigSyntaxType *modified_ruby_syntax;

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
    
    // Use the default (Ruby) syntax, with one adjustment: we want \d, \s and \w to work across scripts
    modified_ruby_syntax = (OnigSyntaxType *) malloc(sizeof(OnigSyntaxType));
    onig_copy_syntax(modified_ruby_syntax, ONIG_SYNTAX_RUBY);
    ONIG_OPTION_OFF(modified_ruby_syntax->options, ONIG_OPTION_ASCII_RANGE);
    
    return R_NilValue;
}

// R wrapper function for onig_end(); called when the package is unloaded
SEXP ore_done ()
{
    onig_free(group_number_regex);
    onig_free(group_name_regex);
    free(modified_ruby_syntax);
    
    onig_end();
    
    return R_NilValue;
}
