#include <string.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "onigmo.h"
#include "compile.h"
#include "escape.h"
#include "match.h"
#include "print.h"
#include "split.h"
#include "subst.h"
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
    const char group_number_pattern[8] = "\\\\(\\d+)";
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

static R_CallMethodDef callMethods[] = {
    { "ore_build",          (DL_FUNC) &ore_build,           4 },
    { "ore_escape",         (DL_FUNC) &ore_escape,          1 },
    { "ore_search_all",     (DL_FUNC) &ore_search_all,      6 },
    { "ore_print_match",    (DL_FUNC) &ore_print_match,     5 },
    { "ore_split",          (DL_FUNC) &ore_split,           4 },
    { "ore_substitute_all", (DL_FUNC) &ore_substitute_all,  6 },
    { "ore_replace_all",    (DL_FUNC) &ore_replace_all,     7 },
    { "ore_init",           (DL_FUNC) &ore_init,            0 },
    { "ore_done",           (DL_FUNC) &ore_done,            0 },
    { NULL, NULL, 0 }
};

void R_init_ore (DllInfo *info)
{
   R_registerRoutines(info, NULL, callMethods, NULL, NULL);
   R_useDynamicSymbols(info, FALSE);
   R_forceSymbols(info, TRUE);
}
