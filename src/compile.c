#include <string.h>

#include <R.h>
#include <Rdefines.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <R_ext/Connections.h>

#include "text.h"
#include "compile.h"

OnigSyntaxType *modified_ruby_syntax;

// Finaliser to clear up garbage-collected "ore" objects
static void ore_regex_finaliser (SEXP regex_ptr)
{
    regex_t *regex = (regex_t *) R_ExternalPtrAddr(regex_ptr);
    onig_free(regex);
    R_ClearExternalPtr(regex_ptr);
}

// Insert a group name into an R vector; used as a callback by ore_build()
static int ore_store_name (const UChar *name, const UChar *name_end, int n_groups, int *group_numbers, regex_t *regex, void *arg)
{
    SEXP name_vector = (SEXP) arg;
    for (int i=0; i<n_groups; i++)
        SET_STRING_ELT(name_vector, group_numbers[i]-1, mkChar((const char *) name));
    
    return 0;
}

// Interface to onig_new(), used to create compiled regex objects
regex_t * ore_compile (const char *pattern, const char *options, encoding_t *encoding, const char *syntax_name)
{
    int return_value;
    OnigErrorInfo einfo;
    regex_t *regex;
    
    // Parse options and convert to onig option flags
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
    
    OnigSyntaxType *syntax;
    if (strncmp(syntax_name, "ruby", 4) == 0)
        syntax = modified_ruby_syntax;
    else if (strncmp(syntax_name, "fixed", 5) == 0)
        syntax = (OnigSyntaxType *) ONIG_SYNTAX_ASIS;
    else
        error("Syntax name \"%s\" is invalid\n", syntax_name);
    
    // Create the regex struct, and check for errors
    return_value = onig_new(&regex, (UChar *) pattern, (UChar *) pattern+strlen(pattern), onig_options, encoding->onig_enc, syntax, &einfo);
    if (return_value != ONIG_NORMAL)
    {
        char message[ONIG_MAX_ERROR_MESSAGE_LEN];
        onig_error_code_to_str((UChar *) message, return_value, &einfo);
        error("Oniguruma compile: %s\n", message);
    }
    
    return regex;
}

// Retrieve a regex_t object from the specified R object, which may be of class "ore" or just text
regex_t * ore_retrieve (SEXP regex_, encoding_t *encoding)
{
    regex_t *regex = NULL;
    
    // Check the class of the regex object; if it's "ore", look for a valid pointer
    if (inherits(regex_, "ore"))
        regex = (regex_t *) R_ExternalPtrAddr(getAttrib(regex_, install(".compiled")));
    
    if (regex == NULL)
    {
        if (!isString(regex_) || length(regex_) == 0)
            error("The specified regex must be a single character string");
        else if (length(regex_) > 1)
            warning("Only the first element of the specified regex vector will be used");
        
        // Compile the regex and return
        regex = ore_compile(CHAR(STRING_ELT(regex_,0)), "", encoding, "ruby");
    }
    
    return regex;
}

// Free the specified regex object, unless it was retrieved from an external pointer that owns the memory
void ore_free (regex_t *regex, SEXP source)
{
    if (regex == NULL)
        return;
    else if (source == NULL || !inherits(source, "ore"))
        onig_free(regex);
    else if (R_ExternalPtrAddr(getAttrib(source, install(".compiled"))) == NULL)
        onig_free(regex);
}

// Create a pattern string by concatenating the elements of the supplied vector, parenthesising named elements
static char * ore_build_pattern (SEXP pattern_)
{
    const int pattern_parts = length(pattern_);
    if (pattern_parts < 1)
        error("Pattern vector is empty");
    
    // Count up the full length of the string
    size_t pattern_len = 0;
    for (int i=0; i<pattern_parts; i++)
        pattern_len += strlen(CHAR(STRING_ELT(pattern_, i)));
    
    // Allocate memory for all parts, plus surrounding parentheses
    char *pattern = R_alloc(2*pattern_parts + pattern_len, 1);
    
    // Retrieve element names
    SEXP names = getAttrib(pattern_, R_NamesSymbol);
    char *ptr = pattern;
    for (int i=0; i<pattern_parts; i++)
    {
        const char *current_string = CHAR(STRING_ELT(pattern_, i));
        size_t current_len = strlen(current_string);
        Rboolean name_present = (!isNull(names) && strcmp(CHAR(STRING_ELT(names,i)), "") != 0);
        
        if (name_present)
            *ptr++ = '(';
        
        // Copy in the element
        memcpy(ptr, current_string, current_len);
        ptr += current_len;
        
        if (name_present)
            *ptr++ = ')';
    }
    
    // Nul-terminate the string
    *ptr = '\0';
    
    return pattern;
}

static Rboolean ore_group_name_vector (SEXP vec, regex_t *regex)
{
    const int n_groups = onig_number_of_captures(regex);
    
    for (int i=0; i<n_groups; i++)
        SET_STRING_ELT(vec, i, NA_STRING);
    
    onig_foreach_name(regex, &ore_store_name, vec);
    
    for (int i=0; i<n_groups; i++)
    {
        if (STRING_ELT(vec, i) != NA_STRING)
            return TRUE;
    }
    
    return FALSE;
}

// R wrapper for ore_compile(): builds the regex and creates an R "ore" object
SEXP ore_build (SEXP pattern_, SEXP options_, SEXP encoding_name_, SEXP syntax_name_)
{
    SEXP result, regex_ptr;
    
    // Obtain pointers to content
    const char *pattern = (const char *) ore_build_pattern(pattern_);
    const char *options = CHAR(STRING_ELT(options_, 0));
    const char *encoding_name = CHAR(STRING_ELT(encoding_name_, 0));
    const char *syntax_name = CHAR(STRING_ELT(syntax_name_, 0));
    
    encoding_t *encoding;
    if (ore_strnicmp(encoding_name, "auto", 4) == 0)
    {
        cetype_t r_enc = getCharCE(STRING_ELT(pattern_, 0));
        encoding = ore_encoding(NULL, NULL, &r_enc);
    }
    else
        encoding = ore_encoding(encoding_name, NULL, NULL);
    
    // Compile the regex
    regex_t *regex = ore_compile(pattern, options, encoding, syntax_name);
    
    // Get and store number of captured groups
    const int n_groups = onig_number_of_captures(regex);
    
    PROTECT(result = mkString(pattern));
    
    // Create R external pointer to compiled regex
    PROTECT(regex_ptr = R_MakeExternalPtr(regex, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(regex_ptr, &ore_regex_finaliser, FALSE);
    setAttrib(result, install(".compiled"), regex_ptr);
    
    setAttrib(result, install("options"), PROTECT(ScalarString(STRING_ELT(options_, 0))));
    setAttrib(result, install("syntax"), PROTECT(ScalarString(STRING_ELT(syntax_name_, 0))));
    setAttrib(result, install("encoding"), PROTECT(ScalarString(STRING_ELT(encoding_name_, 0))));
    setAttrib(result, install("nGroups"), PROTECT(ScalarInteger(n_groups)));
    
    // Obtain group names, if available
    if (n_groups > 0)
    {
        SEXP names = PROTECT(NEW_CHARACTER(n_groups));
        if (ore_group_name_vector(names, regex))
            setAttrib(result, install("groupNames"), names);
        UNPROTECT(1);
    }
    
    setAttrib(result, R_ClassSymbol, mkString("ore"));
    
    UNPROTECT(6);
    return result;
}
