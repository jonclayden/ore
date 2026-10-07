#include <string.h>

#include <R.h>
#include <Rdefines.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <R_ext/Riconv.h>

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
// The name is in the regex's encoding, and is converted to UTF-8 if R can't represent that directly
static int ore_store_name (const UChar *name, const UChar *name_end, int n_groups, int *group_numbers, regex_t *regex, void *arg)
{
    SEXP name_vector = (SEXP) arg;
    OnigEncoding onig_enc = onig_get_encoding(regex);
    encoding_t *encoding = ore_encoding((const char *) onig_enc->name, onig_enc, NULL);
    SEXP name_ = PROTECT(ore_bytes_to_rchar((const char *) name, (size_t) (name_end - name), encoding));
    for (int i=0; i<n_groups; i++)
        SET_STRING_ELT(name_vector, group_numbers[i]-1, name_);
    
    UNPROTECT(1);
    return 0;
}

// Convert a pattern from its R encoding to a regex encoding that isn't ASCII-compatible (i.e. UTF-16 or UTF-32), storing the length of the result
static const char * ore_convert_pattern (const char *pattern, const cetype_t pattern_enc, OnigEncoding onig_enc, size_t *converted_len)
{
    const char *source_name = (pattern_enc == CE_UTF8) ? "UTF-8" : ((pattern_enc == CE_LATIN1) ? "latin1" : "");
    void *iconv_handle = Riconv_open(onig_enc->name, source_name);
    if (iconv_handle == (void *) -1)
        error("The regex cannot be converted to encoding \"%s\"", onig_enc->name);
    
    // Each character takes at most four bytes in UTF-16 or UTF-32
    size_t pattern_len = strlen(pattern);
    size_t buffer_len = 4 * pattern_len;
    char *buffer = R_alloc(buffer_len + 4, 1);
    char *buffer_ptr = buffer;
    const size_t result = Riconv(iconv_handle, &pattern, &pattern_len, &buffer_ptr, &buffer_len);
    Riconv_close(iconv_handle);
    if (result == (size_t) -1)
        error("The regex cannot be converted to encoding \"%s\"", onig_enc->name);
    
    *converted_len = (size_t) (buffer_ptr - buffer);
    return buffer;
}

// Interface to onig_new(), used to create compiled regex objects
// The pattern is a nul-terminated string in the specified R encoding
regex_t * ore_compile (const char *pattern, const cetype_t pattern_enc, const char *options, encoding_t *encoding, const char *syntax_name)
{
    int return_value;
    OnigErrorInfo einfo;
    regex_t *regex;
    
    // The pattern must be in the regex's encoding, so convert it if that encoding isn't ASCII-compatible
    size_t pattern_len = strlen(pattern);
    if (ONIGENC_MBC_MINLEN(encoding->onig_enc) > 1)
        pattern = ore_convert_pattern(pattern, pattern_enc, encoding->onig_enc, &pattern_len);
    
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
            
            default:
            warning("Option \"%c\" is not supported, and will be ignored", *option_pointer);
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
    return_value = onig_new(&regex, (UChar *) pattern, (UChar *) pattern+pattern_len, onig_options, encoding->onig_enc, syntax, &einfo);
    if (return_value != ONIG_NORMAL)
    {
        char message[ONIG_MAX_ERROR_MESSAGE_LEN];
        onig_error_code_to_str((UChar *) message, return_value, &einfo);
        error("Oniguruma compile: %s\n", message);
    }
    
    return regex;
}

// Obtain the compiled regex stored in an "ore" object, or NULL if there isn't one
static regex_t * ore_compiled_regex (SEXP regex_)
{
    if (!inherits(regex_, "ore"))
        return NULL;
    
    SEXP regex_ptr = getAttrib(regex_, install(".compiled"));
    if (TYPEOF(regex_ptr) != EXTPTRSXP)
        return NULL;
    else
        return (regex_t *) R_ExternalPtrAddr(regex_ptr);
}

// Retrieve the value of a string attribute, or a default value if it isn't set
static const char * ore_string_attribute (SEXP object, const char *name, const char *default_value)
{
    SEXP value = getAttrib(object, install(name));
    if (isString(value) && length(value) > 0 && STRING_ELT(value,0) != NA_STRING)
        return CHAR(STRING_ELT(value, 0));
    else
        return default_value;
}

// Create the encoding for a regex from its name, where "auto" means that of the pattern
static encoding_t * ore_regex_encoding (const char *encoding_name, SEXP pattern)
{
    if (ore_strnicmp(encoding_name, "auto", 4) == 0)
        return ore_string_encoding(pattern);
    else
        return ore_encoding(encoding_name, NULL, NULL);
}

// Retrieve a regex_t object from the specified R object, which may be of class "ore" or just text
regex_t * ore_retrieve (SEXP regex_, encoding_t *encoding)
{
    // If the regex object is of class "ore", look for a valid pointer
    regex_t *regex = ore_compiled_regex(regex_);
    
    if (regex == NULL)
    {
        if (!isString(regex_) || length(regex_) == 0 || STRING_ELT(regex_,0) == NA_STRING)
            error("The specified regex must be a single character string");
        else if (length(regex_) > 1)
            warning("Only the first element of the specified regex vector will be used");
        
        SEXP pattern = STRING_ELT(regex_, 0);
        if (inherits(regex_, "ore"))
        {
            // An "ore" object without a compiled regex has probably been saved and reloaded, so recompile it using the stored settings
            const char *options = ore_string_attribute(regex_, "options", "");
            const char *syntax_name = ore_string_attribute(regex_, "syntax", "ruby");
            encoding_t *regex_encoding = ore_regex_encoding(ore_string_attribute(regex_, "encoding", "auto"), pattern);
            regex = ore_compile(CHAR(pattern), getCharCE(pattern), options, regex_encoding, syntax_name);
            
            // Store the result for future use, if possible (so that the object then owns it)
            SEXP regex_ptr = getAttrib(regex_, install(".compiled"));
            if (TYPEOF(regex_ptr) == EXTPTRSXP)
            {
                R_SetExternalPtrAddr(regex_ptr, regex);
                R_RegisterCFinalizerEx(regex_ptr, &ore_regex_finaliser, FALSE);
            }
        }
        else
            regex = ore_compile(CHAR(pattern), getCharCE(pattern), "", encoding, "ruby");
    }
    
    return regex;
}

// Free the specified regex object, unless it was retrieved from an external pointer that owns the memory
void ore_free (regex_t *regex, SEXP source)
{
    if (regex == NULL)
        return;
    else if (source == NULL || ore_compiled_regex(source) != regex)
        onig_free(regex);
}

// Retrieve a regex suitable for searching a particular text element
// This is the main regex if its encoding is consistent with the element's; otherwise, if the regex was given as a string, it is compiled again in the element's encoding
// Any such alternative regex is stored for reuse, and should be freed by the caller when finished with; NULL is returned if no suitable regex is available
regex_t * ore_element_regex (SEXP regex_, regex_t *regex, encoding_t *encoding, regex_t **alternative)
{
    if (ore_consistent_encodings(encoding, regex->enc))
        return regex;
    else if (inherits(regex_, "ore"))
        return NULL;
    
    if (*alternative == NULL || !ore_consistent_encodings(encoding, (*alternative)->enc))
    {
        if (*alternative != NULL)
            onig_free(*alternative);
        *alternative = NULL;
        SEXP pattern = STRING_ELT(regex_, 0);
        *alternative = ore_compile(CHAR(pattern), getCharCE(pattern), "", encoding, "ruby");
    }
    
    return *alternative;
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
    {
        if (STRING_ELT(pattern_, i) == NA_STRING)
            error("The regex pattern contains missing values");
        pattern_len += strlen(CHAR(STRING_ELT(pattern_, i)));
    }
    
    // Allocate memory for all parts, plus surrounding parentheses and a terminating nul
    char *pattern = R_alloc(2*pattern_parts + pattern_len + 1, 1);
    
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

// Insert group names into an R character vector of appropriate size
Rboolean ore_group_name_vector (SEXP vec, regex_t *regex)
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
    
    // The full pattern takes its declared encoding from the first part that has one
    cetype_t pattern_enc = CE_NATIVE;
    for (int i=0; i<length(pattern_); i++)
    {
        const cetype_t part_enc = getCharCE(STRING_ELT(pattern_, i));
        if (part_enc == CE_UTF8 || part_enc == CE_LATIN1)
        {
            pattern_enc = part_enc;
            break;
        }
    }
    PROTECT(result = ScalarString(mkCharCE(pattern, pattern_enc)));
    
    // Compile the regex
    encoding_t *encoding = ore_regex_encoding(encoding_name, STRING_ELT(result, 0));
    regex_t *regex = ore_compile(pattern, pattern_enc, options, encoding, syntax_name);
    
    // Get and store number of captured groups
    const int n_groups = onig_number_of_captures(regex);
    
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
