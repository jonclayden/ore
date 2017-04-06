#include <string.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "compile.h"

OnigSyntaxType *modified_ruby_syntax;

// Not strictly part of the API, but useful for case-insensitive string comparison
extern int onigenc_with_ascii_strnicmp (OnigEncoding enc, const UChar *p, const UChar *end, const UChar *sascii, int n);

int ore_strnicmp (const char *str1, const char *str2, size_t num)
{
    return onigenc_with_ascii_strnicmp(ONIG_ENCODING_ASCII, (const UChar *) str1, (const UChar *) str1 + num, (const UChar *) str2, num);
}

// Finaliser to clear up garbage-collected "ore" objects
void ore_regex_finaliser (SEXP regex_ptr)
{
    regex_t *regex = (regex_t *) R_ExternalPtrAddr(regex_ptr);
    onig_free(regex);
    R_ClearExternalPtr(regex_ptr);
}

// Insert a group name into an R vector; used as a callback by ore_build()
int ore_store_name (const UChar *name, const UChar *name_end, int n_groups, int *group_numbers, regex_t *regex, void *arg)
{
    SEXP name_vector = (SEXP) arg;
    for (int i=0; i<n_groups; i++)
        SET_STRING_ELT(name_vector, group_numbers[i]-1, mkChar((const char *) name));
    
    return 0;
}

// Convert an R encoding value to its Oniguruma equivalent
OnigEncoding ore_r_to_onig_enc (cetype_t encoding)
{
    switch (encoding)
    {
        case CE_UTF8:
        return ONIG_ENCODING_UTF8;
        
        case CE_LATIN1:
        return ONIG_ENCODING_ISO_8859_1;
        
        default:
        return ONIG_ENCODING_ASCII;
    }
}

// Convert an encoding string to its Oniguruma equivalent
OnigEncoding ore_name_to_onig_enc (const char *enc)
{
    if (ore_strnicmp(enc,"ASCII",5) == 0 || ore_strnicmp(enc,"US-ASCII",8) == 0)
        return ONIG_ENCODING_ASCII;
    else if (ore_strnicmp(enc,"UTF-8",5) == 0 || ore_strnicmp(enc,"UTF8",4) == 0)
        return ONIG_ENCODING_UTF8;
    else if (ore_strnicmp(enc,"ISO_8859-1",10) == 0 || ore_strnicmp(enc,"ISO-8859-1",10) == 0 || ore_strnicmp(enc,"ISO8859-1",9) == 0 || ore_strnicmp(enc,"LATIN1",6) == 0)
        return ONIG_ENCODING_ISO_8859_1;
    else if (ore_strnicmp(enc,"ISO_8859-2",10) == 0 || ore_strnicmp(enc,"ISO-8859-2",10) == 0 || ore_strnicmp(enc,"ISO8859-2",9) == 0 || ore_strnicmp(enc,"LATIN2",6) == 0)
        return ONIG_ENCODING_ISO_8859_2;
    else if (ore_strnicmp(enc,"ISO_8859-3",10) == 0 || ore_strnicmp(enc,"ISO-8859-3",10) == 0 || ore_strnicmp(enc,"ISO8859-3",9) == 0 || ore_strnicmp(enc,"LATIN3",6) == 0)
        return ONIG_ENCODING_ISO_8859_3;
    else if (ore_strnicmp(enc,"ISO_8859-4",10) == 0 || ore_strnicmp(enc,"ISO-8859-4",10) == 0 || ore_strnicmp(enc,"ISO8859-4",9) == 0 || ore_strnicmp(enc,"LATIN4",6) == 0)
        return ONIG_ENCODING_ISO_8859_4;
    else if (ore_strnicmp(enc,"ISO_8859-5",10) == 0 || ore_strnicmp(enc,"ISO-8859-5",10) == 0 || ore_strnicmp(enc,"ISO8859-5",9) == 0 || ore_strnicmp(enc,"LATIN5",6) == 0)
        return ONIG_ENCODING_ISO_8859_5;
    else if (ore_strnicmp(enc,"ISO_8859-6",10) == 0 || ore_strnicmp(enc,"ISO-8859-6",10) == 0 || ore_strnicmp(enc,"ISO8859-6",9) == 0 || ore_strnicmp(enc,"LATIN6",6) == 0)
        return ONIG_ENCODING_ISO_8859_6;
    else if (ore_strnicmp(enc,"ISO_8859-7",10) == 0 || ore_strnicmp(enc,"ISO-8859-7",10) == 0 || ore_strnicmp(enc,"ISO8859-7",9) == 0 || ore_strnicmp(enc,"LATIN7",6) == 0)
        return ONIG_ENCODING_ISO_8859_7;
    else if (ore_strnicmp(enc,"ISO_8859-8",10) == 0 || ore_strnicmp(enc,"ISO-8859-8",10) == 0 || ore_strnicmp(enc,"ISO8859-8",9) == 0 || ore_strnicmp(enc,"LATIN8",6) == 0)
        return ONIG_ENCODING_ISO_8859_8;
    else if (ore_strnicmp(enc,"ISO_8859-9",10) == 0 || ore_strnicmp(enc,"ISO-8859-9",10) == 0 || ore_strnicmp(enc,"ISO8859-9",9) == 0 || ore_strnicmp(enc,"LATIN9",6) == 0)
        return ONIG_ENCODING_ISO_8859_9;
    else if (ore_strnicmp(enc,"ISO_8859-10",11) == 0 || ore_strnicmp(enc,"ISO-8859-10",11) == 0 || ore_strnicmp(enc,"ISO8859-10",10) == 0 || ore_strnicmp(enc,"LATIN10",7) == 0)
        return ONIG_ENCODING_ISO_8859_10;
    else if (ore_strnicmp(enc,"ISO_8859-11",11) == 0 || ore_strnicmp(enc,"ISO-8859-11",11) == 0 || ore_strnicmp(enc,"ISO8859-11",10) == 0 || ore_strnicmp(enc,"LATIN11",7) == 0)
        return ONIG_ENCODING_ISO_8859_11;
    else if (ore_strnicmp(enc,"ISO_8859-13",11) == 0 || ore_strnicmp(enc,"ISO-8859-13",11) == 0 || ore_strnicmp(enc,"ISO8859-13",10) == 0 || ore_strnicmp(enc,"LATIN13",7) == 0)
        return ONIG_ENCODING_ISO_8859_13;
    else if (ore_strnicmp(enc,"ISO_8859-14",11) == 0 || ore_strnicmp(enc,"ISO-8859-14",11) == 0 || ore_strnicmp(enc,"ISO8859-14",10) == 0 || ore_strnicmp(enc,"LATIN14",7) == 0)
        return ONIG_ENCODING_ISO_8859_14;
    else if (ore_strnicmp(enc,"ISO_8859-15",11) == 0 || ore_strnicmp(enc,"ISO-8859-15",11) == 0 || ore_strnicmp(enc,"ISO8859-15",10) == 0 || ore_strnicmp(enc,"LATIN15",7) == 0)
        return ONIG_ENCODING_ISO_8859_15;
    else if (ore_strnicmp(enc,"ISO_8859-16",11) == 0 || ore_strnicmp(enc,"ISO-8859-16",11) == 0 || ore_strnicmp(enc,"ISO8859-16",10) == 0 || ore_strnicmp(enc,"LATIN16",7) == 0)
        return ONIG_ENCODING_ISO_8859_16;
    else if (ore_strnicmp(enc,"UTF-16BE",8) == 0)
        return ONIG_ENCODING_UTF16_BE;
    else if (ore_strnicmp(enc,"UTF-16LE",8) == 0)
        return ONIG_ENCODING_UTF16_LE;
    else if (ore_strnicmp(enc,"UTF-32BE",8) == 0)
        return ONIG_ENCODING_UTF32_BE;
    else if (ore_strnicmp(enc,"UTF-32LE",8) == 0)
        return ONIG_ENCODING_UTF32_LE;
    else if (ore_strnicmp(enc,"BIG5",4) == 0 || ore_strnicmp(enc,"BIG-5",5) == 0 || ore_strnicmp(enc,"BIGFIVE",7) == 0 || ore_strnicmp(enc,"BIG-FIVE",8) == 0)
        return ONIG_ENCODING_BIG5;
    else if (ore_strnicmp(enc,"CP932",5) == 0)
        return ONIG_ENCODING_CP932;
    else if (ore_strnicmp(enc,"CP1250",6) == 0 || ore_strnicmp(enc,"WINDOWS-1250",12) == 0)
        return ONIG_ENCODING_WINDOWS_1250;
    else if (ore_strnicmp(enc,"CP1251",6) == 0 || ore_strnicmp(enc,"WINDOWS-1251",12) == 0)
        return ONIG_ENCODING_WINDOWS_1251;
    else if (ore_strnicmp(enc,"CP1252",6) == 0 || ore_strnicmp(enc,"WINDOWS-1252",12) == 0)
        return ONIG_ENCODING_WINDOWS_1252;
    else if (ore_strnicmp(enc,"CP1253",6) == 0 || ore_strnicmp(enc,"WINDOWS-1253",12) == 0)
        return ONIG_ENCODING_WINDOWS_1253;
    else if (ore_strnicmp(enc,"CP1254",6) == 0 || ore_strnicmp(enc,"WINDOWS-1254",12) == 0)
        return ONIG_ENCODING_WINDOWS_1254;
    else if (ore_strnicmp(enc,"CP1257",6) == 0 || ore_strnicmp(enc,"WINDOWS-1257",12) == 0)
        return ONIG_ENCODING_WINDOWS_1257;
    else if (ore_strnicmp(enc,"EUC-JP",6) == 0 || ore_strnicmp(enc,"EUCJP",5) == 0)
        return ONIG_ENCODING_EUC_JP;
    else if (ore_strnicmp(enc,"EUC-KR",6) == 0 || ore_strnicmp(enc,"EUCKR",5) == 0)
        return ONIG_ENCODING_EUC_KR;
    else if (ore_strnicmp(enc,"EUC-TW",6) == 0 || ore_strnicmp(enc,"EUCTW",5) == 0)
        return ONIG_ENCODING_EUC_TW;
    else if (ore_strnicmp(enc,"GB18030",7) == 0)
        return ONIG_ENCODING_GB18030;
    else if (ore_strnicmp(enc,"KOI8-R",6) == 0)
        return ONIG_ENCODING_KOI8_R;
    else if (ore_strnicmp(enc,"KOI8-U",4) == 0)
        return ONIG_ENCODING_KOI8_U;
    else if (ore_strnicmp(enc,"SHIFT_JIS",9) == 0 || ore_strnicmp(enc,"SHIFT-JIS",9) == 0 || ore_strnicmp(enc,"SJIS",4) == 0)
        return ONIG_ENCODING_SJIS;
    else
    {
        warning("Encoding \"%s\" is not supported by Oniguruma - using ASCII", enc);
        return ONIG_ENCODING_ASCII;
    }
}

// Interface to onig_new(), used to create compiled regex objects
regex_t * ore_compile (const char *pattern, const char *options, OnigEncoding encoding, const char *syntax_name)
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
    return_value = onig_new(&regex, (UChar *) pattern, (UChar *) pattern+strlen(pattern), onig_options, encoding, syntax, &einfo);
    if (return_value != ONIG_NORMAL)
    {
        char message[ONIG_MAX_ERROR_MESSAGE_LEN];
        onig_error_code_to_str((UChar *) message, return_value, &einfo);
        error("Oniguruma compile: %s\n", message);
    }
    
    return regex;
}

// Retrieve a rawmatch_t object from the specified R object, which may be of class "ore" or just text
regex_t * ore_retrieve (SEXP regex_, SEXP text_, const Rboolean using_file)
{
    // Check the class of the regex object; if it's text this will be NULL
    SEXP class = getAttrib(regex_, R_ClassSymbol);
    if (isNull(class) || strcmp(CHAR(STRING_ELT(class,0)), "ore") != 0)
    {
        if (!isString(regex_))
            error("The specified regex must be of character mode");
        
        if (using_file)
        {
            SEXP encoding_name = getAttrib(text_, install("encoding"));
            return ore_compile(CHAR(STRING_ELT(regex_,0)), "", ore_name_to_onig_enc(CHAR(STRING_ELT(encoding_name,0))), "ruby");
        }
        else
        {
            // Take the encoding from the search text in this case
            cetype_t encoding = CE_NATIVE;
            for (int i=0; i<length(text_); i++)
            {
                const cetype_t current_encoding = getCharCE(STRING_ELT(text_, i));
                if (current_encoding == CE_UTF8 || current_encoding == CE_LATIN1)
                {
                    encoding = current_encoding;
                    break;
                }
            }
        
            // Compile the regex and return
            return ore_compile(CHAR(STRING_ELT(regex_,0)), "", ore_r_to_onig_enc(encoding), "ruby");
        }
    }
    else
        return (regex_t *) R_ExternalPtrAddr(getAttrib(regex_, install(".compiled")));
}

// Create a pattern string by concatenating the elements of the supplied vector, parenthesising named elements
char * ore_build_pattern (SEXP pattern_)
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
            strncpy(ptr++, "(", 1);
        
        // Copy in the element
        strncpy(ptr, current_string, current_len);
        ptr += current_len;
        
        if (name_present)
            strncpy(ptr++, ")", 1);
    }
    
    // Nul-terminate the string
    *ptr = '\0';
    
    return pattern;
}

// R wrapper for ore_compile(): builds the regex and creates an R "ore" object
SEXP ore_build (SEXP pattern_, SEXP options_, SEXP encoding_name_, SEXP syntax_name_)
{
    regex_t *regex;
    int n_groups;
    SEXP result, regex_ptr;
    
    // Obtain pointers to content
    const char *pattern = (const char *) ore_build_pattern(pattern_);
    const char *options = CHAR(STRING_ELT(options_, 0));
    const char *encoding_name = CHAR(STRING_ELT(encoding_name_, 0));
    const char *syntax_name = CHAR(STRING_ELT(syntax_name_, 0));
    
    cetype_t encoding;
    if (ore_strnicmp(encoding_name, "auto", 4) == 0)
        encoding = getCharCE(STRING_ELT(pattern_, 0));
    else if (ore_strnicmp(encoding_name, "UTF8", 4) == 0)
        encoding = CE_UTF8;
    else if (ore_strnicmp(encoding_name, "UTF-8", 5) == 0)
        encoding = CE_UTF8;
    else if (ore_strnicmp(encoding_name, "LATIN1", 6) == 0)
        encoding = CE_LATIN1;
    else
        encoding = CE_NATIVE;
        
    regex = ore_compile(pattern, options, ore_r_to_onig_enc(encoding), syntax_name);
    
    // Get and store number of captured groups
    n_groups = onig_number_of_captures(regex);
    
    PROTECT(result = mkString(pattern));
    
    // Create R external pointer to compiled regex
    PROTECT(regex_ptr = R_MakeExternalPtr(regex, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(regex_ptr, &ore_regex_finaliser, FALSE);
    setAttrib(result, install(".compiled"), regex_ptr);
    
    setAttrib(result, install("options"), PROTECT(ScalarString(STRING_ELT(options_, 0))));
    setAttrib(result, install("syntax"), PROTECT(ScalarString(STRING_ELT(syntax_name_, 0))));
    
    switch (encoding)
    {
        case CE_UTF8:
        setAttrib(result, install("encoding"), PROTECT(mkString("UTF-8")));
        break;
        
        case CE_LATIN1:
        setAttrib(result, install("encoding"), PROTECT(mkString("latin1")));
        break;
        
        default:
        setAttrib(result, install("encoding"), PROTECT(mkString("unknown")));
        break;
    }
    
    setAttrib(result, install("nGroups"), PROTECT(ScalarInteger(n_groups)));
    
    // Obtain group names, if available
    if (n_groups > 0)
    {
        SEXP names;
        Rboolean named = FALSE;
        
        PROTECT(names = NEW_CHARACTER(n_groups));
        for (int i=0; i<n_groups; i++)
            SET_STRING_ELT(names, i, NA_STRING);
        
        onig_foreach_name(regex, &ore_store_name, names);
        
        for (int i=0; i<n_groups; i++)
        {
            if (STRING_ELT(names, i) != NA_STRING)
            {
                named = TRUE;
                break;
            }
        }
        
        if (named)
            setAttrib(result, install("groupNames"), names);
        
        UNPROTECT(1);
    }
    
    setAttrib(result, R_ClassSymbol, mkString("ore"));
    
    UNPROTECT(6);
    return result;
}
