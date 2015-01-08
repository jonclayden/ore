#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <string.h>

#include "oniguruma.h"
#include "main.h"

// Not strictly part of the API, but needed for implementing the "start" argument
extern UChar * onigenc_step (OnigEncoding enc, const UChar *p, const UChar *end, int n);

// Also not part of the API, but useful for case-insensitive string comparison
extern int onigenc_with_ascii_strnicmp (OnigEncoding enc, const UChar *p, const UChar *end, const UChar *sascii, int n);

// Block size for match data; memory is allocated in chunks this big
#define MATCH_BLOCK_SIZE    128

static regex_t *group_number_regex;
static regex_t *group_name_regex;

// R wrapper function for onig_init(); called when the packge is loaded
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

// R wrapper function for onig_end(); called when the packge is unloaded
SEXP ore_done ()
{
    onig_free(group_number_regex);
    onig_free(group_name_regex);
    
    onig_end();
    
    return R_NilValue;
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

// Interface to onig_new(), used to create compiled regex objects
regex_t * ore_compile (const char *pattern, const char *options, cetype_t encoding)
{
    int return_value;
    OnigErrorInfo einfo;
    regex_t *regex;
    
    // Convert R encoding to onig data type
    OnigEncoding onig_encoding;
    switch (encoding)
    {
        case CE_UTF8:
        onig_encoding = ONIG_ENCODING_UTF8;
        break;
        
        case CE_LATIN1:
        onig_encoding = ONIG_ENCODING_ISO_8859_1;
        break;
        
        default:
        onig_encoding = ONIG_ENCODING_ASCII;
        break;
    }
    
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
    
    // Use the default (Ruby) syntax, with one adjustment: we want \d, \s and \w to work across scripts
    OnigSyntaxType *syntax = ONIG_SYNTAX_RUBY;
    ONIG_OPTION_OFF(syntax->options, ONIG_OPTION_ASCII_RANGE);
    
    // Create the regex struct, and check for errors
    return_value = onig_new(&regex, (UChar *) pattern, (UChar *) pattern+strlen(pattern), onig_options, onig_encoding, syntax, &einfo);
    if (return_value != ONIG_NORMAL)
    {
        char message[ONIG_MAX_ERROR_MESSAGE_LEN];
        onig_error_code_to_str((UChar *) message, return_value, &einfo);
        error("Oniguruma compile: %s\n", message);
    }
    
    return regex;
}

// Retrieve a rawmatch_t object from the specified R object, which may be of class "ore" or just text
regex_t * ore_retrieve (SEXP regex_, SEXP text_)
{
    // Check the class of the regex object; if it's text this will be NULL
    SEXP class = getAttrib(regex_, R_ClassSymbol);
    if (isNull(class) || strcmp(CHAR(STRING_ELT(class,0)), "ore") != 0)
    {
        if (!isString(regex_))
            error("The specified regex must be of character mode");
        
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
        return ore_compile(CHAR(STRING_ELT(regex_,0)), "", encoding);
    }
    else
        return (regex_t *) R_ExternalPtrAddr(getAttrib(regex_, install(".compiled")));
}

// R wrapper for ore_compile(): builds the regex and creates an R "ore" object
SEXP ore_build (SEXP pattern_, SEXP options_, SEXP encoding_name_)
{
    regex_t *regex;
    int n_groups, return_value;
    SEXP result, regex_ptr;
    
    if (length(pattern_) < 1)
        error("Pattern vector is empty");
    if (length(pattern_) > 1)
        warning("Pattern vector has more than one element");
    
    // Obtain pointers to content
    const char *pattern = CHAR(STRING_ELT(pattern_, 0));
    const char *options = CHAR(STRING_ELT(options_, 0));
    const UChar *encoding_name = (const UChar *) CHAR(STRING_ELT(encoding_name_, 0));
    
    cetype_t encoding;
    if (onigenc_with_ascii_strnicmp(ONIG_ENCODING_ASCII, encoding_name, encoding_name + 4, (const UChar *) "auto", 4) == 0)
        encoding = getCharCE(STRING_ELT(pattern_, 0));
    else if (onigenc_with_ascii_strnicmp(ONIG_ENCODING_ASCII, encoding_name, encoding_name + 4, (const UChar *) "utf8", 4) == 0)
        encoding = CE_UTF8;
    else if (onigenc_with_ascii_strnicmp(ONIG_ENCODING_ASCII, encoding_name, encoding_name + 5, (const UChar *) "utf-8", 5) == 0)
        encoding = CE_UTF8;
    else if (onigenc_with_ascii_strnicmp(ONIG_ENCODING_ASCII, encoding_name, encoding_name + 6, (const UChar *) "latin1", 6) == 0)
        encoding = CE_LATIN1;
    else
        encoding = CE_NATIVE;
        
    regex = ore_compile(pattern, options, encoding);
    
    // Get and store number of captured groups
    n_groups = onig_number_of_captures(regex);
    
    PROTECT(result = ScalarString(STRING_ELT(pattern_, 0)));
    
    // Create R external pointer to compiled regex
    PROTECT(regex_ptr = R_MakeExternalPtr(regex, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(regex_ptr, &ore_regex_finaliser, FALSE);
    setAttrib(result, install(".compiled"), regex_ptr);
    
    setAttrib(result, install("options"), ScalarString(STRING_ELT(options_, 0)));
    
    switch (encoding)
    {
        case CE_UTF8:
        setAttrib(result, install("encoding"), mkString("UTF-8"));
        break;
        
        case CE_LATIN1:
        setAttrib(result, install("encoding"), mkString("latin1"));
        break;
        
        default:
        setAttrib(result, install("encoding"), mkString("unknown"));
        break;
    }
    
    setAttrib(result, install("nGroups"), ScalarInteger(n_groups));
    
    // Obtain group names, if available
    if (n_groups > 0)
    {
        SEXP names;
        Rboolean named = FALSE;
        
        PROTECT(names = NEW_CHARACTER(n_groups));
        for (int i=0; i<n_groups; i++)
            SET_STRING_ELT(names, i, NA_STRING);
        
        return_value = onig_foreach_name(regex, &ore_store_name, names);
        
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
    
    UNPROTECT(2);
    return result;
}

// Allocate memory for a rawmatch_t object with capacity one block, and its contents
rawmatch_t * ore_rawmatch_alloc (const int n_regions)
{
    // Allocate memory for the struct itself, and set its initial capacity
    rawmatch_t *match = (rawmatch_t *) R_alloc(1, sizeof(rawmatch_t));
    match->capacity = MATCH_BLOCK_SIZE;
    match->n_regions = n_regions;
    
    // Allocate memory for matrix variables
    const size_t len = (size_t) match->capacity * match->n_regions;
    match->offsets = (int *) R_alloc(len, sizeof(int));
    match->byte_offsets = (int *) R_alloc(len, sizeof(int));
    match->lengths = (int *) R_alloc(len, sizeof(int));
    match->byte_lengths = (int *) R_alloc(len, sizeof(int));
    match->matches = (char **) R_alloc(len, sizeof(char *));
    
    return match;
}

// Extend a vector to hold more values
// NB: This function is less efficient than standard C realloc(), because it always results in a copy, but using R_alloc simplifies things. The R API function S_realloc() is closely related, but seems to exist only "for compatibility with older versions of S", and zeroes out the extra memory, which is unnecessary here.
char * ore_realloc (const void *ptr, const size_t new_len, const size_t old_len, const int element_size)
{
    if (ptr == NULL)
        return (char *) R_alloc(new_len, element_size);
    else if (new_len <= old_len)
        return (char *) ptr;
    else
    {
        char *new_ptr;
        const size_t old_byte_len = old_len * element_size;
        
        new_ptr = R_alloc(new_len, element_size);
        memcpy(new_ptr, (const char *) ptr, old_byte_len);
        return new_ptr;
    }
}

// Extend an existing rawmatch_t object, increasing its capacity by MATCH_BLOCK_SIZE and reallocating memory accordingly
void ore_rawmatch_extend (rawmatch_t *match)
{
    const size_t old_len = (size_t) match->capacity * match->n_regions;
    match->capacity += MATCH_BLOCK_SIZE;
    const size_t new_len = (size_t) match->capacity * match->n_regions;
    
    match->offsets = (int *) ore_realloc(match->offsets, new_len, old_len, sizeof(int));
    match->byte_offsets = (int *) ore_realloc(match->byte_offsets, new_len, old_len, sizeof(int));
    match->lengths = (int *) ore_realloc(match->lengths, new_len, old_len, sizeof(int));
    match->byte_lengths = (int *) ore_realloc(match->byte_lengths, new_len, old_len, sizeof(int));
    match->matches = (char **) ore_realloc(match->matches, new_len, old_len, sizeof(char *));
}

// Insert a string into a rawmatch_t object, allocating space for it first
void ore_rawmatch_store_string (rawmatch_t *match, const size_t loc, const char *string, const int length)
{
    match->matches[loc] = R_alloc(length+1, 1);
    strncpy(match->matches[loc], string, length);
    *(match->matches[loc] + length) = '\0';
}

// Search a single string for matches to a regex
rawmatch_t * ore_search (regex_t *regex, const char *text, const Rboolean all, const size_t start)
{
    int return_value, length;
    rawmatch_t *result = NULL;
    
    // Create region object to capture match data
    OnigRegion *region = onig_region_new();
    
    // The number of matches found so far
    int match_number = 0;
    
    // If we're not starting at the beginning, step forward the required number of characters
    UChar *end_ptr = (UChar *) text + strlen(text);
    UChar *start_ptr;
    if (start == 0)
        start_ptr = (UChar *) text;
    else
        start_ptr = onigenc_step(regex->enc, (UChar *) text, end_ptr, (int) start);
    
    // The offset (in chars) corresponding to start_ptr
    int start_offset = (int) start;
    
    // The loop is broken when there are no more matches, or max matches have been obtained
    while (TRUE)
    {
        // Call the API to do the search
        return_value = onig_search(regex, (UChar *) text, end_ptr, start_ptr, end_ptr, region, ONIG_OPTION_NONE);
        
        // If there are no more matches, stop
        if (return_value == ONIG_MISMATCH)
            break;
        else if (return_value >= 0)
        {
            // Set up output data structures the first time
            if (result == NULL)
                result = ore_rawmatch_alloc(region->num_regs);
            else if (match_number >= result->capacity)
                ore_rawmatch_extend(result);
            
            // Regions are the whole match and then subgroups
            for (int i=0; i<region->num_regs; i++)
            {
                // Work out the offset and length of the region, in bytes and chars
                length = region->end[i] - region->beg[i];
                const size_t loc = match_number * region->num_regs + i;
                
                result->byte_offsets[loc] = region->beg[i];
                result->byte_lengths[loc] = length;
                
                // If we're using a single-byte encoding the offsets and byte offsets will be the same
                if (regex->enc->max_enc_len == 1)
                {
                    result->offsets[loc] = result->byte_offsets[loc];
                    result->lengths[loc] = result->byte_lengths[loc];
                }
                else
                {
                    result->offsets[loc] = start_offset + onigenc_strlen(regex->enc, start_ptr, (UChar *) text+region->beg[i]);
                    result->lengths[loc] = onigenc_strlen(regex->enc, (UChar *) text+region->beg[i], (UChar *) text+region->end[i]);
                }
                
                // Set missing groups (which must be optional) to NULL; otherwise store match text
                if (length == 0)
                    result->matches[loc] = NULL;
                else
                    ore_rawmatch_store_string(result, loc, text+region->beg[i], length);
            }
            
            // Advance the starting point beyond the current match
            start_ptr = (UChar *) text + region->end[0];
            const size_t loc = match_number * region->num_regs;
            start_offset = result->offsets[loc] + result->lengths[loc];
            match_number++;
        }
        else
        {
            // Report the error message if there was one
            char message[ONIG_MAX_ERROR_MESSAGE_LEN];
            onig_error_code_to_str((UChar *) message, return_value);
            error("Oniguruma search: %s\n", message);
        }
        
        // Tidy up
        onig_region_free(region, 0);
        
        // If "all" is not true, the loop is only ever completed once
        if (!all)
            break;
    }
    
    // Store the number of matches
    if (result != NULL)
        result->n_matches = match_number;
    
    // Tidy up completely
    onig_region_free(region, 1);
    
    return result;
}

// Copy integer data from a rawmatch_t to an R vector
void ore_int_vector (SEXP vec, const int *data, const int n_regions, const int n_matches, const int increment)
{
    int *ptr = INTEGER(vec);
    for (int i=0; i<n_matches; i++)
        ptr[i] = data[i*n_regions] + increment;
}

// Copy string data from a rawmatch_t to an R vector
void ore_char_vector (SEXP vec, const char **data, const int n_regions, const int n_matches, const cetype_t encoding)
{
    for (int i=0; i<n_matches; i++)
        SET_STRING_ELT(vec, i, mkCharCE(data[i*n_regions],encoding));
}

// Copy integer data for groups into an R matrix
void ore_int_matrix (SEXP mat, const int *data, const int n_regions, const int n_matches, const SEXP col_names, const int increment)
{
    int *ptr = INTEGER(mat);
    for (int i=0; i<n_matches; i++)
    {
        for (int j=1; j<n_regions; j++)
            ptr[(j-1)*n_matches + i] = data[i*n_regions + j] + increment;
    }
    
    // Set column names if supplied
    if (!isNull(col_names))
    {
        SEXP my_col_names, dim_names;
        PROTECT(my_col_names = duplicate(col_names));
        PROTECT(dim_names = NEW_LIST(2));
        SET_VECTOR_ELT(dim_names, 0, R_NilValue);
        SET_VECTOR_ELT(dim_names, 1, my_col_names);
        setAttrib(mat, R_DimNamesSymbol, dim_names);
        UNPROTECT(2);
    }
}

// Copy string data from groups into an R matrix
void ore_char_matrix (SEXP mat, const char **data, const int n_regions, const int n_matches, const SEXP col_names, const cetype_t encoding)
{
    for (int i=0; i<n_matches; i++)
    {
        for (int j=1; j<n_regions; j++)
        {
            // Missing groups are assigned NA
            const char *element = data[i*n_regions + j];
            if (element == NULL)
                SET_STRING_ELT(mat, (j-1)*n_matches + i, NA_STRING);
            else
                SET_STRING_ELT(mat, (j-1)*n_matches + i, mkCharCE(element,encoding));
        }
    }
    
    // Set column names if supplied
    if (!isNull(col_names))
    {
        SEXP my_col_names, dim_names;
        PROTECT(my_col_names = duplicate(col_names));
        PROTECT(dim_names = NEW_LIST(2));
        SET_VECTOR_ELT(dim_names, 0, R_NilValue);
        SET_VECTOR_ELT(dim_names, 1, my_col_names);
        setAttrib(mat, R_DimNamesSymbol, dim_names);
        UNPROTECT(2);
    }
}

// Vectorised wrapper around ore_search(), which handles the R API stuff
SEXP ore_search_all (SEXP regex_, SEXP text_, SEXP all_, SEXP start_, SEXP simplify_)
{
    // Convert R objects to C types
    regex_t *regex = ore_retrieve(regex_, text_);
    SEXP group_names = getAttrib(regex_, install("groupNames"));
    const Rboolean all = asLogical(all_) == TRUE;
    const Rboolean simplify = asLogical(simplify_) == TRUE;
    int *start = INTEGER(start_);
    
    // Obtain the lengths of the text and start vectors (the latter will be recycled if necessary)
    const int text_len = length(text_);
    const int start_len = length(start_);
    
    // Check for sensible input
    if (text_len < 1)
        error("The text vector is empty");
    if (start_len < 1)
        error("The vector of starting positions is empty");
    
    SEXP results;
    PROTECT(results = NEW_LIST(text_len));
    
    // Step through each string to be searched
    for (int i=0; i<text_len; i++)
    {
        const cetype_t encoding = getCharCE(STRING_ELT(text_, i));
        if ((encoding == CE_UTF8 && regex->enc == ONIG_ENCODING_ISO_8859_1) || (encoding == CE_LATIN1 && regex->enc == ONIG_ENCODING_UTF8))
        {
            warning("Encoding of text element %d does not match the regex", i+1);
            SET_ELEMENT(results, i, R_NilValue);
            continue;
        }
        
        // Do the match
        rawmatch_t *raw_match = ore_search(regex, CHAR(STRING_ELT(text_,i)), all, (size_t) start[i % start_len] - 1);
        
        // Assign NULL if there's no match, otherwise build up an "orematch" object
        if (raw_match == NULL)
            SET_ELEMENT(results, i, R_NilValue);
        else
        {
            SEXP result, result_names, text, n_matches, offsets, byte_offsets, lengths, byte_lengths, matches;
            
            // Allocate memory for data structures
            PROTECT(result = NEW_LIST(raw_match->n_regions < 2 ? 7 : 8));
            PROTECT(result_names = NEW_CHARACTER(raw_match->n_regions < 2 ? 7 : 8));
            
            // List element names
            SET_STRING_ELT(result_names, 0, mkChar("text"));
            SET_STRING_ELT(result_names, 1, mkChar("nMatches"));
            SET_STRING_ELT(result_names, 2, mkChar("offsets"));
            SET_STRING_ELT(result_names, 3, mkChar("byteOffsets"));
            SET_STRING_ELT(result_names, 4, mkChar("lengths"));
            SET_STRING_ELT(result_names, 5, mkChar("byteLengths"));
            SET_STRING_ELT(result_names, 6, mkChar("matches"));
            
            // Convert elements of the raw match data to R vectors
            PROTECT(text = ScalarString(STRING_ELT(text_,i)));
            PROTECT(n_matches = ScalarInteger(raw_match->n_matches));
            PROTECT(offsets = NEW_INTEGER(raw_match->n_matches));
            ore_int_vector(offsets, raw_match->offsets, raw_match->n_regions, raw_match->n_matches, 1);
            PROTECT(byte_offsets = NEW_INTEGER(raw_match->n_matches));
            ore_int_vector(byte_offsets, raw_match->byte_offsets, raw_match->n_regions, raw_match->n_matches, 1);
            PROTECT(lengths = NEW_INTEGER(raw_match->n_matches));
            ore_int_vector(lengths, raw_match->lengths, raw_match->n_regions, raw_match->n_matches, 0);
            PROTECT(byte_lengths = NEW_INTEGER(raw_match->n_matches));
            ore_int_vector(byte_lengths, raw_match->byte_lengths, raw_match->n_regions, raw_match->n_matches, 0);
            PROTECT(matches = NEW_CHARACTER(raw_match->n_matches));
            ore_char_vector(matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, encoding);
            
            // Put everything in place
            SET_ELEMENT(result, 0, text);
            SET_ELEMENT(result, 1, n_matches);
            SET_ELEMENT(result, 2, offsets);
            SET_ELEMENT(result, 3, byte_offsets);
            SET_ELEMENT(result, 4, lengths);
            SET_ELEMENT(result, 5, byte_lengths);
            SET_ELEMENT(result, 6, matches);
            
            // Unprotect everything back to "text"
            UNPROTECT(7);
            
            // If there are groups present, extract them
            if (raw_match->n_regions > 1)
            {
                SEXP groups, groups_element_names;
                
                PROTECT(groups = NEW_LIST(5));
                PROTECT(groups_element_names = NEW_CHARACTER(5));
                
                SET_STRING_ELT(groups_element_names, 0, mkChar("offsets"));
                SET_STRING_ELT(groups_element_names, 1, mkChar("byteOffsets"));
                SET_STRING_ELT(groups_element_names, 2, mkChar("lengths"));
                SET_STRING_ELT(groups_element_names, 3, mkChar("byteLengths"));
                SET_STRING_ELT(groups_element_names, 4, mkChar("matches"));
                
                // Convert elements of the raw match data to R matrices (one row per match)
                PROTECT(offsets = allocMatrix(INTSXP, raw_match->n_matches, raw_match->n_regions-1));
                ore_int_matrix(offsets, raw_match->offsets, raw_match->n_regions, raw_match->n_matches, group_names, 1);
                PROTECT(byte_offsets = allocMatrix(INTSXP, raw_match->n_matches, raw_match->n_regions-1));
                ore_int_matrix(byte_offsets, raw_match->byte_offsets, raw_match->n_regions, raw_match->n_matches, group_names, 1);
                PROTECT(lengths = allocMatrix(INTSXP, raw_match->n_matches, raw_match->n_regions-1));
                ore_int_matrix(lengths, raw_match->lengths, raw_match->n_regions, raw_match->n_matches, group_names, 0);
                PROTECT(byte_lengths = allocMatrix(INTSXP, raw_match->n_matches, raw_match->n_regions-1));
                ore_int_matrix(byte_lengths, raw_match->byte_lengths, raw_match->n_regions, raw_match->n_matches, group_names, 0);
                PROTECT(matches = allocMatrix(STRSXP, raw_match->n_matches, raw_match->n_regions-1));
                ore_char_matrix(matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, group_names, encoding);
                
                // Put everything in place
                SET_ELEMENT(groups, 0, offsets);
                SET_ELEMENT(groups, 1, byte_offsets);
                SET_ELEMENT(groups, 2, lengths);
                SET_ELEMENT(groups, 3, byte_lengths);
                SET_ELEMENT(groups, 4, matches);
                
                // Set names and insert result into main list
                setAttrib(groups, R_NamesSymbol, groups_element_names);
                SET_ELEMENT(result, 7, groups);
                SET_STRING_ELT(result_names, 7, mkChar("groups"));
                
                UNPROTECT(7);
            }
            
            // Set names and class, and insert into full list
            setAttrib(result, R_NamesSymbol, result_names);
            setAttrib(result, R_ClassSymbol, mkString("orematch"));
            SET_ELEMENT(results, i, result);
            UNPROTECT(2);
        }
    }
    
    UNPROTECT(1);
    
    // Return just the first (and only) element of the full list, if requested
    if (simplify && text_len == 1)
        return VECTOR_ELT(results, 0);
    else
        return results;
}

// Split the strings provided at matches to the regex
SEXP ore_split (SEXP regex_, SEXP text_, SEXP start_, SEXP simplify_)
{
    if (isNull(regex_))
        error("The specified regex object is not valid");
    
    // Convert R objects to C types
    regex_t *regex = (regex_t *) ore_retrieve(regex_, text_);
    const Rboolean simplify = asLogical(simplify_) == TRUE;
    int *start = INTEGER(start_);
    
    // Obtain the lengths of the text and start vectors (the latter will be recycled if necessary)
    const int text_len = length(text_);
    const int start_len = length(start_);
    
    // Check for sensible input
    if (text_len < 1)
        error("The text vector is empty");
    if (start_len < 1)
        error("The vector of starting positions is empty");
    
    SEXP results;
    PROTECT(results = NEW_LIST(text_len));
    
    // Step through each string to be searched
    for (int i=0; i<text_len; i++)
    {
        const cetype_t encoding = getCharCE(STRING_ELT(text_, i));
        if ((encoding == CE_UTF8 && regex->enc == ONIG_ENCODING_ISO_8859_1) || (encoding == CE_LATIN1 && regex->enc == ONIG_ENCODING_UTF8))
        {
            warning("Encoding of text element %d does not match the regex", i+1);
            SET_ELEMENT(results, i, ScalarString(STRING_ELT(text_,i)));
            continue;
        }
        
        const char *text = CHAR(STRING_ELT(text_, i));
        
        // Do the match
        rawmatch_t *raw_match = ore_search(regex, text, TRUE, (size_t) start[i % start_len] - 1);
        
        // If there's no match the return value is the original string
        if (raw_match == NULL)
            SET_ELEMENT(results, i, ScalarString(STRING_ELT(text_,i)));
        else
        {
            // Create a vector long enough to hold the pieces
            SEXP result;
            PROTECT(result = NEW_CHARACTER(raw_match->n_matches + 1));
    
            int start = 0;
            char *fragment;
            size_t current_length;
            for (int j=0; j<raw_match->n_matches; j++)
            {
                const size_t loc = j * raw_match->n_regions;
                
                // Work out the length of the piece and allocate memory for it
                current_length = raw_match->byte_offsets[loc] - start;
                fragment = R_alloc(current_length+1, 1);
        
                // Copy text in, and insert the string into the return value
                if (current_length > 0)
                    strncpy(fragment, text+start, current_length);
                *(fragment + current_length) = '\0';
                SET_STRING_ELT(result, j, mkCharCE(fragment,encoding));
                start += current_length + raw_match->byte_lengths[loc];
            }
    
            // Likewise for the last piece
            current_length = strlen(text) - start;
            fragment = R_alloc(current_length+1, 1);
            if (current_length > 0)
                strncpy(fragment, text+start, current_length);
            *(fragment + current_length) = '\0';
            SET_STRING_ELT(result, raw_match->n_matches, mkCharCE(fragment,encoding));
            
            SET_ELEMENT(results, i, result);
            UNPROTECT(1);
        }
    }
    
    UNPROTECT(1);
    
    // Return just the first (and only) element of the full list, if requested
    if (simplify && text_len == 1)
        return VECTOR_ELT(results, 0);
    else
        return results;
}

// Replace substrings at the specified (byte) offsets with the literal replacements given
char * ore_substitute (const char *text, const int n_matches, const int *offsets, const int *lengths, const char **replacements)
{
    // Work out the length of each replacement string, and of the final text
    int *rep_lengths = (int *) R_alloc(n_matches, sizeof(int));
    size_t orig_len = strlen(text);
    size_t string_len = orig_len;
    for (int i=0; i<n_matches; i++)
    {
        rep_lengths[i] = strlen(replacements[i]);
        string_len += rep_lengths[i] - lengths[i];
    }
    
    // Work through the string, drawing from the original and the replacements in turn
    int start = 0;
    char *result = R_alloc(string_len+1, 1);
    char *result_ptr = result;
    for (int i=0; i<n_matches; i++)
    {
        strncpy(result_ptr, text+start, offsets[i]-start);
        result_ptr += offsets[i] - start;
        strncpy(result_ptr, replacements[i], rep_lengths[i]);
        result_ptr += rep_lengths[i];
        start = offsets[i] + lengths[i];
    }
    
    // Add any text after the last match
    if (start < orig_len)
        strncpy(result_ptr, text+start, orig_len-start);
    *(result + string_len) = '\0';
    
    return result;
}

// Thin R wrapper for ore_substitute(); substitutes literal replacements into a single string
SEXP ore_substitute_substrings (SEXP text_, SEXP n_matches_, SEXP offsets_, SEXP lengths_, SEXP replacements_)
{
    SEXP result;
    
    // Convert R objects to C types
    const char *text = CHAR(STRING_ELT(text_, 0));
    const cetype_t encoding = getCharCE(STRING_ELT(text_, 0));
    const int n_matches = asInteger(n_matches_);
    int *offsets = INTEGER(offsets_);
    const int *lengths = INTEGER(lengths_);
    
    for (int i=0; i<length(offsets_); i++)
        offsets[i]--;
    
    // Extract the replacements as C strings, from the R character vector provided
    const char **replacements = (const char **) R_alloc(n_matches, sizeof(char *));
    for (int j=0; j<n_matches; j++)
        replacements[j] = (const char *) CHAR(STRING_ELT(replacements_,j));
    
    // Do the substitution, and return the result
    char *result_string = ore_substitute(text, n_matches, offsets, lengths, replacements);
    PROTECT(result = NEW_CHARACTER(1));
    SET_STRING_ELT(result, 0, mkCharCE(result_string,encoding));
    UNPROTECT(1);
    
    return result;
}

// Find named or numbered back-references in a replacement string
backref_info_t * ore_find_backrefs (const char *replacement, SEXP group_names)
{
    // Match against global regexes for each type of back-reference
    rawmatch_t *group_number_match = ore_search(group_number_regex, replacement, TRUE, 0);
    rawmatch_t *group_name_match = ore_search(group_name_regex, replacement, TRUE, 0);
    
    // If there is no back-reference, return
    if (group_number_match == NULL && group_name_match == NULL)
        return NULL;
    else
    {
        backref_info_t *info = (backref_info_t *) R_alloc(1, sizeof(backref_info_t));
        const int n_number_matches = (group_number_match == NULL ? 0 : group_number_match->n_matches);
        const int n_name_matches = (group_name_match == NULL ? 0 : group_name_match->n_matches);
        
        // Set up backref_info_t struct
        info->n = n_number_matches + n_name_matches;
        info->offsets = (int *) R_alloc(info->n, sizeof(int));
        info->lengths = (int *) R_alloc(info->n, sizeof(int));
        info->group_numbers = (int *) R_alloc(info->n, sizeof(int));
        
        // We need to put back-reference locations in order for ore_substitute, whether named or numbered
        int i = 0, j = 0;
        int next_number_match = (group_number_match == NULL ? INT_MAX : group_number_match->byte_offsets[0]);
        int next_name_match = (group_name_match == NULL ? INT_MAX : group_name_match->byte_offsets[0]);
        for (int l=0; l<info->n; l++)
        {
            // Check which comes first
            if (next_number_match < next_name_match)
            {
                // If it's a number, extract the location and convert group number string to int
                const size_t loc = i * group_number_match->n_regions;
                info->offsets[l] = group_number_match->byte_offsets[loc];
                info->lengths[l] = group_number_match->byte_lengths[loc];
                info->group_numbers[l] = (int) strtol(group_number_match->matches[loc+1], NULL, 10);
                
                // Find the next number match, if there is one
                i++;
                next_number_match = (group_number_match->n_matches <= i ? INT_MAX : group_number_match->byte_offsets[i]);
            }
            else
            {
                const size_t loc = j * group_name_match->n_regions;
                info->offsets[l] = group_name_match->byte_offsets[loc];
                info->lengths[l] = group_name_match->byte_lengths[loc];
                
                // Look for the group name in the list of names specified
                Rboolean found = FALSE;
                for (int k=0; k<length(group_names); k++)
                {
                    if (strcmp(CHAR(STRING_ELT(group_names,k)), group_name_match->matches[loc+1]) == 0)
                    {
                        info->group_numbers[l] = k + 1;
                        found = TRUE;
                    }
                }
                
                // If it's not found, raise an error
                if (!found)
                    error("Back-reference does not match a named group");
                
                // Find the next name match, if there is one
                j++;
                next_name_match = (group_name_match->n_matches <= j ? INT_MAX : group_name_match->byte_offsets[j]);
            }
        }
        
        return info;
    }
}

// Vectorised substitution with a single replacement string, or R function
SEXP ore_substitute_all (SEXP regex_, SEXP replacement_, SEXP text_, SEXP all_, SEXP environment, SEXP function_args)
{
    if (isNull(regex_))
        error("The specified regex object is not valid");
    
    // Convert R objects to C types
    regex_t *regex = (regex_t *) ore_retrieve(regex_, text_);
    SEXP group_names = getAttrib(regex_, install("groupNames"));
    const Rboolean all = asLogical(all_) == TRUE;
    
    // Obtain the lengths of the text vector, and check it's sensible
    const int text_len = length(text_);
    if (text_len < 1)
        error("The text vector is empty");
    
    // Look for back-references in the replacement, if it's a string
    backref_info_t *backref_info = NULL;
    if (isString(replacement_))
    {
        if (length(replacement_) < 1)
            error("No replacement has been given");
        else if (length(replacement_) > 1)
            warning("All replacement strings after the first will be ignored");
        
        backref_info = ore_find_backrefs(CHAR(STRING_ELT(replacement_,0)), group_names);
    }
    
    SEXP results = PROTECT(NEW_CHARACTER(text_len));
    
    // Step through each string to be searched
    for (int i=0; i<text_len; i++)
    {
        // Find and check the encoding of the search string
        const cetype_t encoding = getCharCE(STRING_ELT(text_, i));
        if ((encoding == CE_UTF8 && regex->enc == ONIG_ENCODING_ISO_8859_1) || (encoding == CE_LATIN1 && regex->enc == ONIG_ENCODING_UTF8))
        {
            warning("Encoding of text element %d does not match the regex", i+1);
            SET_ELEMENT(results, i, ScalarString(STRING_ELT(text_,i)));
            continue;
        }
        
        const char *text = CHAR(STRING_ELT(text_, i));
        
        // Do the match
        rawmatch_t *raw_match = ore_search(regex, text, all, 0);
        
        // If there's no match the return value is the original string
        if (raw_match == NULL)
            SET_STRING_ELT(results, i, STRING_ELT(text_,i));
        else
        {
            const char **replacements = (const char **) R_alloc(raw_match->n_matches, sizeof(char *));
            
            // If the replacement is a function, construct a call to the function and run it
            if (isFunction(replacement_))
            {
                // Create an R character vector containing the matches
                SEXP matches = PROTECT(NEW_CHARACTER(raw_match->n_matches));
                ore_char_vector(matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, encoding);
                
                // This is arcane R API territory: we create a LANGSXP (an evaluable pairlist), and append the "..." pairlist, then evaluate the result and coerce to a character vector. For now the result must be the same length as the vector of matches
                SEXP call = PROTECT(listAppend(lang2(replacement_, matches), function_args));
                SEXP result = PROTECT(coerceVector(eval(call, environment), STRSXP));
                if (length(result) != length(matches))
                    error("The replacement function did not generate results of the same length as the input");
                
                // Extract the replacements as C strings, from the R character vector of results
                for (int j=0; j<raw_match->n_matches; j++)
                    replacements[j] = (const char *) CHAR(STRING_ELT(result,j));
                
                UNPROTECT(3);
            }
            else
            {
                // If the replacement is a string, then we may need to do another level of substitutions, if there are back-references
                const char *replacement_template = CHAR(STRING_ELT(replacement_, 0));
                if (backref_info != NULL)
                {
                    for (int j=0; j<raw_match->n_matches; j++)
                    {
                        const char **backref_replacements = (const char **) R_alloc(backref_info->n, sizeof(char *));
                        for (int k=0; k<backref_info->n; k++)
                            backref_replacements[k] = raw_match->matches[j*raw_match->n_regions + backref_info->group_numbers[k]];
                        replacements[j] = ore_substitute(replacement_template, backref_info->n, backref_info->offsets, backref_info->lengths, backref_replacements);
                    }
                }
                else
                {
                    // If not, the replacements are just the literal replacement string, so we reuse its pointer
                    for (int j=0; j<raw_match->n_matches; j++)
                        replacements[j] = replacement_template;
                }
            }
            
            // Since offsets and lengths are not contiguous if there are groups, we need to create new vectors that are
            int *offsets = (int *) R_alloc(raw_match->n_matches, sizeof(int));
            int *lengths = (int *) R_alloc(raw_match->n_matches, sizeof(int));
            for (int j=0; j<raw_match->n_matches; j++)
            {
                offsets[j] = raw_match->byte_offsets[j*raw_match->n_regions];
                lengths[j] = raw_match->byte_lengths[j*raw_match->n_regions];
            }
            
            // Do the main substitution, and insert the result
            char *result = ore_substitute(text, raw_match->n_matches, offsets, lengths, replacements);
            SET_STRING_ELT(results, i, mkCharCE(result,encoding));
        }
    }
    
    UNPROTECT(1);
    return results;
}
