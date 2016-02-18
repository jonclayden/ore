#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Riconv.h>

#include <string.h>

#include "compile.h"
#include "match.h"

// Not strictly part of the API, but needed for implementing the "start" argument
extern UChar * onigenc_step (OnigEncoding enc, const UChar *p, const UChar *end, int n);

// Block size for match data; memory is allocated in chunks this big
#define MATCH_BLOCK_SIZE        128

// Initial buffer size when reading from a connection; scales exponentially
#define CONNECTION_BUFFER_SIZE  1024

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
void ore_char_vector (SEXP vec, const char **data, const int n_regions, const int n_matches, const cetype_t encoding, const char *old_enc_name)
{
    void *iconv_handle = NULL;
    if (old_enc_name != NULL && strlen(old_enc_name) > 0)
        iconv_handle = Riconv_open("UTF-8", old_enc_name);
    
    for (int i=0; i<n_matches; i++)
    {
        if (data[i*n_regions] == NULL)
            SET_STRING_ELT(vec, i, mkCharCE("",encoding));
        else if (iconv_handle)
        {
            const char *element = data[i*n_regions];
            size_t old_size = strlen(element);
            size_t new_size = old_size * 6;
            char *buffer = R_alloc(new_size+1, 1);
            char *buffer_start = buffer;
            size_t written = Riconv(iconv_handle, &element, &old_size, &buffer, &new_size);
            *buffer = '\0';
            SET_STRING_ELT(vec, i, mkCharCE(buffer_start,CE_UTF8));
        }
        else
            SET_STRING_ELT(vec, i, mkCharCE(data[i*n_regions],encoding));
    }
    
    if (iconv_handle)
        Riconv_close(iconv_handle);
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

// Read text from a connection into a buffer
char * ore_read_connection (Rconnection connection)
{
#if (R_CONNECTIONS_VERSION > 1)
    warning("Connection API may have changed");
#endif
    
    size_t old_buffer_size = 0;
    size_t buffer_size = CONNECTION_BUFFER_SIZE;
    char *buffer = (char *) R_alloc(buffer_size, 1);
    char *ptr = buffer;
    
    Rboolean opened = FALSE;
    if (!connection->isopen)
        opened = connection->open(connection);
    
    while (TRUE)
    {
        size_t n = buffer_size - old_buffer_size;
        size_t bytes_read = R_ReadConnection(connection, ptr, n);
        if (bytes_read < n)
            return buffer;
        else
        {
            old_buffer_size = buffer_size;
            buffer_size *= 2;
            ore_realloc(buffer, buffer_size, old_buffer_size, 1);
            ptr = buffer + old_buffer_size;
        }
    }
    
    if (opened)
        connection->destroy(connection);
}

// Vectorised wrapper around ore_search(), which handles the R API stuff
SEXP ore_search_all (SEXP regex_, SEXP text_, SEXP all_, SEXP start_, SEXP simplify_)
{
    // Convert R objects to C types
    SEXP group_names = getAttrib(regex_, install("groupNames"));
    const Rboolean all = asLogical(all_) == TRUE;
    const Rboolean simplify = asLogical(simplify_) == TRUE;
    int *start = INTEGER(start_);
    
    // Check whether the text argument is actually a connection
    Rboolean using_connection = FALSE;
    SEXP textClass = getAttrib(text_, R_ClassSymbol);
    for (int i=0; i<length(textClass); i++)
    {
        if (strcmp(CHAR(STRING_ELT(textClass,i)), "connection") == 0)
        {
            using_connection = TRUE;
            break;
        }
    }
    
    if (!using_connection)
        PROTECT(text_ = AS_CHARACTER(text_));
    
    // Retrieve the regex
    regex_t *regex = ore_retrieve(regex_, text_, using_connection);
    
    // Obtain the lengths of the text and start vectors (the latter will be recycled if necessary)
    const int text_len = using_connection ? 1 : length(text_);
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
        rawmatch_t *raw_match;
        cetype_t encoding;
        Rconnection connection;
        char *content;
        
        if (using_connection)
        {
            // Check the connection's encoding
            connection = getConnection(asInteger(text_));
            OnigEncoding encoding = ore_con_to_onig_enc(connection);
            if (encoding != regex->enc)
                warning("Connection encoding does not match the regex");
            
            // Do the match
            content = ore_read_connection(connection);
            raw_match = ore_search(regex, content, all, (size_t) start[0] - 1);
        }
        else
        {
            // Check the string's encoding
            encoding = getCharCE(STRING_ELT(text_, i));
            if ((encoding == CE_UTF8 && regex->enc == ONIG_ENCODING_ISO_8859_1) || (encoding == CE_LATIN1 && regex->enc == ONIG_ENCODING_UTF8))
            {
                warning("Encoding of text element %d does not match the regex", i+1);
                SET_ELEMENT(results, i, R_NilValue);
                continue;
            }
        
            // Do the match
            raw_match = ore_search(regex, CHAR(STRING_ELT(text_,i)), all, (size_t) start[i % start_len] - 1);
        }
        
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
            // Note that the text can't easily be returned from a connection because offsets and lengths may be wrong after translation between encodings
            if (using_connection)
                text = R_NilValue;
            else
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
            if (using_connection)
                ore_char_vector(matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, encoding, connection->encname);
            else
                ore_char_vector(matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, encoding, NULL);
            
            // Put everything in place
            SET_ELEMENT(result, 0, text);
            SET_ELEMENT(result, 1, n_matches);
            SET_ELEMENT(result, 2, offsets);
            SET_ELEMENT(result, 3, byte_offsets);
            SET_ELEMENT(result, 4, lengths);
            SET_ELEMENT(result, 5, byte_lengths);
            SET_ELEMENT(result, 6, matches);
            
            // Unprotect everything back to "text"
            UNPROTECT(using_connection ? 6 : 7);
            
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
                // ore_char_matrix(matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, group_names, encoding);
                
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
    
    UNPROTECT(using_connection ? 1 : 2);
    
    // Return just the first (and only) element of the full list, if requested
    if (simplify && text_len == 1)
        return VECTOR_ELT(results, 0);
    else
        return results;
}
