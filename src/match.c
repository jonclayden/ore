#include <string.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Riconv.h>

#include "compile.h"
#include "text.h"
#include "match.h"

// Not strictly part of the API, but needed for implementing the "start" argument
extern UChar * onigenc_step (OnigEncoding enc, const UChar *p, const UChar *end, int n);

// Block size for match data; memory is allocated in chunks this big
#define MATCH_BLOCK_SIZE    128

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
rawmatch_t * ore_search (regex_t *regex, const char *text, const char *text_end, const Rboolean all, const size_t start)
{
    int return_value, length;
    rawmatch_t *result = NULL;
    
    // Create region object to capture match data
    OnigRegion *region = onig_region_new();
    
    // The number of matches found so far
    int match_number = 0;
    
    // Find the end-point of the text (for binary data it may include null bytes)
    UChar *end_ptr;
    if (text_end != NULL)
        end_ptr = (UChar *) text_end;
    else
        end_ptr = (UChar *) text + strlen(text);
    
    // If we're not starting at the beginning, step forward the required number of characters
    UChar *start_ptr;
    if (start == 0)
        start_ptr = (UChar *) text;
    else if (regex->enc->max_enc_len == 1)
        start_ptr = (UChar *) text + start;
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
void ore_char_vector (SEXP vec, const char **data, const int n_regions, const int n_matches, encoding_t *encoding)
{
    void *iconv_handle = NULL;
    if (encoding != NULL)
    {
        if (ore_strnicmp(encoding->name, "native.enc", 10) == 0)
            iconv_handle = Riconv_open("UTF-8", "");
        else
            iconv_handle = Riconv_open("UTF-8", encoding->name);
        encoding->r_enc = CE_UTF8;
    }
    
    for (int i=0; i<n_matches; i++)
    {
        if (data[i*n_regions] == NULL)
            SET_STRING_ELT(vec, i, mkCharCE("",encoding->r_enc));
        else
            SET_STRING_ELT(vec, i, mkCharCE(ore_iconv(iconv_handle,data[i*n_regions]), encoding->r_enc));
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
void ore_char_matrix (SEXP mat, const char **data, const int n_regions, const int n_matches, const SEXP col_names, encoding_t *encoding)
{
    void *iconv_handle = NULL;
    if (encoding != NULL)
    {
        if (ore_strnicmp(encoding->name, "native.enc", 10) == 0)
            iconv_handle = Riconv_open("UTF-8", "");
        else
            iconv_handle = Riconv_open("UTF-8", encoding->name);
        encoding->r_enc = CE_UTF8;
    }
    
    for (int i=0; i<n_matches; i++)
    {
        for (int j=1; j<n_regions; j++)
        {
            // Missing groups are assigned NA
            const char *element = data[i*n_regions + j];
            if (element == NULL)
                SET_STRING_ELT(mat, (j-1)*n_matches + i, NA_STRING);
            else
                SET_STRING_ELT(mat, (j-1)*n_matches + i, mkCharCE(ore_iconv(iconv_handle,element), encoding->r_enc));
        }
    }
    
    if (iconv_handle)
        Riconv_close(iconv_handle);
    
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
SEXP ore_search_all (SEXP regex_, SEXP text_, SEXP all_, SEXP start_, SEXP simplify_, SEXP incremental_)
{
    // Convert R objects to C types
    SEXP group_names = getAttrib(regex_, install("groupNames"));
    const Rboolean all = asLogical(all_) == TRUE;
    const Rboolean simplify = asLogical(simplify_) == TRUE;
    const Rboolean incremental = asLogical(incremental_) == TRUE;
    int *start = INTEGER(start_);
    
    // Check whether the text argument is actually a file path
    const Rboolean using_file = inherits(text_, "orefile") || inherits(text_, "connection");
    if (!using_file)
        PROTECT(text_ = AS_CHARACTER(text_));
    
    // Retrieve the text and the regex
    text_t *text = ore_text(text_);
    regex_t *regex = ore_retrieve(regex_, text->encoding);
    
    // Obtain the length of the start vector (which will be recycled if necessary)
    const int start_len = length(start_);
    
    // Check for sensible input
    if (start_len < 1)
        error("The vector of starting positions is empty");
    
    SEXP results;
    PROTECT(results = NEW_LIST(text->length));
    
    // Step through each string to be searched
    for (size_t i=0; i<text->length; i++)
    {
        text_element_t *text_element;
        rawmatch_t *raw_match;
        
        if (incremental && !all && text->source == FILE_SOURCE)
        {
            for (int j=1; ; j++)
            {
                text_element = ore_text_element(text, j);
                if (j == 1 && !ore_consistent_encodings(text_element->encoding->onig_enc, regex->enc))
                {
                    warning("File encoding does not match the regex");
                    SET_ELEMENT(results, i, R_NilValue);
                    break;
                }
                
                raw_match = ore_search(regex, text_element->start, text_element->end, all, (size_t) start[0] - 1);
                
                size_t end_of_last_match = 0;
                if (raw_match != NULL)
                    end_of_last_match = (size_t) raw_match->byte_offsets[raw_match->n_matches-1] + raw_match->byte_lengths[raw_match->n_matches-1];
                
                if (!text_element->incomplete || (raw_match != NULL && end_of_last_match < (text_element->end - text_element->start)))
                    break;
            }
        }
        else
        {
            text_element = ore_text_element(text, i);
            if (!ore_consistent_encodings(text_element->encoding->onig_enc, regex->enc))
            {
                warning("Encoding of text element %d does not match the regex", i+1);
                SET_ELEMENT(results, i, R_NilValue);
                continue;
            }
            
            // Do the match
            raw_match = ore_search(regex, text_element->start, text_element->end, all, (size_t) start[i % start_len] - 1);
        }
        
        // Assign NULL if there's no match, otherwise build up an "orematch" object
        if (raw_match == NULL)
            SET_ELEMENT(results, i, R_NilValue);
        else
        {
            SEXP result, result_names, result_text, n_matches, offsets, byte_offsets, lengths, byte_lengths, matches;
            
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
            // Note that the text can't easily be returned from a file because offsets and lengths may be wrong after translation between encodings
            if (using_file)
                result_text = R_NilValue;
            else
                PROTECT(result_text = ScalarString(STRING_ELT(text_,i)));
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
            ore_char_vector(matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, text_element->encoding);
            
            // Put everything in place
            SET_ELEMENT(result, 0, result_text);
            SET_ELEMENT(result, 1, n_matches);
            SET_ELEMENT(result, 2, offsets);
            SET_ELEMENT(result, 3, byte_offsets);
            SET_ELEMENT(result, 4, lengths);
            SET_ELEMENT(result, 5, byte_lengths);
            SET_ELEMENT(result, 6, matches);
            
            // Unprotect everything back to "result_text"
            UNPROTECT(using_file ? 6 : 7);
            
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
                ore_char_matrix(matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, group_names, text_element->encoding);
                
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
    
    ore_text_done(text);
    
    UNPROTECT(using_file ? 1 : 2);
    
    // Return just the first (and only) element of the full list, if requested
    if (simplify && text->length == 1)
        return VECTOR_ELT(results, 0);
    else
    {
        setAttrib(results, R_ClassSymbol, mkString("orematches"));
        return results;
    }
}
