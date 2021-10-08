#include <string.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "compile.h"
#include "text.h"
#include "match.h"
#include "subst.h"

regex_t *group_number_regex;
regex_t *group_name_regex;

typedef struct {
    int     n;
    int   * offsets;
    int   * lengths;
    int   * group_numbers;
} backref_info_t;

// Replace substrings at the specified (byte) offsets with the literal replacements given
static char * ore_substitute (const char *text, const int n_matches, const int *offsets, const int *lengths, const char **replacements)
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

// Find named or numbered back-references in a replacement string
static backref_info_t * ore_find_backrefs (const char *replacement, SEXP group_names)
{
    // Match against global regexes for each type of back-reference
    rawmatch_t *group_number_match = ore_search(group_number_regex, replacement, NULL, TRUE, 0);
    rawmatch_t *group_name_match = ore_search(group_name_regex, replacement, NULL, TRUE, 0);
    
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

// Substitution vectorised over matches, with replacement functions called once per string
SEXP ore_substitute_all (SEXP regex_, SEXP replacement_, SEXP text_, SEXP all_, SEXP environment, SEXP function_args)
{
    if (isNull(regex_))
        error("The specified regex object is not valid");
    
    // Convert R objects to C types
    text_t *text = ore_text(text_);
    regex_t *regex = ore_retrieve(regex_, text->encoding);
    const int n_groups = onig_number_of_captures(regex);
    SEXP group_names = getAttrib(regex_, install("groupNames"));
    const Rboolean all = asLogical(all_) == TRUE;
    
    const char nul = '\0';
    
    // Look for back-references in the replacement, if it's character-mode
    backref_info_t **backref_info = NULL;
    int replacement_len = 1;
    if (isString(replacement_))
    {
        replacement_len = length(replacement_);
        if (replacement_len < 1)
            error("No replacement has been given");
        
        backref_info = (backref_info_t **) R_alloc(replacement_len, sizeof(backref_info_t *));
        for (int j=0; j<replacement_len; j++)
        {
            backref_info[j] = ore_find_backrefs(CHAR(STRING_ELT(replacement_,j)), group_names);
            if (backref_info[j] != NULL)
            {
                for (int k=0; k<backref_info[j]->n; k++)
                {
                    if (backref_info[j]->group_numbers[k] > n_groups)
                        error("Replacement %d references a group number (%d) that isn't captured", j+1, backref_info[j]->group_numbers[k]);
                }
            }
        }
    }
    
    SEXP results = PROTECT(NEW_CHARACTER(text->length));
    
    // Step through each string to be searched
    for (int i=0; i<text->length; i++)
    {
        text_element_t *text_element = ore_text_element(text, i, FALSE, NULL);
        if (!ore_consistent_encodings(text_element->encoding->onig_enc, regex->enc))
        {
            warning("Encoding of text element %d does not match the regex", i+1);
            SET_STRING_ELT(results, i, ore_text_element_to_rchar(text_element));
            continue;
        }
        
        // Do the match
        rawmatch_t *raw_match = ore_search(regex, text_element->start, text_element->end, all, 0);
        
        // If there's no match the return value is the original string
        if (raw_match == NULL)
            SET_STRING_ELT(results, i, ore_text_element_to_rchar(text_element));
        else
        {
            const char **replacements = (const char **) R_alloc(raw_match->n_matches, sizeof(char *));
            
            // If the replacement is a function, construct a call to the function and run it
            if (isFunction(replacement_))
            {
                // Create an R character vector containing the matches
                SEXP matches = PROTECT(NEW_CHARACTER(raw_match->n_matches));
                ore_char_vector(matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, text_element->encoding);
                
                // If there are groups, extract them and put them in an attribute
                if (raw_match->n_regions > 1)
                {
                    SEXP group_matches = PROTECT(allocMatrix(STRSXP, raw_match->n_matches, raw_match->n_regions-1));
                    ore_char_matrix(group_matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, -1, group_names, text_element->encoding);
                    setAttrib(matches, install("groups"), group_matches);
                    UNPROTECT(1);
                }
                
                setAttrib(matches, R_ClassSymbol, mkString("orearg"));
                
                // This is arcane R API territory: we create a LANGSXP (an evaluable pairlist), and append the "..." pairlist, then evaluate the result and coerce to a character vector
                SEXP call = PROTECT(listAppend(lang2(replacement_, matches), function_args));
                SEXP result = PROTECT(eval(call, environment));
                SEXP char_result = PROTECT(coerceVector(result, STRSXP));
                const int result_len = length(char_result);
                
                // Extract the replacements as C strings, from the R character vector of results
                for (int j=0; j<raw_match->n_matches; j++)
                {
                    if (result_len == 0)
                        replacements[j] = &nul;
                    else
                        replacements[j] = (const char *) CHAR(STRING_ELT(char_result, j % result_len));
                }
                
                UNPROTECT(4);
            }
            else
            {
                // If the replacement is a string, then we may need to do another level of substitutions, if there are back-references
                for (int j=0; j<raw_match->n_matches; j++)
                {
                    // This subindex determines which replacement element is used for this match
                    const int jj = j % replacement_len;
                    const char *replacement_template = CHAR(STRING_ELT(replacement_, jj));
                    if (backref_info[jj] != NULL)
                    {
                        const char **backref_replacements = (const char **) R_alloc(backref_info[jj]->n, sizeof(char *));
                        for (int k=0; k<backref_info[jj]->n; k++)
                            backref_replacements[k] = raw_match->matches[j*raw_match->n_regions + backref_info[jj]->group_numbers[k]];
                        replacements[j] = ore_substitute(replacement_template, backref_info[jj]->n, backref_info[jj]->offsets, backref_info[jj]->lengths, backref_replacements);
                    }
                    else
                    {
                        // If not, the replacements are just the literal replacement string, so we reuse its pointer
                        for (int j=0; j<raw_match->n_matches; j++)
                            replacements[j] = replacement_template;
                    }
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
            char *result = ore_substitute(text_element->start, raw_match->n_matches, offsets, lengths, replacements);
            SET_STRING_ELT(results, i, ore_string_to_rchar(result, text_element->encoding));
        }
    }
    
    ore_text_done(text);
    
    UNPROTECT(1);
    return results;
}

// Substitution vectorised over replacements, with replacement functions called once per match
SEXP ore_replace_all (SEXP regex_, SEXP replacement_, SEXP text_, SEXP all_, SEXP simplify_, SEXP environment, SEXP function_args)
{
    if (isNull(regex_))
        error("The specified regex object is not valid");
    
    // Convert R objects to C types
    text_t *text = ore_text(text_);
    regex_t *regex = ore_retrieve(regex_, text->encoding);
    const int n_groups = onig_number_of_captures(regex);
    SEXP group_names = getAttrib(regex_, install("groupNames"));
    const Rboolean all = asLogical(all_) == TRUE;
    const Rboolean simplify = asLogical(simplify_) == TRUE;
    
    const char nul = '\0';
    
    // Look for back-references in the replacement, if it's character-mode
    backref_info_t **backref_info = NULL;
    int base_replacement_len = 1;
    if (isString(replacement_))
    {
        base_replacement_len = length(replacement_);
        if (base_replacement_len < 1)
            error("No replacement has been given");
        
        backref_info = (backref_info_t **) R_alloc(base_replacement_len, sizeof(backref_info_t *));
        for (int j=0; j<base_replacement_len; j++)
        {
            backref_info[j] = ore_find_backrefs(CHAR(STRING_ELT(replacement_,j)), group_names);
            if (backref_info[j] != NULL)
            {
                for (int k=0; k<backref_info[j]->n; k++)
                {
                    if (backref_info[j]->group_numbers[k] > n_groups)
                        error("Replacement %d references a group number (%d) that isn't captured", j+1, backref_info[j]->group_numbers[k]);
                }
            }
        }
    }
    
    SEXP results = PROTECT(NEW_LIST(text->length));
    
    // Step through each string to be searched
    for (int i=0; i<text->length; i++)
    {
        text_element_t *text_element = ore_text_element(text, i, FALSE, NULL);
        if (!ore_consistent_encodings(text_element->encoding->onig_enc, regex->enc))
        {
            warning("Encoding of text element %d does not match the regex", i+1);
            SET_ELEMENT(results, i, ScalarString(ore_text_element_to_rchar(text_element)));
            continue;
        }
        
        // Do the match
        rawmatch_t *raw_match = ore_search(regex, text_element->start, text_element->end, all, 0);
        
        int replacement_len = base_replacement_len;
        
        // A 2D array of strings to hold literal replacements for each match
        const char ***replacements = NULL;
        
        // If there are matches, process the replacements
        if (raw_match != NULL)
        {
            if (isFunction(replacement_))
            {
                SEXP parts = PROTECT(NEW_LIST(raw_match->n_matches));
                for (int l=0; l<raw_match->n_matches; l++)
                {
                    SEXP match = PROTECT(NEW_CHARACTER(1));
                    ore_char_vector(match, (const char **) &raw_match->matches[l], raw_match->n_regions, 1, text_element->encoding);
                    
                    if (raw_match->n_regions > 1)
                    {
                        SEXP group_matches = PROTECT(allocMatrix(STRSXP, 1, raw_match->n_regions-1));
                        ore_char_matrix(group_matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, l, group_names, text_element->encoding);
                        setAttrib(match, install("groups"), group_matches);
                        UNPROTECT(1);
                    }
                    
                    setAttrib(match, R_ClassSymbol, mkString("orearg"));
                    
                    SEXP call = PROTECT(listAppend(lang2(replacement_, match), function_args));
                    SEXP result = PROTECT(eval(call, environment));
                    SEXP char_result = PROTECT(coerceVector(result, STRSXP));
                    
                    const int result_len = length(char_result);
                    if (result_len > replacement_len)
                        replacement_len = result_len;
                    
                    SET_ELEMENT(parts, l, char_result);
                    UNPROTECT(4);
                }
                
                replacements = (const char ***) R_alloc(replacement_len, sizeof(char **));
                for (int j=0; j<replacement_len; j++)
                {
                    replacements[j] = (const char **) R_alloc(raw_match->n_matches, sizeof(char *));
                    for (int l=0; l<raw_match->n_matches; l++)
                    {
                        SEXP element = VECTOR_ELT(parts, l);
                        if (length(element) == 0)
                            replacements[j][l] = &nul;
                        else
                            replacements[j][l] = CHAR(STRING_ELT(element, j % length(element)));
                    }
                }
            }
            else
            {
                replacements = (const char ***) R_alloc(replacement_len, sizeof(char **));
                for (int j=0; j<replacement_len; j++)
                {
                    replacements[j] = (const char **) R_alloc(raw_match->n_matches, sizeof(char *));
                    for (int l=0; l<raw_match->n_matches; l++)
                    {
                        const char *replacement_template = CHAR(STRING_ELT(replacement_, j));
                        if (backref_info[j] != NULL)
                        {
                            const char **backref_replacements = (const char **) R_alloc(backref_info[j]->n, sizeof(char *));
                            for (int k=0; k<backref_info[j]->n; k++)
                                backref_replacements[k] = raw_match->matches[l*raw_match->n_regions + backref_info[j]->group_numbers[k]];
                            replacements[j][l] = ore_substitute(replacement_template, backref_info[j]->n, backref_info[j]->offsets, backref_info[j]->lengths, backref_replacements);
                        }
                        else
                            replacements[j][l] = replacement_template;
                    }
                }
            }
        }
        
        SEXP result = PROTECT(NEW_CHARACTER(replacement_len));
        for (int j=0; j<replacement_len; j++)
        {
            // If there is no match there is no replacement, so the return value is the original string
            if (replacements == NULL || replacements[j] == NULL)
                SET_STRING_ELT(result, j, ore_text_element_to_rchar(text_element));
            else
            {
                int *offsets = (int *) R_alloc(raw_match->n_matches, sizeof(int));
                int *lengths = (int *) R_alloc(raw_match->n_matches, sizeof(int));
                for (int l=0; l<raw_match->n_matches; l++)
                {
                    offsets[l] = raw_match->byte_offsets[l*raw_match->n_regions];
                    lengths[l] = raw_match->byte_lengths[l*raw_match->n_regions];
                }
                
                // Do the main substitution, and insert the result
                char *result_str = ore_substitute(text_element->start, raw_match->n_matches, offsets, lengths, replacements[j]);
                SET_STRING_ELT(result, j, ore_string_to_rchar(result_str, text_element->encoding));
            }
        }
        
        SET_ELEMENT(results, i, result);
        
        UNPROTECT(raw_match != NULL && isFunction(replacement_) ? 2 : 1);
    }
    
    ore_text_done(text);
    
    UNPROTECT(1);
    
    // Return just the first (and only) element of the full list, if requested
    if (simplify && text->length == 1)
        return VECTOR_ELT(results, 0);
    else
        return results;
}

SEXP ore_switch_all (SEXP text_, SEXP mappings_, SEXP options_, SEXP encoding_name_)
{
    if (length(mappings_) == 0)
        error("No mappings have been given");
    if (!isString(mappings_))
        error("Mappings should be character strings");
    
    text_t *text = ore_text(text_);
    SEXP patterns = getAttrib(mappings_, R_NamesSymbol);
    const char *options = CHAR(STRING_ELT(options_, 0));
    const char *encoding_name = CHAR(STRING_ELT(encoding_name_, 0));
    
    encoding_t *encoding;
    if (ore_strnicmp(encoding_name, "auto", 4) == 0)
    {
        cetype_t r_enc = getCharCE(STRING_ELT(patterns, 0));
        encoding = ore_encoding(NULL, NULL, &r_enc);
    }
    else
        encoding = ore_encoding(encoding_name, NULL, NULL);
    
    Rboolean *done = (Rboolean *) R_alloc(text->length, sizeof(Rboolean));
    for (int i=0; i<text->length; i++)
        done[i] = FALSE;
    
    SEXP results = PROTECT(NEW_CHARACTER(text->length));
    for (int i=0; i<text->length; i++)
        SET_STRING_ELT(results, i, NA_STRING);
    
    for (int j=0; j<length(mappings_); j++)
    {
        // Compile the regex
        regex_t *regex = NULL;
        backref_info_t *backref_info = NULL;
        SEXP mapping = STRING_ELT(mappings_, j);
        if (!isNull(patterns) && *CHAR(STRING_ELT(patterns, j)) != '\0')
        {
            regex = ore_compile(CHAR(STRING_ELT(patterns,j)), options, encoding, "ruby");
            
            const int n_groups = onig_number_of_captures(regex);
            SEXP group_names = PROTECT(NEW_CHARACTER(n_groups));
            
            if (ore_group_name_vector(group_names, regex))
                backref_info = ore_find_backrefs(CHAR(mapping), group_names);
            else
                backref_info = ore_find_backrefs(CHAR(mapping), R_NilValue);
            
            for (int k=0; k<backref_info->n; k++)
            {
                if (backref_info->group_numbers[k] > n_groups)
                    error("Template %d references a group number (%d) that isn't captured", j+1, backref_info->group_numbers[k]);
            }
            
            UNPROTECT(1);
        }
        
        for (int i=0; i<text->length; i++)
        {
            if (done[i])
                continue;
            else if (regex == NULL)
            {
                SET_STRING_ELT(results, i, mapping);
                done[i] = TRUE;
            }
            else
            {
                text_element_t *text_element = ore_text_element(text, i, FALSE, NULL);
                if (!ore_consistent_encodings(text_element->encoding->onig_enc, regex->enc))
                    continue;
                
                // Do the match
                rawmatch_t *raw_match = ore_search(regex, text_element->start, text_element->end, FALSE, 0);
                
                if (raw_match == NULL)
                    continue;
                
                const char **backref_replacements = (const char **) R_alloc(backref_info->n, sizeof(char *));
                for (int k=0; k<backref_info->n; k++)
                    backref_replacements[k] = raw_match->matches[backref_info->group_numbers[k]];
                char *result = ore_substitute(CHAR(mapping), backref_info->n, backref_info->offsets, backref_info->lengths, backref_replacements);
                
                SET_STRING_ELT(results, i, ore_string_to_rchar(result, text_element->encoding));
                done[i] = TRUE;
            }
        }
    }
    
    ore_text_done(text);
    
    UNPROTECT(1);
    return results;
}
