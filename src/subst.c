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

// Vectorised substitution with a single replacement string, or R function
SEXP ore_substitute_all (SEXP regex_, SEXP replacement_, SEXP text_, SEXP all_, SEXP environment, SEXP function_args)
{
    if (isNull(regex_))
        error("The specified regex object is not valid");
    
    // Convert R objects to C types
    text_t *text = ore_text(text_);
    regex_t *regex = ore_retrieve(regex_, text->encoding);
    SEXP group_names = getAttrib(regex_, install("groupNames"));
    const Rboolean all = asLogical(all_) == TRUE;
    
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
    
    SEXP results = PROTECT(NEW_CHARACTER(text->length));
    
    // Step through each string to be searched
    for (int i=0; i<text->length; i++)
    {
        text_element_t *text_element = ore_text_element(text, i);
        if (!ore_consistent_encodings(text_element->encoding, regex->enc))
        {
            warning("Encoding of text element %d does not match the regex", i+1);
            SET_ELEMENT(results, i, ScalarString(ore_text_element_to_rchar(text_element, text->encoding_name)));
            continue;
        }
        
        // Do the match
        rawmatch_t *raw_match = ore_search(regex, text_element->start, text_element->end, all, 0);
        
        // If there's no match the return value is the original string
        if (raw_match == NULL)
            SET_STRING_ELT(results, i, ore_text_element_to_rchar(text_element, text->encoding_name));
        else
        {
            const char **replacements = (const char **) R_alloc(raw_match->n_matches, sizeof(char *));
            const cetype_t r_encoding = ore_onig_to_r_enc(text_element->encoding);
            
            // If the replacement is a function, construct a call to the function and run it
            if (isFunction(replacement_))
            {
                // Create an R character vector containing the matches
                SEXP matches = PROTECT(NEW_CHARACTER(raw_match->n_matches));
                ore_char_vector(matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, r_encoding, NULL);
                
                // If there are groups, extract them and put them in an attribute
                if (raw_match->n_regions > 1)
                {
                    SEXP group_matches = PROTECT(allocMatrix(STRSXP, raw_match->n_matches, raw_match->n_regions-1));
                    ore_char_matrix(group_matches, (const char **) raw_match->matches, raw_match->n_regions, raw_match->n_matches, group_names, r_encoding, NULL);
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
                    replacements[j] = (const char *) CHAR(STRING_ELT(char_result, j % result_len));
                
                UNPROTECT(4);
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
            char *result = ore_substitute(text_element->start, raw_match->n_matches, offsets, lengths, replacements);
            SET_STRING_ELT(results, i, ore_string_to_rchar(result, ore_onig_to_r_enc(text_element->encoding), text->encoding_name));
        }
    }
    
    ore_text_done(text);
    
    UNPROTECT(1);
    return results;
}
