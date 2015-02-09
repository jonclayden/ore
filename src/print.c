#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <string.h>

#include "match.h"
#include "print.h"

SEXP ore_get_list_element (SEXP list, const char *name)
{
    SEXP element = R_NilValue;
    SEXP names = getAttrib(list, R_NamesSymbol);
    
    for (int i=0; i<length(names); i++)
    {
    	if (strcmp(CHAR(STRING_ELT(names,i)), name) == 0)
        {
    	   element = VECTOR_ELT(list, i);
    	   break;
       }
    }
    
    return element;
}

SEXP ore_print_match (SEXP match, SEXP context_, SEXP width_, SEXP max_lines_, SEXP use_colour_)
{
    const int context = asInteger(context_);
    const int width = asInteger(width_);
    const int max_lines = asInteger(max_lines_);
    const Rboolean use_colour = (asLogical(use_colour) == TRUE);
    
    const int n_matches = asInteger(ore_get_list_element(match, "nMatches"));
    const int *offsets = (const int *) INTEGER(ore_get_list_element(match, "offsets"));
    const int *byte_offsets = (const int *) INTEGER(ore_get_list_element(match, "byteOffsets"));
    const int *lengths = (const int *) INTEGER(ore_get_list_element(match, "lengths"));
    const int *byte_lengths = (const int *) INTEGER(ore_get_list_element(match, "byteLengths"));
    
    int start = 0;
    for (int l=0; l<max_lines; l++)
    {
        int left = width - 9;
        
    }
}
