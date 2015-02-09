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

SEXP ore_print_match (SEXP match, SEXP context_, SEXP width_, SEXP max_lines_)
{
    const int context = asInteger(context_);
    const int width = asInteger(width_);
    const int max_lines = asInteger(max_lines_);
    const int n_matches = asInteger(ore_get_list_element(match, "nMatches"));
    
    if (n_matches)
    for (int i=0; i<n_matches; i++)
    {
        
    }
}
