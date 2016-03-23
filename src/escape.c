#include <string.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "escape.h"

SEXP ore_escape (SEXP text_)
{
    const int text_len = length(text_);
    SEXP results = PROTECT(NEW_CHARACTER(text_len));
    
    for (int i=0; i<text_len; i++)
    {
        const char *element = CHAR(STRING_ELT(text_, i));
        char *result = R_alloc(2*strlen(element) + 1, 1);
        char *ptr = (char *) element;
        char *result_ptr = result;
        
        while (*ptr)
        {
            switch (*ptr)
            {
                case '.':
                case '?':
                case '*':
                case '+':
                case '^':
                case '$':
                case '[':
                case ']':
                case '\\':
                case '(':
                case ')':
                case '{':
                case '}':
                case '|':
                *(result_ptr++) = '\\';
                *(result_ptr++) = *(ptr++);
                break;
                
                case '\t':
                *(result_ptr++) = '\\';
                *(result_ptr++) = 't';
                ptr++;
                break;
                
                case '\n':
                *(result_ptr++) = '\\';
                *(result_ptr++) = 'n';
                ptr++;
                break;
                
                case '\r':
                *(result_ptr++) = '\\';
                *(result_ptr++) = 'r';
                ptr++;
                break;
                
                default:
                *(result_ptr++) = *(ptr++);
            }
        }
        
        *result_ptr = '\0';
        SET_STRING_ELT(results, i, mkChar(result));
    }
    
    UNPROTECT(1);
    return (results);
}
