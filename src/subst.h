#ifndef _SUBST_H_
#define _SUBST_H_

SEXP ore_substitute_all (SEXP regex_, SEXP replacement_, SEXP text_, SEXP all_, SEXP start_, SEXP environment, SEXP function_args);

SEXP ore_replace_all (SEXP regex_, SEXP replacement_, SEXP text_, SEXP all_, SEXP start_, SEXP simplify_, SEXP environment, SEXP function_args);

SEXP ore_switch_all (SEXP text_, SEXP mappings_, SEXP options_, SEXP encoding_name_);

#endif
