#' Get or set entries in the pattern dictionary
#' 
#' This function allows the user to get or set entries in the pattern
#' dictionary, a library of regular expressions whose elements can be referred
#' to by name in \code{\link{ore}}, and therefore easily reused.
#' 
#' @param ... One or more strings or dictionary keys. Unnamed, literal strings
#'   will be returned unmodified, named strings will be added to the
#'   dictionary, and unquoted names will be resolved using the dictionary.
#' @param enclos Enclosure for resolving names not present in the dictionary.
#'   Passed to \code{\link[base]{eval}}.
#' @return If no arguments are provided, the whole dictionary is returned.
#'   Otherwise the return value is a (possibly named) character vector of
#'   resolved strings.
#' 
#' @examples
#' # Literal strings are returned as-is
#' ore.dict("protocol")
#' 
#' # Named arguments are added to the dictionary
#' ore.dict(protocol="\\w+://")
#' 
#' # ... and can be retrieved by name
#' ore.dict(protocol)
#' 
#' @seealso \code{\link{ore}}, which passes its arguments through this function
#' @aliases ore_dict
#' @export ore.dict ore_dict
ore.dict <- ore_dict <- function (..., enclos = parent.frame())
{
    if (!exists("dictionary", .Workspace))
        .Workspace$dictionary <- list()
    
    args <- eval(substitute(list(...)), .Workspace$dictionary, enclos=enclos)
    names <- sapply(substitute(list(...)), as.character)[-1]
    
    if (length(args) == 0)
        return (.Workspace$dictionary)
    else
    {
        valid <- sapply(args, function(x) is.character(x) && length(x) == 1)
        if (any(!valid))
        {
            warning("Patterns that do not consist of a single string will be ignored")
            args <- args[valid]
        }
        
        if (!is.null(names(args)))
        {
            indices <- which(names(args) != "")
            .Workspace$dictionary[names(args)[indices]] <- args[indices]
            names[indices] <- names(args)[indices]
        }
        
        # Most likely the argument includes a function call
        if (is.list(names) || length(names) != length(args))
            names <- NULL
        else
        {
            names[unlist(args) == names | is.na(match(names,names(.Workspace$dictionary)))] <- ""
            if (all(names == ""))
                names <- NULL
        }
        
        result <- unlist(args)
        names(result) <- names
        
        return (result)
    }
}
