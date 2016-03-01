#' Search for matches to a regular expression
#' 
#' Search a character vector for one or more matches to an Oniguruma-compatible
#' regular expression. The result is of class \code{"orematch"}, for which
#' printing and indexing methods are available.
#' 
#' @param regex A single character string or object of class \code{"ore"}. In
#'   the former case, this will first be passed through \code{\link{ore}}.
#' @param text A vector of strings to match against, or the result of a call to
#'   \code{\link{ore.file}} to search in a file. In the latter case, match
#'   offsets will be relative to the file's encoding.
#' @param all If \code{TRUE}, then all matches within each element of
#'   \code{text} will be found. Otherwise, the search will stop at the first
#'   match.
#' @param start An optional vector of offsets (in characters) at which to start
#'   searching. Will be recycled to the length of \code{text}.
#' @param simplify If \code{TRUE}, an object of class \code{"orematch"} will
#'   be returned if \code{text} is of length 1. Otherwise, a list of such
#'   objects will always be returned.
#' @param x An R object.
#' @param i For indexing into an \code{"orematch"} object, the match number.
#' @param j For indexing into an \code{"orematch"} object, the group number.
#' @param lines The maximum number of lines to print. If \code{NULL}, this
#'   defaults to the value of the \code{"ore.lines"} option, or 0 if that is
#'   unset or invalid. Zero means no limit.
#' @param context The number of characters of context to include either side
#'   of each match. If \code{NULL}, this defaults to the value of the
#'   \code{"ore.context"} option, or 30 if that is unset or invalid.
#' @param width The number of characters in each line of printed output. If
#'   \code{NULL}, this defaults to the value of the standard \code{"width"}
#'   option.
#' @param ... Ignored.
#' @return For \code{ore.search}, an \code{"orematch"} object, or a list of
#'   the same, each with elements
#'     \item{text}{A copy of the \code{text} element for the current match.}
#'     \item{nMatches}{The number of matches found.}
#'     \item{offsets}{The offsets (in characters) of each match.}
#'     \item{byteOffsets}{The offsets (in bytes) of each match.}
#'     \item{lengths}{The lengths (in characters) of each match.}
#'     \item{byteLengths}{The lengths (in bytes) of each match.}
#'     \item{matches}{The matched substrings.}
#'     \item{groups}{Equivalent metadata for each parenthesised subgroup in
#'       \code{regex}, in a series of matrices. If named groups are present in
#'       the regex then \code{dimnames} will be set appropriately.}
#'   For \code{is.orematch}, a logical vector indicating whether the specified
#'   object has class \code{"orematch"}. For extraction with one index, a
#'   vector of matched substrings. For extraction with two indices, a vector
#'   or matrix of substrings corresponding to captured groups.
#' @note
#' Only named *or* unnamed groups will currently be captured, not both. If
#' there are named groups in the pattern, then unnamed groups will be ignored.
#' 
#' By default the \code{print} method uses the \code{crayon} package (if it is
#' available) to determine whether or not the R terminal supports colour.
#' Alternatively, colour printing may be forced or disabled by setting the
#' \code{"ore.colour"} (or \code{"ore.color"}) option to a logical value.
#' 
#' @examples
#' # Pick out pairs of consecutive word characters
#' match <- ore.search("(\\w)(\\w)", "This is a test", all=TRUE)
#' 
#' # Find the second matched substring ("is", from "This")
#' match[2]
#' 
#' # Find the content of the second group in the second match ("s")
#' match[2,2]
#' @seealso \code{\link{ore}} for creating regex objects; \code{\link{matches}}
#' and \code{\link{groups}} for an alternative to indexing for extracting
#' matching substrings.
#' @aliases orematch ore_search
#' @export ore.search ore_search
ore.search <- ore_search <- function (regex, text, all = FALSE, start = 1L, simplify = TRUE)
{
    match <- .Call("ore_search_all", regex, text, as.logical(all), as.integer(start), as.logical(simplify), PACKAGE="ore")
    
    .Workspace$lastMatch <- match
    return (match)
}

#' @rdname ore.search
#' @aliases is_orematch
#' @export is.orematch is_orematch
is.orematch <- is_orematch <- function (x)
{
    return ("orematch" %in% class(x))
}

#' @rdname ore.search
#' @export
"[.orematch" <- function (x, i, j, ...)
{
    if (missing(j))
        return (x$matches[i])
    else
        return (x$groups$matches[i,j])
}

#' @rdname ore.search
#' @export
print.orematch <- function (x, lines = NULL, context = NULL, width = NULL, ...)
{
    # Generally x$nMatches should not be zero (because non-matches return NULL), but cover it anyway
    if (x$nMatches == 0)
        cat("<no match>\n")
    else if (is.null(x$text))
        cat(es("<#{x$nMatches} match(es)>\n"))
    else
    {
        getOptionWithDefault <- function (value, name, default)
        {
            if (is.numeric(value))
                return (as.integer(value))
            else if (is.numeric(getOption(name)))
                return (as.integer(getOption(name)))
            else
                return (as.integer(default))
        }
        
        # Check the colour option; if unset, use the crayon package to check if the terminal supports colour
        if (!is.null(getOption("ore.colour")))
            usingColour <- isTRUE(getOption("ore.colour"))
        else if (!is.null(getOption("ore.color")))
            usingColour <- isTRUE(getOption("ore.color"))
        else
            usingColour <- (system.file(package="crayon") != "" && crayon::has_color())
        
        # Other printing options
        lines <- getOptionWithDefault(lines, "ore.lines", 0L)
        context <- getOptionWithDefault(context, "ore.context", 30L)
        width <- getOptionWithDefault(width, "width", 80L)
        
        .Call("ore_print_match", x, context, width, lines, usingColour, PACKAGE="ore")
    }
    
    invisible(NULL)
}

#' Extract matching substrings
#' 
#' These functions extract entire matches, or just subgroup matches, from
#' objects of class \code{"orematch"}. They can also be applied to lists of
#' these objects, as returned by \code{\link{ore.search}} when more than one
#' string is searched. For other objects they return \code{NA}.
#' 
#' @param object An R object. Methods are provided for generic lists and
#'   \code{"orematch"} objects.
#' @param simplify For the list methods, should nonmatching elements be removed
#'   from the result?
#' @param ... Further arguments to methods.
#' @return A vector, matrix, array, or list of the same, containing full
#'   matches or subgroups. If \code{simplify} is \code{TRUE}, the result may
#'   have a \code{dropped} attribute, giving the indices of nonmatching
#'   elements.
#' @seealso \code{\link{ore.search}}
#' @export
matches <- function (object, ...)
{
    UseMethod("matches")
}

#' @rdname matches
#' @export
matches.list <- function (object, simplify = TRUE, ...)
{
    if (simplify)
    {
        matched <- !sapply(object, is.null)
        result <- sapply(object[matched], matches, ...)
        if (any(!matched))
            attr(result, "dropped") <- which(!matched)
        return (result)
    }
    else
        return (sapply(object, matches, ...))
}

#' @rdname matches
#' @export
matches.orematch <- function (object, ...)
{
    return (object$matches)
}

#' @rdname matches
#' @export
matches.default <- function (object, ...)
{
    return (NA_character_)
}

#' @rdname matches
#' @export
groups <- function (object, ...)
{
    UseMethod("groups")
}

#' @rdname matches
#' @export
groups.list <- function (object, simplify = TRUE, ...)
{
    if (simplify)
    {
        matched <- !sapply(object, is.null)
        result <- sapply(object[matched], groups, ..., simplify="array")
        if (any(!matched))
            attr(result, "dropped") <- which(!matched)
        return (result)
    }
    else
        return (sapply(object, groups, ..., simplify="array"))
}

#' @rdname matches
#' @export
groups.orematch <- function (object, ...)
{
    return (object$groups$matches)
}

#' @rdname matches
#' @export
groups.orearg <- function (object, ...)
{
    return (attr(object, "groups"))
}

#' @rdname matches
#' @export
groups.default <- function (object, ...)
{
    return (NA_character_)
}

#' Retrieve the last match
#' 
#' This function can be used to obtain the \code{"orematch"} object, or list,
#' corresponding to the last call to \code{\link{ore.search}}. This can be
#' useful after performing a search implicitly, for example with \code{\%~\%}.
#' 
#' @param simplify If \code{TRUE} and the last match was against a single
#'   string, then the \code{"orematch"} object will be returned, instead of a
#'   list with one element.
#' @return An \code{"orematch"} object or list. See \code{\link{ore.search}}
#'   for details.
#' @aliases ore_lastmatch
#' @export ore.lastmatch ore_lastmatch
ore.lastmatch <- ore_lastmatch <- function (simplify = TRUE)
{
    if (!exists("lastMatch", envir=.Workspace))
        return (NULL)
    else if (simplify && is.list(.Workspace$lastMatch) && length(.Workspace$lastMatch) == 1)
        return (.Workspace$lastMatch[[1]])
    else
        return (.Workspace$lastMatch)
}

#' Does text match a regex?
#' 
#' These functions test whether the elements of a character vector match a
#' Oniguruma regular expression. The actual match can be retrieved using
#' \code{\link{ore.lastmatch}}.
#' 
#' The \code{\%~\%} infix shorthand corresponds to \code{ore.ismatch(..., 
#' all=FALSE)}, while \code{\%~~\%} corresponds to \code{ore.ismatch(...,
#' all=TRUE)}. Either way, the first argument can be an \code{"ore"} object,
#' in which case the second is the text to search, or a character vector, in
#' which case the second argument is assumed to contain the regex. The
#' \code{\%~|\%} shorthand returns just those elements of the text vector which
#' match the regular expression.
#' 
#' @param regex A single character string or object of class \code{"ore"}.
#' @param text A character vector of strings to search.
#' @param all Passed to \code{\link{ore.search}}. Makes no difference to the
#'   return value of these functions, but influences the underlying
#'   \code{"orematch"} object, which can be retrieved afterwards.
#' @param X A character vector or \code{"ore"} object. See Details.
#' @param Y A character vector. See Details.
#' @return A logical vector, indicating whether elements of \code{text} match
#'   \code{regex}, or not.
#' 
#' @examples
#' # Test for the presence of a vowel
#' ore.ismatch("[aeiou]", c("sky","lake"))  # => c(FALSE,TRUE)
#' 
#' # The same thing, in shorter form
#' c("sky","lake") %~% "[aeiou]"
#' 
#' # Same again: the first argument must be an "ore" object this way around
#' ore("[aeiou]") %~% c("sky","lake")
#' @seealso \code{\link{ore.search}}
#' @aliases ore_ismatch
#' @export ore.ismatch ore_ismatch
ore.ismatch <- ore_ismatch <- function (regex, text, all = FALSE)
{
    match <- ore.search(regex, text, all=all, start=1L, simplify=FALSE)
    return (!sapply(match, is.null))
}

#' @rdname ore.ismatch
#' @export
"%~%" <- function (X, Y)
{
    if (is.ore(X))
        return (ore.ismatch(X, Y, all=FALSE))
    else
        return (ore.ismatch(Y, X, all=FALSE))
}

#' @rdname ore.ismatch
#' @export
"%~~%" <- function (X, Y)
{
    if (is.ore(X))
        return (ore.ismatch(X, Y, all=TRUE))
    else
        return (ore.ismatch(Y, X, all=TRUE))
}

#' @rdname ore.ismatch
#' @export
"%~|%" <- function (X, Y)
{
    if (is.ore(X))
        return (Y[ore.ismatch(X,Y)])
    else
        return (X[ore.ismatch(Y,X)])
}

#' Split strings using a regex
#' 
#' This function breaks up the strings provided at regions matching a regular
#' expression, removing those regions from the result. It is analogous to the
#' \code{\link[base]{strsplit}} function in base R.
#' 
#' @inheritParams ore.search
#' @param text A vector of strings to match against.
#' @param simplify If \code{TRUE}, a character vector containing the pieces
#'   will be returned if \code{text} is of length 1. Otherwise, a list of such
#'   objects will always be returned.
#' @return A character vector or list of substrings.
#' 
#' @examples
#' ore.split("-?\\d+", "I have 2 dogs, 3 cats and 4 hamsters")
#' @seealso \code{\link{ore.search}}
#' @aliases ore_split
#' @export ore.split ore_split
ore.split <- ore_split <- function (regex, text, start = 1L, simplify = TRUE)
{
    if (!is.character(text))
        text <- as.character(text)
    
    return (.Call("ore_split", regex, text, as.integer(start), as.logical(simplify), PACKAGE="ore"))
}

#' Replace matched substrings with new text
#' 
#' This function substitutes new text into strings in regions that match a
#' regular expression. The substitutions may be simple text, may include
#' references to matched subgroups, or may be created by an R function.
#' 
#' If \code{replacement} is a function, then it will be passed as its first
#' argument an object of class \code{"orearg"}. This is a character vector
#' containing as its elements the matched substrings, and with an attribute
#' containing the matches for parenthesised subgroups, if there are any. A
#' \code{\link{groups}} method is available for this class, so the groups
#' attribute can be easily obtained that way. The substitution function will be
#' called once per element of \code{text}.
#' 
#' @inheritParams ore.search
#' @param text A vector of strings to match against.
#' @param replacement A single character string, or a function to be applied
#'   to the matches.
#' @param ... Further arguments to \code{replacement}, if it is a function.
#' @return A version of \code{text} with the substitutions made.
#' 
#' @examples
#' # Simple text substitution (produces "no dogs")
#' ore.subst("\\d+", "no", "2 dogs")
#' 
#' # Back-referenced substitution (produces "22 dogs")
#' ore.subst("(\\d+)", "\\1\\1", "2 dogs")
#' 
#' # Function-based substitution (produces "4 dogs")
#' ore.subst("\\d+", function(i) as.numeric(i)^2, "2 dogs")
#' @seealso \code{\link{ore.search}}
#' @aliases ore_subst
#' @export ore.subst ore_subst
ore.subst <- ore_subst <- function (regex, replacement, text, ..., all = FALSE)
{
    if (!is.character(text))
        text <- as.character(text)
    if (!is.character(replacement))
        replacement <- match.fun(replacement)
        
    return (.Call("ore_substitute_all", regex, replacement, text, as.logical(all), new.env(), pairlist(...), PACKAGE="ore"))
}
