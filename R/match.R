.Workspace <- new.env()

#' Search for matches to a regular expression
#' 
#' Search a character vector for one or more matches to an Oniguruma-compatible
#' regular expression. The result is of class \code{"orematch"}, for which
#' printing and indexing methods are available. The \code{\link{print}} method
#' uses the \code{crayon} package, if it is available.
#' 
#' @param regex A single character string or object of class \code{"ore"}. In
#'   the former case, this will first be passed through \code{\link{ore}}.
#' @param text A vector of strings to match against.
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
#' @aliases orematch
#' @export
ore.search <- function (regex, text, all = FALSE, start = 1L, simplify = TRUE)
{
    if (!is.character(text))
        text <- as.character(text)
    if (!is.ore(regex))
        regex <- ore(regex, encoding=.getEncoding(text))
    
    if (length(text) < 1)
        error("The text vector is empty")
    else if (length(text) > 1)
    {
        start <- rep(start, length.out=length(text))
        match <- lapply(seq_along(text), function(i) ore.search(regex, text=text[i], all=all, start=start[i]))
    }
    else
    {
        result <- .Call("ore_search", attr(regex,".compiled"), text, as.logical(all), as.integer(start), PACKAGE="ore")
        
        if (is.null(result))
            match <- NULL
        else
        {
            nMatches <- result[[1]]
            indices <- seq_len(nMatches * (attr(regex,"nGroups") + 1))
            offsets <- t(matrix(result[[2]][indices], ncol=nMatches))
            byteOffsets <- t(matrix(result[[3]][indices], ncol=nMatches))
            lengths <- t(matrix(result[[4]][indices], ncol=nMatches))
            byteLengths <- t(matrix(result[[5]][indices], ncol=nMatches))
            matchdata <- t(matrix(result[[6]][indices], ncol=nMatches))
            
            match <- structure(list(text=text, nMatches=nMatches, offsets=offsets[,1], byteOffsets=byteOffsets[,1], lengths=lengths[,1], byteLengths=byteLengths[,1], matches=matchdata[,1]), class="orematch")
            
            sourceEncoding <- .getEncoding(text)
            Encoding(match$matches) <- sourceEncoding
            
            if (attr(regex, "nGroups") > 0)
            {
                match$groups <- list(offsets=offsets[,-1,drop=FALSE], byteOffsets=byteOffsets[,-1,drop=FALSE], lengths=lengths[,-1,drop=FALSE], byteLengths=byteLengths[,-1,drop=FALSE], matches=matchdata[,-1,drop=FALSE])
                if (!is.null(attr(regex, "groupNames")))
                {
                    groupNames <- attr(regex, "groupNames")
                    colnames(match$groups$offsets) <- groupNames
                    colnames(match$groups$byteOffsets) <- groupNames
                    colnames(match$groups$lengths) <- groupNames
                    colnames(match$groups$byteLengths) <- groupNames
                    colnames(match$groups$matches) <- groupNames
                }
                Encoding(match$groups$matches) <- sourceEncoding
            }
        }
        
        if (!simplify)
            match <- list(match)
    }
    
    .Workspace$lastMatch <- match
    return (match)
}

#' @rdname ore.search
#' @export
is.orematch <- function (x)
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
print.orematch <- function (x, ...)
{
    if (x$nMatches == 0)
        cat(paste(text, "\n", sep=""))
    else
    {
        # Is the "crayon" package available, and does the terminal support colour?
        usingColour <- (system.file(package="crayon") != "" && crayon::has_color())
        
        context <- "context: "
        matches <- "  match: "
        numbers <- " number: "
        
        if (usingColour)
        {
            colouredText <- .Call("ore_substitute", x$text, x$nMatches, x$byteOffsets, x$byteLengths, crayon::cyan(x$matches), PACKAGE="ore")
            matches <- paste0(matches, colouredText)
        }
        
        ends <- c(1, x$offsets+x$lengths)
        for (i in 1:x$nMatches)
        {
            leadingSpace <- paste(rep(" ",x$offsets[i]-ends[i]), collapse="")
            
            if (!usingColour)
            {
                context <- paste0(context, substr(x$text,ends[i],x$offsets[i]-1), paste(rep(" ",x$lengths[i]),collapse=""))
                matches <- paste0(matches, leadingSpace, x$matches[i])
            }
            
            # NB: this could break for matches with numbers > 9 whose length is 1
            if (x$nMatches > 1)
                numbers <- paste0(numbers, leadingSpace, i, paste(rep("=",x$lengths[i]-nchar(as.character(i))),collapse=""))
        }
        context <- paste0(context, substr(x$text,ends[length(ends)],nchar(x$text)))
        
        cat(paste0(matches, "\n"))
        if (!usingColour)
            cat(paste0(context, "\n"))
        if (x$nMatches > 1)
            cat(paste0(numbers, "\n"))
    }
}

#' Extract matching substrings
#' 
#' These functions extract entire matches, or just subgroup matches, from
#' objects of class \code{"orematch"}. They can also be applied to lists of
#' these objects, as returned by \code{\link{ore.search}} when more than one
#' string is searched.
#' 
#' @param object An R object. Methods are provided for generic lists and
#'   \code{"orematch"} objects.
#' @param ... Further arguments to methods. Unused here.
#' @return A vector, matrix, array, or list of the same, containing full
#'   matches or subgroups.
#' @seealso \code{\link{ore.search}}
#' @export
matches <- function (object, ...)
{
    UseMethod("matches")
}

#' @rdname matches
#' @export
matches.list <- function (object, ...)
{
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
groups <- function (object, ...)
{
    UseMethod("groups")
}

#' @rdname matches
#' @export
groups.list <- function (object, ...)
{
    return (sapply(object, groups, ..., simplify="array"))
}

#' @rdname matches
#' @export
groups.orematch <- function (object, ...)
{
    return (object$groups$matches)
}

#' Retrieve the last match
#' 
#' This function can be used to obtain the \code{"orematch"} object, or list,
#' corresponding to the last call to \code{\link{ore.search}}. This can be
#' useful after performing a search implicitly, for example with \code{\%~\%}.
#' 
#' @return An \code{"orematch"} object or list. See \code{\link{ore.search}}
#'   for details.
#' @export
ore.lastmatch <- function ()
{
    if (!exists("lastMatch", envir=.Workspace))
        return (NULL)
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
#' all=TRUE)}. Either way, the first argument can an \code{"ore"} object, in
#' which case the second is the text to search, or a character vector, in
#' which case the second argument is assumed to contain the regex.
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
#' @export
ore.ismatch <- function (regex, text, all = FALSE)
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

#' Split strings using a regex
#' 
#' This function breaks up the strings provided at regions matching a regular
#' expression, removing those regions from the result. It is analogous to the
#' \code{\link{strsplit}} function in base R.
#' 
#' @inheritParams ore.search
#' @param simplify If \code{TRUE}, a character vector containing the pieces
#'   will be returned if \code{text} is of length 1. Otherwise, a list of such
#'   objects will always be returned.
#' @return A character vector or list of substrings.
#' 
#' @examples
#' ore.split("-?\\d+", "I have 2 dogs, 3 cats and 4 hamsters")
#' @seealso \code{\link{ore.search}}
#' @export
ore.split <- function (regex, text, start = 1L, simplify = TRUE)
{
    sourceEncoding <- .getEncoding(text)
    match <- ore.search(regex, text, all=TRUE, start=start, simplify=FALSE)
    result <- lapply(seq_along(text), function(i) {
        if (match[[i]]$nMatches == 0)
            return (text[i])
        else
        {
            parts <- .Call("ore_split", text[i], match[[i]]$nMatches, match[[i]]$byteOffsets, match[[i]]$byteLengths, PACKAGE="ore")
            Encoding(parts) <- sourceEncoding
            return (parts)
        }
    })
    
    if (simplify && length(result) == 1)
        return (result[[1]])
    else
        return (result)
}

#' Replace matched substrings with new text
#' 
#' This function substitutes new text into strings in regions that match a
#' regular expression. The substitutions may be simple text, may include
#' references to matched subgroups, or may be created by an R function.
#' 
#' @inheritParams ore.search
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
#' @export
ore.subst <- function (regex, replacement, text, all = FALSE, ...)
{
    do.subst <- function (match, replacement, text)
    {
        result <- .Call("ore_substitute", text, match$nMatches, match$byteOffsets, match$byteLengths, as.character(replacement), PACKAGE="ore")
        return (result)
    }
    
    if (is.character(replacement))
    {
        if (!exists("groupNumberRegex", .Workspace))
        {
            .Workspace$groupNumberRegex <- ore("\\\\(\\d+)")
            .Workspace$groupNameRegex <- ore("\\\\\\k\\<(\\w+)\\>")
        }
        groupNumberMatch <- ore.search(.Workspace$groupNumberRegex, replacement, all=TRUE)
        groupNameMatch <- ore.search(.Workspace$groupNameRegex, replacement, all=TRUE)
    }
    else
        replacement <- match.fun(replacement)
    
    result <- character(length(text))
    for (i in seq_along(text))
    {
        currentMatch <- ore.search(regex, text[i], all=all)
        if (is.null(currentMatch))
            result[i] <- text[i]
        else
        {
            if (is.function(replacement))
                currentReplacements <- replacement(currentMatch$matches, ...)
            else
            {
                currentReplacements <- rep(replacement, length.out=currentMatch$nMatches)
                if (!is.null(groupNumberMatch))
                    currentReplacements <- apply(currentMatch$groups$matches[as.integer(groupNumberMatch$groups$matches),,drop=FALSE], 2, function(x) do.subst(groupNumberMatch,x,replacement))
                if (!is.null(groupNameMatch))
                    currentReplacements <- apply(currentMatch$groups$matches[groupNameMatch$groups$matches,,drop=FALSE], 2, function(x) do.subst(groupNameMatch,x,replacement))
            }
            
            result[i] <- do.subst(currentMatch, currentReplacements, text[i])
        }
    }
    
    names(result) <- NULL
    Encoding(result) <- .getEncoding(text)
    return (result)
}
