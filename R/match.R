ore.search <- function (regex, text, matches = TRUE, all = FALSE, start = 1L, envir = parent.frame())
{
    if (!is.ore(regex))
        regex <- ore(regex)
    if (!is.character(text))
        text <- as.character(text)
    
    if (length(text) < 1)
        error("The text vector is empty")
    else if (length(text) > 1)
    {
        start <- rep(start, length.out=length(text))
        match <- lapply(seq_along(text), function(i) ore.search(regex, text=text[i], matches=matches, all=all, start=start[i], envir=NULL))
    }
    else
    {
        result <- .Call("chariot_search", attr(regex,".compiled"), text, as.logical(all), as.integer(start), PACKAGE="chariot")
    
        if (is.null(result))
            match <- NULL
        else
        {
            nMatches <- result[[1]]
            offsets <- matrix(result[[2]], nrow=attr(regex,"nGroups")+1)[,1:nMatches,drop=FALSE]
            lengths <- matrix(result[[3]], nrow=attr(regex,"nGroups")+1)[,1:nMatches,drop=FALSE]
            if (matches)
                matchdata <- sapply(1:length(offsets), function(i) substr(text, offsets[i], offsets[i]+lengths[i]-1))
            else
                matchdata <- rep(NA, length(offsets))
            dim(matchdata) <- dim(offsets)
        
            match <- list(nMatches=nMatches, offsets=offsets[1,,drop=!all], lengths=lengths[1,,drop=!all], matches=matchdata[1,,drop=!all])
            if (attr(regex, "nGroups") > 0)
            {
                match$groups <- list(offsets=offsets[-1,,drop=FALSE], lengths=lengths[-1,,drop=FALSE], matches=matchdata[-1,,drop=FALSE])
                if (!is.null(attr(regex, "groupNames")))
                {
                    groupNames <- attr(regex, "groupNames")
                    rownames(match$groups$offsets) <- groupNames
                    rownames(match$groups$lengths) <- groupNames
                    rownames(match$groups$matches) <- groupNames
                }
            }
        }
    }
    
    if (!is.null(envir))
        assign(".ore.last", match, envir=envir)
    
    return (match)
}

ore.ismatch <- function (regex, text, all = FALSE, envir = parent.frame())
{
    match <- ore.search(regex, text, matches=TRUE, all=all, start=1L, envir=envir)
    
    if (length(text) == 1)
        return (!is.null(match))
    else
        return (!sapply(match, is.null))
}

"%~%" <- function (X, Y)
{
    return (ore.ismatch(Y, X, all=FALSE, envir=parent.frame()))
}

"%~~%" <- function (X, Y)
{
    return (ore.ismatch(Y, X, all=TRUE, envir=parent.frame()))
}

# NB: allow "replacement" to be a function
ore.sub <- function (regex, replacement, text, global = FALSE, ...)
{
    if (is.character(replacement))
    {
        groupNumberRegex <- ore("\\\\(\\d+)")
        groupNameRegex <- ore("\\\\\\k\\<(\\w+)\\>")
    }
    else
        replacement <- match.fun(replacement)
    
    numberReplaceFunction <- function (match, groupNumbers) match$groups$matches[as.integer(groupNumbers),]
    nameReplaceFunction <- function (match, groupNames) match$groups$matches[groupNames,]
    
    result <- sapply(text, function(currentText) {
        currentMatch <- ore.search(regex, currentText, all=global)
        if (is.null(currentMatch))
            return (currentText)
        else
        {
            if (is.function(replacement))
                currentReplacements <- replacement(currentMatch, ...)
            else
            {
                if (replacement %~~% groupNumberRegex)
                    return (ore.subst(groupNumberRegex, numberReplaceFunction, replacement, global=TRUE, groupMatches=))
            }
        else
        {
            if (replacement %~~% groupNumberRegex)
                return (ore.subst(groupNumberRegex, numberReplaceFunction, replacement, global=TRUE, groupMatches=))
        }
    })
}
