.Workspace <- new.env()

ore.search <- function (regex, text, all = FALSE, start = 1L)
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
        result <- .Call("chariot_search", attr(regex,".compiled"), text, as.logical(all), as.integer(start), PACKAGE="chariot")
    
        if (is.null(result))
            match <- NULL
        else
        {
            nMatches <- result[[1]]
            indices <- seq_len(nMatches * (attr(regex,"nGroups") + 1))
            offsets <- matrix(result[[2]][indices], ncol=nMatches)
            lengths <- matrix(result[[3]][indices], ncol=nMatches)
            matchdata <- matrix(result[[4]][indices], ncol=nMatches)
        
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
    
    .Workspace$lastMatch <- match
    return (invisible(match))
}

ore.lastmatch <- function ()
{
    if (!exists("lastMatch", envir=.Workspace))
        return (NULL)
    else
        return (.Workspace$lastMatch)
}

ore.ismatch <- function (regex, text, all = FALSE)
{
    match <- ore.search(regex, text, all=all, start=1L)
    
    if (length(text) == 1)
        return (!is.null(match))
    else
        return (!sapply(match, is.null))
}

"%~%" <- function (X, Y)
{
    return (ore.ismatch(Y, X, all=FALSE))
}

"%~~%" <- function (X, Y)
{
    return (ore.ismatch(Y, X, all=TRUE))
}

ore.sub <- function (regex, replacement, text, global = FALSE, ...)
{
    doSubst <- function (match, replacement, text)
    {
        result <- .Call("chariot_substitute", text, match$nMatches, match$offsets, match$lengths, replacement, PACKAGE="chariot")
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
        currentMatch <- ore.search(regex, text[i], all=global)
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
                    currentReplacements <- apply(currentMatch$groups$matches[as.integer(groupNumberMatch$groups$matches),,drop=FALSE], 2, function(x) doSubst(groupNumberMatch,x,replacement))
                if (!is.null(groupNameMatch))
                    currentReplacements <- apply(currentMatch$groups$matches[groupNameMatch$groups$matches,,drop=FALSE], 2, function(x) doSubst(groupNameMatch,x,replacement))
            }
            
            result[i] <- doSubst(currentMatch, currentReplacements, text[i])
        }
    }
    
    names(result) <- NULL
    return (result)
}
