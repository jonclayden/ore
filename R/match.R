.Workspace <- new.env()

ore.search <- function (regex, text, all = FALSE, start = 1L, envir = parent.frame())
{
    if (!is.character(text))
        text <- as.character(text)
    if (!is.ore(regex))
        regex <- ore(regex, encoding=Encoding(text))
    
    if (length(text) < 1)
        error("The text vector is empty")
    else if (length(text) > 1)
    {
        start <- rep(start, length.out=length(text))
        match <- lapply(seq_along(text), function(i) ore.search(regex, text=text[i], all=all, start=start[i], envir=NULL))
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
    
    if (!is.null(envir))
        assign(".ore.last", match, envir=envir)
    
    return (match)
}

ore.ismatch <- function (regex, text, all = FALSE, envir = parent.frame())
{
    match <- ore.search(regex, text, all=all, start=1L, envir=envir)
    
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

.ore.substitute <- function (match, replacement, text)
{
    start <- 1
    result <- ""
    for (i in seq_len(match$nMatches))
    {
        result <- paste(result, substr(text,start,match$offsets[i]-1), replacement[i], sep="")
        start <- match$offsets[i] + match$lengths[i]
    }
    result <- paste(result, substr(text,start,nchar(text)), sep="")
    return (result)
}

ore.sub <- function (regex, replacement, text, global = FALSE, ...)
{
    if (is.character(replacement))
    {
        if (!exists("groupNumberRegex", .Workspace))
        {
            .Workspace$groupNumberRegex <- ore("\\\\(\\d+)")
            .Workspace$groupNameRegex <- ore("\\\\\\k\\<(\\w+)\\>")
        }
        groupNumberMatch <- ore.search(.Workspace$groupNumberRegex, replacement, all=TRUE, envir=NULL)
        groupNameMatch <- ore.search(.Workspace$groupNameRegex, replacement, all=TRUE, envir=NULL)
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
                    currentReplacements <- apply(currentMatch$groups$matches[as.integer(groupNumberMatch$groups$matches),,drop=FALSE], 2, function(x) .ore.substitute(groupNumberMatch,x,replacement))
                if (!is.null(groupNameMatch))
                    currentReplacements <- apply(currentMatch$groups$matches[groupNameMatch$groups$matches,,drop=FALSE], 2, function(x) .ore.substitute(groupNameMatch,x,replacement))
            }
            
            result[i] <- .ore.substitute(currentMatch, currentReplacements, text[i])
        }
    }
    
    names(result) <- NULL
    return (result)
}
