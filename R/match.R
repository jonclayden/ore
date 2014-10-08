.Workspace <- new.env()

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
            offsets <- matrix(result[[2]][indices], ncol=nMatches)
            byteOffsets <- matrix(result[[3]][indices], ncol=nMatches)
            lengths <- matrix(result[[4]][indices], ncol=nMatches)
            byteLengths <- matrix(result[[5]][indices], ncol=nMatches)
            matchdata <- matrix(result[[6]][indices], ncol=nMatches)
            
            match <- structure(list(text=text, nMatches=nMatches, offsets=offsets[1,,drop=!all], byteOffsets=byteOffsets[1,,drop=!all], lengths=lengths[1,,drop=!all], byteLengths=byteLengths[1,,drop=!all], matches=matchdata[1,,drop=!all]), class="orematch")
            
            sourceEncoding <- .getEncoding(text)
            Encoding(match$matches) <- sourceEncoding
            
            if (attr(regex, "nGroups") > 0)
            {
                match$groups <- list(offsets=offsets[-1,,drop=FALSE], byteOffsets=byteOffsets[-1,,drop=FALSE], lengths=lengths[-1,,drop=FALSE], byteLengths=byteLengths[-1,,drop=FALSE], matches=matchdata[-1,,drop=FALSE])
                if (!is.null(attr(regex, "groupNames")))
                {
                    groupNames <- attr(regex, "groupNames")
                    rownames(match$groups$offsets) <- groupNames
                    rownames(match$groups$byteOffsets) <- groupNames
                    rownames(match$groups$lengths) <- groupNames
                    rownames(match$groups$byteLengths) <- groupNames
                    rownames(match$groups$matches) <- groupNames
                }
                Encoding(match$groups$matches) <- sourceEncoding
            }
        }
        
        if (!simplify)
            match <- list(match)
    }
    
    .Workspace$lastMatch <- match
    return (invisible(match))
}

is.orematch <- function (x)
{
    return ("orematch" %in% class(x))
}

print.orematch <- function (x, ...)
{
    if (x$nMatches == 0)
        cat(paste(text, "\n", sep=""))
    else
    {
        context <- "context: "
        matches <- "  match: "
        numbers <- " number: "
        
        ends <- c(1, x$offsets+x$lengths)
        for (i in 1:x$nMatches)
        {
            leadingSpace <- paste(rep(" ",x$offsets[i]-ends[i]), collapse="")
            context <- paste0(context, substr(x$text,ends[i],x$offsets[i]-1), paste(rep(" ",x$lengths[i]),collapse=""))
            matches <- paste0(matches, leadingSpace, x$matches[i])
            
            # NB: this could break for matches with numbers > 9 whose length is 1
            if (x$nMatches > 1)
                numbers <- paste0(numbers, leadingSpace, i, paste(rep("=",x$lengths[i]-nchar(as.character(i))),collapse=""))
        }
        context <- paste0(context, substr(x$text,ends[length(ends)],nchar(x$text)))
        
        cat(paste(matches, "\n", context, "\n", sep=""))
        if (x$nMatches > 1)
            cat(paste(numbers, "\n", sep=""))
    }
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
    match <- ore.search(regex, text, all=all, start=1L, simplify=FALSE)
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

ore.map <- function (regex, text, fun, ..., all = FALSE, start = 1L)
{
    fun <- match.fun(fun)
    match <- ore.search(regex, text, all=all, start=start, simplify=FALSE)
    result <- sapply(match, function(x) {
        if (all)
        {
            sapply(1:x$nMatches, function(i) {
                fun(x$matches[,i], x$groups$matches[,i], ...)
            })
        }
        else
            fun(x$matches, drop(x$groups$matches), ...)
    })
    
    return (result)
}

ore.split <- function (regex, text, start = 1L)
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
    
    return (result)
}

ore.sub <- function (regex, replacement, text, global = FALSE, ...)
{
    doSubst <- function (match, replacement, text)
    {
        result <- .Call("ore_substitute", text, match$nMatches, match$byteOffsets, match$byteLengths, replacement, PACKAGE="ore")
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
    Encoding(result) <- .getEncoding(text)
    return (result)
}
