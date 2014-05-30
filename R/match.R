ore.search <- function (regex, text, envir=parent.frame())
{
    if (!is.ore(regex))
        regex <- ore(regex)
    if (!is.character(text))
        text <- as.character(text)
    
    result <- .Call("chariot_search", attr(regex,".compiled"), text, PACKAGE="chariot")
    
    if (is.na(result[[1]]))
        match <- NULL
    else
    {
        match <- list(offset=result[[2]][1], length=result[[3]][1], text=substr(text,result[[2]][1],result[[2]][1]+result[[3]][1]-1))
        if (attr(regex, "nGroups") > 0)
        {
            match$groups <- list(offsets=result[[2]][-1], lengths=result[[3]][-1])
            match$groups$text <- sapply(seq_len(length(result[[2]])-1), function(i) substr(text,result[[2]][i+1],result[[2]][i+1]+result[[3]][i+1]-1))
            if (!is.null(attr(regex, "groupNames")))
            {
                groupNames <- attr(regex, "groupNames")
                groupNames[is.na(groupNames)] <- ""
                names(match$groups$offsets) <- groupNames
                names(match$groups$lengths) <- groupNames
                names(match$groups$text) <- groupNames
            }
        }
    }
    
    assign(".ore.last", match, envir=envir)
    return (match)
}

# NB: allow "replacement" to be a function
ore.sub <- function (regex, replacement, text, global = FALSE, ...)
{
    
}
