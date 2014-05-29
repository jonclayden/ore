ore.search <- function (regex, text)
{
    if (!is.ore(regex))
        regex <- ore(regex)
    if (!is.character(text))
        text <- as.character(text)
    
    result <- .Call("chariot_search", attr(regex,".compiled"), text, PACKAGE="chariot")
    
    if (is.na(result[[1]]))
        return (NULL)
    else
    {
        match <- list(offset=result[[2]][1], length=result[[3]][1])
        if (attr(regex, "nGroups") > 0)
        {
            match$groups <- list(offsets=result[[2]][-1], lengths=result[[3]][-1])
            if (!is.null(attr(regex, "groupNames")))
            {
                groupNames <- attr(regex, "groupNames")
                groupNames[is.na(groupNames)] <- ""
                names(match$groups$offsets) <- groupNames
                names(match$groups$lengths) <- groupNames
            }
        }
        
        return (match)
    }
}
