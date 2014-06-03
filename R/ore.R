ore <- function (pattern, options = "", encoding = "auto")
{
    pattern <- as.character(pattern)
    if (length(pattern) < 1)
        error("Pattern should be of length 1")
    if (length(pattern) > 1)
        warning("Pattern vector has more than one element")
    
    if (encoding == "auto")
        encoding <- Encoding(pattern)
    encodingCode <- switch(tolower(encoding), "utf-8"=1L, "utf8"=1L, "latin1"=2L, 0L)
    encodingName <- switch(tolower(encoding), "utf-8"="UTF-8", "utf8"="UTF-8", "latin1"="latin1", "bytes"="bytes", "unknown")
    
    result <- .Call("chariot_compile", pattern, as.character(options), encodingCode, PACKAGE="chariot")
    
    regex <- structure(pattern, .compiled=result[[1]], options=options, encoding=encodingName, class="ore")
    if (length(result) == 2)
    {
        attr(regex, "nGroups") <- length(result[[2]])
        if (any(result[[2]] != ""))
            attr(regex, "groupNames") <- result[[2]]
    }
    else
        attr(regex, "nGroups") <- 0L
    
    return (regex)
}

is.ore <- function (x)
{
    return ("ore" %in% class(x))
}

print.ore <- function (x, ...)
{
    cat(paste("Oniguruma regular expression: /", x, "/", paste(sort(unlist(strsplit(attr(x,"options"),""))),collapse=""), "\n", sep=""))
    
    cat(paste(" - ", attr(x,"nGroups"), " groups", sep=""))
    if (!is.null(attr(x, "groupNames")))
        cat(paste(", ", sum(!is.na(attr(x,"groupNames"))), " named", sep=""))
    cat("\n")
    
    cat(paste(" - ", attr(x,"encoding"), " encoding\n", sep=""))
}
