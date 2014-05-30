ore <- function (pattern, options = "")
{
    pattern <- as.character(pattern)
    if (length(pattern) < 1)
        error("Pattern should be of length 1")
    if (length(pattern) > 1)
        warning("Pattern vector has more than one element")
    
    result <- .Call("chariot_compile", pattern, as.character(options), PACKAGE="chariot")
    
    regex <- structure(pattern, .compiled=result[[1]], class="ore")
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
    cat("Oniguruma regular expression:  /");
    cat(x)
    cat(paste("/\n  - ", attr(x,"nGroups"), " groups", sep=""))
    
    if (!is.null(attr(x, "groupNames")))
        cat(paste(", ", sum(!is.na(attr(x,"groupNames"))), " named"))
    
    cat("\n")
}
