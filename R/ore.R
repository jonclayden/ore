.getEncoding <- function (x)
{
    encoding <- Encoding(x)
    notUnknown <- unique(encoding[encoding != "unknown"])
    if (length(notUnknown) == 0)
        return ("unknown")
    else if (length(notUnknown) == 1)
        return (notUnknown)
    else
        stop("Mixed-encoding character vectors are currently not supported")
}

#' Oniguruma regular expressions
#' 
#' Create, test for, and print objects of class \code{"ore"}, which represent
#' Oniguruma regular expressions. These are length-1 character vectors with
#' additional attributes, including a pointer to the compiled version.
#' 
#' @param pattern A single string containing a valid regular expression. Note
#'   that backslashes should be doubled, to avoid them being interpreted as
#'   character escapes by R.
#' @param options A string composed of characters indicating variations on the
#'   usual interpretation of the regex. These may currently include \code{"i"}
#'   for case-insensitive matching, and \code{"m"} for multiline matching (in
#'   which case \code{"."} matches the newline character).
#' @param encoding The encoding that matching will take place in, a string
#'   string naming one of the encoding types discussed in
#'   \code{\link{Encoding}}, or \code{"auto"}. In the latter case, the encoding
#'   of \code{pattern} will be used.
#' @param x An R object.
#' @param ... Ignored.
#' @return The \code{ore} function returns (the first element of)
#'   \code{pattern}, with class \code{"ore"} and the following attributes:
#'     \item{.compiled}{A low-level pointer to the compiled version of the
#'       regular expresion.}
#'     \item{options}{Options, copied from the argument of the same name.}
#'     \item{encoding}{The specified or detected encoding.}
#'   The \code{is.ore} function returns a logical vector indicating whether
#'   its argument represents an \code{"ore"} object.
#' @examples
#' # This matches a positive or negative integer
#' ore("-?\\d+")
#' 
#' # This matches words of exactly four characters
#' ore("\\b\\w{4}\\b")
#' @seealso For full details of supported syntax, please see
#' \url{https://raw.githubusercontent.com/k-takata/Onigmo/master/doc/RE}. The
#' \code{\link{regex}} page is also useful as a quick reference, since PCRE
#' (used by base R) and Oniguruma (used by \code{ore}) have similar features.
#' @export
ore <- function (pattern, options = "", encoding = "auto")
{
    regex <- .Call("ore_build", as.character(pattern), as.character(options), as.character(encoding), PACKAGE="ore")
    return (regex)
}

#' @rdname ore
#' @export
is.ore <- function (x)
{
    return ("ore" %in% class(x))
}

#' @rdname ore
#' @export
print.ore <- function (x, ...)
{
    cat(paste("Oniguruma regular expression: /", x, "/", paste(sort(unlist(strsplit(attr(x,"options"),""))),collapse=""), "\n", sep=""))
    
    cat(paste(" - ", attr(x,"nGroups"), " groups", sep=""))
    if (!is.null(attr(x, "groupNames")))
        cat(paste(", ", sum(!is.na(attr(x,"groupNames"))), " named", sep=""))
    cat("\n")
    
    cat(paste(" - ", attr(x,"encoding"), " encoding\n", sep=""))
}
