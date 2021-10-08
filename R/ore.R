#' Oniguruma regular expressions
#' 
#' Create, test for, and print objects of class \code{"ore"}, which represent
#' Oniguruma regular expressions. These are unit-length character vectors with
#' additional attributes, including a pointer to the compiled version.
#' 
#' @param ... One or more strings or dictionary labels, constituting a valid
#'   regular expression after being concatenated together. Elements drawn from
#'   the dictionary will be surrounded by parentheses, turning them into
#'   groups. Note that backslashes should be doubled, to avoid them being
#'   interpreted as character escapes by R. The \code{...} argument is ignored
#'   by the \code{print} method.
#' @param options A string composed of characters indicating variations on the
#'   usual interpretation of the regex. These may currently include \code{"i"}
#'   for case-insensitive matching, and \code{"m"} for multiline matching (in
#'   which case \code{"."} matches the newline character).
#' @param encoding A string specifying the encoding that matching will take
#'   place in. The default is given by the \code{"ore.encoding"} option, which
#'   is usually set automatically from the current locale when the package is
#'   loaded, but can be modified if needed.
#' @param syntax The regular expression syntax being used. The default is
#'   \code{"ruby"}, which reflects the syntax of the Ruby language, which is
#'   very similar to that of Perl. An alternative is \code{"fixed"}, for
#'   literal matching without special treatment of characters.
#' @param x An R object.
#' @return The \code{ore} function returns the final pattern, with class
#'   \code{"ore"} and the following attributes:
#'   \describe{
#'     \item{.compiled}{A low-level pointer to the compiled version of the
#'       regular expression.}
#'     \item{options}{Options, copied from the argument of the same name.}
#'     \item{encoding}{The specified or detected encoding.}
#'     \item{syntax}{The specified syntax type.}
#'     \item{nGroups}{The number of groups in the pattern.}
#'     \item{groupNames}{Group names, if applicable.}
#'   }
#'   The \code{is_ore} function returns a logical vector indicating whether
#'   its argument represents an \code{"ore"} object.
#' @examples
#' # This matches a positive or negative integer
#' ore("-?\\d+")
#' 
#' # This matches words of exactly four characters
#' ore("\\b\\w{4}\\b")
#' @seealso For full details of supported syntax, please see
#' \url{https://raw.githubusercontent.com/k-takata/Onigmo/master/doc/RE}. The
#' \code{\link[base]{regex}} page is also useful as a quick reference, since
#' PCRE (used by base R) and Oniguruma (used by \code{ore}) have similar
#' features. See \code{\link{ore_dict}} for details of the pattern dictionary.
#' @export
ore <- function (..., options = "", encoding = getOption("ore.encoding"), syntax = c("ruby","fixed"))
{
    regex <- .Call(C_ore_build, ore_dict(...,enclos=parent.frame()), as.character(options), as.character(encoding), match.arg(syntax))
    return (regex)
}

#' @rdname ore
#' @aliases is.ore
#' @export is.ore is_ore
is_ore <- is.ore <- function (x)
{
    return (inherits(x, "ore"))
}

#' @rdname ore
#' @export
print.ore <- function (x, ...)
{
    cat(paste("Oniguruma regular expression: /", x, "/", paste(sort(unlist(strsplit(attr(x,"options"),""))),collapse=""), "\n", sep=""))
    
    nGroups <- attr(x, "nGroups")
    cat(paste0(" - ", nGroups, ifelse(nGroups==1," group"," groups")))
    if (!is.null(attr(x, "groupNames")))
        cat(paste0(", ", sum(!is.na(attr(x,"groupNames"))), " named"))
    cat("\n")
    
    cat(paste0(" - ", attr(x,"encoding"), " encoding\n"))
    cat(paste0(" - ", attr(x,"syntax"), " syntax\n"))
}

#' Escape regular expression special characters
#' 
#' Escape characters that would usually be interpreted specially in a regular
#' expression, returning a modified version of the argument. This can be
#' useful when incorporating a general-purpose string into a larger regex.
#' 
#' @param text A character vector.
#' @return A modified version of the argument, with special characters escaped
#'   by prefixing them with a backslash.
#' 
#' @seealso \code{\link{ore}}
#' @aliases ore.escape
#' @export ore.escape ore_escape
ore_escape <- ore.escape <- function (text)
{
    .Call(C_ore_escape, as.character(text))
}
