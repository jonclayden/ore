#' Expression substitution
#' 
#' Evaluate R expressions and substitute their values into one or more strings.
#' 
#' Each part of the string surrounded by \code{"#{}"} is extracted, evaluated
#' as R code in the specified environment, and then its value is substituted
#' back into the string. The literal string \code{"#{}"} can be obtained by
#' escaping the hash character, viz. \code{"\\\\#{}"}. The block may contain
#' multiple R expressions, separated by semicolons, but may not contain
#' additional braces.
#' 
#' @param text A vector of strings to substitute into.
#' @param round \code{NULL} or a single integer, giving the number of decimal
#'   digits for rounding numeric expressions. This argument takes priority over
#'   \code{signif}.
#' @param signif \code{NULL} or a single integer, giving the number of
#'   significant decimal digits to use for numeric expressions. The
#'   \code{round} argument takes priority over this one, and will be used if
#'   not \code{NULL}.
#' @param envir The environment to evaluate expressions in.
#' @return The final strings, with expression values substituted into them.
#' 
#' @examples
#' es("pi is #{pi}")
#' es("pi is \\#{pi}")
#' es("The square-root of pi is approximately #{sqrt(pi)}", signif=4)
#' es("1/(1+x) for x=3 is #{x <- 3; 1/(1+x)}")
#' @seealso \code{\link{ore.subst}}
#' @export
es <- function (text, round = NULL, signif = NULL, envir = parent.frame())
{
    # Rounding function
    if (!is.null(round))
        rfun <- function(x) round(x, round)
    else if (!is.null(signif))
        rfun <- function(x) signif(x, signif)
    else
        rfun <- function(x) x
    
    # Vectorised version of eval(), which also does rounding
    veval <- function(x,e) {
        sapply(x, function(xi) {
            value <- eval(parse(text=xi),envir=e)
            if (is.double(value))
                value <- rfun(value)
            return (value)
        })
    }
    
    # Do the main substitution
    results <- ore.subst("(?<!\\\\)\\#\\{([^\\}]+)\\}", function(match,envir) veval(groups(match),envir), text, envir=envir, all=TRUE)
    
    # Replace escaped '#' characters
    results <- ore.subst(ore("\\#",syntax="fixed"), "#", results, all=TRUE)
    
    return (results)
}
