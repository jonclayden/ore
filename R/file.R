#' Use a file as a text source
#' 
#' Identify a file path to be used as a text source for a subsequent call to
#' \code{\link{ore_search}}.
#' 
#' @param path A character string giving the file path.
#' @param encoding A character string giving the encoding of the file. This
#'   should match the encoding of the regular expression used in a call to
#'   \code{\link{ore_search}}.
#' @param binary A logical value: if \code{TRUE}, the file will be search
#'   bytewise, and \code{encoding} will be fixed to be \code{"ASCII"}.
#' 
#' @seealso \code{\link{ore_search}} for actually searching through the file.
#' @aliases orefile ore.file
#' @export ore.file ore_file
ore_file <- ore.file <- function (path, encoding = getOption("ore.encoding"), binary = FALSE)
{
    path <- path.expand(path)
    if (!file.exists(path))
        stop(paste0("\"", path, "\": file not found"))
    else
        return (structure(path, encoding=ifelse(binary,"ASCII",encoding), class="orefile"))
}
