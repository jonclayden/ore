#' @useDynLib ore, .registration = TRUE, .fixes = "C_"
.onLoad <- function (libname, pkgname)
{
    .Call(C_ore_init)
    
    match <- ore_search("^([A-Z_ ]+)\\.([\\w\\-.:]+)$", toupper(Sys.getlocale("LC_CTYPE")))
    if (is.null(match))
    {
        .Workspace$message <- "ore: Cannot determine native encoding - you may want to set the \"ore.encoding\" option manually"
        encoding <- "ASCII"
    }
    else
    {
        needle <- match[,2]
        haystack <- toupper(iconvlist())
        if (isTRUE(needle %in% haystack))
            encoding <- needle
        else if (isTRUE(paste0("CP", needle) %in% haystack))
            encoding <- paste0("CP", needle)
        else
        {
            .Workspace$message <- "ore: Cannot match native encoding - you may want to set the \"ore.encoding\" option manually"
            encoding <- "ASCII"
        }
    }
    
    options(ore.encoding=encoding)
}

.onAttach <- function (libname, pkgname)
{
    if (!is.null(.Workspace$message))
    {
        packageStartupMessage(.Workspace$message)
        .Workspace$message <- NULL
    }
}

.onUnload <- function (libpath)
{
    .Call(C_ore_done)
}

#' Multilingual sample text
#' 
#' This dataset contains translations into many languages of the esoteric
#' sentence "I can eat glass and it doesn't hurt me", UTF-8 encoded. Since
#' this dataset uses characters from a range of scripts, it provides a useful
#' test set for text handling and character encodings.
#' 
#' @format A named character vector, whose elements are translations of the
#'   sentence, and are named for the appropriate language in each case.
#' @source The translations were gathered by Frank da Cruz and written by a
#'   large group of contributors. Notes, commentary and a full list of credits
#'   are online at \url{https://kermitproject.org/utf8.html}.
"glass"
