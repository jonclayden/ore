#' @useDynLib ore, .registration = TRUE, .fixes = "C_"
.onLoad <- function (libname, pkgname)
{
    .Call(C_ore_init)
    
    match <- ore_search("^(([A-Z_ ]+)\\.)?([\\w\\-.:]+)$", toupper(Sys.getlocale("LC_CTYPE")))
    if (is.null(match))
    {
        .Workspace$message <- "ore: Cannot determine native encoding - you may want to set the \"ore.encoding\" option manually"
        encoding <- "ASCII"
    }
    else
    {
        needle <- match[,3]
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
