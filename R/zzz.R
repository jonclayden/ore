#' @useDynLib ore, .registration = TRUE, .fixes = "C_"
.onLoad <- function (libname, pkgname)
{
    .Call(C_ore_init)
    
    match <- ore_search("^([A-Z_ ]+)\\.([\\w\\-.:]+)$", toupper(Sys.getlocale("LC_CTYPE")))
    if (is.null(match))
        encoding <- "ASCII"
    else
    {
        needle <- match[,2]
        haystack <- toupper(iconvlist())
        if (isTRUE(needle %in% haystack))
            encoding <- needle
        else if (isTRUE(paste0("CP", needle) %in% haystack))
            encoding <- paste0("CP", needle)
        else
            encoding <- "ASCII"
    }
    
    options(ore.encoding=encoding)
}

.onUnload <- function (libpath)
{
    .Call(C_ore_done)
}
