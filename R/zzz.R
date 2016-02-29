#' @useDynLib ore
.onLoad <- function (libname, pkgname)
{
    .Call("ore_init", PACKAGE="ore")
    
    if (Sys.getlocale("LC_CTYPE") %~% "^([A-Za-z_]+)\\.([\\w\\-.:]+)$")
    {
        if (isTRUE(ore.lastmatch()[,2] %in% iconvlist()))
            options(ore.encoding=ore.lastmatch()[,2])
        else
            options(ore.encoding="ASCII")
    }
}

.onUnload <- function (libpath)
{
    .Call("ore_done", PACKAGE="ore")
}
