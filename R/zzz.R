#' @useDynLib ore, .registration = TRUE, .fixes = "C_"
.onLoad <- function (libname, pkgname)
{
    .Call(C_ore_init)
    
    if (Sys.getlocale("LC_CTYPE") %~% "^([A-Za-z_ ]+)\\.([\\w\\-.:]+)$")
    {
        if (isTRUE(ore.lastmatch()[,2] %in% iconvlist()))
            options(ore.encoding=ore.lastmatch()[,2])
        else
            options(ore.encoding="ASCII")
    }
}

.onUnload <- function (libpath)
{
    .Call(C_ore_done)
}
