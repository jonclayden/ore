#' @useDynLib ore
.onLoad <- function (libname, pkgname)
{
    .Call("ore_init", PACKAGE="ore")
}

.onUnload <- function (libpath)
{
    .Call("ore_done", PACKAGE="ore")
}
