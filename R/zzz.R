.onLoad <- function (libname, pkgname)
{
    .Call("chariot_init", PACKAGE="chariot")
}

.onUnload <- function (libpath)
{
    .Call("chariot_done", PACKAGE="chariot")
}
