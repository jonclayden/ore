#' @export
ore.file <- ore_file <- function (path, encoding = getOption("ore.encoding"))
{
    if (!file.exists(path))
        stop(paste0("\"", path, "\": file not found"))
    else
        return (structure(path, encoding=encoding, class="orefile"))
}
