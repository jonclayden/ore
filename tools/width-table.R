# Generate the table of wide character ranges in src/width.c from the Unicode Character Database
# Run from the package root, e.g. "Rscript tools/width-table.R 17.0.0", using the Unicode version of the regex library's tables

args <- commandArgs(trailingOnly=TRUE)
version <- if (length(args) > 0) args[1] else "17.0.0"
target <- file.path("src", "width.c")
if (!file.exists(target))
    stop("This script should be run from the package root")

path <- tempfile()
download.file(sprintf("https://www.unicode.org/Public/%s/ucd/EastAsianWidth.txt", version), path, quiet=TRUE)
lines <- readLines(path, encoding="UTF-8")
unlink(path)

# Unassigned code points in these blocks and planes are documented as defaulting to "W"
wide <- logical(0x110000)
defaults <- rbind(c(0x3400,0x4DBF), c(0x4E00,0x9FFF), c(0xF900,0xFAFF), c(0x20000,0x2FFFD), c(0x30000,0x3FFFD))
for (i in seq_len(nrow(defaults)))
    wide[(defaults[i,1]:defaults[i,2]) + 1] <- TRUE

# Explicit entries override the defaults; lines look like "1100..115F ; W  # ..." or "3000 ; F  # ..."
data <- sub("\\s*#.*$", "", lines)
data <- data[nzchar(data)]
fields <- strsplit(data, "\\s*;\\s*")
ranges <- strsplit(sapply(fields, "[", 1), "..", fixed=TRUE)
values <- sapply(fields, "[", 2)
for (i in seq_along(ranges))
{
    bounds <- strtoi(ranges[[i]], 16L)
    wide[(bounds[1]:bounds[length(bounds)]) + 1] <- values[i] %in% c("W","F")
}

# Collapse to contiguous ranges
runs <- rle(wide)
ends <- cumsum(runs$lengths) - 1
starts <- ends - runs$lengths + 1
starts <- starts[runs$values]
ends <- ends[runs$values]

table <- c(sprintf("// Unicode %s: %d ranges", version, length(starts)),
           "static const OnigCodePoint wide_ranges[][2] = {",
           paste0("    { ", sprintf("0x%04X, 0x%04X", starts, ends), " }", c(rep(",",length(starts)-1),"")),
           "};")

source <- readLines(target)
begin <- grep("^// BEGIN GENERATED TABLE", source)
end <- grep("^// END GENERATED TABLE", source)
if (length(begin) != 1 || length(end) != 1 || end < begin)
    stop("Generated table markers not found in ", target)
writeLines(c(source[1:begin], table, source[end:length(source)]), target)
cat(sprintf("Wrote %d ranges for Unicode %s to %s\n", length(starts), version, target))
