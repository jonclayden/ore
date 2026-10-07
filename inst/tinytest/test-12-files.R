expect_error(ore_file("nonesuch.txt"))

# Check whether iconv can actually convert to an encoding: iconvlist() may be
# a static list fixed when R was built, so it can include encodings whose
# conversion modules are not installed (e.g. in minimal Linux containers)
can_convert <- function (encoding)
{
    tryCatch(!is.null(iconv("a", "UTF-8", encoding, toRaw=TRUE)[[1]]), error=function(e) FALSE)
}

# Local iconv support for SHIFT-JIS is necessary for these tests
if (can_convert("SHIFT-JIS"))
{
    # Four ways to search in a SHIFT-JIS-encoded file:
    #   1. Use ore_file(), declaring the encoding, and use the internal
    #      file-handling code to read the file directly.
    #   2. Use readLines() to read in the file, declaring the encoding,
    #      which implicitly converts it to UTF-8 because R doesn't understand
    #      SHIFT-JIS internally. Search in this converted text, bearing in mind
    #      that byte offsets will reflect UTF-8 encoding, and so won't match
    #      those in the file.
    #   3. Use iconv() to recreate a SHIFT-JIS-encoded string in R, and create
    #      an ore regex to match. The latter's encoding must be specified,
    #      because R doesn't retain encoding information except for latin1 and
    #      UTF-8.
    #   4. Create a connection, declaring the encoding, and pass it directly
    #      to ore_search() - assuming connection support is available.
    s1 <- ore_search("\\p{Katakana}+", ore_file("sjis.txt",encoding="SHIFT-JIS"))
    con <- file("sjis.txt", encoding="SHIFT-JIS")
    text <- readLines(con)
    s2 <- ore_search("\\p{Katakana}+", text)
    s3 <- ore_search(ore("\\p{Katakana}+",encoding="SHIFT-JIS"), iconv(text,"UTF-8","SHIFT-JIS"))
    close(con)
    con <- file("sjis.txt", encoding="SHIFT-JIS")
    s4 <- ore_search("\\p{Katakana}+", con)
    close(con)
    
    # Check the match was found in each case
    results <- list(s1, s2, s3, s4)
    expect_false(any(sapply(results, is.null)))
    
    # Same character offsets but different byte offsets
    expect_equal(sapply(results,"[[","offsets"), c(14L,14L,14L,14L))
    expect_equal(sapply(results,"[[","byteOffsets"), c(18L,22L,18L,18L))
    
    # Text read from a file is converted to UTF-8 when it's returned to R
    expect_equal(matches(s1), matches(s2))
    expect_equal(Encoding(matches(s1)), "UTF-8")
    expect_equal(matches(s4), matches(s2))
    
    # Binary search
    expect_equal(matches(ore_search("\\w+",ore_file("hello.bin",binary=TRUE))), "Hello")
}

# An encoding name that is unknown to both Oniguruma and iconv gives a warning, not a crash
path <- tempfile()
writeLines("abc", path)
expect_warning(result <- ore_search("b", ore_file(path,encoding="nonesuch")), "not supported")
expect_equal(matches(result), "b")
unlink(path)

# Files in UTF-16 or UTF-32, which are not ASCII-compatible
if (can_convert("UTF-16LE") && can_convert("UTF-32BE"))
{
    for (encoding in c("UTF-16LE","UTF-32BE"))
    {
        path <- tempfile()
        writeBin(iconv("abc d\u00e9f", "UTF-8", encoding, toRaw=TRUE)[[1]], path)
        match <- ore_search(ore("(?<word>d\\w+)",encoding=encoding), ore_file(path,encoding=encoding))
        expect_equal(matches(match), "d\u00e9f", info=encoding)
        expect_equal(match$offsets, 5L, info=encoding)
        expect_equal(colnames(groups(match)), "word", info=encoding)
        expect_equal(matches(ore_search("\\w+", ore_file(path,encoding=encoding), all=TRUE)), c("abc","d\u00e9f"), info=encoding)
        unlink(path)
    }
}
expect_warning(ore_search(ore("a",encoding="UTF-16LE"), "abc"), "does not match")

# Connections that were opened for searching are closed again afterwards, but others are left open
path <- tempfile()
writeLines("abc", path)
con <- file(path)
ore_search("b", con)
expect_false(isOpen(con))
close(con)
con <- file(path, "r")
ore_search("b", con)
expect_true(isOpen(con))
close(con)
unlink(path)
