expect_error(ore_file("nonesuch.txt"))

# Local iconv support for SHIFT-JIS is necessary for these tests
if ("SHIFT-JIS" %in% iconvlist())
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
    
    # Binary search
    expect_equal(matches(ore_search("\\w+",ore_file("hello.bin",binary=TRUE))), "Hello")
}
