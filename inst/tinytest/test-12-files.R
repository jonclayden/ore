# Calling readLines() will convert the text to UTF-8
if ("SHIFT-JIS" %in% iconvlist())
{
    s1 <- ore.search("\\p{Katakana}+", ore.file("sjis.txt",encoding="SHIFT-JIS"))
    con <- file("sjis.txt", encoding="SHIFT-JIS")
    s2 <- ore.search("\\p{Katakana}+", readLines(con))
    close(con)
    
    # Same character offsets but different byte offsets
    expect_equal(c(s1$offsets,s2$offsets), c(14L,14L))
    expect_equal(c(s1$byteOffsets,s2$byteOffsets), c(18L,22L))
    
    expect_equal(ore::matches(ore.search("\\w+",ore.file("hello.bin",binary=TRUE))), "Hello")
}
