context("Matching and related operations")

test_that("Oniguruma regular expression matching works", {
    regex <- ore("\\b\\w{4}\\b")
    regexUtf8 <- ore("\\b\\w{4}\\b", encoding="UTF-8")
    text <- readLines("drink.txt", encoding="UTF-8")
    
    expect_that(ore::matches(ore.search(regex,text)), equals("have"))
    expect_that(ore.search(regex,text,all=TRUE)$offsets, equals(6L))
    expect_that(ore.search(regex,text,start=10L), is_null())
    expect_that(ore.search(regexUtf8,text,all=TRUE)$offsets, equals(c(6L,13L)))
    expect_that(ore.search(regexUtf8,text,start=10L)$offsets, equals(13L))
    
    expect_that(ore.search(ore("Have"),text), is_null())
    expect_that(ore::matches(ore.search(ore("Have",options="i"),text)), equals("have"))
    
    expect_that(ore::matches(ore.search(ore(".+"),"one\ntwo")), equals("one"))
    expect_that(ore::matches(ore.search(ore(".+",options="m"),"one\ntwo")), equals("one\ntwo"))
    
    expect_that(ore::matches(ore.search(ore("."),"1.7")), equals("1"))
    expect_that(ore::matches(ore.search(ore(".",syntax="fixed"),"1.7")), equals("."))
    
    expect_that(ore.ismatch("[aeiou]",c("sky","lake")), equals(c(FALSE,TRUE)))
    expect_that(ore.ismatch("^\\s*$",""), is_true())
})

test_that("searching in a file works", {
    # Calling readLines() will convert the text to UTF-8
    if (!("SHIFT-JIS" %in% iconvlist()))
        skip("The local \"iconv\" implementation doesn't support Shift-JIS")
    else
    {
        s1 <- ore.search("\\p{Katakana}+", ore.file("sjis.txt",encoding="SHIFT-JIS"))
        con <- file("sjis.txt", encoding="SHIFT-JIS")
        s2 <- ore.search("\\p{Katakana}+", readLines(con))
        close(con)
    
        # Same character offsets but different byte offsets
        expect_that(c(s1$offsets,s2$offsets), equals(c(14L,14L)))
        expect_that(c(s1$byteOffsets,s2$byteOffsets), equals(c(18L,22L)))
        expect_that(ore::matches(ore.search("\\w+",ore.file("hello.bin",binary=TRUE))), equals("Hello"))
    }
})

test_that("group extraction and indexing works", {
    match <- ore.search("(\\w)(\\w)", "This is a test", all=TRUE)
    
    expect_that(dim(groups(match)), equals(c(5L,2L)))
    expect_that(match[2], equals("is"))
    expect_that(match[2,2], equals("s"))
})

test_that("literal, back-referenced and functional substitution work", {
    expect_that(ore.subst("\\d+","no","2 dogs"), equals("no dogs"))
    expect_that(ore.subst("(\\d+)","\\1\\1","2 dogs"), equals("22 dogs"))
    expect_that(ore.subst("\\d+",function(i) as.numeric(i)^2,"2 dogs"), equals("4 dogs"))
})

test_that("splitting a string by a regular expression works", {
    expect_that(ore.split("[\\s\\-()]+","(801) 234-5678"), equals(c("","801","234","5678")))
})
