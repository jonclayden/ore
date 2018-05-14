context("Matching and related operations")

test_that("Oniguruma regular expression matching works", {
    regex <- ore("\\b\\w{4}\\b")
    regexUtf8 <- ore("\\b\\w{4}\\b", encoding="UTF-8")
    text <- readLines("drink.txt", encoding="UTF-8")
    
    expect_equal(ore::matches(ore.search(regex,text)), "have")
    expect_equal(ore.search(regex,text,all=TRUE)$offsets, 6L)
    expect_null(ore.search(regex,text,start=10L))
    expect_equal(ore.search(regexUtf8,text,all=TRUE)$offsets, c(6L,13L))
    expect_equal(ore.search(regexUtf8,text,start=10L)$offsets, 13L)
    
    expect_identical(ore.search(regex,NULL), structure(list(),class="orematches"))
    
    expect_null(ore.search(ore("Have"),text))
    expect_equal(ore::matches(ore.search(ore("Have",options="i"),text)), "have")
    
    expect_equal(ore::matches(ore.search(ore(".+"),"one\ntwo")), "one")
    expect_equal(ore::matches(ore.search(ore(".+",options="m"),"one\ntwo")), "one\ntwo")
    
    expect_equal(ore::matches(ore.search(ore("."),"1.7")), "1")
    expect_equal(ore::matches(ore.search(ore(".",syntax="fixed"),"1.7")), ".")
    
    expect_equal(ore.ismatch("[aeiou]",c("sky","lake")), c(FALSE,TRUE))
    expect_true(ore.ismatch("^\\s*$",""))
    
    # Check infix syntax and implicit last match argument to matches()
    expect_equal(if ("lake" %~% "[aeiou]") ore::matches(), "a")
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
        expect_equal(c(s1$offsets,s2$offsets), c(14L,14L))
        expect_equal(c(s1$byteOffsets,s2$byteOffsets), c(18L,22L))
        expect_equal(ore::matches(ore.search("\\w+",ore.file("hello.bin",binary=TRUE))), "Hello")
    }
})

test_that("group extraction and indexing works", {
    match <- ore.search("(\\w)(\\w)", "This is a test", all=TRUE)
    expect_equal(dim(groups(match)), c(5L,2L))
    expect_equal(match[2], "is")
    expect_equal(match[2,2], "s")
    
    matches <- ore.search("(\\w)(\\w)", c("This","is","a","test"), all=TRUE)
    expect_equal(matches[1,2], "is")
    expect_equal(matches[1,2,2], "s")
    expect_equal(matches[,1], c("Th","is",NA,"te"))
    expect_equal(matches[,1,2], c("h","s",NA,"e"))
})

test_that("literal, back-referenced and functional substitution work", {
    expect_equal(ore.subst("\\d+","no","2 dogs"), "no dogs")
    expect_equal(ore.subst("(\\d+)","\\1\\1","2 dogs"), "22 dogs")
    expect_equal(ore.subst("\\d+",function(i) as.numeric(i)^2,"2 dogs"), "4 dogs")
    expect_equal(ore.subst("\\d+",function(i) max(as.numeric(i)), "2, 4, 6 or 8 dogs?", all=TRUE), "8, 8, 8 or 8 dogs?")
})

test_that("splitting a string by a regular expression works", {
    expect_equal(ore.split("[\\s\\-()]+","(801) 234-5678"), c("","801","234","5678"))
})
