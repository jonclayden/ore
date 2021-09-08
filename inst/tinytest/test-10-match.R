# For these purposes we treat ASCII as the default encoding
options(ore.encoding="ASCII")

regex <- ore("\\b\\w{4}\\b")
regexUtf8 <- ore("\\b\\w{4}\\b", encoding="UTF-8")
text <- readLines("drink.txt", encoding="UTF-8")

expect_equal(matches(ore_search(regex,text)), "have")
expect_equal(ore_search(regex,text,all=TRUE)$offsets, 6L)
expect_null(ore_search(regex,text,start=10L))
expect_equal(ore_search(regexUtf8,text,all=TRUE)$offsets, c(6L,13L))
expect_equal(ore_search(regexUtf8,text,start=10L)$offsets, 13L)

expect_identical(ore_search(regex,NULL), structure(list(),class="orematches"))

expect_null(ore_search(ore("Have"),text))
expect_equal(matches(ore_search(ore("Have",options="i"),text)), "have")

expect_equal(matches(ore_search(ore(".+"),"one\ntwo")), "one")
expect_equal(matches(ore_search(ore(".+",options="m"),"one\ntwo")), "one\ntwo")

expect_equal(matches(ore_search(ore("."),"1.7")), "1")
expect_equal(matches(ore_search(ore(".",syntax="fixed"),"1.7")), ".")

expect_equal(ore_ismatch("[aeiou]",c("sky","lake")), c(FALSE,TRUE))
expect_true(ore_ismatch("^\\s*$",""))

# Check infix syntax and implicit last match argument to matches()
expect_equal(if ("lake" %~% "[aeiou]") matches(), "a")

# Check single-match indexing
match <- ore_search("(\\w)(\\w)", "This is a test", all=TRUE)
expect_equal(dim(groups(match)), c(5L,2L))
expect_equal(match[2], "is")
expect_equal(match[2,2], "s")

# ... and multiple-match indexing
matches <- ore_search("(\\w)(\\w)", c("This","is","a","test"), all=TRUE)
expect_equal(matches[1,2], "is")
expect_equal(matches[1,2,2], "s")
expect_equal(matches[,1], c("Th","is",NA,"te"))
expect_equal(matches[,1,2], c("h","s",NA,"e"))
