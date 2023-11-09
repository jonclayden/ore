# For these purposes we treat ASCII as the default encoding
options(ore.encoding="ASCII", ore.colour=FALSE)

regex <- ore("\\b\\w{4}\\b")
regexUtf8 <- ore("\\b\\w{4}\\b", encoding="UTF-8")
text <- readLines("drink.txt", encoding="UTF-8")

# Check match class and content
expect_true(is_orematch(ore_search(regex,text)))
expect_equal(matches(ore_search(regex,text)), "have")
expect_equal(ore_search(regex,text,all=TRUE)$offsets, 6L)
expect_null(ore_search(regex,text,start=10L))
expect_equal(ore_search(regexUtf8,text,all=TRUE)$offsets, c(6L,13L))
expect_equal(ore_search(regexUtf8,text,start=10L)$offsets, 13L)

# Check printing output
expect_stdout(print(ore_search(regexUtf8,text,all=TRUE)), "1===")

# Default and empty cases
expect_identical(ore_search(regex,NULL), structure(list(),class="orematches"))
expect_identical(matches(NULL), NA_character_)
expect_identical(groups(NULL), NA_character_)

# Case-sensitivity option
expect_null(ore_search(ore("Have"),text))
expect_equal(matches(ore_search(ore("Have",options="i"),text)), "have")

# Multiline option
expect_equal(matches(ore_search(ore(".+"),"one\ntwo")), "one")
expect_equal(matches(ore_search(ore(".+",options="m"),"one\ntwo")), "one\ntwo")

# Fixed versus standard Ruby syntax
expect_equal(matches(ore_search(ore("."),"1.7")), "1")
expect_equal(matches(ore_search(ore(".",syntax="fixed"),"1.7")), ".")

# Check boolean results
expect_equal(ore_ismatch("[aeiou]",c("sky","lake",NA)), c(FALSE,TRUE,FALSE))
expect_equal(ore_ismatch("[aeiou]",c("sky","lake",NA),keepNA=TRUE), c(FALSE,TRUE,NA))
expect_true(ore_ismatch("^\\s*$",""))

# Check infix syntax and implicit last match argument to matches() and groups()
expect_equal(if ("lake" %~% "[aeiou]") matches(), "a")
expect_equal(if ("lake" %~~% "([aeiou])") groups(), matrix(c("a","e"),ncol=1L))
expect_equal(c("lake","sky","tree") %~|% "[aeiou]", c("lake","tree"))

# Check single-match indexing
result <- ore_search("(\\w)(\\w)", "This is a test", all=TRUE)
expect_equal(dim(groups(result)), c(5L,2L))
expect_equal(result[2], "is")
expect_equal(result[2,2], "s")

# ... and multiple-match indexing
results <- ore_search("(\\w)(\\w)", c("This","is","a","test"), all=TRUE)
expect_equal(results[1,2], "is")
expect_equal(results[1,2,2], "s")
expect_equal(results[,1], c("Th","is",NA,"te"))
expect_equal(results[,1,2], c("h","s",NA,"e"))
expect_equal(unlist(matches(results)), c("Th","is","is","te","st"))
expect_equal(groups(results)[[1]], matrix(c("T","h","i","s"),ncol=2,byrow=TRUE))
expect_stdout(print(results), "5 matches in 4 strings")

# Check named groups
regexString <- "(?<numbers>\\d+)"
regex <- ore(regexString)
expect_equal(dimnames(groups(ore_search(regex, "1.7"))), list(NULL,"numbers"))
expect_equal(dimnames(groups(ore_search(regexString, "1.7"))), list(NULL,"numbers"))
