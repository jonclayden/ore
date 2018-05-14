context("Regular expression compilation")

test_that("Oniguruma regular expression compilation and interpretation works", {
    simpleRegex <- ore("-?\\d+")
    groupedRegex <- ore("(.)-(.)")
    regexWithOption <- ore("[abc]", options="i")
    regexWithEncoding <- ore("-?\\d+", encoding="UTF-8")
    regexWithSyntax <- ore(".", syntax="fixed")
    
    # To check that ore.dict() picks up the right enclosure
    regexGenerator <- function() { str <- "-?\\d+"; ore(str) }
    
    expect_is(simpleRegex, "ore")
    expect_identical(attr(groupedRegex,"nGroups"), 2L)
    expect_equal(attr(regexWithOption,"options"), "i")
    expect_equal(attr(regexWithEncoding,"encoding"), "UTF-8")
    expect_equal(attr(regexWithSyntax,"syntax"), "fixed")
    expect_error(ore("(\\w+"))
    expect_equal(regexGenerator(), simpleRegex, check.attributes=FALSE)
    expect_equal(ore.escape("-?\\d+"), "-\\?\\\\d\\+")
})
