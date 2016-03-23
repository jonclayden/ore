context("Regular expression compilation")

test_that("Oniguruma regular expression compilation and interpretation works", {
    simpleRegex <- ore("-?\\d+")
    groupedRegex <- ore("(.)-(.)")
    regexWithOption <- ore("[abc]", options="i")
    regexWithEncoding <- ore("-?\\d+", encoding="UTF-8")
    regexWithSyntax <- ore(".", syntax="fixed")
    
    # To check that ore.dict() picks up the right enclosure
    regexGenerator <- function() { str <- "-?\\d+"; ore(str) }
    
    expect_that(simpleRegex, is_a("ore"))
    expect_that(attr(groupedRegex,"nGroups"), equals(2L))
    expect_that(attr(regexWithOption,"options"), equals("i"))
    expect_that(attr(regexWithEncoding,"encoding"), equals("UTF-8"))
    expect_that(attr(regexWithSyntax,"syntax"), equals("fixed"))
    expect_that(ore("(\\w+"), throws_error())
    expect_that(regexGenerator(), equals(simpleRegex,check.attributes=FALSE))
    expect_that(ore.escape("-?\\d+"), equals("-\\?\\\\d\\+"))
})
