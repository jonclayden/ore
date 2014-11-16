context("Compatibility with rex")

test_that("ore search works with rex output", {
    if (system.file(package="rex") == "")
        skip("The \"rex\" package is not available")
    else
    {
        suppressPackageStartupMessages(library(rex))
        regex <- rex(start,
            ## match the protocol -- may exist or may not
            maybe(capture(
                capture(except_some_of(":")),
                "://"
            )),
        
            ## match the path
            capture(one_or_more(not(":/"))),
        
            ## get the port
            maybe(capture(":", capture(numbers))),
        
            ## and the rest
            maybe(capture("/", anything)),
        
            end)
    
        url <- "http://www.clayden.org/"
        expect_that(ore::matches(ore.search(regex,url)), equals(url))
    }
})
