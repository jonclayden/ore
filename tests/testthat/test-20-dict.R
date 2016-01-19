context("Pattern dictionary functionality")

test_that("the pattern dictionary can be used", {
    ore.dict(protocol="\\w+://")
    
    expect_that(ore.dict("protocol"), equals("protocol"))
    expect_that(ore.dict(paste0("proto","col")), equals("protocol"))
    expect_that(ore.dict(protocol), equals(structure("\\w+://",names="protocol")))
    expect_that(as.character(ore(protocol, "?www\\.")), equals("(\\w+://)?www\\."))
})
