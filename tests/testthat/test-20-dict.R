context("Pattern dictionary functionality")

test_that("the pattern dictionary can be used", {
    ore.dict(protocol="\\w+://")
    
    expect_equal(ore.dict("protocol"), "protocol")
    expect_equal(ore.dict(paste0("proto","col")), "protocol")
    expect_equal(ore.dict(protocol), structure("\\w+://",names="protocol"))
    expect_equal(as.character(ore(protocol, "?www\\.")), "(\\w+://)?www\\.")
})
