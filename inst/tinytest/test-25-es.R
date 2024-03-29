x <- 3
name <- "Pi"
expect_equal(es("x is #{3}"), "x is 3")
expect_equal(es("x^2 is #{x^2}"), "x^2 is 9")
expect_equal(as.numeric(es("#{pi}",round=2)), 3.14)
expect_equal(es("#{name} is #{pi}",round=2), "Pi is 3.14")
expect_equal(es("x is #{x} and x^2 is #{x^2}"), "x is 3 and x^2 is 9")
expect_equal(es("Test #{1:2} of #{2}"), c("Test 1 of 2","Test 2 of 2"))
expect_equal(es("#{}"), "")
