context("Expression substitution")

test_that("expression substitution works", {
    x <- 3
    name <- "Pi"
    expect_that(es("x is #{3}"), equals("x is 3"))
    expect_that(es("x^2 is #{x^2}"), equals("x^2 is 9"))
    expect_that(as.numeric(es("#{pi}",round=2)), equals(3.14))
    expect_that(es("#{name} is #{pi}",round=2), equals("Pi is 3.14"))
    expect_that(es("x is #{x} and x^2 is #{x^2}"), equals("x is 3 and x^2 is 9"))
})
