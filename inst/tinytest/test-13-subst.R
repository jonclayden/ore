expect_equal(ore_subst("\\d+","no","2 dogs"), "no dogs")
expect_equal(ore_subst("(\\d+)","\\1\\1","2 dogs"), "22 dogs")
expect_equal(ore_subst("\\d+",function(i) as.numeric(i)^2,"2 dogs"), "4 dogs")
expect_equal(ore_subst("\\d+",function(i) max(as.numeric(i)), "2, 4, 6 or 8 dogs?", all=TRUE), "8, 8, 8 or 8 dogs?")

expect_equal(ore_split("[\\s\\-()]+","(801) 234-5678"), c("","801","234","5678"))
