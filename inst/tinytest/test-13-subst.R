expect_equal(ore_subst("\\d+","no","2 dogs"), "no dogs")
expect_equal(ore_subst("(\\d+)","\\1\\1","2 dogs"), "22 dogs")
expect_equal(ore_subst("\\d+",function(i) as.numeric(i)^2,"2 dogs"), "4 dogs")
expect_equal(ore_subst("\\d+",function(i) max(as.numeric(i)), "2, 4, 6 or 8 dogs?", all=TRUE), "8, 8, 8 or 8 dogs?")
expect_equal(ore_repl("\\d+",function(i) max(as.numeric(i)), "2, 4, 6 or 8 dogs?", all=TRUE), "2, 4, 6 or 8 dogs?")

expect_equal(ore_subst("\\d+",function(i) c("no","all the"),c("2 dogs","some dogs")), c("no dogs","some dogs"))
expect_equal(ore_repl("\\d+",function(i) c("no","all the"),c("2 dogs","some dogs")), list(c("no dogs","all the dogs"), c("some dogs","some dogs")))
expect_equal(ore_subst("\\d+",c("no","some"),c("2 dogs","3 cats")), c("no dogs","no cats"))
expect_equal(ore_repl("\\d+",c("no","some"),c("2 dogs","3 cats")), list(c("no dogs","some dogs"), c("no cats","some cats")))
expect_equal(ore_subst("\\d+",c("no","some"),"2 dogs and 3 cats",all=TRUE), "some dogs and some cats")
expect_equal(ore_repl("\\d+",c("no","some"),"2 dogs and 3 cats",all=TRUE), c("no dogs and no cats","some dogs and some cats"))

expect_equal(ore_split("[\\s\\-()]+","(801) 234-5678"), c("","801","234","5678"))