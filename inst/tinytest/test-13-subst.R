# Substitutions of various kinds
expect_equal(ore_subst("\\d+","no","2 dogs"), "no dogs")
expect_equal(ore_subst("\\d+","no","2 dogs",start=3L), "2 dogs")
expect_equal(ore_subst("(\\d+)","\\1\\1","2 dogs"), "22 dogs")
expect_equal(ore_subst("\\d+",function(i) as.numeric(i)^2,"2 dogs"), "4 dogs")
expect_equal(ore_subst("\\d+",function(i) max(as.numeric(i)), "2, 4, 6 or 8 dogs?", all=TRUE), "8, 8, 8 or 8 dogs?")
expect_equal(ore_repl("\\d+",function(i) max(as.numeric(i)), "2, 4, 6 or 8 dogs?", all=TRUE), "2, 4, 6 or 8 dogs?")
expect_equal(ore_subst("(?<numbers>\\d+)","\\k<numbers>+\\k<numbers>","2 dogs"), "2+2 dogs")

# Differences between ore_subst() and ore_repl()
expect_equal(ore_subst("\\d+",function(i) c("no","all the"),c("2 dogs","some dogs")), c("no dogs","some dogs"))
expect_equal(ore_repl("\\d+",function(i) c("no","all the"),c("2 dogs","some dogs")), list(c("no dogs","all the dogs"), "some dogs"))
expect_equal(ore_subst("\\d+",c("no","some"),c("2 dogs","3 cats")), c("no dogs","no cats"))
expect_equal(ore_repl("\\d+",c("no","some"),c("2 dogs","3 cats")), list(c("no dogs","some dogs"), c("no cats","some cats")))
expect_equal(ore_subst("\\d+",c("no","some"),"2 dogs and 3 cats",all=TRUE), "some dogs and some cats")
expect_equal(ore_repl("\\d+",c("no","some"),"2 dogs and 3 cats",all=TRUE), c("no dogs and no cats","some dogs and some cats"))

# Check that encodings are preserved
text <- readLines("drink.txt", encoding="UTF-8")
switched <- ore_subst("(\\w)(\\w)", "\\2\\1", text, all=TRUE)
expect_equal(Encoding(switched), "UTF-8")

# Potentially pathological edge-cases
expect_error(ore_subst("\\d+",character(0),"2 dogs"), "No replacement")
expect_error(ore_subst("\\d+","\\k<name>","2 dogs"))
expect_error(ore_subst("\\d+","\\1","2 dogs"))
expect_equal(ore_subst("\\d+",function(i) NULL,"2 dogs"), " dogs")

# Check string splitting
expect_equal(ore_split("[\\s\\-()]+","(801) 234-5678"), c("","801","234","5678"))
