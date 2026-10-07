simpleRegex <- ore("-?\\d+")
groupedRegex <- ore("(.)-(.)")
regexWithOption <- ore("[abc]", options="i")
regexWithEncoding <- ore("-?\\d+", encoding="UTF-8")
regexWithSyntax <- ore(".", syntax="fixed")

# To check that ore_dict() picks up the right enclosure
regexGenerator <- function() { str <- "-?\\d+"; ore(str) }

expect_inherits(simpleRegex, "ore")
expect_identical(attr(groupedRegex,"nGroups"), 2L)
expect_equal(attr(regexWithOption,"options"), "i")
expect_equal(attr(regexWithEncoding,"encoding"), "UTF-8")
expect_equal(attr(regexWithSyntax,"syntax"), "fixed")
expect_error(ore("(\\w+"))
expect_equal(regexGenerator(), simpleRegex, check.attributes=FALSE)
expect_equal(ore_escape("-?\\d+"), "-\\?\\\\d\\+")

expect_stdout(print(simpleRegex), "0 groups")
expect_stdout(print(ore("(?<numbers>\\d+)")), "1 group, 1 named")

# Encoding names are matched in full, with or without separators
expect_true("\xa8" %~% ore("\\w", encoding="ISO-8859-15"))
expect_true("\xa8" %~% ore("\\w", encoding="iso8859_15"))
expect_false("\xa8" %~% ore("\\w", encoding="ISO-8859-1"))
expect_true("\xa8" %~% ore("\\w", encoding="LATIN9"))
expect_true("\xfd" %~% ore("\\w", encoding="LATIN5"))
expect_warning(ore("a", encoding="UTF-8-ish"), "not supported")

# Regexes that have been saved and reloaded keep their settings
path <- tempfile()
saveRDS(list(ore("abc",options="i"), ore("a.c",syntax="fixed"), ore("(?<first>a)b(?<second>c)")), path)
regexes <- readRDS(path)
unlink(path)
expect_true("ABC" %~% regexes[[1]])
expect_true("a.c" %~% regexes[[2]])
expect_false("abc" %~% regexes[[2]])
expect_equal(groups(ore_search(regexes[[3]], "abc")), groups(ore_search(ore("(?<first>a)b(?<second>c)"), "abc")))

# Missing values in patterns, and unsupported options, are reported
expect_error(ore(NA_character_), "missing values")
expect_warning(ore("a", options="z"), "not supported")

# Escaping keeps the declared encoding
expect_equal(Encoding(ore_escape(iconv("caf\u00e9.","UTF-8","latin1"))), "latin1")
