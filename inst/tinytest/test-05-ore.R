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
