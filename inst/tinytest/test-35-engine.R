# Regression tests for Onigmo fixes backported from Ruby (bug numbers refer to bugs.ruby-lang.org)

# Premature end of a character property (Bug #17340)
expect_error(ore("\\p{", encoding="UTF-8"), "invalid character property name")
expect_error(ore("\\p{", encoding="ASCII"), "invalid character property name")

# Quantifiers must not be reduced if that would change the match
expect_equal(ore_search("(a+?)*", "aa")$matches, "aa")
expect_equal(ore_search("(?:a+?)*", "aa")$matches, "aa")
quantifiers <- c("?", "*", "+", "??", "*?", "+?")
for (q1 in quantifiers)
{
    for (q2 in quantifiers)
        expect_equal(ore_search(paste0("(?:a",q1,")",q2), "aa")$matches, ore_search(paste0("(a",q1,")",q2), "aa")$matches)
}

# Backreference number overrun (Bug #16376)
expect_error(ore("(())(?<X>)((?(90000)))"), "invalid backref number")

# Multiplex backreferences near the end of the string (Bug #18631)
expect_false(is.null(ore_search("(?<x>a)(?<x>aa)\\k<x>", "aaaaa")))
expect_false(is.null(ore_search("(?<x>a)(?<x>aa)\\k<x>", "aaaa")))
expect_false(is.null(ore_search("(?<x>a)(?<x>aa)\\k<x>", "aaaab")))

# Absent operator at the end of the input string
expect_true("" %~% "(?~(a))")

# Subexpression calls inside a repeat (Bug #20246)
expect_equal(ore_search("(\\d+)(\\.\\g<1>){2}", "1.2.3")$matches, "1.2.3")
expect_equal(ore_search("((?:\\d|foo|bar)+)(\\.\\g<1>){2}", "1.2.3")$matches, "1.2.3")

# Case-insensitive character classes with small code points (Bugs #16145 and #21176)
for (enc in c("UTF-8", "latin1"))
{
    o_acute <- iconv(c("\u00F3","abc\u00D3"), "UTF-8", enc)
    e_acute <- iconv(c("\u00E9","CAF\u00C9"), "UTF-8", enc)
    expect_true(o_acute[2] %~% ore("[x", o_acute[1], "]", options="i", encoding=enc), info=enc)
    expect_true(e_acute[2] %~% ore("[x", e_acute[1], "]", options="i", encoding=enc), info=enc)
}

# Repeat ranges that are too big, or too numerous
expect_error(ore("|{1000000}"), "too big number for repeat range")
if (at_home())
{
    expect_error(ore(strrep("(?:foobar){0,100}", 100000)))
    expect_error(ore(strrep("(?:(?:foo)?|(?:bar)?)*", 100000)))
}

# Nested repeats whose expanded size would overflow must not be unrolled
expect_null(ore_search("(?:.{90000,}){90000}", strrep("x",10)))
expect_null(ore_search("(?:.{46341,}){46341}", strrep("x",10)))

# Extended grapheme clusters, which previously crashed in UTF-8 because of outdated Unicode tables
clusters <- function (text) ore_search(ore("\\X",encoding="UTF-8"), enc2utf8(text), all=TRUE)$matches
expect_equal(clusters("e\u0301a"), c("e\u0301","a"))
expect_equal(clusters("a\r\nb"), c("a","\r\n","b"))
expect_equal(clusters("\u1100\u1161\u11A8x"), c("\u1100\u1161\u11A8","x"))
expect_equal(clusters("\U0001F1EC\U0001F1E7\U0001F1EF\U0001F1F5"), c("\U0001F1EC\U0001F1E7","\U0001F1EF\U0001F1F5"))
expect_equal(clusters("\U0001F468\u200D\U0001F469\u200D\U0001F467!"), c("\U0001F468\u200D\U0001F469\u200D\U0001F467","!"))
expect_equal(clusters("\u0915\u094D\u0937\u093F"), "\u0915\u094D\u0937\u093F")
expect_equal(clusters("\u0B95\u0BCD\u0BB7"), c("\u0B95\u0BCD","\u0BB7"))
expect_equal(ore_search(ore("\\X",encoding="latin1"), "ab", all=TRUE)$matches, c("a","b"))

# Properties from recent Unicode versions
expect_true("\U00010D50" %~% "\\p{Garay}")
expect_true("\u10D0" %~% ore("\u1C90", options="i"))
