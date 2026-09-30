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
    o_acute <- iconv(c("ó","abcÓ"), "UTF-8", enc)
    e_acute <- iconv(c("é","CAFÉ"), "UTF-8", enc)
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
