# Oniguruma Regular Expressions

Welcome to the `ore` package for R. This package provides an alternative to R's standard functions for manipulating strings with regular expressions, based on the Oniguruma regular expression library (rather than PCRE, as in `base`). Although the regex features of the two libraries are quite similar, the R interface provided by `ore` has some notable advantages:

- Regular expressions are themselves first-class objects (of class `ore`), stored with attributes containing information such as the number of parenthesised groups present within them. This means that it is not necessary to compile a particular regex more than once.
- Search results focus around the matched substrings rather than the locations of matches. This saves extra work with `substr` to extract the matches themselves.
- Substitutions can be functions as well as strings.

This `README` covers the package's R interface only, and assumes that the reader is already familiar with regular expressions. Please see the [official reference document](http://www.geocities.jp/kosako3/oniguruma/doc/RE.txt) for details of supported regular expression syntax.

## Contents

- [Installation](#installation)
- [Function mapping](#function-mapping)
- [Basic usage](#basic-usage)
- [Encodings](#encodings)
- [Substitutions](#substitutions)
- [Splitting](#splitting)
- [Additional convenience functions](#additional-convenience-functions)

## Installation

The package can be installed directly from GitHub using the `devtools` package.

```R
# install.packages("devtools")
devtools::install_github("jonclayden/ore")
```

It is also planned to make it available via CRAN shortly, for installation via the standard `install.packages` function.

## Function mapping

| Effect                        | `ore` syntax                                   | Base R syntax                           |
| ----------------------------- | ---------------------------------------------- | --------------------------------------- |
| Create a regex object         | `regex <- ore(regex_string)`                   | (no equivalent)                         |
| Is there a match?             | `ore.ismatch(regex, text)` or `text %~% regex` | `grepl(regex, text, perl=TRUE)`         |
| Find the first match          | `ore.search(regex, text)`                      | `regexpr(regex, text, perl=TRUE)`       |
| Find match after character 10 | `ore.search(regex, text, start=10)`            | (no equivalent)                         |
| Find all matches              | `ore.search(regex, text, all=TRUE)`            | `gregexpr(regex, text, perl=TRUE)`      |
| Replace first match           | `ore.subst(regex, replace, text)`              | `sub(regex, replace, text, perl=TRUE)`  |
| Replace all matches           | `ore.subst(regex, replace, text, all=TRUE)`    | `gsub(regex, replace, text, perl=TRUE)` |
| Split at matches              | `ore.split(regex, text)`                       | `strsplit(text, regex, perl=TRUE)`      |

## Basic usage

Let's consider a very simple example: a regular expression for matching a single integer, either positive or negative. We create this regex as follows:

```R
library(ore)

re <- ore("-?\\d+")
```

This syntax matches an optional minus sign, followed by one or more digits. Here we immediately introduce one of the differences between the regular expression capabilities of base R and the `ore` package: in the latter, regular expressions have class `ore`, rather than just being standard strings. We can find the class of the regex object, and print it:

```R
class(re)
# [1] "ore"

re
# Oniguruma regular expression: /-?\d+/
#  - 0 groups
#  - unknown encoding
```

The `ore()` function compiles the regex string, retaining the compiled version for later use. The number of groups in the string is obtained definitively, because the string is parsed by the full Oniguruma parser.

Once we have compiled the regex, we can search another string for matches:

```R
match <- ore.search(re, "I have 2 dogs, 3 cats and 4 hamsters")

class(match)
# [1] "orematch"

match
#   match:        2
# context: I have   dogs, 3 cats and 4 hamsters
```

Notice that the result of the search is an object of class `orematch`. This contains elements giving the offsets, lengths and content of matches, as well as those of any parenthesised groups. When printed, the object shows the original text with the matched substring extracted onto the line above. This can be useful to check that the regular expression is capturing the expected text.

The `start` parameter to `ore.search()` can be used to indicate where in the text the search should begin. All matches (after the starting point) will be returned with `all=TRUE`:

```R
(ore.search(re, "I have 2 dogs, 3 cats and 4 hamsters", start=10))
#   match:                3
# context: I have 2 dogs,   cats and 4 hamsters

(ore.search(re, "I have 2 dogs, 3 cats and 4 hamsters", all=TRUE))
#   match:        2       3          4
# context: I have   dogs,   cats and   hamsters
#  number:        1       2          3
```

The text to be searched for matches can be a vector, in which case the return value will be a list of `orematch` objects:

```R
(ore.search(re, c("2 dogs","3 cats","4 hamsters")))
# [[1]]
#   match: 2
# context:   dogs
# 
# [[2]]
#   match: 3
# context:   cats
# 
# [[3]]
#   match: 4
# context:   hamsters
```

If there is no match the return value will be `NULL`, or a list with `NULL` for elements with no match.

## Encodings

Both R and Oniguruma support alternative character encodings for strings, and this can affect matches. Consider the regular expression `\b\w{4}\b`, which matches words of exactly four letters. It behaves differently depending on the encoding that it is declared with:

```R
re1 <- ore("\\b\\w{4}\\b")
re2 <- ore("\\b\\w{4}\\b", encoding="utf8")
text <- enc2utf8("I'll have a piña colada")

(ore.search(re1, text, all=TRUE))
#   match:      have
# context: I'll      a piña colada

(ore.search(re2, text, all=TRUE))
#   match:      have   piña
# context: I'll      a      colada
#  number:      1===   2===
```

Note that, without a declared encoding, only ASCII word characters are matched to the `\w` character class. Since "ñ" is not directly representable in ASCII, the word "piña" is not considered a match.

If `ore.search()` is called with a string rather than an `ore` object for the regular expression, then the encoding of the text will also be associated with the regex. This this should generally produce the most sensible result.

```R
(ore.search("\\b\\w{4}\\b", text, all=TRUE))
#   match:      have   piña
# context: I'll      a      colada
#  number:      1===   2===
```

Notice that base R's regular expression functions will not find the second match:

```R
gregexpr("\\b\\w{4}\\b", text, perl=TRUE)
# [[1]]
# [1] 6
# attr(,"match.length")
# [1] 4
```

## Substitutions

The `ore.subst()` function can be used to substitute regex matches with new text. Matched subgroups may be referred to using numerical or named back-references.

```R
re <- ore("\\b(\\w)(\\w)(\\w)(\\w)\\b", encoding="utf8")
text <- enc2utf8("I'll have a piña colada")
ore.subst(re, "\\3\\1\\2\\4", text, all=TRUE)
# [1] "I'll vhae a ñpia colada"

re <- ore("\\b(?<first>\\w)(?<second>\\w)(?<third>\\w)(?<fourth>\\w)\\b", encoding="utf8")
ore.subst(re, "\\k<third>\\k<first>\\k<second>\\k<fourth>", text, all=TRUE)
# [1] "I'll vhae a ñpia colada"
```

A function may also be provided, which will be used to generate replacement strings. For example, we could find all integers in a string and replace them with their squares:

```R
re <- ore("-?\\d+")
text <- "I have 2 dogs, 3 cats and 4 hamsters"
ore.subst(re, function(i) as.integer(i)^2, text, all=TRUE)
# [1] "I have 4 dogs, 9 cats and 16 hamsters"
```

## Splitting

Strings can be split into parts using the `ore.split()` function.

```R
ore.split("-?\\d+", "I have 2 dogs, 3 cats and 4 hamsters")
# [[1]]
# [1] "I have "    " dogs, "    " cats and " " hamsters"
```

## Additional convenience functions

The `ore.ismatch` function will return a logical vector indicating whether or not a match is present in each element of a character vector. The infix notation `%~%` is a shorthand way to achieve the same thing. Either way, the full match data can be obtained without repeating the search, using the `ore.lastmatch()` function.

```R
if ("I have 2 dogs, 3 cats and 4 hamsters" %~% "-?\\d+")
  print(ore.lastmatch())
# [[1]]
#   match:        2
# context: I have   dogs, 3 cats and 4 hamsters
```

The `%~~%` operator works likewise, except that all matches will be found (i.e. it sets `all=TRUE` when calling `ore.search()`).

Text matching the entire regex, or parenthesised groups, can be extracted using the `matches()` and `groups()` convenience functions.

```R
# An example from ?regexpr
re <- "^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)"
text <- "http://stat.umn.edu:80/xyz"
match <- ore.search(re, text)
matches(match)
# "http://stat.umn.edu:80/xyz"

groups(match)
#      [,1]      [,2]   [,3]           [,4]  [,5] [,6]  
# [1,] "http://" "http" "stat.umn.edu" ":80" "80" "/xyz"
```
