# Oniguruma Regular Expressions (for R)

**NB: If you are looking for [Oracle R Enterprise](http://www.oracle.com/technetwork/database/database-technologies/r/r-enterprise/overview/index.html), please note you're in the wrong place!**

Welcome to the `ore` package for R. This package provides an alternative to R's standard functions for manipulating strings with regular expressions, based on the Oniguruma regular expression library (rather than PCRE, as in `base`). Although the regex features of the two libraries are quite similar, the R interface provided by `ore` has some notable advantages:

- Regular expressions are themselves first-class objects (of class `ore`), stored with attributes containing information such as the number of parenthesised groups present within them. This means that it is not necessary to compile a particular regex more than once.
- Search results focus around the matched substrings (including parenthesised groups), rather than the locations of matches. This saves extra work with `substr` to extract the matches themselves.
- Performance is [substantially better](https://github.com/jonclayden/regex-performance), especially when matching against long strings.
- Substitutions [can be functions](https://github.com/jonclayden/ore/wiki/Substitution-Functions), as well as literal or back-referenced strings.
- Common subexpressions can be easily stored in and retrieved from a dictionary.
- Matches can be efficiently obtained over only part of the strings.
- There are fewer core functions, and they have more consistent names.

Oniguruma (or rather, the [Onigmo](https://github.com/k-takata/Onigmo) fork of it) is the regular expression library used by the [Ruby programming language](http://ruby-lang.org/), and `ore` is somewhat inspired by Ruby's regular expression features; although it is implemented in what aims to be a natural way for R users, including full vectorisation.

This `README` covers the package's R interface only, and assumes that the reader is already familiar with regular expressions. Please see the [official reference document](https://raw.githubusercontent.com/k-takata/Onigmo/master/doc/RE) for details of supported regular expression syntax.

If you prefer the more verbose but also more friendly approach to creating regular expressions provided by Kevin Ushey and Jim Hester's excellent [`rex` package](https://github.com/kevinushey/rex), you can still use `ore` for performing the actual matching, and working with the results. None of the syntax generated by `rex` is known to be incompatible with Oniguruma.

[![Build Status](https://travis-ci.org/jonclayden/ore.svg?branch=master)](https://travis-ci.org/jonclayden/ore)

## Contents

- [Installation](#installation)
- [Function mapping](#function-mapping)
- [Basic usage](#basic-usage)
- [Encodings](#encodings)
- [Alternative syntaxes](#alternative-syntaxes)
- [Substitutions](#substitutions)
- [Splitting](#splitting)
- [The pattern dictionary](#the-pattern-dictionary)
- [Additional convenience functions](#additional-convenience-functions)
- [Searching in files](#searching-in-files)

## Installation

The package can be installed directly from GitHub using the `devtools` package.

```R
# install.packages("devtools")
devtools::install_github("jonclayden/ore")
```

It is also available [via CRAN](https://cran.r-project.org/package=ore), although the version published there can be a little older than the latest release tagged here, due to CRAN's restrictions on the regularity of updates.

## Function mapping

| Effect                        | `ore` syntax                                   | Base R syntax                           |
| ----------------------------- | ---------------------------------------------- | --------------------------------------- |
| Create a regex object         | `regex <- ore(regex_string)`                   | *(no equivalent)*                       |
| Is there a match?             | `ore.ismatch(regex, text)` or `text %~% regex` | `grepl(regex, text, perl=TRUE)`         |
| Find the first match          | `ore.search(regex, text)`                      | `regexpr(regex, text, perl=TRUE)`       |
| Find match after character 10 | `ore.search(regex, text, start=10)`            | *(no equivalent)*                       |
| Find all matches              | `ore.search(regex, text, all=TRUE)`            | `gregexpr(regex, text, perl=TRUE)`      |
| Replace first match           | `ore.subst(regex, replace, text)`              | `sub(regex, replace, text, perl=TRUE)`  |
| Replace all matches           | `ore.subst(regex, replace, text, all=TRUE)`    | `gsub(regex, replace, text, perl=TRUE)` |
| Split at matches              | `ore.split(regex, text)`                       | `strsplit(text, regex, perl=TRUE)`      |

As of `ore` version 1.2.0, it is also possible to use underscore-separated versions of the package's functions, i.e., `ore_search`, `ore_split` and so on. Some users may prefer these for consistency of style in their own code.

## Basic usage

Let's consider a very simple example: a regular expression for matching a single decimal integer, either positive or negative. We create this regex as follows:

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

Notice that the result of the search is an object of class `orematch`. This contains elements giving the offsets, lengths and content of matches, as well as those of any parenthesised groups. When printed, the object shows the original text with the matched substring extracted onto the line above (or coloured, if the `crayon` package is installed and a colour terminal is being used). This can be useful to check that the regular expression is capturing the text expected.

The `start` parameter to `ore.search()` can be used to indicate where in the text the search should begin. All matches (after the starting point) will be returned with `all=TRUE`:

```R
ore.search(re, "I have 2 dogs, 3 cats and 4 hamsters", start=10)
#   match:                3
# context: I have 2 dogs,   cats and 4 hamsters

ore.search(re, "I have 2 dogs, 3 cats and 4 hamsters", all=TRUE)
#   match:        2       3          4
# context: I have   dogs,   cats and   hamsters
#  number:        1       2          3
```

The text to be searched for matches can be a vector, in which case the return value will be a list of `orematch` objects:

```R
ore.search(re, c("2 dogs","3 cats","4 hamsters"))
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

ore.search(re1, text, all=TRUE)
#   match:      have
# context: I'll      a piña colada

ore.search(re2, text, all=TRUE)
#   match:      have   piña
# context: I'll      a      colada
#  number:      1===   2===
```

Note that, without a declared encoding, only ASCII word characters are matched to the `\w` character class. Since "ñ" is not directly representable in ASCII, the word "piña" is not considered a match.

If `ore.search()` is called with a string rather than an `ore` object for the regular expression, then the encoding of the text will also be associated with the regex. This this should generally produce the most sensible result.

```R
ore.search("\\b\\w{4}\\b", text, all=TRUE)
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

## Alternative syntaxes

By default, Oniguruma and `ore` use Ruby's regular expression syntax, which is very similar to Perl's (and hence that of base R with `perl=TRUE`). However, the library does support alternative syntaxes, and `ore` currently also allows for literal string matching, which is equivalent to `fixed=TRUE` in base R.

Notice the difference in interpretation of a period in the following example:

```R
ore.search(ore("."), "1.7")
#   match: 1  
# context:  .7

ore.search(ore(".",syntax="fixed"), "1.7")
#   match:  . 
# context: 1 7
```

In the first case the period has the usual regular expression interpretation of "any character", so it matches the first available character, the 1. In the second case the period has no special meaning, and it only matches a literal period in the search string.

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
# [1] "I have "    " dogs, "    " cats and " " hamsters"
```

This finds all matches to the pattern, discards them, and then returns the remaining pieces of the original string.

## The pattern dictionary

It's not unusual to reuse parts of a regular expression many times. Perhaps, once you have an expression that captures certain common elements of your text, you might want to store it for regular use. Or maybe you want to make your regexes more readable by breaking them down into manageable chunks. The `ore` package's pattern dictionary can help.

To take a simple example, let's just consider a pattern for digits. We can add it to the dictionary using the `ore.dict()` function.

```R
ore.dict(digits="\\d+")
# digits 
# "\\d+"
```

Now, we can create a regex using this pattern by naming it in a call to `ore()`.

```R
ore(digits)
# Oniguruma regular expression: /(\d+)/
#  - 1 groups
#  - unknown encoding
```

Notice the lack of quotation marks around the name, which distinguishes it from a normal pattern string. We can also reuse it multiple times, and add other regex syntax around it. Say, for example, that we want to find two sets of digits separated by word characters and/or space.

```R
re <- ore(digits, "[\\w\\s]+", digits)
re
# Oniguruma regular expression: /(\d+)[\w\s]+(\d+)/
#  - 2 groups
#  - unknown encoding
```

Notice that `ore()` constructs a full regex from the parts, wrapping each dictionary element in parentheses to make it a group. Now we can match it against our text.

```R
ore.search(re, "I have 2 dogs, 3 cats and 4 hamsters")
#   match:                3 cats and 4         
# context: I have 2 dogs,              hamsters
```

The package comes with a small dictionary of fairly robust regexes for matching common elements like numbers or email addresses. These can be used "out of the box". For example,

```R
ore.search(ore(number), "Numbers in various formats: -23, 0xbead5, .409 and 1.4e-5", all=TRUE)
#   match:                             -23  0xbead5  .409     1.4e-5
# context: Numbers in various formats:    ,        ,      and       
#  number:                             1==  2======  3===     4=====
```

Notice that, when using the dictionary, the `ore()` function must be called explicitly.

## Additional convenience functions

The `ore.ismatch` function will return a logical vector indicating whether or not a match is present in each element of a character vector. The infix notation `%~%` is a shorthand way to achieve the same thing. Either way, the full match data can be obtained without repeating the search, using the `ore.lastmatch()` function.

```R
if ("I have 2 dogs, 3 cats and 4 hamsters" %~% "-?\\d+")
  print(ore.lastmatch())
# [[1]]
#   match:        2
# context: I have   dogs, 3 cats and 4 hamsters
```

The `%~~%` operator works likewise, except that all matches will be found (i.e. it sets `all=TRUE` when calling `ore.search()`). Finally, the `%~|%` operator filters a vector, returning just elements which match the regular expression.

Text matching the entire regex, or parenthesised groups, can be extracted using the `matches()` and `groups()` convenience functions, or even more concisely using indexing.

```R
# An example from ?regexpr
re <- "^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)"
text <- "http://stat.umn.edu:80/xyz"
match <- ore.search(re, text)
matches(match)
# [1] "http://stat.umn.edu:80/xyz"

match[1]
# [1] "http://stat.umn.edu:80/xyz"

groups(match)
#      [,1]      [,2]   [,3]           [,4]  [,5] [,6]  
# [1,] "http://" "http" "stat.umn.edu" ":80" "80" "/xyz"

match[1,3]
# [1] "stat.umn.edu"
```

## Searching in files

Since version 1.3.0 of the package, it has been possible to search directly within files, using their native encoding if it is supported by Onigmo (which supports many more encodings than R does internally). Binary files may also be searched, but in that case the regex is fixed to use ASCII encoding, and the file is examined byte-by-byte.

For example, using a test file [provided with the package source](https://github.com/jonclayden/ore/blob/master/tests/testthat/sjis.txt), and if your local `iconv` supports the [Shift JIS encoding](https://en.wikipedia.org/wiki/Shift_JIS), you can try

```R
match <- ore.search("\\p{Katakana}+", ore.file("sjis.txt",encoding="SHIFT_JIS"), all=TRUE)
matches(match)
# [1] "コ"       "ディング" "ファイル"
```

Note that if you read the file using R's `readLines` function, it will be re-encoded to UTF-8. The same matches will be found, but the byte offsets are different:

```R
match <- ore.search("\\p{Katakana}+", ore.file("sjis.txt",encoding="SHIFT_JIS"), all=TRUE)
match$byteOffsets
# [1] 18 22 44

match <- ore.search("\\p{Katakana}+", readLines(file("sjis.txt",encoding="SHIFT_JIS")), all=TRUE)
match$byteOffsets
# [1] 22 28 61
```

Hence, if you want to know where in a file the match can be found, the first of these approaches will give the right answer, while the latter will not.
