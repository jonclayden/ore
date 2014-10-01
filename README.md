Welcome to the `ore` package for R. This package provides an alternative to R's standard functions for manipulating strings with regular expressions, based on the Oniguruma regular expression library (rather than PCRE, as in `base`). Although the regex features of the two libraries are quite similar, the R interface provided by `ore` has some notable advantages:

- Regular expressions are themselves first-class objects (of class `ore`), stored with attributes containing information such as the number of parenthesised groups present within them. This means that it is not necessary to compile a particular regex more than once.
- Search results focus around the matched substrings rather than the locations of matches. This saves extra work with `substr` to extract the matches themselves.
- Substitutions can be functions as well as strings.

This `README` covers the package's R interface only, and assumes that the reader is already familiar with regular expressions. Please see the [official reference document](http://www.geocities.jp/kosako3/oniguruma/doc/RE.txt) for details of supported regular expression syntax.

## Installation

The package can be installed directly from GitHub using the `devtools` package.

```R
library(devtools)
install_github("jonclayden/ore")
```

It is also planned to make it available via CRAN shortly, for installation via the standard `install.packages` function.

## Basic usage

Let's consider a very simple example: a regular expression for matching a single integer, either positive or negative. We create this regex as follows:

```R
library(ore)

re <- ore("-?\\d+")
```

This syntax says: *expect an optional minus sign, followed by one or more digits*. Here we immediately introduce one of the differences between the regular expression capabilities of base R and the `ore` package: in the latter, regular expressions are objects of a particular type, rather than just standard strings. We can find the class of the regex object, and print it:

```R
class(re)
# [1] "ore"

re
# Oniguruma regular expression: /-?\d+/
#  - 0 groups
#  - unknown encoding
```

[To be continued...]
