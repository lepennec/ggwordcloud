
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggwordcloud <img src="man/figures/logo.png" align="right" height=140/>

[![Travis build
status](https://travis-ci.org/lepennec/ggwordcloud.svg?branch=master)](https://travis-ci.org/lepennec/ggwordcloud)
[![Coverage
status](https://codecov.io/gh/lepennec/ggwordcloud/branch/master/graph/badge.svg)](https://codecov.io/github/lepennec/ggwordcloud?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggwordcloud)](https://cran.r-project.org/package=ggwordcloud)

`ggwordcloud` provides a word cloud text geom for `ggplot2`. The
placement algorithm implemented in C++ is close to the one used in
`wordcloud2.js` and thus aims to be a replacement of `wordcloud2` that
produces `ggplot2` type plot instead of an html widget. Note that the
current version does not provide the shape and mask possibility of
`wordcloud2`. The algorithm of `wordcloud` is similar but `ggwordcloud`
allows arbitrary rotations of the words.

## Installation

You can install the released version of ggwordcloud from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ggwordcloud")
```

or the development version from the github repository

``` r
devtools::install_github("lepennec/ggwordcloud")
```

# A simple word cloud example

As an example, we will use the `mtcars` dataset

``` r
library(ggwordcloud)
#> Loading required package: ggplot2
dat <- mtcars
dat$name <- row.names(mtcars)
dat$size <- dat$mpg
dat$size[c(1,4)] <- dat$size[c(1,4)] + 100
set.seed(42)
ggplot(data = dat, aes(label = name, size = size)) + geom_text_wordcloud() +
  theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

More examples are available in the vignette.
