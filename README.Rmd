# mertree

[![Build Status](https://travis-ci.org/bgreenwell/mertree.svg?branch=master)](https://travis-ci.org/bgreenwell/mertree)
[![Coverage Status](https://img.shields.io/codecov/c/github/bgreenwell/mertree.svg)](https://codecov.io/github/bgreenwell/mertree?branch=master)

`mertree` (mixed-effects regression trees) is an alternative implementation of [`REEMtree`](http://pages.stern.nyu.edu/~jsimonof/REEMtree/) and [`REEMctree`](http://people.stern.nyu.edu/jsimonof/unbiasedREEM/) that uses [`lme4`](https://cran.r-project.org/web/packages/lme4/index.html) for efficient computation of mixed-effects models with large data sets.

## Installation

Package `mertree` is not currently available from CRAN, but the development version is hosted on GitHub at https://github.com/bgreenwell/mertree and can be downloaded using [`devtools`](https://github.com/hadley/devtools):
```r
# Assuming devtools is already installed
devtools::install_github("bgreenwell/mertree")
```
Bug reports should be submitted to https://github.com/bgreenwell/mertree/issues.

## Basic usage

```{r, warning=FALSE}
# Load required packages
library(mertree)
library(pdp)

# Fit a mixed-effects regression tree
fm <- mertree(y ~ time + x1 + x2 + x3 + x4 + x5 + x6 + (1 | subject),
              data = simd, unbiased = TRUE, do.trace = TRUE)

# Partial dependence of response on time
partial(fm, pred.var = "time", plot = TRUE, train = simd)

# Partial dependence of response on covariates (notice x3 and x6 are flat!)
par(mfrow = c(3, 2))
for (i in 1:6) {
  partial(fm, pred.var = paste0("x", i), plot = TRUE, train = simd)
}

# Is there an interaction between x1 and x4?
partial(fm, pred.var = c("x1", "x4"), plot = TRUE, train= simd)
```

## References

Rebecca J. Sela and Jeffrey S. Simonoff (2012). "RE-EM Trees: A Data Mining Approach for Longitudinal and Clustered Data". _Machine Learning_, 86(2), 169-207.

Wei Fu and Jeffrey S. Simonoff (2015), "Unbiased Regression Trees for Longitudinal and Clustered Data". _Computational Statistics and Data Analysis_, 88, 53-74.

Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker (2015). "Fitting Linear Mixed-Effects Models Using lme4". _Journal of Statistical Software_, 67(1), 53-74. doi:10.18637/jss.v067.i01.


