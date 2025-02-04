
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spiro <img src="man/figures/logo.svg" align="right" height=140/>

[![CRAN
status](https://www.r-pkg.org/badges/version/spiro)](https://cran.r-project.org/package=spiro)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/wjschne/spiro.svg?branch=master)](https://travis-ci.org/wjschne/spiro)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/wjschne/spiro?branch=master&svg=true)](https://ci.appveyor.com/project/wjschne/spiro)

The spiro package creates spirographs in the .svg file format. There are
functions in spiro that transform and animate your spirographs after
they have been created.

  - [Tutorial](https://wjschne.github.io/spiro/articles/HowToUse/spiro.html)
  - [Gallery](https://wjschne.github.io/spiro/articles/Gallery/Gallery.html)
  - [Function
    References](https://wjschne.github.io/spiro/reference/index.html)

## Installation

You can install spiro from github with:

``` r
# install.packages("remotes")
remotes::install_github("wjschne/spiro")
```

## Example

Here is a basic spirograph.

``` r
library(spiro)
spiro(
  fixed_radius = 11, 
  cycling_radius = 4, 
  pen_radius = 9, 
  file = "example.svg") 
```

![](man/figures/example.svg)

Please note that the spiro project is released with a [Contributor Code
of
Conduct](https://github.com/wjschne/spiro/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
