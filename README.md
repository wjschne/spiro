
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spiro <a href="https://wjschne.github.io/spiro/index.html"><img src="man/figures/logo.svg" align="right" height="139" alt="spiro website" /></a>

[![CRAN
status](https://www.r-pkg.org/badges/version/spiro)](https://cran.r-project.org/package=spiro)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

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

## Code of Conduct

Please note that the spiro project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
