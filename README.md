
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spiro

The `spiro` package creates spirographs in the .svg file format. There
are functions in `spiro` that can can alter and animate your spirographs
after they have been created.

## Installation

You can install spiro from github with:

``` r
# install.packages("devtools")
devtools::install_github("wjschne/spirographer")
```

## Example

Here is a basic spirograph.

``` r
library(spiro)
spiro(fixed_radius = 11, cycling_radius = 4) 
```

![](spiro020.svg)<!-- -->
