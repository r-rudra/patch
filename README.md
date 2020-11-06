
<!-- README.md is generated from README.Rmd. Please edit that file -->

# patch

<!-- badges: start -->

<!-- badges: end -->

**Modify or Patch any Function Programmatically**

Patch functions programmatically in a robust way so that the patch will
work even if there is a change (up to an extent) in the target function.
The changes are additive. The changes can be reverted back to the
original (within a single session of the package).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# You need {remotes} for that (I think you already have that)
# install.packages("remotes")
remotes::install_github("r-rudra/patch")
```

## Example

Just try out following:

``` r
library(patch)
help("patch_function")
# possibly you'll be interested in 
# source(system.file("embedded","usecases.R",package = "patch"))
# but read warnings before it :-)
```
