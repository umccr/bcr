
# bcbio Result Comparison

<!--[![Travis build status](https://travis-ci.org/umccr/bcr.svg?branch=master)](https://travis-ci.org/umccr/bcr)
[![Coverage status](https://codecov.io/gh/umccr/bcr/branch/master/graph/badge.svg)](https://codecov.io/github/umccr/bcr?branch=master) -->

`bcr` is an R package that (hopefully) helps with the comparison of
[bcbio](https://github.com/bcbio/bcbio-nextgen) runs, as used at
[UMCCR](https://research.unimelb.edu.au/centre-for-cancer-research/our-research/genomics-platform-group).

You can do the following:

  - A
  - B
  - C

## Installation

### devtools

You can install the development version of `bcr` from
[GitHub](https://github.com/umccr/bcr) with:

``` r
# install.packages("devtools") # if not pre-installed
devtools::install_github("umccr/bcr") # master version
devtools::install_github("umccr/bcr@v1.2.3") # release v1.2.3
devtools::install_github("umccr/bcr@abcd") # commit abcd
```

### conda

There is a conda package version at
<https://anaconda.org/pdiakumis/r-bcr> which is updated regularly.

You need to create a conda environment, and then install with:

``` bash
conda install -c pdiakumis r-bcr
```

## Usage

``` r
require(bcr)
```
