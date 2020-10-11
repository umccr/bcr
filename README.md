[![Conda install](https://anaconda.org/pdiakumis/r-woofr/badges/installer/conda.svg)](https://anaconda.org/pdiakumis/r-woofr)
[![Conda github action status](https://github.com/umccr/woofr/workflows/conda-upload/badge.svg)](https://github.com/umccr/woofr/actions?query=workflow%3Aconda-upload)

woofr
=====


`woofr` is an R package containing R helper functions for
[woof](https://github.com/umccr/woof).

You can do the following:

  - A
  - B
  - C

## Installation

### devtools

You can install the development version of `woofr` from
[GitHub](https://github.com/umccr/woofr) with:

``` r
# install.packages("devtools") # if not pre-installed
devtools::install_github("pdiakumis/woofr") # master version
devtools::install_github("pdiakumis/woofr@v1.2.3") # release v1.2.3
devtools::install_github("pdiakumis/woofr@abcd") # commit abcd
```

### conda

There is a conda package version at
<https://anaconda.org/pdiakumis/r-woofr> which is updated regularly.

You need to create a conda environment, and then install with:

``` bash
conda install -c pdiakumis r-woofr
```

## Usage

``` r
require(woofr)
```
