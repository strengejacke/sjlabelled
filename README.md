# sjlabelled - Labelled Data Utility Functions <img src="man/figures/logo.png" align="right" />

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1249215.svg)](https://doi.org/10.5281/zenodo.1249215) &#160;&#160; [![Documentation](https://img.shields.io/badge/documentation-sjlabelled-orange.svg?colorB=E91E63)](https://strengejacke.github.io/sjlabelled/) &#160;&#160; [![Build Status](https://travis-ci.org/strengejacke/sjlabelled.svg?branch=master)](https://travis-ci.org/strengejacke/sjlabelled)

This package contains utility functions that are useful when working with labelled data (especially intended for people coming from 'SPSS', 'SAS' or 'Stata' and/or who are new to R).

Basically, this package covers reading and writing data between other statistical packages (like 'SPSS') and R, based on the haven and foreign packages; hence, this package also includes functions to make working with labelled data easier. This includes easy ways to get, set or change value and variable label attributes, to convert labelled vectors into factors or numeric (and vice versa), or to deal with multiple declared missing values.

## Installation

### Latest development build

To install the latest development snapshot (see latest changes below), type following commands into the R console:

```r
library(devtools)
devtools::install_github("strengejacke/sjlabelled")
```

### Officiale, stable release

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sjlabelled)](https://cran.r-project.org/package=sjlabelled) &#160;&#160; [![Documentation](https://img.shields.io/badge/documentation-sjlabelled-orange.svg?colorB=E91E63)](https://strengejacke.github.io/sjlabelled/) &#160;&#160; [![Build Status](https://travis-ci.org/strengejacke/sjlabelled.svg?branch=master)](https://travis-ci.org/strengejacke/sjlabelled) &#160;&#160;
[![downloads](http://cranlogs.r-pkg.org/badges/sjlabelled)](http://cranlogs.r-pkg.org/)
&#160;&#160;
[![total](http://cranlogs.r-pkg.org/badges/grand-total/sjlabelled)](http://cranlogs.r-pkg.org/)

To install the latest stable release from CRAN, type following command into the R console:

```r
install.packages("sjlabelled")
```

## Citation

In case you want / have to cite my package, please use `citation('sjlabelled')` for citation information. 

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1249215.svg)](https://doi.org/10.5281/zenodo.1249215)

