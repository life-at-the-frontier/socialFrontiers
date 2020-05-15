# socialFrontiers

socialFrontiers is an R package for detecting social frontiers using spatial data. 
A social frontier denotes places where there is a sharp difference in social/ethnic 
characteristics between neighbouring communities. The package use an detection
model detailed in Dean et al 2016 [1]. This package contains functions for 
implementing the detection model and extracting its results.


[1] Dean, Nema, Guanpeng Dong, Aneta Piekut, and Gwilym Pryce. 2016. ‘Frontiers in Residential Segregation : Understanding Neighbourhood Boundaries and Their Impacts’ 00 (00): 1–22. https://doi.org/10.1111/tesg.12316.


##  How to install

This package relies on dependencies found in CRAN except for the INLA package.

The INLA package must be installed.
See: http://www.r-inla.org/download


### Installing from github:

Below option also builds vignettes.

```r
require(devtools)
install_github("menglezhang/socialfrontiers", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
```
