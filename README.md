# socialFrontiers

socialFrontiers is an R package for detecting social frontiers using spatial data. 
A social frontier denotes places where there is a sharp difference in social/ethnic 
characteristics between neighbouring communities. The package use an detection
model detailed in Dean et al 2016 [1] and originally developed in Lee and Mitchell 2013 [2]. This package contains functions for 
implementing the detection model and extracting its results.


[1] Dean, Nema, Guanpeng Dong, Aneta Piekut, and Gwilym Pryce. 2016. ‘Frontiers in Residential Segregation : Understanding Neighbourhood Boundaries and Their Impacts’ 00 (00): 1–22. https://doi.org/10.1111/tesg.12316.

[2] Lee, Duncan, and Richard Mitchell. ‘Locally Adaptive Spatial Smoothing Using Conditional Auto-Regressive Models’. Journal of the Royal Statistical Society: Series C (Applied Statistics) 62, no. 4 (August 2013): 593–608. https://doi.org/10.1111/rssc.12009.

Massive thanks for Simon Carrignon (@simoncarrignon) for looking over issues and improving the code. 

##  How to install

This package relies on dependencies found in CRAN except for the INLA package.

The INLA package must be installed.
See: https://www.r-inla.org/download-install


### Installing from github:

Below option also builds vignettes.

```r
require(devtools)
install_github("menglezhang/socialfrontiers", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
```

Older versions of this package can be installed using:

```r
require(devtools)
install_github("menglezhang/socialfrontiers@__VERSION__", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
```

Where the `__VERSION__` is to be replaced with the package version (e.g. `menglezhang/socialfrontiers@v0.1`. Options are:
- v0.1 The first releast 
- v0.2 This is hotfix version for a later version of INLA that broke v0.1


## Legacy / installing a working version of this package

This package isn't on CRAN because I cannot maintain it. If you encounter any errors, it is likely due to changes in the package dependencies (probably INLA). In which case, it is easier to install an older version of R and it's packages using the checkpoint package. Here's a guide using:
- R v4.0.0
- R packages in the CRAN repo on 1 Jan 2020 (using checkpoint)
- INLA version 18.12.12

1. Download version R v4.0.0 and install 
2. Install the checkpoint package. If your version of R is too old then I recommend downloading and installing an older version of checkpoint from [here](https://cran.r-project.org/src/contrib/Archive/checkpoint/)
3. Before running anything set up checkpoint. This will ensure that you are using archived R packages:
```
library(checkpoint)
checkpoint('2020-01-01')
```
4. Install an older version of INLA. I would check the INLA website for instructions but I usually encounter error. Best to download and install an older verstion from their archives [here](https://inla.r-inla-download.org/R/stable/src/contrib/). I recommend version 18.12.12.
5. Install this package
```r
require(devtools)
install_github("menglezhang/socialfrontiers", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
```
6. Then run your analysis and save the outputs for use elsewhere (e.g. in newer versions of R, python etc). 


