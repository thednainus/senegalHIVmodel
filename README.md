# Senegal HIV Model

This repository contains the research compendium organized as an R package for Nascimento _et al_. 2018 (_In prep._). Here you can find all scripts and data used in our paper. Vignettes are also provided to reproduce our analysis.

Please note that the functions provided in this research compendium were optimized for our data, and might not work to your own data as it is. However, reproducing our analysis using the vignettes may help you setting up your own analysis for epidemiology using structured coaslecent models as described in [Volz, 2012](http://www.genetics.org/content/190/1/187).

## Authors
Fabricia F. Nascimento (thednainus@yahoo.com)

Erik M. Volz (e.volz@imperial.ac.uk)

## How to install it?

```r
#it will only work when repository is made public
devtools::install_github(repo = "thednainus/senegalHIVmodel", build_vignettes = T)

# After installing the package:
library(senegalHIVmodel)
```
## How to use it?

You can find the scripts used for the Senegal HIV model in the [scripts](https://github.com/thednainus/senegalHIVmodel/tree/master/analysis/scripts) directory.

The [data](https://github.com/thednainus/senegalHIVmodel/tree/master/inst/data) directory contains all the data used to estimate the epidemiological parameters in the Senegal HIV model.

After installing the research compendium, you can read the vignette for a guide on how the analysese were arried out using the R code below.

```r
devtools::vignette("Senegal_HIV_model")
```
