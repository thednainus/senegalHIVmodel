# Senegal HIV Model

This repository contains the research compendium organized as an R package for [Nascimento _et al_. 2020](https://doi.org/10.1016/j.epidem.2019.100376). Here you can find all scripts and data used in our paper. In the future I will also add vignettes to reproduce our analyses.

Please note that the functions provided in this research compendium were optimized for our data, and might not work on your own data as it is. However, reproducing our analysis using the vignettes may help you setting up your own analysis for epidemiology using structured coaslecent models as described in [Volz, 2012](http://www.genetics.org/content/190/1/187).

## Authors
Fabricia F. Nascimento (thednainus@yahoo.com)

Erik M. Volz (e.volz@imperial.ac.uk)

## How to install it?

```r
devtools::install_github(repo = "thednainus/senegalHIVmodel", build_vignettes = T)

# After installing the package:
library(senegalHIVmodel)
```
## How to use it?

You can find the scripts used for the Senegal HIV model in the [scripts](https://github.com/thednainus/senegalHIVmodel/tree/master/analyses/scripts) directory.

The [data](https://github.com/thednainus/senegalHIVmodel/tree/master/inst/data) directory contains all the data used to estimate the epidemiological parameters in the Senegal HIV model.
