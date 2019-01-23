Directories are organized by (1) analyses by subtype (subtype C and CRF 02_AG) and (2) analyses for subtypes combined (including subtypes B, C and CRF 02_AG). In each directory will you find one the following files summarized below. In the OSG directory will you find the scripts used in the [Open Scince Grid computer resources](https://opensciencegrid.org/).

# Summary of each file

**1.model.R**

Mathematical model for the Senegal HIV dynamics

**2.load_data.R**

Load the necessary data for the phylodynamic analysis

**3.mcmc.R**

Source the files *"1.model.R"* and *"2.load_data.R"*.
Perform the MCMC using the BayesianTools R package

**4.mcmc_zMatrix.R**

Source the files *"1.model.R"* and *"2.load_data.R"*.
Perform the MCMC using the BayesianTools R package. 
The difference here is that it uses a previous run to generate
values for a z-matrix. For more information see https://github.com/florianhartig/BayesianTools/issues/79

