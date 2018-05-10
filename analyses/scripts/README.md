# Summary of each file

**1.model.R**

Mathematical model for the Senegal HIV dynamics

**2.load_data.R**

Load the necessary data for the phylodynamics analysis

**3.mcmc.R**

Source the files *"1.model.R"* and *"2.load_data.R"*.
Perform the MCMC using the BayesianTools package

**4. merge_trees.R**

Script used to merge each of the 3 HIV phylogenetic tree (for each subtype) into a single tree

**5. results.R**

Script used to plot the trajectory (demographic process)
