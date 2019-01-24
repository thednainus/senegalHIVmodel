These R scripts were used to calculate the trajectories for our simulations using the posterior distribution of the Markov chain Monte Carlo (MCMC) analyses to estimate the parameter of our models. See *Trajectories.R* (analyses by subtype) and *Trajectories_allSubtypes.R* (analyses using the combined data) for more details.

After we estimated the trajectories, we used that information to estimate the effective number of infections and the population attributable fraction (PAF) for each set of analyses. We also estimated the proportion of infections in one deme attributable to another deme, for example, from *msm* (men who have sex with other men) to *gpf* (heterosexual females from the general population). See the other R scripts in this directory on how these were calculated.

The script *read_mcmc_runs.R* was used to read the results of each MCMC run and merge the best runs into a single run. See paper for more details.
