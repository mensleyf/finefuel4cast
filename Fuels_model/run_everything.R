# Code to run Fuels Model for Ensley-Field et al. 

# ----------------------- packages----------------------


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

(current_folder <- (dirname(getActiveDocumentContext()$path))) #works only in Rstudio
setwd(current_folder)

# ----------------------- run models : final model, posterior predictive check, sensitivity analysis ----------------------

# run model
source(file = "fuels_model_run/fuels_model_run.R")
gc()
source(file = "fuels_model_run/fuels_model_run_ppc.R")
gc()
source(file = "fuels_model_run/fuels_model_prior_sensitivity_analysis.R") #takes a long time (days) to run
gc()


# ------------------------ look at rstan code -------------------------------------

file.show("stan_txts/fuels_model_code.txt")

# a very annotated version, because we think that^ is confusing and we wrote it
file.show("stan_txts/fuels_model_code_annotated.txt")

#-------------------------- model checking and visualization -----------

# our favorite way to check a model
fit1<-readRDS( "model_outputs/fuels_model.rds")
#shinystan::launch_shinystan(fit1)

#traceplots and rhat scores: makes traceplots and prints rhat scores
source(file= "model_checking_and_validation/convergeance_check.R")
# look 
# browseURL( "Supplemental_Info_figs/alpha_traceplot.png")
# browseURL( "Supplemental_Info_figs/beta_traceplot.png")
# browseURL( "Supplemental_Info_figs/sig_o_traceplot.png")
# browseURL( "Supplemental_Info_figs/sig_f_traceplot.png")
gc()

#makes and shows ppc plot in 'Figures' folder 
source(file= "model_checking_and_validation/predictive_posterior_plots.R")
#look
# browseURL("Figures/ppc.png")
gc()

#prior posterior plots
source(file= "model_checking_and_validation/prior_posterior_plots.R")
# look
browseURL("Figures/posterior_distributions.png")
gc()

# coverage:
source(file="model_checking_and_validation/coverage_function.R")
# this function partitions error by source, and uses a few other helper functions
# it returns predictions for t+1 using model's a/b point estimates, sig_o / sig_p iterations, and input prod/fuel data
uncertainty<-plot_coverage(fit1, q_upper=.975, q_lower=.025,
                           nIters=1000, mod_name="(Default model)",
                           fuel_dat=fuel_data_z, prod_dat=prod_data_z,
                           prod_start_val_vec<-meta_dat$P0,
                           prod_m_match<-meta_dat$prod_m_match,
                           mean_samp=4.6, sig_f_par=.8, show_plot=F)
#can change 'show_plot=F' argument to T
gc()

#sensitivity analyses vis: this will take a long time (>30 min) to run
# you can look at diagnostic plots, coverage plots, and changes in posteriors in "Supplemental_Info_figs/prior_sensitivity_analysis_figs" 
# all plots are organized such that x axis is changes in one part of a prior
source(file="model_checking_and_validation/sensitivity_analysis_vis.R")
#look (2 plots of many)
# browseURL("Supplemental_Info_figs/prior_sensitivity_analysis_figs/alpha_mu_coverage.png")
# browseURL("Supplemental_Info_figs/prior_sensitivity_analysis_figs/alpha_mu.png")

# parameter retrieval from simulated data
# runs model (relatively fast, <10 minutes) on simulated data
# this is checking a simulated dataset similar to our results
source(file = "fuels_model_run/sim_data_model_run.R")

#look
#browseURL("Supplemental_Info_figs/simulation_retrieval.png")
gc()

# parameter range retrieval from simulated data, takes a while (hours) to run
# runs model on 66 different simulated data sets
# the first 33 have a higher carryover term alpha of 0.5, and the obdervation error ranges from 0.01 to 3
# the second 33 have a lower conversion term beta of 0.1, and the observation error ranges from 0.01 to 3
# this is checking if we would retrieve the alpha and beta terms if they are actually higher and lower than our results indicate
# if we're wrong about observation error

source(file = "fuels_model_run/simulation_loop_run.R")
source(file = "model_checking_and_validation/simulation_loop_vis.R")

# look
# browseURL("Supplemental_Info_figs/simulation_figs/higher_alpha.png")
# browseURL("Supplemental_Info_figs/simulation_figs/higher_beta.png")

# gc()


#-------Supplemental Information / other considerations -----------


# inital conditions simulation
# simulates productivity data for >50 years from actual productivity data
# simulates latent fuels data from fuels model
# looks at how sensitive latent fuel estimation is to initial value (not very)
# figures saved to 'Supplemental_Info_figs/spin_up_test_figs' folder and also shown
source(file="model_checking_and_validation/initial_conditions_spin_up.R")
#look
# browseURL("Supplemental_Info_figs/spin_up_test_figs/sensitivity_to_start_val.png")
gc()

# a quick look using linear regression at how parameter estimates of carryover (a), conversion (b),
# as well as correlation, r squared, and mean absolute predictive error change by data transformation
source(file="model_checking_and_validation/transformation_figs.R")
# look
# browseURL("Supplemental_Info_figs/spin_up_test_figs/data_transformation_figs.png")

gc()
