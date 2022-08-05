# Code to run Productivity Forecast analyses for Ensley-Field et al. 


## ---------- set-up ----------------

# install and update packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

# Set working directory to source file location (this method only works on Rstudio)
current_folder <- dirname(getActiveDocumentContext()$path)
setwd(current_folder)
#memory.limit(size=10000000000000000000) #can't increase; some kind of bug (?)

## ---------- assembling data to run models ----------------

# These scripts will download three tiffs from Google Earth Engine with all covariates used in Ensley-Field et al
# The  spatial extent and resolution can be changed; a smaller extent and higher resolution will help this run faster
# see readme for more details
# you will need to move the tiff files produced from your 
# google drive folder to the '~finefuel4cast\Prod_Forecast_model\gee_4cast_data\prod_4cast_tiffs' folder

# RAP_gee_tiffs: https://code.earthengine.google.com/1344640e568dc96d032fecd99d45e4f3
# Spatial_gee_tiffs: https://code.earthengine.google.com/92f1428e7183d07afe5c4d7e42bc374a
# Temporal_gee_tiffs: https://code.earthengine.google.com/02399e2d37e3e6fd92da7fa200038c88
 
 
# script to convert tiff files downloaded from GEE to csvs and formats csv files to include distinguishable date and variable names and columns.

source(file="model_assembly_scripts/spatial_data_script.R")
source(file= "model_assembly_scripts/RAP_data_script.R")
source(file= "model_assembly_scripts/temporal_data_script.R") #this takes a long time (hours) to run
gc() 

# assembles final csv to use in productivity forecast models
# this script takes ~ 10 min.
source(file= "model_assembly_scripts/march_model_assemble.R")

# check data assembly

#plausible values? correct time range?
mod_dat<-fread("gee_4cast_data/model_csvs/march_all_model_csv.csv")
head(mod_dat)
range(mod_dat$year)
rm(mod_dat)
# 
forecast_dat<-fread("gee_4cast_data/model_csvs/march_forecast_2021_csv.csv")
head(forecast_dat)
range(forecast_dat$year)

## ---------- running models ----------------

# Runs the forecast, null, ndvi, and climate_ndvi models referenced in paper,
# saves leave-one-year out predictions for 1987-2020
source(file= "running_model_scripts/run_all_models.R")
gc()

#closer look at our forecast model, creates correlation matrix of weather covariates and .csv of all covariates
source(file= "running_model_scripts/lm_closer_look.R")
# browseURL("figures/covariate_correlation.png")
# print(read.csv("prod_model_outputs/lm_coeffs.csv"))

# Forecasts 2021 productivity and calculates process and parameter uncertainty from the forecast regression model
# you might need to lower 'nIters' from the default (100) depending on computational limits of your system and the spatial extent you run this over
nIters<-100
source(file= "running_model_scripts/forecast_lm_uncertainty.R")


## ---------- creating figures ----------------

# Creates predictive scores comparing four models referenced in manuscript
# time series are saved in 'figures' folder
source(file= "visualization_scripts/model_errors_time_series.R") 

# Time series of model predictions in four BLM districts
source(file="visualization_scripts/time_series_CI_vis.R") 

# Creates spatial snapshots of residuals from 1987-2020 of the predict_lm model in the 'figures/model_residuals' folder
# has commented out code for variograms
source(file= "visualization_scripts/spatial_residuals_by_model.R") 

## time 