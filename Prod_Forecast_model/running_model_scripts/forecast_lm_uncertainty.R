# this script forecasts 2021 productivity and 
# calculates parameter and process error uncertainty from the forecast_lm model 
# using monte carlo sampling

## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table,dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,glmnet,mvtnorm, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

save_loc<-"/figures/march_vis/z_forecast/"
save_name<-"z_march_forecast_lm"

# model object (with training data)
forecast_mod<-readRDS( "prod_model_outputs/march_forecast.rds")

# 2021 data for prediction
model_df_orig2021<-read.csv("gee_4cast_data/model_csvs/march_forecast_2021_csv.csv")
model_df2021<-model_df_orig2021
unique(model_df2021$year)
# add an interaction term
model_df2021$z_prOctMar_z_ssm <- model_df2021$z_pr_Oct2Mar*model_df2021$z_ssm
# fill in missing ndvi data with zeros (assume mean ndvi for each site)
model_df2021$z_ndvi_Feb[is.na(model_df2021$z_ndvi_Feb)] <- 0

nLocs <- nrow(model_df2021)

# # ----------------------- no uncertainty  -------------------------

# predict fitted values from saved model
march_forecast_point_est <- predict(forecast_mod$mod_fit,new=model_df2021)

write.csv(march_forecast_point_est,"prod_model_outputs/forecast2021/march_forecast_point_est_2021.csv",row.names = F )


# # ----------------------- param uncertainty  -------------------------

#use vcov() to get covariance matrix of coeff and rmvnorm to sample

march_forecast_param<-matrix(NA,nLocs,ncol=nIters)
mu = forecast_mod$mod_fit$coefficients
sigma = vcov(forecast_mod$mod_fit)
coef_samples = rmvnorm(nIters,mean=mu,sigma=sigma)  # sample values of coefficients

# build model matrix with 2021 data
modmat <-cbind(rep(1,nLocs),model_df2021[,c("prev_z_agb", "z_pr_Oct2Mar","z_tmmx_Oct2Mar","z_vpd_Oct2Mar","z_ndvi_Feb","pr_frac",
                                       "prev_pfg_cover","z_ssm","z_bulk_dens","z_prOctMar_z_ssm")])
modmat<-as.matrix(modmat)

# loop over parameter draws 
for (i in 1:nIters){
  march_forecast_param[,i]<-modmat%*%coef_samples[i,]
}

# check: compare to point estimates
# plot(rowMeans(march_forecast_param),march_forecast_point_est)

# monte carlo sampling param uncertainty
fwrite(march_forecast_param, "prod_model_outputs/forecast2021/march_forecast_param_2021.csv",row.names = F )


# # ----------------------- proc uncertainty  -------------------------
# add proc err with rnorm() on to point estimates
proc_err<-summary(forecast_mod$mod_fit)$sigma
march_forecast_proc <- matrix(march_forecast_point_est,nLocs,ncol=nIters)
march_forecast_proc <- march_forecast_proc + matrix(rnorm(nLocs*nIters,0,proc_err),nLocs,nIters)

# check: compare to point estimates
# plot(rowMeans(march_forecast_proc),march_forecast_point_est)

fwrite(march_forecast_proc,  "prod_model_outputs/forecast2021/march_forecast_proc_2021.csv",row.names = F )



