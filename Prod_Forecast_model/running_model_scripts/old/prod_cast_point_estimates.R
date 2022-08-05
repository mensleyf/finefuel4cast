# this script creates point estimates for a forecast of 2021 as well as hindcasts of 1988-2021
# saves results in a .csv file
# output is fairly redundant with .rds files stored in prod_model_outputs, 
# but we will reuse this code structure to get parameter uncertainty

## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,glmnet,mvtnorm, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

model_df_orig<-fread(file="gee_4cast_data/model_csvs/march_all_model_csv.csv",header=T)
model_df <- as.data.frame(model_df_orig)
unique(model_df$year)
# add an interaction term
model_df$z_prOctMar_z_ssm <- model_df$z_pr_Oct2Mar*model_df$z_ssm

model_df_orig2021<-read.csv("gee_4cast_data/model_csvs/march_forecast_2021_csv.csv")
model_df2021<-model_df_orig2021
unique(model_df2021$year)
# add an interaction term
model_df2021$z_prOctMar_z_ssm <- model_df2021$z_pr_Oct2Mar*model_df2021$z_ssm
# fill in missing ndvi data with zeros (assume mean ndvi for each site)
model_df2021$z_ndvi_Feb[is.na(model_df2021$z_ndvi_Feb)] <- 0

mod1_data2021<-subset(model_df2021, select=c(z_agb,prev_z_agb,year, z_pr_Oct2Mar,z_tmmx_Oct2Mar,z_vpd_Oct2Mar,z_ndvi_Feb,pr_frac,
                                             prev_pfg_cover,z_ssm,z_bulk_dens,z_prOctMar_z_ssm))

lm_coeffs<-read.csv("prod_model_outputs/lm_coeffs.csv")

nLocs<-nrow(mod1_data2021)

# # ----------------------- Forecast point estimates (2021)  -------------------------
#use lm_coeffs for point estimates of coefficients
nIters<-1
lm_coeffs[,2]<-as.numeric(lm_coeffs[,2])

march_forecast_point_est2021<-lm_coeffs[lm_coeffs[,1]=="int",2]+
  lm_coeffs[lm_coeffs[,1]=="prev_z_agb",2]*mod1_data2021$prev_z_agb +
  lm_coeffs[lm_coeffs[,1]=="z_pr_Oct2Mar",2]*mod1_data2021$z_pr_Oct2Mar   +
  lm_coeffs[lm_coeffs[,1]=="z_tmmx_Oct2Mar",2]*mod1_data2021$z_tmmx_Oct2Mar     +
  lm_coeffs[lm_coeffs[,1]=="z_vpd_Oct2Mar",2]*mod1_data2021$z_vpd_Oct2Mar    +
  lm_coeffs[lm_coeffs[,1]=="z_ndvi_Feb",2]*mod1_data2021$z_ndvi_Feb     +
  lm_coeffs[lm_coeffs[,1]=="pr_frac",2]*mod1_data2021$pr_frac +
  lm_coeffs[lm_coeffs[,1]=="prev_pfg_cover",2]*mod1_data2021$prev_pfg_cover  +
  lm_coeffs[lm_coeffs[,1]=="z_ssm",2]*mod1_data2021$z_ssm +
  lm_coeffs[lm_coeffs[,1]=="z_bulk_dens",2]*mod1_data2021$z_bulk_dens +
  lm_coeffs[lm_coeffs[,1]=="z_prOctMar_z_ssm",2]*mod1_data2021$z_prOctMar_z_ssm   

march_forecast_point_est2021<-data.frame(
  location=model_df_orig2021$location,
  forecast2021=march_forecast_point_est2021)


# # ----------------------- Hindcasts point estimates (1988-2020) -------------------------

length(seq(1988,2020,1))
nLocs<-nrow(model_df)/34
hindcast_point_est = matrix(NA,nrow=nLocs,ncol=33)

count=1
for (cur_yr in 1988:2020){
  
  # separate test and training data
  trainD <- model_df[which(model_df$year!=cur_yr),]
  testD <- model_df[which(model_df$year==cur_yr),]
  
  mod_fit<-lm(z_agb~prev_z_agb+year+ z_pr_Oct2Mar+z_tmmx_Oct2Mar+z_vpd_Oct2Mar+z_ndvi_Feb+pr_frac+
              prev_pfg_cover+z_ssm+z_bulk_dens+z_prOctMar_z_ssm,data=trainD) 
  
  # predictions for left-out year
  hindcast_point_est[,count] <- predict(mod_fit,new=testD)
  count=count+1
}

hindcast_point_est<-as.data.frame(hindcast_point_est)
names(hindcast_point_est)<-paste0(rep("hindcast",33), seq(1988,2020,1))
# write.csv(hindcast_point_est,"prod_model_outputs/forecast2021/hindcast_point_est_1988_2020.csv",row.names = F )

all_casts<-cbind(march_forecast_point_est2021$location, hindcast_point_est, march_forecast_point_est2021$forecast2021)
fwrite(all_casts,"prod_model_outputs/forecast2021/prod_cast_1988_2021.csv",row.names = F )

