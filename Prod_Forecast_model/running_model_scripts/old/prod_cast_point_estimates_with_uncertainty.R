# this script creates point estimates for a forecast of 2021 as well as hindcasts of 1988-2021
# saves results in a .csv file

## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,glmnet,mvtnorm, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

model_df_orig<-read.csv("gee_4cast_data/model_csvs/march_all_model_csv.csv")
model_df<-model_df_orig
unique(model_df$yr)

model_df_orig2021<-read.csv("gee_4cast_data/model_csvs/march_forecast_2021_csv.csv")
model_df2021<-model_df_orig2021
unique(model_df_orig2021$yr)

mod1_data2021<-subset(model_df2021, select=c(z_agb,prev_z_agb,yr, z_pr,z_tmmx,z_vpd,z_ndvi,pr_frac,
                                             prev_pfg_frac,z_ssm,z_bulk_dens,z_pr_z_ssm))

(lm_coeffs<-read.csv( "prod_model_outputs/march_forecast_lm_coeffs.csv"))

forecast_lm<-readRDS("prod_model_outputs/march_forecast_lm.rds")$mod_fit
(proc_err<-summary(forecast_lm)$sigma)

nLocs<-nrow(mod1_data2021)

# # ----------------------- Forecast point estimates (2021)  -------------------------
#use lm_coeffs for point estimates of coefficients
nIters<-100
lm_coeffs[,2]<-as.numeric(lm_coeffs[,2])
march_forecast_point_est2021<-matrix(NA,nrow=nLocs,ncol=nIters)
march_temp<-matrix(NA,nrow=nLocs,ncol=1)

for( n in 1:nIters){
  
march_temp<-lm_coeffs[lm_coeffs[,1]=="int",2]+
  lm_coeffs[lm_coeffs[,1]=="prev_z_agb",2]*mod1_data2021$prev_z_agb +
  lm_coeffs[lm_coeffs[,1]=="z_pr",2]*mod1_data2021$z_pr +
  lm_coeffs[lm_coeffs[,1]=="z_tmmx",2]*mod1_data2021$z_tmmx +
  lm_coeffs[lm_coeffs[,1]=="z_vpd",2]*mod1_data2021$z_vpd +
  lm_coeffs[lm_coeffs[,1]=="z_ndvi",2]*mod1_data2021$z_ndvi +
  lm_coeffs[lm_coeffs[,1]=="pr_frac",2]*mod1_data2021$pr_frac +
  lm_coeffs[lm_coeffs[,1]=="prev_pfg_frac",2]*mod1_data2021$prev_pfg_frac +
  lm_coeffs[lm_coeffs[,1]=="z_ssm",2]*mod1_data2021$z_ssm +
  lm_coeffs[lm_coeffs[,1]=="z_bulk_dens",2]*mod1_data2021$z_bulk_dens +
  lm_coeffs[lm_coeffs[,1]=="z_pr_z_ssm",2]*mod1_data2021$z_pr_z_ssm +
  rnorm(nLocs,0,proc_err)

  march_forecast_point_est2021[,n] <- as.vector(march_temp)
}

head(march_forecast_point_est2021)
# # ----------------------- Hindcasts point estimates (1988-2020) -------------------------
nIters<-100

length(seq(1988,2020,1))
(nLocs<-nrow(model_df)/34)
hindcast_point_est = matrix(NA,nrow=nLocs,ncol=33)
hindcast_big = matrix(NA, nrow=nLocs*33,ncol=nIters)
count=1

for ( n in 1:nIters){
for ( cur_yr in 1988:2020){
  
  prev_yr=cur_yr-1
  cur_yr_df<-subset(model_df, model_df$yr==prev_yr)
  
  #use lm_coeffs for point estimates of coefficients
  lm_coeffs[,2]<-as.numeric(lm_coeffs[,2])
  march_forecast_point_est<-matrix(NA,nrow=1,ncol=nLocs)
  
  march_forecast_point_est<-lm_coeffs[lm_coeffs[,1]=="int",2]+
    lm_coeffs[lm_coeffs[,1]=="prev_z_agb",2]*cur_yr_df$prev_z_agb +
    lm_coeffs[lm_coeffs[,1]=="z_pr",2]*cur_yr_df$z_pr +
    lm_coeffs[lm_coeffs[,1]=="z_tmmx",2]*cur_yr_df$z_tmmx +
    lm_coeffs[lm_coeffs[,1]=="z_vpd",2]*cur_yr_df$z_vpd +
    lm_coeffs[lm_coeffs[,1]=="z_ndvi",2]*cur_yr_df$z_ndvi +
    lm_coeffs[lm_coeffs[,1]=="pr_frac",2]*cur_yr_df$pr_frac +
    lm_coeffs[lm_coeffs[,1]=="prev_pfg_frac",2]*cur_yr_df$prev_pfg_frac +
    lm_coeffs[lm_coeffs[,1]=="z_ssm",2]*cur_yr_df$z_ssm +
    lm_coeffs[lm_coeffs[,1]=="z_bulk_dens",2]*cur_yr_df$z_bulk_dens +
    lm_coeffs[lm_coeffs[,1]=="z_pr_z_ssm",2]*cur_yr_df$z_pr_z_ssm 
  
  hindcast_point_est[,count]=march_forecast_point_est
  count=count+1
}
hindcast_big[,n]<-as.vector(hindcast_point_est)+ rnorm(nLocs*33, 0 , proc_err)
count=1
}

# check for 1988
# march_forecast_point_est[1:10]
# mod$fitted.values[1:10]

hindcast_point_est<-as.data.frame(hindcast_point_est)
names(hindcast_point_est)<-paste0(rep("hindcast",33), seq(1988,2020,1))

hindcast_big<-as.data.frame(hindcast_big)
# write.csv(hindcast_point_est,"prod_model_outputs/forecast2021/hindcast_point_est_1988_2020.csv",row.names = F )



all_casts_big<-rbind(hindcast_big, march_forecast_point_est2021)
write.csv(all_cast_big,"prod_model_outputs/forecast2021/prod_cast_1988_2021_uncert.csv",row.names = F )


#check...residuals and fitted values should match residuals form model rds files

# #format actual data 1987-2020
# prod_data_check<-matrix(NA, nrow=nLocs, ncol=34)
# 
# count<-1
# #not including 1987
# for ( i in 1:34){
#   prod_data_check[1:nLocs, count]<-model_df$z_agb[(nLocs*(i-1)+1):(i*nLocs)]
#   count=count+1
# }
# model_df[48273,1:10]
# 
# model_df[48274,1:10]
# 
# subset(model_df, model_df$yr==1988)[1,1:10]
# 
# prod_data_check<-as.data.frame(prod_data_check)
# names(prod_data_check)<-paste0(rep("prod",33), seq(1988,2020,1))
# rand_subset<-sample(1:nLocs, 1000)
# 
# 
# resids_df_check<-matrix(NA, nrow=nLocs, ncol=33)
# 
# predictions_lm<-readRDS("prod_model_outputs/march_forecast_lm.rds")
# resids<-predictions_lm$resids
# mod<-predictions_lm$mod_fit
# resids2<-mod$residuals
# 
# coefficients(mod)
# 
# for (i in 1:33){
#   # plot(prod_data_check[rand_subset,i],all_casts[rand_subset,i] )
#   # print(paste0((1987+i), " : correlation = " , cor(prod_data_check[,i], all_casts[,i])))
#   print(paste0((1987+i), " :hindcast mape = " , mean(abs(prod_data_check[,i]- all_casts[,i]))))
#   print(paste0((1987+i), " :from model mape = " , mean(abs(resids[,(i)]))))
#   resids_df_check[,i]<- all_casts[,i] - prod_data_check[,i]
#   
# }
# 
# 
# plot(resids[1:50,1],as.vector(resids2[1:50]))
# plot(resids[1:50,1],resids_df_check[1:50,1])
# plot(all_casts[,13], prod_data_check[,13])
# 
# 
# plot(y=all_casts[777,1:33], x=seq(1988,2020), col="red", type="l")
# lines(y=prod_data_check[777,], x=seq(1988,2020),col="black")
