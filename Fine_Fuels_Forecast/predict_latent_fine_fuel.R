#This script makes hindcasts of latent fine fuel from 1988-2020 and forecasts for 2021 with a 'spin-up' on real prod data of 10 years

#---------- loading pcks, data, model-------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,data.table,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer,rgdal, raster, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

##--------- extracting information from Prod_Forecast model ------------------

#fitted model
forecast_rds<-readRDS("Prod_Forecast_model/prod_model_outputs/march_forecast.rds")
forecast_mod<-forecast_rds$mod_fit
proc_err<-summary(forecast_mod)$sigma

#actual prod data from 1987-2020
prod_data<-as.data.frame(forecast_rds$agb_m)
names(prod_data)<-seq(1987,2020)

#forecasted data 1987-2020
prod_preds <- as.data.frame(forecast_rds$prod_mean)
names(prod_preds)<-as.character(seq(1987,2020,1))
prod_preds <- prod_preds[,-1] #drop 1987
# add 2021 forecast
# START HERE
tmp<-read.csv("Prod_Forecast_model/prod_model_outputs/forecast2021/march_forecast_point_est_2021.csv",header=T)
prod_preds <- cbind(prod_preds,tmp)
names(prod_preds)[ncol(prod_preds)] <- "2021"

nLocs<-nrow(prod_preds)

# param uncertainty from monte carlo sampling 
march_forecast_param<-data.table::fread(file="Prod_Forecast_model/prod_model_outputs/forecast2021/march_forecast_param_2021.csv",header=T)
march_forecast_param <- as.data.frame(march_forecast_param)

param_err<-mean(apply(march_forecast_param,1,sd), na.rm=T)


# -------------- extracting information from Fuels Model ----------------

# want point estimates and distributions for each parameter

fit1<-readRDS("Fuels_model/model_outputs/fuels_model.rds")

alphaout<-rstan::extract(fit1, 'alpha', permuted=F);alphaout<-apply(alphaout, 3,c)
alpha_est<-mean(matrix(apply(alphaout,2,mean)))

betaout<-rstan::extract(fit1, 'beta', permuted=F);betaout<-apply(betaout, 3,c)
beta_est<-mean(matrix(apply(betaout,2,mean)))

sig_oout<-rstan::extract(fit1, 'sig_o', permuted=F);sig_oout<-apply(sig_oout, 3,c)
sig_o_est<-mean(matrix(apply(sig_oout,2,mean)))

sig_pout<-rstan::extract(fit1, 'sig_p', permuted=F);sig_pout<-apply(sig_pout, 3,c)
sig_p_est<-mean(matrix(apply(sig_pout,2,mean)))

##------------------- Forecasting latent fuel of year 'y' by spinning up previous 10 years  ------------------------
# 'spin-up' latent fuel from y-10 years using Fuels Model param distributions and actual productivity data
# in year 'y' spin up using forecasted productivity data + forecast uncertainty


prod_start_yr<-1 #eg. 1987
latent_forecast_yr<-prod_start_yr+10 # eg. 1998

# matrix to hold spin-ups 
Fspin<-matrix(NA,nrow=nrow(prod_data), ncol=11)  #eg spin-up = 1988 thru 1998; forecast = 1999
Fspin_check<-matrix(NA,nrow=nrow(prod_data), ncol=11) #doing without process error to check

#set first val to prod 1987 
Fspin[,1]<-prod_data[,prod_start_yr]

#random iterations (out of 8000) to spin over
nIters<-100 #(hitting memory limits) 
random_iters<-sample(1:length(betaout),nIters)
Fspin_iters<-matrix(NA,nrow=nLocs*24,ncol=nIters) #holds all iterations as columns, each loc and year (1998-2021) as rows
Fspin_iters_check<-matrix(NA,nrow=nLocs*24,ncol=nIters) #holds all iterations as columns, each loc and year (1998-2021) as rows

length(seq(1998,2021,1))

prod_data_subset<-prod_data[,prod_start_yr:(prod_start_yr+10)] #1987:1997
prod_preds_subset<-prod_preds[,latent_forecast_yr] #1998

#check
(prod_data_subset[1:5,]) #1987-1997
(prod_preds[1:5,]) #1998
(prod_preds_subset[1:5]) #1998

# loop over sampling iteractions
i<-1
count=1

for (i in 1:nIters){
  count=1
  for (prod_start_yr in 1:24){
    latent_forecast_yr<-prod_start_yr+10 #starting at 1998
    prod_data_subset<-prod_data[,prod_start_yr:(prod_start_yr+10)] #subset prod_data to 10 years
    prod_preds_subset<-prod_preds[,latent_forecast_yr] #subset forecasted data to 1 year
    
    for ( cur_yr in 2:11){  #loop through real data first 10 yrs
      if(cur_yr!=11){ 
        iter<-random_iters[i]
        prev_yr<-cur_yr-1
        yr_name<-1986+cur_yr
        Fspin[,cur_yr]<-alphaout[iter]*Fspin[,prev_yr]+betaout[iter]*prod_data_subset[,cur_yr] + rnorm(nLocs,0,sig_pout[iter])
        Fspin_check[,cur_yr]<-alphaout[iter]*Fspin[,prev_yr]+betaout[iter]*prod_data_subset[,cur_yr] #try without error
        names(Fspin)<-seq(1987,1997)
        names(Fspin)[prev_yr]<-yr_name
        
      }
      
      if(cur_yr==11){ #last year, spin up on forecasted data, add to data frame
        (iter<-random_iters[i])
        prev_yr<-cur_yr-1
        yr_name<-1986+cur_yr
        Fspin_check[,cur_yr]<-alphaout[iter]*Fspin[,prev_yr]+betaout[iter]*prod_preds_subset #check without error
        Fspin[,cur_yr]<-alphaout[iter]*Fspin[,prev_yr]+betaout[iter]*(prod_preds_subset+rnorm(nLocs, 0, proc_err)) + rnorm(nLocs,0,sig_pout[iter])
        Fspin_iters[(nLocs*(count-1)+1):(count*nLocs),i]<-Fspin[,11]
        Fspin_iters_check[(nLocs*(count-1)+1):(count*nLocs),i]<-Fspin_check[,11]
        
      }
    }   
    count=count+1 
  }
}


#--------------------------checking-------------------

# prod_preds_subset[1:10]
# prod_preds[1:10,34]
# 
# # check; rows = locations and years, columns = iterations
# # View(Fspin_iters[(nLocs-30):(nLocs+30),])
# # View(Fspin_iters_check[(nLocs-30):(nLocs+30),])
# #nrow(Fspin_iters)/nLocs
# 
# # does latent fuel load look like productivity of that year?
# (Fspin_iters[555,]) #first year= 1989
# (Fspin_iters_check[555,])
# prod_data[555,]
# 
# #year 1 of latent = 1998
# plot(apply(Fspin_iters[1:100,], 1,mean), apply(Fspin_iters_check[1:100,],1,mean))
# plot(apply(Fspin_iters[1:500,], 1,mean), prod_data[1:500,12], pch=19) #should NOT be similar
# plot(apply(Fspin_iters[1:500,], 1,mean), prod_data[1:500,11], pch=19) #should be similar
# 
# #1998...are you off by a year? ir matches better next year?
# cor(apply(Fspin_iters_check[1:nLocs,], 1,mean), prod_data[,11]) #forecast w/ 1987 prod
# cor(apply(Fspin_iters_check[1:nLocs,], 1,mean), prod_data[,12]) #forecast w/ 1988 year
# cor(apply(Fspin_iters_check[(nLocs+1):(nLocs*2),], 1,mean), prod_data[,12]) #??
# 
# #2020
# cor(apply(Fspin_iters_check[(nLocs*23+1):(nLocs*24),], 1,mean), prod_data[,34]) #??
# plot(apply(Fspin_iters_check[(nLocs*23+1):(nLocs*24),], 1,mean), prod_data[,33]) #??
# 
# 
# 
# # Fspin_iters_check[555,], pch=19)
# plot(Fspin_iters[5,], Fspin_iters_check[5,], pch=25)
# 
# prod_data[555,]

# View(Fspin_iters[(nLocs-30):(nLocs+30),])

#write.csv(Fspin_iters, "Fine_Fuels_Forecast/output_data/latent_forecast_distrib.csv", row.names=F)
data.table::fwrite(Fspin_iters,file="Fine_Fuels_Forecast/output_data/latent_forecast_distrib.csv",sep=",",row.names=F)

##---------------------------------- changing into smaller, more intuitive format --------------------

#change very large Fspin_iters into 80% confidence interval in nLocs*nYears matrices
Fspin_iters_90<-matrix(NA,nrow=nLocs,ncol=24)
Fspin_iters_10<-matrix(NA,nrow=nLocs,ncol=24)
Fspin_iters_est<-matrix(NA,nrow=nLocs,ncol=24)

# this chunk is slow (many minutes)
for ( y in 1:24){
  cur_Fspin<-as.data.frame(Fspin_iters[((y-1)*nLocs+1):(y*nLocs),]) #just one year of locations, all iterations
  Fspin_iters_90[,y]<-apply(cur_Fspin,1,quantile, probs=.9,na.rm=T)
  Fspin_iters_10[,y]<-apply(cur_Fspin,1,quantile, probs=.1,na.rm=T)
  Fspin_iters_est[,y]<-apply(cur_Fspin,1,mean,na.rm=T)
}

dim(cur_Fspin) 
dim(Fspin_iters_90)

# backtransform: get location means and standard deviations
tmp <- fread(file="Prod_Forecast_model/gee_4cast_data/model_csvs/march_all_model_csv.csv",header=T)
tmp <- subset(tmp,year==1987)
long_term_sd<-tmp$sd_agb
long_term_mean<-tmp$mean_agb
rm(tmp)

Fspin_raw<-matrix(NA,nrow=nLocs,ncol=24); Fspin_90_raw<-matrix(NA,nrow=nLocs,ncol=24); Fspin_10_raw<-matrix(NA,nrow=nLocs,ncol=24)
for ( i in 1:ncol(Fspin_iters_est)){
  Fspin_raw[,i]<-(Fspin_iters_est[,i] * long_term_sd)+long_term_mean
  Fspin_90_raw[,i]<-(Fspin_iters_90[,i] * long_term_sd)+long_term_mean
  Fspin_10_raw[,i]<-(Fspin_iters_10[,i] * long_term_sd)+long_term_mean
}

# change to % above/below normal
Fspin_perc<-matrix(NA,nrow=nLocs,ncol=24); Fspin_90_perc<-matrix(NA,nrow=nLocs,ncol=24); Fspin_10_perc<-matrix(NA,nrow=nLocs,ncol=24)
for ( i in 1:ncol(Fspin_iters_est)){
  Fspin_perc[,i]<-(Fspin_raw[,i]-long_term_mean)/long_term_mean*100
  Fspin_90_perc[,i]<-(Fspin_90_raw[,i]-long_term_mean)/long_term_mean*100
  Fspin_10_perc[,i]<-(Fspin_10_raw[,i]-long_term_mean)/long_term_mean*100
}

range(Fspin_perc, na.rm=T)

colMeans(Fspin_10_perc)
colMeans(Fspin_90_perc)

# par(mfrow=c(1,1))
# nYears<-24
# plot(x=seq(1998,2021), y=colMeans(Fspin_10_perc), col="red", lwd=2, lty=2, type="l", ylim=c(-50,50),
#      main = "Percent above long-term normal", xlab="year", ylab="% above long term average", cex.main=2)
# lines(x=seq(1998,2021), y=colMeans(Fspin_90_perc), col="red", lty=2, lwd=2)
# lines(x=seq(1998,2021), y=colMeans(Fspin_perc), col="red", lwd=3)
# lines(x=seq(1998,2021), y=rep(0,24), lty=2)
# legend("topright", legend= c("10% CI", "90% CI"), col="red", lty=2, lwd=2, cex=2)
# 

# join to BLM districts
district_coords<-read.csv("Fine_Fuels_Forecast/output_data/district_coords.csv" )
district_coords <- district_coords[,c("location","long","lat","PARENT_N")]
district_coords <- merge(district_coords,forecast_rds$coord_df,all.y=T) 
# a few NAs in PARENT_N, not every pixel mapped to a location, it's ok


Fspin_iters_10_sp<-as.data.frame(cbind(Fspin_10_perc, district_coords))
names(Fspin_iters_10_sp)<-c(paste0(rep("pred",24), seq(1998,2021)), names(district_coords))
Fspin_iters_90_sp<-as.data.frame(cbind(Fspin_90_perc, district_coords))
names(Fspin_iters_90_sp)<-c(paste0(rep("pred",24), seq(1998,2021)), names(district_coords))
Fspin_iters_est_sp<-as.data.frame(cbind(Fspin_perc, district_coords))
names(Fspin_iters_est_sp)<-c(paste0(rep("pred",24), seq(1998,2021)), names(district_coords))

#save output
latent_forecast_perc<-list(Fspin_iters_10_sp, Fspin_iters_90_sp,Fspin_iters_est_sp)
names(latent_forecast_perc)<-c("CI10", "CI90", "mean")
saveRDS(latent_forecast_perc, "Fine_Fuels_Forecast/output_data/latent_fuel_perc_sp.rds")

# clean up everything (CAREFUL!)
rm(list=ls())
gc()
