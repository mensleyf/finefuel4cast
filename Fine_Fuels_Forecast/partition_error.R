#---------- loading pcks, data, model-------------

if(!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,data.table,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer,rgdal, raster, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

##--------- extracting information from Prod_Forecast model ------------------

#import fitted model and productivity data, preditions
forecast_rds<-readRDS( "Prod_Forecast_model/prod_model_outputs/march_forecast.rds")
proc_err<-summary(forecast_rds$mod_fit)$sigma

# select random locations
num_locs<-400
keep<- sample(1:nrow(forecast_rds$coord_df),num_locs)

#actual prod data from 1987-2020
prod_data<-as.data.frame(forecast_rds$agb_m)
names(prod_data)<-seq(1987,2020)
prod_data <- prod_data[keep,]

#forecasted/hindcasted data 1988-2021
prod_preds<-as.data.frame(forecast_rds$prod_mean)
names(prod_preds)<-seq(1987,2020)
prod_preds <- prod_preds[keep,]

nLocs<-nrow(prod_preds)

# param uncertainty from monte carlo sampling 
march_forecast_param <- fread(file="Prod_Forecast_model/prod_model_outputs/forecast2021/march_forecast_param_2021.csv",header=T)
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





# #forecasted prod data
# agb_forecast_point_estimate<-agb_forecast_point_estimate[coords$keeps,1]
# 
# #forecast model
# forecast_mod<-forecast_rds$mod_fit
# (proc_err<-summary(forecast_mod)$sigma)
# 
# # monte carlo sampling param uncertainty
# march_forecast_param<-read.csv(paste0(prod_folder, "/forecasted_data/march_forecast_param_2021.csv"))
# (param_err<-mean(apply(march_forecast_param,2,sd), na.rm=T)) #point estimate of param uncertainty


##------------------- spin up ------------------------

#matrix to hold current year spin-up

#all/none
Fspin_all<-matrix(NA,nrow=num_locs,ncol=11)
Fspin_none<-matrix(NA,nrow=num_locs,ncol=11)

#1way
Fspin_prod_mod_proc<-matrix(NA,nrow=num_locs,ncol=11)
Fspin_prod_mod_par<-matrix(NA,nrow=num_locs,ncol=11)
Fspin_fuels_mod_proc<-matrix(NA,nrow=num_locs,ncol=11)
Fspin_fuels_mod_par<-matrix(NA,nrow=num_locs,ncol=11)

#2way
Fspin_prod_mod<-matrix(NA,nrow=num_locs,ncol=11)
Fspin_fuels_mod<-matrix(NA,nrow=num_locs,ncol=11)
Fspin_prod_mod_proc_fuels_mod_proc<-matrix(NA,nrow=num_locs,ncol=11)
Fspin_prod_mod_proc_fuels_mod_par<-matrix(NA,nrow=num_locs,ncol=11)
Fspin_prod_mod_par_fuels_mod_par<-matrix(NA,nrow=num_locs,ncol=11)
Fspin_prod_mod_par_fuels_mod_proc<-matrix(NA,nrow=num_locs,ncol=11)


#set first val to prod 2010
Fspin_all[,1]<-prod_data$'2010'
Fspin_none[,1]<-prod_data$'2010'

#1way
Fspin_prod_mod_proc[,1]<-prod_data$'2010'
Fspin_prod_mod_par[,1]<-prod_data$'2010'
Fspin_fuels_mod_proc[,1]<-prod_data$'2010'
Fspin_fuels_mod_par[,1]<-prod_data$'2010'

#2way
Fspin_prod_mod[,1]<-prod_data$'2010'
Fspin_fuels_mod[,1]<-prod_data$'2010'
Fspin_prod_mod_proc_fuels_mod_proc[,1]<-prod_data$'2010'
Fspin_prod_mod_proc_fuels_mod_par[,1]<-prod_data$'2010'
Fspin_prod_mod_par_fuels_mod_par[,1]<-prod_data$'2010'
Fspin_prod_mod_par_fuels_mod_proc[,1]<-prod_data$'2010'

#random iterations (out of 8000) to spin over
nIters<-500 #(hitting memory limits) (could start in 2010?)
random_iters<-sample(1:length(betaout),nIters)

#all/none
Fspin_iters_all<-matrix(NA,nrow=num_locs*11,ncol=nIters)
Fspin_iters_none<-matrix(NA,nrow=num_locs*11,ncol=nIters)

#1way
Fspin_iters_prod_mod_proc<-matrix(NA,nrow=num_locs*11,ncol=nIters)
Fspin_iters_prod_mod_par<-matrix(NA,nrow=num_locs*11,ncol=nIters)
Fspin_iters_fuels_mod_proc<-matrix(NA,nrow=num_locs*11,ncol=nIters)
Fspin_iters_fuels_mod_par<-matrix(NA,nrow=num_locs*11,ncol=nIters)

#2way
Fspin_iters_prod_mod<-matrix(NA,nrow=num_locs*11,ncol=nIters)
Fspin_iters_fuels_mod<-matrix(NA,nrow=num_locs*11,ncol=nIters)
Fspin_iters_prod_mod_proc_fuels_mod_proc<-matrix(NA,nrow=num_locs*11,ncol=nIters)
Fspin_iters_prod_mod_proc_fuels_mod_par<-matrix(NA,nrow=num_locs*11,ncol=nIters)
Fspin_iters_prod_mod_par_fuels_mod_par<-matrix(NA,nrow=num_locs*11,ncol=nIters)
Fspin_iters_prod_mod_par_fuels_mod_proc<-matrix(NA,nrow=num_locs*11,ncol=nIters)

# loop over random locations and years
for (i in 1:nIters){
  for ( cur_yr in 2:11){
    if(cur_yr!=11){ #thru 2020 on real data
      iter<-random_iters[i]
      prev_yr<-cur_yr-1
      yr_name<-2010+cur_yr
      
      #all and none 
      Fspin_all[,cur_yr]<-alphaout[iter]*Fspin_all[,prev_yr]+betaout[iter]*prod_data[,cur_yr] + rnorm(num_locs,0,sig_pout[iter])
      Fspin_none[,cur_yr]<-alpha_est*Fspin_none[,prev_yr]+beta_est*prod_data[,cur_yr]
      
      #1way
      Fspin_prod_mod_proc[,cur_yr]<-alpha_est*Fspin_prod_mod_proc[,prev_yr]+beta_est*prod_data[,cur_yr]
      Fspin_prod_mod_par[,cur_yr]<-alpha_est*Fspin_prod_mod_par[,prev_yr]+beta_est*prod_data[,cur_yr]
      Fspin_fuels_mod_proc[,cur_yr]<-alpha_est*Fspin_fuels_mod_proc[,prev_yr]+beta_est*prod_data[,cur_yr] + rnorm(num_locs,0,sig_pout[iter])
      Fspin_fuels_mod_par[,cur_yr]<-alphaout[iter]*Fspin_fuels_mod_par[,prev_yr]+betaout[iter]*prod_data[,cur_yr]
      
      #2way
      Fspin_prod_mod[,cur_yr]<-alpha_est*Fspin_prod_mod[,prev_yr]+beta_est*prod_data[,cur_yr]
      Fspin_fuels_mod[,cur_yr]<-alphaout[iter]*Fspin_fuels_mod[,prev_yr]+betaout[iter]*prod_data[,cur_yr] + rnorm(num_locs,0,sig_pout[iter])
      Fspin_prod_mod_proc_fuels_mod_proc[,cur_yr]<-alpha_est*Fspin_prod_mod_proc_fuels_mod_proc[,prev_yr]+beta_est*prod_data[,cur_yr] + rnorm(num_locs,0,sig_pout[iter])
      Fspin_prod_mod_proc_fuels_mod_par[,cur_yr]<-alphaout[iter]*Fspin_prod_mod_proc_fuels_mod_par[,prev_yr]+betaout[iter]*prod_data[,cur_yr]
      Fspin_prod_mod_par_fuels_mod_par[,cur_yr]<-alphaout[iter]*Fspin_prod_mod_par_fuels_mod_par[,prev_yr]+betaout[iter]*prod_data[,cur_yr] 
      Fspin_prod_mod_par_fuels_mod_proc[,cur_yr]<-alpha_est*Fspin_prod_mod_par_fuels_mod_proc[,prev_yr]+beta_est*prod_data[,cur_yr] + rnorm(num_locs,0,sig_pout[iter])
      
    }
    if(cur_yr==11){ #last year on forecasted data
      (iter<-random_iters[i])
      prev_yr<-cur_yr-1
      yr_name<-1986+cur_yr
      
      #all/none
      Fspin_all[,cur_yr]<-alphaout[iter]*Fspin_all[,prev_yr]+betaout[iter]*(prod_preds[,34]+ rnorm(num_locs,0,param_err)+rnorm(num_locs, 0, proc_err)) + rnorm(num_locs,0,sig_pout[iter])
      Fspin_none[,cur_yr]<-alpha_est*Fspin_none[,prev_yr]+beta_est*prod_preds[,34]
      
      #1way
      Fspin_prod_mod_proc[,cur_yr]<-alpha_est*Fspin_prod_mod_proc[,prev_yr]+beta_est*(prod_preds[,34]+ rnorm(num_locs,0,proc_err))
      Fspin_prod_mod_par[,cur_yr]<-alpha_est*Fspin_prod_mod_par[,prev_yr]+beta_est*(prod_preds[,34]+ rnorm(num_locs,0,param_err))
      Fspin_fuels_mod_proc[,cur_yr]<-alpha_est*Fspin_fuels_mod_proc[,prev_yr]+beta_est*prod_preds[,34] + rnorm(num_locs,0,sig_pout[iter])
      Fspin_fuels_mod_par[,cur_yr]<-alphaout[iter]*Fspin_fuels_mod_par[,prev_yr]+betaout[iter]*prod_preds[,34]
      
      #2way
      Fspin_prod_mod[,cur_yr]<-alpha_est*Fspin_prod_mod[,prev_yr]+beta_est*(prod_preds[,34]+ rnorm(num_locs,0,proc_err) +rnorm(num_locs,0,param_err))
      Fspin_fuels_mod[,cur_yr]<-alphaout[iter]*Fspin_fuels_mod[,prev_yr]+betaout[iter]*prod_preds[,34]  + rnorm(num_locs,0,sig_pout[iter])
      Fspin_prod_mod_proc_fuels_mod_proc[,cur_yr]<-alpha_est*Fspin_prod_mod_proc_fuels_mod_proc[,prev_yr]+beta_est*(prod_preds[,34]+ rnorm(num_locs,0,proc_err)) + rnorm(num_locs,0,sig_pout[iter])
      Fspin_prod_mod_proc_fuels_mod_par[,cur_yr]<-alphaout[iter]*Fspin_prod_mod_proc_fuels_mod_par[,prev_yr]+betaout[iter]*(prod_preds[,34]+ rnorm(num_locs,0,proc_err))
      Fspin_prod_mod_par_fuels_mod_par[,cur_yr]<-alphaout[iter]*Fspin_prod_mod_par_fuels_mod_par[,prev_yr]+betaout[iter]*(prod_preds[,34] +rnorm(num_locs,0,param_err))
      Fspin_prod_mod_par_fuels_mod_proc[,cur_yr]<-alpha_est*Fspin_prod_mod_par_fuels_mod_proc[,prev_yr]+beta_est*(prod_preds[,34] +rnorm(num_locs,0,param_err)) + rnorm(num_locs,0,sig_pout[iter])
     
      names(Fspin_all)[prev_yr]<-yr_name;names(Fspin_none)[prev_yr]<-yr_name;
      names(Fspin_prod_mod_proc)[prev_yr]<-yr_name;names(Fspin_prod_mod_par)[prev_yr]<-yr_name;
      names(Fspin_fuels_mod_proc)[prev_yr]<-yr_name;names(Fspin_fuels_mod_par)[prev_yr]<-yr_name;
      names(Fspin_fuels_mod)[prev_yr]<-yr_name;names(Fspin_prod_mod)[prev_yr]<-yr_name;
      names(Fspin_prod_mod_proc_fuels_mod_proc)[prev_yr]<-yr_name;names(Fspin_prod_mod_proc_fuels_mod_par)[prev_yr]<-yr_name
      names(Fspin_prod_mod_par_fuels_mod_par)[prev_yr]<-yr_name;names(Fspin_prod_mod_par_fuels_mod_proc)[prev_yr]<-yr_name
      
      }
  }
  
  Fspin_all<-as.data.frame(Fspin_all) ; names(Fspin_all)<-paste0("fuel",seq(2010,2020,1 )) ;Fspin_iters_all[,i]<-unlist(Fspin_all)
  Fspin_none<-as.data.frame(Fspin_none);   names(Fspin_none)<-paste0("fuel",seq(2010,2020,1 )) ;  Fspin_iters_none[,i]<-unlist(Fspin_none)
  
  Fspin_prod_mod_proc<-as.data.frame(Fspin_prod_mod_proc); names(Fspin_prod_mod_proc)<-paste0("fuel",seq(2010,2020,1 )); Fspin_iters_prod_mod_proc[,i]<-unlist(Fspin_prod_mod_proc)
  Fspin_prod_mod_par<-as.data.frame(Fspin_prod_mod_par);names(Fspin_prod_mod_par)<-paste0("fuel",seq(2010,2020,1 ));  Fspin_iters_prod_mod_par[,i]<-unlist(Fspin_prod_mod_par)
  Fspin_fuels_mod_proc<-as.data.frame(Fspin_fuels_mod_proc);   names(Fspin_fuels_mod_proc)<-paste0("fuel",seq(2010,2020,1 ));  Fspin_iters_fuels_mod_proc[,i]<-unlist(Fspin_fuels_mod_proc)
  Fspin_fuels_mod_par<-as.data.frame(Fspin_fuels_mod_par);  names(Fspin_fuels_mod_par)<-paste0("fuel",seq(2010,2020,1 ));  Fspin_iters_fuels_mod_par[,i]<-unlist(Fspin_fuels_mod_par)
  
  Fspin_fuels_mod<-as.data.frame(Fspin_fuels_mod);  names(Fspin_fuels_mod)<-paste0("fuel",seq(2010,2020,1 ));  Fspin_iters_fuels_mod[,i]<-unlist(Fspin_fuels_mod)
  Fspin_prod_mod<-as.data.frame(Fspin_prod_mod);  names(Fspin_prod_mod)<-paste0("fuel",seq(2010,2020,1 ));   Fspin_iters_prod_mod[,i]<-unlist(Fspin_prod_mod)
  Fspin_prod_mod_proc_fuels_mod_proc<-as.data.frame(Fspin_prod_mod_proc_fuels_mod_proc);  names(Fspin_prod_mod_proc_fuels_mod_proc)<-paste0("fuel",seq(2010,2020,1 ));  Fspin_iters_prod_mod_proc_fuels_mod_proc[,i]<-unlist(Fspin_prod_mod_proc_fuels_mod_proc)
  Fspin_prod_mod_proc_fuels_mod_par<-as.data.frame(Fspin_prod_mod_proc_fuels_mod_par);  names(Fspin_prod_mod_proc_fuels_mod_par)<-paste0("fuel",seq(2010,2020,1 ));   Fspin_iters_prod_mod_proc_fuels_mod_par[,i]<-unlist(Fspin_prod_mod_proc_fuels_mod_par)
  Fspin_prod_mod_par_fuels_mod_par<-as.data.frame(Fspin_prod_mod_par_fuels_mod_par);  names(Fspin_prod_mod_par_fuels_mod_par)<-paste0("fuel",seq(2010,2020,1 ));   Fspin_iters_prod_mod_par_fuels_mod_par[,i]<-unlist(Fspin_prod_mod_par_fuels_mod_par)
  Fspin_prod_mod_par_fuels_mod_proc<-as.data.frame(Fspin_prod_mod_par_fuels_mod_proc);  names(Fspin_prod_mod_par_fuels_mod_proc)<-paste0("fuel",seq(2010,2020,1 ));   Fspin_iters_prod_mod_par_fuels_mod_proc[,i]<-unlist(Fspin_prod_mod_par_fuels_mod_proc)
  
}

#saving this
# list_of_forecasts<-list(Fspin_iters_all,Fspin_iters_none,
#                         Fspin_iters_prod_mod_proc,Fspin_iters_prod_mod_par,Fspin_iters_prod_mod,
#                         Fspin_iters_fuels_mod_proc,Fspin_iters_fuels_mod_par,Fspin_iters_fuels_mod
#                         )
# saveRDS(list_of_forecasts, "G:/My Drive/finefuel4cast/Fuels_forecast/spin_up/forecast_list_uncert.RDS")
 
dim(Fspin_iters_all)
nrow(Fspin_iters_all)/11

#just 2021
Fspin_2021_all<-Fspin_iters_all[(num_locs*10):(num_locs*11),]
Fspin_2021_none<-Fspin_iters_none[(num_locs*10):(num_locs*11),]
Fspin_2021_prod_mod_proc<-Fspin_iters_prod_mod_proc[(num_locs*10):(num_locs*11),]
Fspin_2021_prod_mod_par<-Fspin_iters_prod_mod_par[(num_locs*10):(num_locs*11),]
Fspin_2021_fuels_mod_proc<-Fspin_iters_fuels_mod_proc[(num_locs*10):(num_locs*11),]
Fspin_2021_fuels_mod_par<-Fspin_iters_fuels_mod_par[(num_locs*10):(num_locs*11),]
Fspin_2021_prod_mod<-Fspin_iters_prod_mod[(num_locs*10):(num_locs*11),]
Fspin_2021_fuels_mod<-Fspin_iters_fuels_mod[(num_locs*10):(num_locs*11),]
Fspin_2021_prod_mod_proc_fuels_mod_proc<-Fspin_iters_prod_mod_proc_fuels_mod_proc[(num_locs*10):(num_locs*11),]
Fspin_2021_prod_mod_proc_fuels_mod_par<-Fspin_iters_prod_mod_proc_fuels_mod_par[(num_locs*10):(num_locs*11),]
Fspin_2021_prod_mod_par_fuels_mod_par<-Fspin_iters_prod_mod_par_fuels_mod_par[(num_locs*10):(num_locs*11),]
Fspin_2021_prod_mod_par_fuels_mod_proc<-Fspin_iters_prod_mod_par_fuels_mod_proc[(num_locs*10):(num_locs*11),]


mean_all<-apply(Fspin_2021_all,2,mean,na.rm=2)
mean_none<-apply(Fspin_2021_none,2,mean,na.rm=2)
mean_prod_mod_proc<-apply(Fspin_2021_prod_mod_proc,2,mean,na.rm=2)
mean_prod_mod_par<-apply(Fspin_2021_prod_mod_par,2,mean,na.rm=2)
mean_fuels_mod_proc<-apply(Fspin_2021_fuels_mod_proc,2,mean,na.rm=2)
mean_fuels_mod_par<-apply(Fspin_2021_fuels_mod_par,2,mean,na.rm=2)
mean_prod_mod_all<-apply(Fspin_2021_prod_mod,2,mean,na.rm=2)
mean_fuels_mod_all<-apply(Fspin_2021_fuels_mod,2,mean,na.rm=2)
mean_prod_mod_proc_fuels_mod_proc<-apply(Fspin_2021_prod_mod_proc_fuels_mod_proc,2,mean,na.rm=2)
mean_prod_mod_proc_fuels_mod_par<-apply(Fspin_2021_prod_mod_proc_fuels_mod_par,2,mean,na.rm=2)
mean_prod_mod_par_fuels_mod_par<-apply(Fspin_2021_prod_mod_par_fuels_mod_par,2,mean,na.rm=2)
mean_prod_mod_par_fuels_mod_proc<-apply(Fspin_2021_prod_mod_par_fuels_mod_proc,2,mean,na.rm=2)


## Figure

png("Fine_Fuels_Forecast/Figures/error_by_source.png",height=5.5,width=7.5,res=400,units="in")

par(mfrow=c(2,2),mar=c(2,2,2,1),oma=c(2,2,1,0),mgp=c(2,0.5,0),tcl=-0.2)

plot(density(mean_all), main="Fuels Model Process Uncertainty", 
     ylim=c(0,91), xlab="", ylab="")
polygon(density(mean_all), col=rgb(0,0,0,.2))
polygon(density(mean_fuels_mod_proc),col=rgb(1,0,0.1,.2),border=rgb(1,0,0.1))

plot(density(mean_all), main="Productivity Model Process Uncertainty", 
     ylim=c(0,91), xlab="", ylab="")
polygon(density(mean_all), col=rgb(0,0,0,.2))
polygon(density(mean_prod_mod_proc),col=rgb(1,0,0.1,.2),border=rgb(1,0,0.1))

plot(density(mean_all), main="Fuels Model Parameter Uncertainty", 
     ylim=c(0,91), xlab="", ylab="")
polygon(density(mean_all), col=rgb(0,0,0,.2))
polygon(density(mean_fuels_mod_par),col=rgb(1,0,0.1,0.2),border=rgb(1,0,0.1))

plot(density(mean_all), main="Productivity Model Parameter Uncertainty", 
     ylim=c(0,91), xlab="", ylab="")
polygon(density(mean_all), col=rgb(0,0,0,.2))
polygon(density(mean_prod_mod_par),col=rgb(1,0,0.1,.2),border=rgb(1,0,0.1))

mtext("Fuels forecast (2021)",1,outer=T,line=1,cex=1.1)
mtext("Frequency",2,outer=T,line=1,cex=1.1)

dev.off()

#legend, separate
# par(mfrow=c(1,1))
# plot('n')
# legend( "topright", legend = c("Total Uncertainty", "Fuels Model Process Uncertainty", "Productivity Model Process Uncertainty", 
#                                "Fuels Model Parameter Uncertainty","Productivity Model Parameter Uncertainty"),
#        col =c(rgb(0,0,0,.3),
#               rgb(0,1,.6,.4),
#               rgb(0,0,1,.4),
#               rgb(1,0,0,.4),
#               rgb(.5,0,1,.8)
#        ), pch=15)
# 

# #show plot
# par(mfrow=c(2,2))
# plot(density(mean_all), main="Fuels Model Process Uncertainty", 
#      ylim=c(0,25), xlab="", ylab="Frequency", cex.lab=1.5)
# polygon(density(mean_all), col=rgb(0,0,0,.2))
# polygon(density(mean_fuels_mod_proc),col=rgb(0,1,0,.2))
# # polygon(density(mean_fuels_mod_proc),fill=rgb(0,1,0,.2))
# 
# plot(density(mean_all), main="Productivity Model Process Uncertainty", 
#      ylim=c(0,25), xlab="", ylab="")
# polygon(density(mean_all), col=rgb(0,0,0,.2))
# polygon(density(mean_prod_mod_proc),col=rgb(0,0,1,.2))
# 
# plot(density(mean_all), main="Fuels Model Parameter Uncertainty", 
#      ylim=c(0,25), xlab="Variances of forecasts", ylab="Frequency", cex.lab=1.5)
# polygon(density(mean_all), col=rgb(0,0,0,.2))
# polygon(density(mean_fuels_mod_par),col=rgb(1,0,0,1))
# 
# 
# plot(density(mean_all), main="Productivity Model Parameter Uncertainty", 
#      ylim=c(0,25), xlab="Variances of forecasts", ylab="", cex.lab=1.5, cex.main=1)
# polygon(density(mean_all), col=rgb(0,0,0,.2))
# polygon(density(mean_prod_mod_par),col=rgb(1,0,0,.2), border="purple")
