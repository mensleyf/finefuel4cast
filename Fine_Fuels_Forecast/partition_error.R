#---------- loading pcks, data, model-------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer,rgdal, raster, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

##--------- extracting information from Prod_Forecast model ------------------

#fitted model
forecast_rds<-readRDS( "Prod_Forecast_model/prod_model_outputs/march_forecast_lm.rds")
forecast_mod<-forecast_rds$mod_fit
proc_err<-summary(forecast_mod)$sigma

#actual prod data from 1987-2020
prod_data<-as.data.frame(forecast_rds$agb_m)
names(prod_data)<-seq(1987,2020)

#forecasted/hindcasted data 1988-2021
prod_preds<-read.csv("Prod_Forecast_model/prod_model_outputs/forecast2021/prod_cast_1988_2021.csv")

(nLocs<-nrow(prod_preds))

# param uncertainty from monte carlo sampling 
march_forecast_param<-read.csv("Prod_Forecast_model/prod_model_outputs/forecast2021/march_forecast_param_2021.csv")
(param_err<-mean(apply(march_forecast_param,2,sd), na.rm=T))


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


#lets not do this 50000 times
loc_keeps<-read.csv("Prod_Forecast_model/prod_model_outputs/loc_keeps.csv")


coords<-as.data.frame(loc_keeps)
coords<-subset(coords, coords$yr==1987)
(nLocs<-nrow(coords))

num_locs<-100
random_locs<-sample(seq(1:nrow(coords)), num_locs)
loc_keeps<-loc_keeps[random_locs,]
agb_m<-as.data.frame(forecast_rds$agb_m)[random_locs,]
names(agb_m)<-seq(1987,2020)
dim(agb_m)


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

#matrix to holdcurrent year spin-up

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
Fspin_all[,1]<-agb_m[,24]
Fspin_none[,1]<-agb_m[,24]

#1way
Fspin_prod_mod_proc[,1]<-agb_m[,24]
Fspin_prod_mod_par[,1]<-agb_m[,24]
Fspin_fuels_mod_proc[,1]<-agb_m[,24]
Fspin_fuels_mod_par[,1]<-agb_m[,24]

#2way
Fspin_prod_mod[,1]<-agb_m[,24]
Fspin_fuels_mod[,1]<-agb_m[,24]
Fspin_prod_mod_proc_fuels_mod_proc[,1]<-agb_m[,24]
Fspin_prod_mod_proc_fuels_mod_par[,1]<-agb_m[,24]
Fspin_prod_mod_par_fuels_mod_par[,1]<-agb_m[,24]
Fspin_prod_mod_par_fuels_mod_proc[,1]<-agb_m[,24]

#random iterations (out of 8000) to spin over
nIters<-100 #(hitting memory limits) (could start in 2010?)
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


for (i in 1:nIters){
  for ( cur_yr in 2:11){
    if(cur_yr!=11){ #thru 2020 on real data
      (iter<-random_iters[i])
      prev_yr<-cur_yr-1
      yr_name<-2010+cur_yr
      
      #all and none 
      Fspin_all[,cur_yr]<-alphaout[iter]*Fspin_all[,prev_yr]+betaout[iter]*agb_m[,cur_yr] + rnorm(num_locs,0,sig_pout[iter])
      Fspin_none[,cur_yr]<-alpha_est*Fspin_none[,prev_yr]+beta_est*agb_m[,cur_yr]
      
      #1way
      Fspin_prod_mod_proc[,cur_yr]<-alpha_est*Fspin_prod_mod_proc[,prev_yr]+beta_est*agb_m[,cur_yr]
      Fspin_prod_mod_par[,cur_yr]<-alpha_est*Fspin_prod_mod_par[,prev_yr]+beta_est*agb_m[,cur_yr]
      Fspin_fuels_mod_proc[,cur_yr]<-alpha_est*Fspin_fuels_mod_proc[,prev_yr]+beta_est*agb_m[,cur_yr] + rnorm(num_locs,0,sig_pout[iter])
      Fspin_fuels_mod_par[,cur_yr]<-alphaout[iter]*Fspin_fuels_mod_par[,prev_yr]+betaout[iter]*agb_m[,cur_yr]
      
      #2way
      Fspin_prod_mod[,cur_yr]<-alpha_est*Fspin_prod_mod[,prev_yr]+beta_est*agb_m[,cur_yr]
      Fspin_fuels_mod[,cur_yr]<-alphaout[iter]*Fspin_fuels_mod[,prev_yr]+betaout[iter]*agb_m[,cur_yr] + rnorm(num_locs,0,sig_pout[iter])
      Fspin_prod_mod_proc_fuels_mod_proc[,cur_yr]<-alpha_est*Fspin_prod_mod_proc_fuels_mod_proc[,prev_yr]+beta_est*agb_m[,cur_yr] + rnorm(num_locs,0,sig_pout[iter])
      Fspin_prod_mod_proc_fuels_mod_par[,cur_yr]<-alphaout[iter]*Fspin_prod_mod_proc_fuels_mod_par[,prev_yr]+betaout[iter]*agb_m[,cur_yr]
      Fspin_prod_mod_par_fuels_mod_par[,cur_yr]<-alphaout[iter]*Fspin_prod_mod_par_fuels_mod_par[,prev_yr]+betaout[iter]*agb_m[,cur_yr] 
      Fspin_prod_mod_par_fuels_mod_proc[,cur_yr]<-alpha_est*Fspin_prod_mod_par_fuels_mod_proc[,prev_yr]+beta_est*agb_m[,cur_yr] + rnorm(num_locs,0,sig_pout[iter])
      
    }
    if(cur_yr==11){ #last year on forecasted data
      (iter<-random_iters[i])
      prev_yr<-cur_yr-1
      yr_name<-1986+cur_yr
      
      #all/none
      Fspin_all[,cur_yr]<-alphaout[iter]*Fspin_all[,prev_yr]+betaout[iter]*(prod_preds[random_locs,34]+ rnorm(num_locs,0,param_err)+rnorm(num_locs, 0, proc_err)) + rnorm(num_locs,0,sig_pout[iter])
      Fspin_none[,cur_yr]<-alpha_est*Fspin_none[,prev_yr]+beta_est*prod_preds[random_locs,34]
      
      #1way
      Fspin_prod_mod_proc[,cur_yr]<-alpha_est*Fspin_prod_mod_proc[,prev_yr]+beta_est*(prod_preds[random_locs,34]+ rnorm(num_locs,0,proc_err))
      Fspin_prod_mod_par[,cur_yr]<-alpha_est*Fspin_prod_mod_par[,prev_yr]+beta_est*(prod_preds[random_locs,34]+ rnorm(num_locs,0,param_err))
      Fspin_fuels_mod_proc[,cur_yr]<-alpha_est*Fspin_fuels_mod_proc[,prev_yr]+beta_est*prod_preds[random_locs,34] + rnorm(num_locs,0,sig_pout[iter])
      Fspin_fuels_mod_par[,cur_yr]<-alphaout[iter]*Fspin_fuels_mod_par[,prev_yr]+betaout[iter]*prod_preds[random_locs,34]
      
      #2way
      Fspin_prod_mod[,cur_yr]<-alpha_est*Fspin_prod_mod[,prev_yr]+beta_est*(prod_preds[random_locs,34]+ rnorm(num_locs,0,proc_err) +rnorm(num_locs,0,param_err))
      Fspin_fuels_mod[,cur_yr]<-alphaout[iter]*Fspin_fuels_mod[,prev_yr]+betaout[iter]*prod_preds[random_locs,34]  + rnorm(num_locs,0,sig_pout[iter])
      Fspin_prod_mod_proc_fuels_mod_proc[,cur_yr]<-alpha_est*Fspin_prod_mod_proc_fuels_mod_proc[,prev_yr]+beta_est*(prod_preds[random_locs,34]+ rnorm(num_locs,0,proc_err)) + rnorm(num_locs,0,sig_pout[iter])
      Fspin_prod_mod_proc_fuels_mod_par[,cur_yr]<-alphaout[iter]*Fspin_prod_mod_proc_fuels_mod_par[,prev_yr]+betaout[iter]*(prod_preds[random_locs,34]+ rnorm(num_locs,0,proc_err))
      Fspin_prod_mod_par_fuels_mod_par[,cur_yr]<-alphaout[iter]*Fspin_prod_mod_par_fuels_mod_par[,prev_yr]+betaout[iter]*(prod_preds[random_locs,34] +rnorm(num_locs,0,param_err))
      Fspin_prod_mod_par_fuels_mod_proc[,cur_yr]<-alpha_est*Fspin_prod_mod_par_fuels_mod_proc[,prev_yr]+beta_est*(prod_preds[random_locs,34] +rnorm(num_locs,0,param_err)) + rnorm(num_locs,0,sig_pout[iter])
     
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



par(mfrow=c(2,2))

plot(density(mean_all), main="Fuels Model Process Uncertainty", 
     ylim=c(0,15), xlab="Variances of forecasts", ylab="frequency")
polygon(density(mean_all), col=rgb(0,0,0,.2))
polygon(density(mean_fuels_mod_proc),col=rgb(0,1,0,.2))
# polygon(density(mean_fuels_mod_proc),fill=rgb(0,1,0,.2))

plot(density(mean_all), main="Productivity Model Process Uncertainty", 
     ylim=c(0,15), xlab="Variances of forecasts", ylab="frequency")
polygon(density(mean_all), col=rgb(0,0,0,.2))
polygon(density(mean_prod_mod_proc),col=rgb(0,0,1,.2))

plot(density(mean_all), main="Fuels Model Parameter Uncertainty", 
     ylim=c(0,15), xlab="Variances of forecasts", ylab="frequency")
polygon(density(mean_all), col=rgb(0,0,0,.2))
polygon(density(mean_fuels_mod_par),col=rgb(1,0,0,1))


plot(density(mean_all), main="Productivity Model Parameter Uncertainty", 
     ylim=c(0,115), xlab="Variances of forecasts", ylab="frequency")
polygon(density(mean_all), col=rgb(0,0,0,.2))
polygon(density(mean_prod_mod_par),col=rgb(1,0,0,.2), border="purple")

par(mfrow=c(1,1))
plot('n')
legend( "topright", legend = c("Total Uncertainty", "Fuels Model Process Uncertainty", "Productivity Model Process Uncertainty", 
                               "Fuels Model Parameter Uncertainty","Productivity Model Parameter Uncertainty"),
       col =c(rgb(0,0,0,.3),
              rgb(0,1,.6,.4),
              rgb(0,0,1,.4),
              rgb(1,0,0,.4),
              rgb(.5,0,1,.8)
       ), pch=15)
# 
# par(mfrow=c(1,1))
# options(scipen=999)
# uncertainty_source<-as.data.frame(matrix(NA,nrow=11,ncol=0))
# uncertainty_source$source<-c("all sources", 
#                              # "none",
#                              "prod_process","prod_par", "fuels_process", "fuels_par",
#                              "prod_all","fuels_all",
#                              "prod_proc_fuels_proc",
#                              "prod_proc_fuels_par",
#                              "prod_par_fuels_par",
#                              "prod_par_fuels_proc"
#                              
#                              )
# 
# uncertainty_source$uncert<-c(sd(mean_all),
#                           # sd(mean_none),
#                           sd(mean_prod_mod_proc),
#                           sd(mean_prod_mod_par),
#                           sd(mean_fuels_mod_proc),
#                           sd(mean_fuels_mod_par),
#                           sd(mean_prod_mod_all),
#                           sd(mean_fuels_mod_all),
#                           sd(mean_prod_mod_proc_fuels_mod_proc),
#                           sd(mean_prod_mod_proc_fuels_mod_par),
#                           sd(mean_prod_mod_par_fuels_mod_par),
#                           sd(mean_prod_mod_par_fuels_mod_proc)
#                           )
# 
# par(mfrow=c(1,1))
# (uncertainty_source)
# barplot(height=uncertainty_source$uncert,  names.arg=uncertainty_source$source, main = "2021 Forecast sources of uncertainty")
# text(x=seq(0.8,12.8,1.2),y=0.02, labels=round(uncertainty_source$err,3))
# 
# barplot(height=uncertainty_source$uncert[1:5], 
#         names.arg=uncertainty_source$source[1:5], main = "2021 Forecast sources of uncertainty \n all and one-way")
# text(x=seq(0.8,5.8,1.2),y=0.02, labels=round(uncertainty_source$err[1:5],3))
# 
# 
# barplot(height=uncertainty_source$err[6:11], 
#         names.arg=uncertainty_source$source[6:11], main = "2021 Forecast sources of uncertainty  \n 2-way")
# text(x=seq(0.8,6.8,1.2),y=0.02, labels=round(uncertainty_source$err[6:11],3))
# 
# 
# ## one way
# (uncertainty_source$perc<-uncertainty_source$uncert/sum(uncertainty_source$uncert[1])*100)
# barplot(height=uncertainty_source$perc[c(4,2,5,3)], 
#         names.arg=c( "process error", "process error" , "parameter error","parameter error"),
#         main = "2021 Forecast sources of uncertainty", cex.main=3,
#         col=c("red", "blue", "red", "blue", "gray"),
#         ylab= "Percent of total uncertainty", cex.lab=2,
#         ylim=c(0,100))
# legend("topright", legend=c( "Fuels Model", "Productivity Forecast"), col = c("red", "blue") , pch=15, cex=2.5)
# # text(x=seq(0.8,12.8,1.2),y=0.02, labels=round(uncertainty_source$err,3))
# 
# 
# ## 2 way
# (uncertainty_source)
# "prod_proc_fuels_par" 
# [5] "prod_par_fuels_par"   "prod_par_fuels_proc" 
# 
# names_temp<-c("productivity model", 
#               "fuels model", 
#               "prod model process \n  fuels model process",
#               "prod model process \n fuels model parameter", 
#               "prod model parameter \n fuels model parameter",
#               "prod model parameter \n fuels model process"
# )
# barplot(height=uncertainty_source$perc[c(6:11)], 
#         names.arg=names_temp,
#         main = "2021 Forecast sources of uncertainty", cex.main=3,
#         col=c("blue", "red", rep("purple",4)),
#         ylab= "Percent of total uncertainty", cex.lab=2,
#         ylim=c(0,100))
# legend("topright", legend=c( "Fuels Model", "Productivity Forecast"), col = c("red", "blue") , pch=15, cex=2.5)
# # text(x=seq(0.8,12.8,1.2),y=0.02, labels=round(uncertainty_source$err,3))
# 
# 
# 