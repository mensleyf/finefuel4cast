#This runs the final version of the Fuels Model and saves the output in the 'model_outputs' folder

# ----------------------- packgaes ----------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)


# ----------------------- data ----------------------

dat<-readRDS("fuels_model_data/fuels_model_data.rds") 
meta_dat<-readRDS("fuels_model_data/fuels_model_meta_data.rds")

#additional data to run model only on locations and years with few missing data points
row_keeps<-meta_dat$row_keeps
year_length<-as.data.frame(meta_dat$year_length)
sample_data<-as.data.frame(dat$samp[row_keeps])

#standardizing data
prod_data_orig<-dat$prod[row_keeps,11:35]
fuel_data_orig<-dat$fine_fuel_obs[row_keeps,1:25]
fuel_data_z<-matrix(NA,ncol=ncol(fuel_data_orig), nrow=nrow(fuel_data_orig))
prod_data_z<-matrix(NA,ncol=ncol(prod_data_orig), nrow=nrow(prod_data_orig))

for(i in 1:nrow(fuel_data_z)){
  fuel_data_z[i,]<-(fuel_data_orig[i,]-mean(fuel_data_orig[i,],na.rm=TRUE))/sd(fuel_data_orig[i,],na.rm=TRUE)
  prod_data_z[i,]<-(prod_data_orig[i,]-mean(prod_data_orig[i,]))/sd(prod_data_orig[i,])
}

fuel_data_z2<-replace(fuel_data_z, is.na(fuel_data_z), -1000) #it will be painfully obvious if model isn't skipping NA's


#backtransforming sig_o
sig_o_vals<-c(.01, seq(.1,3,.1))
sig_o_trans_vals<-vector()
for ( i in 1:length(sig_o_vals)){
  sig_o_trans_vals[i]<-sig_o_vals[i]*sd(fuel_data_orig[1,],na.rm=T)+mean(fuel_data_orig[1,],na.rm=T)
}


plot(sig_o_vals, sig_o_trans_vals, ylim=c(0,8000), pch=19)

##------------------------------------- plots -----------------------------
# make plots for altered alpha
alpha_sim=.5;beta_sim=.25;sig_p_sim=.6

alpha_vec<-vector(); alpha025_vec<-vector(); alpha975_vec<-vector()
beta_vec<-vector(); beta025_vec<-vector(); beta975_vec<-vector()
sig_o_vec<-vector(); sig_o025_vec<-vector(); sig_o975_vec<-vector()
sig_p_vec<-vector(); sig_p025_vec<-vector(); sig_p975_vec<-vector()

for ( sim in 1:length(sig_o_vals)) {
  sig_o_sim=sig_o_vals[sim]
  print(paste0( "sim: " , sig_o_vals[sim]," num: ", sim ))
  
  # source(file="fuels_model_run/sim_data_retrieval_range_test.R")
  # saveRDS(fit1,file= paste0("model_outputs/simulation/sig_o_",sig_o_vals[sim], ".rds" ))
  
  fit1<-readRDS(paste0("model_outputs/simulation/alpha/sig_o_",sig_o_vals[sim], ".rds" ))
  
  alphaout<-rstan::extract(fit1, 'alpha', permuted=F);alphaout<-apply(alphaout, 3,c)
  betaout<-rstan::extract(fit1, 'beta', permuted=F);betaout<-apply(betaout, 3,c)
  sig_oout<-rstan::extract(fit1, 'sig_o', permuted=F);sig_oout<-apply(sig_oout, 3,c)
  sig_pout<-rstan::extract(fit1, 'sig_p', permuted=F);sig_pout<-apply(sig_pout, 3,c)
  
  alpha_975<-quantile(alphaout,  0.975)
  alpha_025<-quantile(alphaout,  0.025)
  alpha<-quantile(alphaout,  0.5)
  
  beta_975<-quantile(betaout,  0.975)
  beta_025<-quantile(betaout,  0.025)
  beta<-quantile(betaout,  0.5)
  
  sig_o_975<-quantile(sig_oout,  0.975)
  sig_o_025<-quantile(sig_oout,  0.025)
  sig_o<-quantile(sig_oout,  0.5)
  
  sig_p_975<-quantile(sig_pout,  0.975)
  sig_p_025<-quantile(sig_pout,  0.025)
  sig_p<-quantile(sig_pout,  0.5)
  
  alpha_vec[sim]<-mean(alpha)
  alpha025_vec[sim]<-mean(alpha_025)
  alpha975_vec[sim]<-mean(alpha_975)
  
  beta_vec[sim]<-mean(beta)
  beta025_vec[sim]<-mean(beta_025)
  beta975_vec[sim]<-mean(beta_975)
  
  sig_o_vec[sim]<-mean(sig_o)
  sig_o025_vec[sim]<-mean(sig_o_025)
  sig_o975_vec[sim]<-mean(sig_o_975)
  
  sig_p_vec[sim]<-mean(sig_p)
  sig_p025_vec[sim]<-mean(sig_p_025)
  sig_p975_vec[sim]<-mean(sig_p_975)
  
}

#figure
png("Supplemental_Info_figs/simulation_figs/higher_alpha.png",height=4,width=5,units="in",res=400)

par(tcl=-0.2,mgp=c(2,0.5,0),mar=c(3,4,1,1))
plotCI(sig_o_vals, y=alpha_vec, ui= alpha975_vec, li= alpha025_vec,ylim=c(0,3), col="red", pch=19,
       xlab=expression(paste("Simulated value of ",sigma[o])), ylab="Estimated parameter values")
plotCI(sig_o_vals,  y=beta_vec, ui= beta975_vec, li= beta025_vec,, ylim=c(0,3), col="blue", pch=19, add=T)
plotCI(sig_o_vals, y=sig_p_vec, ui= sig_p975_vec, li= sig_p025_vec,, ylim=c(0,3), col="purple", pch=19, add=T)
plotCI(sig_o_vals,  y=sig_o_vec, ui= sig_o975_vec, li= sig_o025_vec,, ylim=c(0,3), col="darkgreen", pch=19, add=T)
# 

lines(sig_o_vals, rep(alpha_sim,length(sig_o_vals)), ylim=c(0,3), col="red", pch=19)
lines(sig_o_vals, rep(beta_sim,length(sig_o_vals)), ylim=c(0,3), col="blue", pch=19)
lines(sig_o_vals, rep(sig_p_sim,length(sig_o_vals)), ylim=c(0,3), col="purple", pch=19)
lines(sig_o_vals, sig_o_vals, ylim=c(0,3), col="darkgreen", pch=19)


legend("topleft", legend=c(expression(alpha), expression(beta), expression(sigma[p], sigma[o])),
       pch=19, lty=1,col=c("red", "blue", "purple" ,"darkgreen"), bty="n")

dev.off()



# make plots for altered beta
alpha_sim=.1;beta_sim=.5;sig_p_sim=.6

alpha_vec<-vector(); alpha025_vec<-vector(); alpha975_vec<-vector()
beta_vec<-vector(); beta025_vec<-vector(); beta975_vec<-vector()
sig_o_vec<-vector(); sig_o025_vec<-vector(); sig_o975_vec<-vector()
sig_p_vec<-vector(); sig_p025_vec<-vector(); sig_p975_vec<-vector()
sim<-1
for ( sim in 1:length(sig_o_vals)) {
  sig_o_sim=sig_o_vals[sim]
  print(paste0( "sim: " , sig_o_vals[sim]," num: ", sim ))
  
  # source(file="fuels_model_run/sim_data_retrieval_range_test.R")
  # saveRDS(fit1,file= paste0("model_outputs/simulation/sig_o_",sig_o_vals[sim], ".rds" ))


  fit1<-readRDS(paste0("model_outputs/simulation/beta/sig_o_",sig_o_vals[sim], ".rds" ))
  
  alphaout<-rstan::extract(fit1, 'alpha', permuted=F);alphaout<-apply(alphaout, 3,c)
  betaout<-rstan::extract(fit1, 'beta', permuted=F);betaout<-apply(betaout, 3,c)
  sig_oout<-rstan::extract(fit1, 'sig_o', permuted=F);sig_oout<-apply(sig_oout, 3,c)
  sig_pout<-rstan::extract(fit1, 'sig_p', permuted=F);sig_pout<-apply(sig_pout, 3,c)
  
  alpha_975<-quantile(alphaout,  0.975)
  alpha_025<-quantile(alphaout,  0.025)
  alpha<-quantile(alphaout,  0.5)
  
  beta_975<-quantile(betaout,  0.975)
  beta_025<-quantile(betaout,  0.025)
  beta<-quantile(betaout,  0.5)
  
  sig_o_975<-quantile(sig_oout,  0.975)
  sig_o_025<-quantile(sig_oout,  0.025)
  sig_o<-quantile(sig_oout,  0.5)
  
  sig_p_975<-quantile(sig_pout,  0.975)
  sig_p_025<-quantile(sig_pout,  0.025)
  sig_p<-quantile(sig_pout,  0.5)
  
  alpha_vec[sim]<-mean(alpha)
  alpha025_vec[sim]<-mean(alpha_025)
  alpha975_vec[sim]<-mean(alpha_975)

    beta_vec[sim]<-mean(beta)
  beta025_vec[sim]<-mean(beta_025)
  beta975_vec[sim]<-mean(beta_975)

  sig_o_vec[sim]<-mean(sig_o)
  sig_o025_vec[sim]<-mean(sig_o_025)
  sig_o975_vec[sim]<-mean(sig_o_975)

  sig_p_vec[sim]<-mean(sig_p)
  sig_p025_vec[sim]<-mean(sig_p_025)
  sig_p975_vec[sim]<-mean(sig_p_975)

}


#figure
png("Supplemental_Info_figs/simulation_figs/higher_beta.png",height=4,width=5,units="in",res=400)

par(tcl=-0.2,mgp=c(2,0.5,0),mar=c(3,4,1,1))
plotCI(sig_o_vals, y=alpha_vec, ui= alpha975_vec, li= alpha025_vec,ylim=c(0,3), col="red", pch=19,
       xlab=expression(paste("Simulated value of ",sigma[o])), ylab="Estimated parameter values")
plotCI(sig_o_vals,  y=beta_vec, ui= beta975_vec, li= beta025_vec,, ylim=c(0,3), col="blue", pch=19, add=T)
plotCI(sig_o_vals, y=sig_p_vec, ui= sig_p975_vec, li= sig_p025_vec,, ylim=c(0,3), col="purple", pch=19, add=T)
plotCI(sig_o_vals,  y=sig_o_vec, ui= sig_o975_vec, li= sig_o025_vec,, ylim=c(0,3), col="darkgreen", pch=19, add=T)
# 

lines(sig_o_vals, rep(alpha_sim,length(sig_o_vals)), ylim=c(0,3), col="red", pch=19)
lines(sig_o_vals, rep(beta_sim,length(sig_o_vals)), ylim=c(0,3), col="blue", pch=19)
lines(sig_o_vals, rep(sig_p_sim,length(sig_o_vals)), ylim=c(0,3), col="purple", pch=19)
lines(sig_o_vals, sig_o_vals, ylim=c(0,3), col="darkgreen", pch=19)


legend("topleft", legend=c(expression(alpha), expression(beta), expression(sigma[p], sigma[o])),
       pch=19, lty=1,col=c("red", "blue", "purple" ,"darkgreen"), bty="n")

dev.off()


png("Supplemental_Info_figs/simulation_figs/higher_beta.png")
plotCI(sig_o_vals, y=alpha_vec, ui= alpha975_vec, li= alpha025_vec,ylim=c(0,3), col="red", pch=19,
       main="parameter retrievel with changing sig_o ",
       xlab="Simulated sig_o parameter", ylab="Model estimates of parameters")
plotCI(sig_o_vals,  y=beta_vec, ui= beta975_vec, li= beta025_vec,, ylim=c(0,3), col="blue", pch=19, add=T)
plotCI(sig_o_vals, y=sig_p_vec, ui= sig_p975_vec, li= sig_p025_vec,, ylim=c(0,3), col="purple", pch=19, add=T)
plotCI(sig_o_vals,  y=sig_o_vec, ui= sig_o975_vec, li= sig_o025_vec,, ylim=c(0,3), col="darkgreen", pch=19, add=T)



lines(sig_o_vals, rep(alpha_sim,length(sig_o_vals)), ylim=c(0,3), col="red", pch=19)
lines(sig_o_vals, rep(beta_sim,length(sig_o_vals)), ylim=c(0,3), col="blue", pch=19)
lines(sig_o_vals, rep(sig_p_sim,length(sig_o_vals)), ylim=c(0,3), col="purple", pch=19)
lines(sig_o_vals, sig_o_vals, ylim=c(0,3), col="darkgreen", pch=19)


legend("topleft", legend=c(expression(alpha), expression(beta), expression(sigma[p], sigma[o])),
       lty=1, col=c("red", "blue", "purple" ,"darkgreen"), title="Actual parameter values")


dev.off()
