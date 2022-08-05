# ----------------------- packges, fncs----------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,stringi,rstudioapi,readtext,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

#function that creates a prediction for latent fuel at t+1 based on previous latent fuel value and productivity

source(file="model_checking_and_validation/coverage_function.R")

mod_name<-"priors_sensitivity_analysis"
sig_f=.8
mean_samp_par=4.5


priors_text<-c("alpha=N(.25,.25)", 
               "beta=N(.25,.25)", 
               "sig_o=N(.5,.1)",
               "sig_p=N(0,.25)",
               "sig_f=.8")

par_labels <- c("alpha","beta","sigma[o]","sigma[p]")


# ----------------------- data ----------------------

dat<-readRDS("fuels_model_data/fuels_model_data.rds")
meta_dat<-readRDS("fuels_model_data/fuels_model_meta_data.rds")

row_keeps<-meta_dat$row_keeps

prod_data_orig<-dat$prod[row_keeps,11:35]
fuel_data_orig<-dat$fine_fuel_obs[row_keeps,1:25]
fuel_data_dev<-matrix(NA,ncol=ncol(fuel_data_orig), nrow=nrow(fuel_data_orig))
prod_data_dev<-matrix(NA,ncol=ncol(prod_data_orig), nrow=nrow(prod_data_orig))
fuel_data_z<-matrix(NA,ncol=ncol(fuel_data_orig), nrow=nrow(fuel_data_orig))
prod_data_z<-matrix(NA,ncol=ncol(prod_data_orig), nrow=nrow(prod_data_orig))

for(i in 1:nrow(fuel_data_z)){
  fuel_data_dev[i,]<-fuel_data_orig[i,]-mean(fuel_data_orig[i,],na.rm=TRUE)
  prod_data_dev[i,]<-prod_data_orig[i,]-mean(prod_data_orig[i,])
  fuel_data_z[i,]<-(fuel_data_orig[i,]-mean(fuel_data_orig[i,],na.rm=TRUE))/sd(fuel_data_orig[i,],na.rm=TRUE)
  prod_data_z[i,]<-(prod_data_orig[i,]-mean(prod_data_orig[i,]))/sd(prod_data_orig[i,])
}


# -----------------------sensitivity ranges  ----------------------

alpha_mu_vec<-seq(0,1,.1) 
alpha_sd_vec<-seq(.05,.5,.05)
beta_mu_vec<-seq(0,1,.1)
beta_sd_vec<-seq(.05,.5,.05)
sig_o_mu_vec<-seq(0,2,.1)
sig_o_sd_vec<-seq(.05,.5,.05)
sig_p_mu_vec<-seq(0,1,.1)
sig_p_sd_vec<-seq(.05,.5,.05)
sig_f_vec<-seq(0.1,2,.1)


var_list_str<-list("alpha_mu","alpha_sd", "beta_mu", "beta_sd","sig_o_mu","sig_o_sd", "sig_p_mu", "sig_p_sd", "sig_f")
var_list_labels<-list("Prior(alpha,mean)","Prior(alpha,st.dev.)", 
                      "Prior(beta,mean)", "Prior(beta,st.dev.)",
                      "Prior(sigma[o],mean)","Prior(sigma[o],st.dev.)", 
                      "Prior(sigma[p],mean)", "Prior(sigma[p],st.dev.)", 
                      "Prior(sigma[f],mean)")
var_list<-list(alpha_mu_vec,alpha_sd_vec, beta_mu_vec, beta_sd_vec,sig_o_mu_vec,sig_o_sd_vec, sig_p_mu_vec, sig_p_sd_vec, sig_f_vec)

stan_list<-list()
alpha_vec<-vector(); alpha_hi<-vector(); alpha_lo<-vector()
beta_vec<-vector()
sig_o_vec<-vector()
sig_p_vec<-vector()
div_trans_vec<-vector()
sig_o_rhat<-vector()
sig_p_rhat<-vector()
q_upper<-.9
q_lower<-.1

for ( v in 1:9){
  var<-var_list[[v]]
  stan_list<-list()
  alpha_vec<-vector(); alpha_hi<-vector(); alpha_lo<-vector()
  beta_vec<-vector();beta_hi<-vector(); beta_lo<-vector()
  sig_o_vec<-vector();sig_o_hi<-vector(); sig_o_lo<-vector()
  sig_p_vec<-vector();sig_p_hi<-vector(); sig_p_lo<-vector()
  div_trans_vec<-vector()
  sig_o_rhat<-vector()
  sig_p_rhat<-vector()
  cov_par<-vector(); cov_obs<-vector(); cov_proc<-vector(); cov_all<-vector()
  for ( i in 1:(length(var))){
    var_str<-var_list_str[[v]]
    var_label <- var_list_labels[[v]]
    print(paste0( "model_outputs/", mod_name, "/", var_str, "/", var_str,"_", var[i], ".rds"))
    fit1<-readRDS(paste0("model_outputs/", mod_name, "/", var_str, "/", var_str,"_",  var[i], ".rds"))
    stan_list[[i]]<-fit1
    names(stan_list)[[i]]<-paste0( var_str,"_",  var[i])
    alphaout<-rstan::extract(fit1, "alpha", permuted=F)
    alpha_vec[i]<-mean(alphaout)
    alpha_hi[i]<-quantile(alphaout, .95)
    alpha_lo[i]<-quantile(alphaout, .05)
    
    betaout<-rstan::extract(fit1, "beta", permuted=F)
    beta_vec[i]<-mean(betaout)
    beta_hi[i]<-quantile(betaout, .95)
    beta_lo[i]<-quantile(betaout, .05)
    
    sig_oout<-rstan::extract(fit1, "sig_o", permuted=F)
    sig_o_vec[i]<-mean(sig_oout)
    sig_o_hi[i]<-quantile(sig_oout, .95)
    sig_o_lo[i]<-quantile(sig_oout, .05)
    
    sig_pout<-rstan::extract(fit1, "sig_p", permuted=F)
    sig_p_vec[i]<-mean(sig_pout)
    sig_p_hi[i]<-quantile(sig_pout, .95)
    sig_p_lo[i]<-quantile(sig_pout, .05)
    
    
    div_trans_vec[i]<-sum(get_divergent_iterations(fit1))
    sig_o_rhat[i]<-summary(fit1)[[1]][3,10]
    sig_p_rhat[i]<-summary(fit1)[[1]][4,10]

   uncertainty<-plot_coverage(fit1, q_upper=.9, q_lower=.1,
                                 nIters=1000, mod_name="stan",
                                 fuel_dat=fuel_data_z, prod_dat=prod_data_z,
                                 prod_start_val_vec<-meta_dat$P0, mean_samp=4.6,
                                 sig_f_par=sig_f, prod_m_match=meta_dat$prod_m_match, show_plot = F)
    cov_par[i]<-uncertainty[1,1]
    cov_obs[i]<-uncertainty[2,1]
    cov_proc[i]<-uncertainty[3,1]
    cov_all[i]<-uncertainty[4,1]
    print(uncertainty)
    }
  
  save_loc1<-paste0("Supplemental_Info_figs/prior_sensitivity_analysis_figs/", var_str, ".png")
  png(save_loc1,height=3.25,width=4,units="in",res=400)
  par(tcl=-0.2,mgp=c(2,0.5,0),mar=c(3,4,1,1))
  plotCI(x=var, y=alpha_vec, li=alpha_lo, ui=alpha_hi, ylim=c(-.5,1),pch=19, 
       xlab= str2expression(var_label), ylab ="Posterior value")
  plotCI(x=var, y=beta_vec, li=beta_lo, ui=beta_hi, col="blue",pch=19, add=T)
  plotCI(x=var, y=sig_o_vec,li=sig_o_lo, ui=sig_o_hi, col="red",pch=19, add=T)
  plotCI(x=var, y=sig_p_vec, li=sig_p_lo, ui=sig_p_hi, col="chartreuse4",pch=19, add=T)
  legend("bottomleft", legend=str2expression(par_labels), pch=c(19,19,19,19), col=c("black", "blue", "red", "chartreuse4"),bty="n")
  dev.off()
  
  save_loc2<-paste0("Supplemental_Info_figs/prior_sensitivity_analysis_figs/", var_str, "_diagnostics.png")
  png(save_loc2)
  plot(x=var, y=sig_o_rhat, main=paste0(var_str, " : diagnostics") ,
       ylim=c(0,2),pch=19, xlab= paste0(var_str, " (prior)"), ylab ="posteriors")
  text(x=var, y=0.5, div_trans_vec, col="darkgreen")
  points(x=jitter(var), y=sig_p_rhat, col="blue",pch=19)
  legend("topleft", legend=c("sig_o rhat", "sig_p rhat","div transitions"), 
         pch=19, col=c("black", "blue", "darkgreen"))
  abline(a=0,b=0,h=1.1,lty=2)
  dev.off()
  
  save_loc3<-paste0("Supplemental_Info_figs/prior_sensitivity_analysis_figs/", var_str, "_coverage.png")
  png(save_loc3)
  barplot(height=cov_all,cex.names=.6, ylim=c(0,1), names.arg=var, xlab=var_str,
          main = paste0(q_upper, "-",q_lower, " coverage \n", paste0(var_str)))
  barplot(height=cov_proc, ylim=c(0,1), col=rgb(0,1,0,.3), add=TRUE)
  barplot(height=cov_obs, ylim=c(0,1), col=rgb(1,0,0,.5), add=TRUE)
  barplot(height=cov_par, ylim=c(0,1), col=rgb(0,0,1,1), add=TRUE)
  legend("topleft", legend=c("all err", "proc err","obs err", "param err"),
         pch=15, col=c("darkgray",  "green", "red","blue"))
  abline(a=(q_upper-q_lower), b=0, h=(q_upper-q_lower), lty=2)
  
  dev.off()
}
