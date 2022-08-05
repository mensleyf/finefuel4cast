#This runs the final version of the Fuels Model and saves the output

# ----------------------- packges, fncs----------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

source(file = "model_checking_and_validation/make_sim_data_function.R")

# ----------------------- data ----------------------

dat<-readRDS("fuels_model_data/fuels_model_data.rds")

#sim params to play with
alpha_sim= .1; beta_sim=.25; sig_o_sim=.8; sig_p_sim=.7

sim_dat<-make_sim_data(  nPlots=198,
                         nYears=25,
                         sig_o=sig_o_sim, #fuel obs error
                         sig_p=sig_p_sim, # process error
                         percent_missing=0,
                         show_plots=TRUE,
                         random=FALSE,
                         alpha=alpha_sim,
                         beta=beta_sim)

meta_dat<-readRDS("fuels_model_data/fuels_model_meta_data.rds")
row_keeps<-meta_dat$row_keeps
fuel_data_z<-as.data.frame(sim_dat$sim_obs_fuel[row_keeps,])
prod_data_z<-as.data.frame(sim_dat$sim_prod[row_keeps,])

#make 'prod_data_vec' from simulation

Fstart_vec<-meta_dat$Fstart_vec
Fstart<-vector()
new_Fstart<-vector()
for ( i in 1:nrow(Fstart_vec)){
  Fstart[i]<-dat$prod[Fstart_vec[i,1], Fstart_vec[i,2]]
  new_Fstart[i]<- Fstart_vec[i,2]-(9)
  
}

new_Fstart<-as.data.frame(new_Fstart)
year_length<-meta_dat$year_length
prod_incl<-matrix(NA,nrow=nrow(prod_data_z), ncol=ncol(prod_data_z))
prod_replace<-matrix(NA,nrow=nrow(prod_data_z), ncol=ncol(prod_data_z))

for ( i in 1:nrow(prod_data_z) ){
  replacement<-as.vector(prod_data_z[i,(new_Fstart[i,1]:(new_Fstart[i,1]+year_length[i,1]-1 ))])
  prod_incl[i,(1:(year_length[i,1]))]<-unlist(replacement[(1:(year_length[i,1]))])
  prod_replace[i,(new_Fstart[i,1]:(new_Fstart[i,1]+year_length[i,1]-1 ))]<-unlist(replacement)
}


prod_m<-make_m(prod_replace)
nOutputs<-nrow(prod_m)

#is it a first value?
first_val_vec<-vector()
for ( i in 2:nrow(prod_m)){
  prev<-i-1
  if (prod_m[i,1]!=prod_m[prev,1]) (first_val_vec[i]<-1)
  if (prod_m[i,1]==prod_m[prev,1]) (first_val_vec[i]<-0)
  
}

first_val_vec[1]<-1
prod_data_vec<-as.numeric(na.omit(as.vector(t(as.matrix(prod_replace)))))

P0<-(sim_dat$Fstart[row_keeps])
sample_data<-as.data.frame(sim_dat$sim_samp[row_keeps])
year_length<-as.data.frame(meta_dat$year_length)

# ----------------------- run model ----------------------
code<-readtext("stan_txts/fuels_model_code.txt")$text


stan.data.sim<-list("nLocs"=meta_dat$nLocs, "nYears"=meta_dat$nYears, "nMiss"=meta_dat$nMiss, "nOutputs"=meta_dat$nOutputs,
                    "P0"=as.vector(P0),
                    "P"= as.numeric(as.vector(prod_data_vec)),"first_val_vec"=as.data.frame(first_val_vec), "S"=sample_data,
                    "fuel_obs_index"=as.data.frame(meta_dat$fuels_obs_index), "prod_index"=as.data.frame(meta_dat$prod_index), "O"=fuel_data_z)



fit1<-stan(model_code=code,data=stan.data.sim,control=list(adapt_delta=.8, max_treedepth=10),warmup=2000,iter=4000, chains=4, save_warmup=F, refresh=0)
# fit1<-stan(model_code=code,data=stan.data.sim,control=list(adapt_delta=.8, max_treedepth=10),warmup=50,iter=100, chains=1, save_warmup=F, refresh=0)

# saveRDS(fit1, "model_outputs/sim_fuels_model.rds")

# ----------------------- quick check ----------------------

print(paste0("divergent iterations: " , sum(get_divergent_iterations(fit1))))
print(summary(fit1)[[1]][seq(1,5,1),c(1,10)])

# ----------------------- check visually ----------------------

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




plotCI(x=alpha_sim, y=alpha,li=alpha_025, ui=alpha_975, xlim=c(0,3.1), ylim=c(0,3.1), col="red", lwd=3, pch=15,
       main="Parameter Retrieval Simulation", ylab="Fuels Model parameter 95% CI estimate", xlab="known parameter values")
plotCI(x=beta_sim, y=beta, li=beta_025, ui=beta_975, xlim=c(0,1), ylim=c(0,1), col="blue", lwd=3, pch=15, add=T)
plotCI(x=sig_o_sim, y=sig_o, li=sig_o_025, ui=sig_o_975, xlim=c(0,1), ylim=c(0,1), col="darkgreen", lwd=3, pch=15, add=T)
plotCI(x=sig_p_sim, y=sig_p, li=sig_p_025, ui=sig_p_975, xlim=c(0,1), ylim=c(0,1), col="purple", lwd=3, pch=15, add=T)
abline(0,1)
legend("bottomright", legend=c("carryover (a)", "conversion (b)", "process err (sig_p)","obs err (sig_o)"),
       pch=19, col=c("red", "blue", "purple" ,"darkgreen"))




