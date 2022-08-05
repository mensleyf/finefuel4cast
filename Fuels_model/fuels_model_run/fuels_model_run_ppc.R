#This runs additional code needed to conduct posterior predictive checks 
# on the final version of the Fuels Model and saves the output  in the 'model_outputs' folder

# ----------------------- packges, fncs----------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)


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

# ----------------------- stan code ----------------------
code<-readtext("stan_txts/fuels_model_code_ppc.txt")$text
# file.show("stan_txts/Fuels_model_code_ppc.txt")

stan.data.real<-list("nLocs"=meta_dat$nLocs, "nYears"=meta_dat$nYears, "nMiss"=meta_dat$nMiss, "nOutputs"=meta_dat$nOutputs,
                     "P0"=meta_dat$P0,"P"=meta_dat$prod_data_vec,"first_val_vec"=as.data.frame(meta_dat$first_val_vec), "S"=sample_data,
                     "fuel_obs_index"=as.data.frame(meta_dat$fuels_obs_index), "prod_index"=as.data.frame(meta_dat$prod_index), "O"=fuel_data_z2)
                     
#fit1<-stan(model_code=code,data=stan.data.real,control=list(adapt_delta=.8, max_treedepth=10),warmup=10000,iter=20000, chains=4, save_warmup=F)
fit1<-stan(model_code=code,data=stan.data.real,control=list(adapt_delta=.8, max_treedepth=10),warmup=5000,iter=10000, chains=4, save_warmup=F)

saveRDS(fit1, "model_outputs/fuels_model_ppc.rds")
print(paste0("divergent iterations: " , sum(get_divergent_iterations(fit1))))
print(summary(fit1)[[1]][seq(1,5,1),c(1,10)])


