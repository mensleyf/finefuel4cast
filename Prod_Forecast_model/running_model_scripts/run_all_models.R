# This script creates a forecast for the null, forecast, clim, and clim_ndvi models using data from years 1987-2021

## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,data.table,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,glmnet,mvtnorm, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

model_df <- fread(file="gee_4cast_data/model_csvs/march_all_model_csv.csv",header=T)
model_df <- as.data.frame(model_df)

# ---------------------------------------------- checking/cleaning ------------------------------------------------

# add an interaction term
model_df$z_prOctMar_z_ssm <- model_df$z_pr_Oct2Mar*model_df$z_ssm


# null model: linear null/spatial vars and previous year veg. data
mod1_data<-subset(model_df, select=c(z_agb,prev_z_agb,year,
                                     prev_pfg_cover,z_ssm,z_bulk_dens))

# early season forecast
mod2_data<-subset(model_df, select=c(z_agb,prev_z_agb,year, z_pr_Oct2Mar,z_tmmx_Oct2Mar,z_vpd_Oct2Mar,z_ndvi_Feb,pr_frac,
                                 prev_pfg_cover,z_ssm,z_bulk_dens,z_prOctMar_z_ssm))

# full growing season data used
mod3_data<-subset(model_df, select=c(z_agb,prev_z_agb,year, z_pr_annual,z_tmmx_annual,z_vpd_annual,z_ndvi_annual,
                                     prev_pfg_cover,z_ssm,z_bulk_dens,z_prOctMar_z_ssm))


##mod4: include late season info with additional climate and ndvi 
## skip this one, not as good as mod3
#mod4_data<-subset(model_df, select=c(z_agb,prev_z_agb,year, z_pr_Oct2Mar,z_tmmx_Oct2Mar,z_vpd_Oct2Mar,z_ndvi_Feb,pr_frac,
#                                     z_pr_Apr2Sep,z_tmmx_Apr2Sep,z_vpd_Apr2Sep,z_ndvi_Apr2Sep,
#                                     prev_pfg_cover,z_ssm,z_bulk_dens,z_prOctMar_z_ssm))


(nLocs<-nrow(mod1_data)/34)

coord_df <-data.frame(model_df[model_df$year==1987,c("location","long","lat")])

#check:
nLocs==nrow(coord_df)

## various models and save locs
mod_data_list<-list(mod1_data,mod2_data,mod3_data)

save_loc1<-"/figures/march_vis/null/"
save_loc2<-"/figures/march_vis/forecast/"
save_loc3<-"/figures/march_vis/lateseason/"
save_loc_list<-list(save_loc1,save_loc2,save_loc3)


save_name1<-"null"
save_name2<-"march_forecast"
save_name3<-"lateseason"
save_name_list<-list(save_name1,save_name2,save_name3)

# -------------- Loop ----------------

# loop over models
for (i in 1:3){
  
  mod_data<-mod_data_list[[i]]
  save_loc<-save_loc_list[[i]]
  save_name<-save_name_list[[i]]

  # loop over years (leave one year out each time)
  # store residuals, data, predictions in wide format
  resid_m<-matrix(NA,nrow=nLocs,ncol=34)
  agb_m<-matrix(NA,nrow=nLocs,ncol=34)
  Ypred_m<-matrix(NA,nrow=nLocs,ncol=34)  
  yrCounter <- 0
  
  for(iYr in 1987:2020){
    
    yrCounter <- yrCounter+1
    
    # separate test and training data, drop year column
    trainD <- mod_data[which(mod_data$year!=iYr),-which(names(mod_data)=="year")]
    testD <- mod_data[which(mod_data$year==iYr),-which(names(mod_data)=="year")]
    
    mod_fit<-lm(z_agb~.,data=trainD) 
    
    # observations, predictions, errors for test set
    agb_m[,yrCounter]<-testD$z_agb
    Ypred_m[,yrCounter]<-predict(mod_fit,new=testD)
    resid_m[,yrCounter]<-agb_m[,yrCounter] - Ypred_m[,yrCounter]
    
    print(paste("model",i,iYr))
    flush.console()
    
  }
  
  # save model object for the forecast model only (not null or late season models)
  if(i==2){
    # refit using all years (we will use this fit to forecast 2021)
    trainD <- mod_data[,-which(names(mod_data)=="year")]
    mod_fit<-lm(z_agb~.,data=trainD)
  }else(
    mod_fit <- NA
  )
  
  prod_preds<-list(Ypred_m, agb_m,resid_m,  coord_df, mod_fit)
  names(prod_preds)<-c("prod_mean", "agb_m","resids", "coord_df", "mod_fit")
  
  saveRDS(prod_preds, paste0("prod_model_outputs/", save_name,".rds"))

}

# clean up
rm("agb_m","i","iYr","mod_data","mod_data_list","mod_fit","mod1_data","mod2_data",     
   "mod3_data", "model_df","nLocs","prod_preds","resid_m",       
   "save_loc" ,"save_loc_list","save_loc1","save_loc2","save_loc3",     
   "save_name", "save_name_list", "save_name1","save_name2","save_name3",  
   "testD", "trainD" ,"Ypred_m", "yrCounter"  )

