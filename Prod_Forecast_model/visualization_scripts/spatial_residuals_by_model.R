## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,glmnet,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

march_predictions<-readRDS("prod_model_outputs/march_forecast.rds")

blm_states<-readOGR( "gee_4cast_data/gis_data/states.shp")
blm_admin<-readOGR("gee_4cast_data/gis_data/admin.shp")

nYears<-34
# nYears<-1
# i<-1
## ---------------------------------------- Variograms ---------------------------------

# for (i in 1:nYears){
#   print(1986+i)
#   cur_df<-as.data.frame(cbind(march_predictions[[3]][,i], march_predictions[[4]]))
#   names(cur_df)<-c("resid", "long", "lat")
#   name<-paste0("Variogram: test year = ", 1986+i)
#   name2<-paste0("Vgram", 1986+i)
#   Vout<-variogram(cur_df$resid~1,loc=~lat+long, data=cur_df,width=.01)
#   # png(paste0(vis_fold, "/march/",name2, ".png"))
#   print(plot(Vout, main=name))
#   # dev.off()
# }

## ---------------------------------------- per pixel residuals by year ---------------------------------

#low/hi resid vals

save_loc<-"figures/model_residuals"

# set up color ramp
cuts <- seq(-6,6,1)
pal <- colorRampPalette(c("red","lightgray","blue"))

# loop over years
for (i in 1:nYears){
  
  cur_df<-as.data.frame(cbind(march_predictions[[3]][,i], march_predictions[[4]][,2:3]))
  names(cur_df)<-c("resid", "long", "lat")
  
  print(i+1986)
  
  # raster plot
  cur_raster<- rasterFromXYZ(cur_df[,c("long","lat","resid")])
  
  name<-paste0("resids",i+1986)
  
  png(paste0("figures/model_residuals/", name, ".png"),height=5,width=5,units="in",res=400)
  
  par(tcl=-0.2)
   
  plot(cur_raster,xlab="Longitude",ylab="Latitude",
       breaks=cuts,col=pal(length(cuts)),
       main=i+1986)
  
  dev.off()
  
}

 
 
