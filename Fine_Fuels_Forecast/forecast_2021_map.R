#makes 3 spatial maps; mean and 80% confidence interval of 2021 forecast

#---------- loading pcks, data, model-------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer,rgdal, raster, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

# import latent fine fuel forecast (% of normal)
fuelcast_perc <- readRDS("Fine_Fuels_Forecast/output_data/latent_fuel_perc_sp.rds") 

# import map features
#blm_states<-readOGR("Prod_Forecast_model/gee_4cast_data/gis_data/states.shp")
#blm_admin<-readOGR("Prod_Forecast_model/gee_4cast_data/gis_data/admin.shp")


##--------------------------- MAPS!!! -------------------------


# make standardized set of colors for this year's maps
cuts <- seq(-250,200,25)
pal <- colorRampPalette(c("blue","lightgray","red"))

png("Fine_Fuels_Forecast/Figures/forecast2021.png",height=3,width=8,res=400,units="in")

par(mfrow=c(1,3),mar=c(3,4,3,1),oma=c(0,0,0,2),tcl=-0.2,mgp=c(2,0.5,0))

# map 10%ile
cur_raster<- rasterFromXYZ(fuelcast_perc$CI10[,c("long","lat","pred2021")])
plot(cur_raster,xlab="Longitude",ylab="Latitude",
     breaks=cuts,col=pal(length(cuts)),
     main="10th percentile",legend=F)
plot(cur_raster, legend.only=TRUE, breaks=cuts,col=pal(length(cuts)),
     axis.args=list(at=cuts, cex.axis=0.8),
     legend.args=list(text='%', side=3,cex=0.8))

# map mean
cur_raster<- rasterFromXYZ(fuelcast_perc$mean[,c("long","lat","pred2021")])
plot(cur_raster,xlab="Longitude",ylab="Latitude",
     breaks=cuts,col=pal(length(cuts)),
     main="Mean",legend=F)
plot(cur_raster, legend.only=TRUE, breaks=cuts,col=pal(length(cuts)),
     axis.args=list(at=cuts, cex.axis=0.8),
     legend.args=list(text='%', side=3,cex=0.8))

# map 90%ile
cur_raster<- rasterFromXYZ(fuelcast_perc$CI90[,c("long","lat","pred2021")])
plot(cur_raster,xlab="Longitude",ylab="Latitude",
     breaks=cuts,col=pal(length(cuts)),
     main="90th percentile",legend=F)
plot(cur_raster, legend.only=TRUE, breaks=cuts,col=pal(length(cuts)),
     axis.args=list(at=cuts, cex.axis=0.8),
     legend.args=list(text='%', side=3,cex=0.8))

dev.off()

# cleanup
rm(cur_raster, fuelcast_perc, cuts, pal)


