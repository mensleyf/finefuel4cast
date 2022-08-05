#This script makes hindcasts for prod data from 1987-2020 by district

#---------- loading pcks, data, model-------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer,rgdal, raster, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

##--------- extracting information from Prod_Forecast model ------------------

district_coords<-read.csv("./../Fine_Fuels_Forecast/output_data/district_coords.csv" )
district_coords <- district_coords[,-1] # drop useless first column

tmp<-readRDS("prod_model_outputs/march_forecast.rds")
prod_forecast <- as.data.frame(cbind(tmp$coord_df$location,tmp$prod_mean))
names(prod_forecast) <- c("location",c(1987:2020))

prod_forecast <- merge(prod_forecast,district_coords[,c("location","PARENT_N")])

proc_err <- summary(tmp$mod_fit)$sigma
lower_CI <- qnorm(0.1,0,proc_err)  # lower 80% confidence limit is this much below the mean
upper_CI <- qnorm(0.9,0,proc_err)  # upper 80% confidence limit is this much abpve the mean

rm(tmp)

## draw figure

#districts to plot
districts<-c("ARIZONA STRIP DISTRICT OFFICE", "Winnemucca District", "West Desert" , "Idaho Falls District Office")
district_titles <- c("Arizona Strip", "Winnemucca", "West Desert" , "Idaho Falls")

png("figures/time_series/district_time_series.png",height=6,width=7.5,units="in",res=400)

par(mfrow=c(2,2),mar=c(2,2,2,1),mgp=c(2,0.5,0),oma=c(2,2,0,0),tcl=-0.2)

for(i in 1:4){
  
  dat<-subset(prod_forecast, prod_forecast$PARENT_N==districts[i])
  year_means <- colMeans(dat[,2:35])
  plot(x=c(1987:2020), y=year_means, col="red", lty=1,lwd=2, type="l", ylim=c(-2,3),
       main = district_titles[i], ylab="", xlab="")
  lines(c(1987:2020), y=year_means+lower_CI, col="red", lwd=1,lty=2)
  lines(c(1987:2020), y=year_means+upper_CI, col="red", lwd=1,lty=2)
  abline(h=0, lty=2)
  
} # next i

mtext("Year",side=1,line=0.5,outer=T,cex=1.1)
mtext("z-score",side=2,line=0.5,outer=T,cex=1.1)

dev.off()

rm(dat,district_coords,district_titles,districts,prod_forecast,year_means,lower_CI,upper_CI,
   i,proc_err)


