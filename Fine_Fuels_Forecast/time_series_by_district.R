
#---------- loading pcks, data, model-------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext, plotrix,RColorBrewer,rgdal, raster, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

##--------- extracting information from Prod_Forecast model ------------------

# load simulations
latent_forecast_perc<-readRDS("Fine_Fuels_Forecast/output_data/latent_fuel_perc_sp.rds")

#districts to plot
districts<-c("ARIZONA STRIP DISTRICT OFFICE", "Winnemucca District", "West Desert" , "Idaho Falls District Office")
district_titles <- c("Arizona Strip", "Winnemucca", "West Desert" , "Idaho Falls")

png("Fine_Fuels_Forecast/Figures/time_series_districts.png",height=6,width=7.5,units="in",res=400)

par(mfrow=c(2,2),mar=c(2,2,2,1),mgp=c(2,0.5,0),oma=c(2,2,0,0),tcl=-0.2)

for(i in 1:4){

  hi<-subset(latent_forecast_perc$CI90, latent_forecast_perc$CI90$PARENT_N==districts[i])
  lo<-subset(latent_forecast_perc$CI10, latent_forecast_perc$CI10$PARENT_N==districts[i])
  est<-subset(latent_forecast_perc$mean, latent_forecast_perc$mean$PARENT_N==districts[i])

  plot(x=seq(1998,2021), y=apply(lo[,1:24], 2, mean ,na.rm=T), col="red", lty=2,lwd=1, type="l", ylim=c(-100,100),
       main = district_titles[i], ylab="", xlab="")
  lines(x=seq(1998,2021), y=apply(hi[,1:24], 2, mean ,na.rm=T), col="red", lwd=1,lty=2)
  lines(x=seq(1998,2021), y=apply(est[,1:24], 2, mean ,na.rm=T), col="red",lwd=2)
  abline(h=0, lty=2)

}

mtext("Year",side=1,line=0.5,outer=T,cex=1.1)
mtext("% of normal",side=2,line=0.5,outer=T,cex=1.1)

dev.off()

#cleanup
rm(est,hi,lo,latent_forecast_perc,i,district_titles,districts)

#to see all
# par(mfrow=c(1,1))
# for ( d in districts){
#   
#   hi<-as.data.frame(subset(Fspin_iters_90_sp, Fspin_iters_90_sp$district==d)[,1:24])
#   lo<-as.data.frame(subset(Fspin_iters_10_sp, Fspin_iters_10_sp$district==d)[,1:24])
#   est<-subset(Fspin_iters_est_sp, Fspin_iters_est_sp$district==d)[,1:24]
#   
#   lo<-apply(lo, 2, as.numeric ,na.rm=T)
#   hi<-apply(hi, 2, as.numeric ,na.rm=T)
#   est<-apply(est, 2, as.numeric ,na.rm=T)
#   
#   plot(x=seq(1998,2021), y=apply(lo, 2, mean ,na.rm=T), col="red", lty=2, type="l", ylim=c(-100,100),
#        main = paste0(d),lwd=3, cex.lab=2,
#        ylab="% above long-term normal",cex=1.5,
#        xlab="Year")
#   lines(x=seq(1998,2021), y=apply(hi, 2, mean ,na.rm=T), col="red", lty=2,lwd=2)
#   lines(x=seq(1998,2021), y=apply(est, 2, mean ,na.rm=T), col="red",lwd=2)
#   lines(x=seq(1998,2021), y=rep(0,24) , lty=2)
#   legend("topright", legend= c("10% CI", "90% CI"), col="red", lty=2, lwd=2, cex=2)
#   
# }

