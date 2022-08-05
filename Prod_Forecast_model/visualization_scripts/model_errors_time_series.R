#loading data and pcks
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr, gstat,stringr,raster,rstudioapi, rgdal, spacetime,ape,FRK,glmnet,install = TRUE, update = getOption("pac_update"), character.only = FALSE)

## ---------------------------------------- MAPE,MSPE,MBIAS ---------------------------------
options(scipen = 999)
null_predictions<-readRDS( "prod_model_outputs/null.rds")
march_predictions<-readRDS("prod_model_outputs/march_forecast.rds")
lateseason_predictions<-readRDS("prod_model_outputs/lateseason.rds")

df_names<-paste0("pred", rep(seq(1987,2020,1)))

resids_null_mat<-null_predictions[[3]]; names(resids_null_mat)<-df_names
resids_march_mat<-march_predictions[[3]]; names(resids_march_mat)<-df_names
resids_lateseason_mat<-lateseason_predictions[[3]]; names(resids_lateseason_mat)<-df_names

R2_fnc<-function(y,resids) {1-(var(as.numeric(resids))/var(as.numeric(y)))}
mape_fnc<-function(data) {mean(as.vector(abs(data)))}
mspe_fnc<-function(data) {mean(as.vector(data)^2)}
mbias_fnc<-function(data) {mean(as.vector(data))}

null_R2 <- R2_fnc(null_predictions$agb_m,null_predictions$resids)
null_mape<-apply(X= resids_null_mat,2, FUN=mape_fnc)
null_mspe<-apply(X= resids_null_mat,2, FUN=mspe_fnc)
null_mbias<-apply(X= resids_null_mat,2, FUN=mbias_fnc)

march_R2 <- R2_fnc(march_predictions$agb_m,march_predictions$resids)
march_mape<-apply(X= resids_march_mat,2, FUN=mape_fnc)
march_mspe<-apply(X= resids_march_mat,2, FUN=mspe_fnc)
march_mbias<-apply(X= resids_march_mat,2, FUN=mbias_fnc)

lateseason_R2 <- R2_fnc(lateseason_predictions$agb_m,lateseason_predictions$resids)
lateseason_mape<-apply(X= resids_lateseason_mat,2, FUN=mape_fnc)
lateseason_mspe<-apply(X= resids_lateseason_mat,2, FUN=mspe_fnc)
lateseason_mbias<-apply(X= resids_lateseason_mat,2, FUN=mbias_fnc)

compare_R2 <- cbind(null_R2,march_R2,lateseason_R2)
print(compare_R2)

mape<-cbind(null_mape,march_mape,lateseason_mape)
print(colMeans(mape))

mspe<-cbind(null_mspe,march_mspe,lateseason_mspe)
colMeans(mspe)

mbias<-cbind(null_mbias,march_mbias,lateseason_mbias)
colMeans(mbias)

# Figure 4
png( "figures/time_series/mae.png",height=3,width=4,res=400,units="in")
par(mfrow=c(1,1),tcl=-0.2,mar=c(3,4,1,1),mgp=c(2,0.5,0))
plot(1987:2020,mape[,1], 'l',ylim=c(.38,1.8), col="black", cex.main=1.2,
     lwd=1.5,
     ylab= "Mean Absolute Error",
     xlab="Year", cex.lab=1.1, cex.axis=1) 
lines(1987:2020,mape[,2], 'l', col="blue",lwd=1.5)
lines(1987:2020,mape[,3], 'l', col="red",lwd=1.5)
legend("top",legend=c("Null", "Early-season", "End-of-season"), 
       col=c("black","blue","red"), cex=1, bty = "n", lty=1, lwd=1.5)
dev.off()

# something but with squared error
png( "figures/time_series/mse.png",height=3,width=4,res=400,units="in")
par(mfrow=c(1,1),tcl=-0.2,mar=c(3,4,1,1),mgp=c(2,0.5,0))
plot(1987:2020,mspe[,1], 'l',ylim=c(0,3.5), col="black", cex.main=1.2,
     lwd=1.5,
     ylab= "Mean Squared Error",
     xlab="Year", cex.lab=1.1, cex.axis=1) 
lines(1987:2020,mspe[,2], 'l', col="blue",lwd=1.5)
lines(1987:2020,mspe[,3], 'l', col="red",lwd=1.5)
legend("top",legend=c("Null", "Early-season", "End-of-season"), 
       col=c("black","blue","red"), cex=0.9, bty = "n", lty=1, lwd=1.5)
dev.off()

# clean up 
tmp <-ls()
rm(tmp) # careful!


