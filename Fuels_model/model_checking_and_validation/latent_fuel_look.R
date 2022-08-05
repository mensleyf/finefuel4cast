##--------------- data, packages --------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)


meta_dat<-readRDS("fuels_model_data/fuels_model_meta_data.rds")

#additional data to run model only on locations and years with few missing data points
row_keeps<-meta_dat$row_keeps
year_length<-as.data.frame(meta_dat$year_length)
sample_data<-as.data.frame(dat$samp[row_keeps])

##--------------- extract posteriors from model --------------------------------
fit1<-readRDS( "model_outputs/fuels_model.rds")
lat_fuel<-rstan::extract(fit1, permuted=T)$Fvec
dim(lat_fuel)
lat_fuel_vec<-apply(lat_fuel,2,mean)

fuel_lat<-matrix(NA,nrow=148,ncol=25)
nOutputs<-1165

p_loc_row <- 0; 
p_loc_col <- 2; 
table(meta_dat$first_val_vec)

count_mat=1
count_vec=1

year_length<-meta_dat$year_length[,1]
for (x in 1:148){ 
    fuel_lat[x,1:year_length[x]]<-lat_fuel_vec[count_vec:(count_vec+year_length[x]-1)]
    count_vec=count_vec+year_length[[x]]
    print(count_vec)
 
}



plot(fuel_lat[1,],pch=19)
#have to re-format fuels data because of NAs
#this takes first >2 stretch for each site
fuels_m<-matrix(NA,nrow=nrow(fuel_lat),ncol=25)
make_m<-function(fuel_data2){
  M<-matrix(data=NA,nrow=nrow(fuel_data2), ncol = ncol(fuel_data2))
  
  for (i in 1:nrow(fuel_data2)){
    for (j in 1:ncol(fuel_data2)){
      M[i,j]<-ifelse(is.na(fuel_data2[i,j]), NA, paste(i,",",j))
    }
  }
  
  M2<-as.vector(t(M))
  M3<-(subset(M2, !is.na(M2)))
  M4<-matrix(data=NA, nrow=length(M3),ncol=2)
  rows<-sapply(strsplit(M3, ","), "[", 1)
  cols<-sapply(strsplit(M3, ","), "[", 2)
  M<-cbind(rows,cols)
  Mrow<-as.vector(as.integer(M[,1]))
  Mcol<-as.vector(as.integer(M[,2]))
  M_final<-as.array(cbind(as.integer(Mrow),as.integer(Mcol)))
  return(M_final)
}
M<-make_m(fuel_lat)
fuel_pacf<-matrix(NA,nrow=nrow(fuel_lat), ncol=25); fuel_acf<-fuel_pacf

for ( i in 1:nrow(fuel_lat)){
  M2<-M[M[,1]==i,]
  cols<-M2[,2]
  #continuous
  if ((cols[length(cols)]-cols[1]+1) ==length(cols)){
    numNA<-25-length(fuel_lat[i,cols])
    fuels_m[i,]<-c(fuel_lat[i,cols], rep(NA,numNA))
  }
  if (!(cols[length(cols)]-cols[1]+1) ==length(cols)){
    find_miss<-(seq(cols[1],cols[length(cols)],1 ) %in% cols)
    stop_here<-which(find_miss == FALSE)[[1]]
    print(paste0("i: ", i , " stop:  ", stop_here))
    M3<-M2[1:stop_here,]
    cols<-M3[,2]
    numNA<-25-length(fuel_lat[i,cols])
    fuels_m[i,]<-c(fuel_lat[i,cols], rep(NA,numNA))
  }
}

# View(fuels_m)
for ( i in 1:nrow(fuel_lat) ){
  vals_pacf<-pacf(na.omit(fuels_m[i,]))$acf
  vals_acf<-acf(na.omit(fuels_m[i,]))$acf
  numNA<-(25-length(vals_pacf))
  numNA2<-(25-length(vals_acf))
  fuel_pacf[i,1:25]<-c(vals_pacf, rep(NA,numNA))
  fuel_acf[i,1:25]<-c(vals_acf, rep(NA,numNA2))
}


par(mfrow=c(1,1))
# pacf
plot(xlim=c(0,11), ylim=c(-1,1), "n", main="Fine fuels autocorrelation by site",
     xaxs="i", xlab="lag years", ylab="autocorrelation",
     cex.main=3, cex.lab=2)
for( i in 1:148){
  lines(0:11,fuel_acf[i,1:12], "l",col="chartreuse3")
  # lines(1:12,fuel_acf[i,1:12], "l", col=fo_cols_df2[i,3])
  
}
lines(0:11,apply(fuel_acf,2,mean,na.rm=TRUE)[1:12],col="darkgreen",lwd=3)
lines(0:11,apply(fuel_acf,2,quantile,.05,na.rm=TRUE)[1:12],col="darkgreen",lty=2,lwd=3)
lines(0:11,apply(fuel_acf,2,quantile,.95,na.rm=TRUE)[1:12],col="darkgreen",lty=2,lwd=3)

legend("topright", legend=c("fuel autocorrelation per site", "mean fuel autocorrelation", 
                            "90% CI"), col=c("chartreuse3","darkgreen", "darkgreen"),
       lty=c(1,1,2), lwd=c(3,5,3), cex=2)



