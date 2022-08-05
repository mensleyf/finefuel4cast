# This combines RAP, spatial, and temporal data csvs and splits, does some seasonal temporal aggregation, and splits dataframes into 1987-2020 and 2021
# changes format from wide to long
# aggregates monthly weather data to October-march, april and may for precipitation (pr), temperature (tmmx) and vapor pressure deficit (vpd)
# selects previous  month for ndvi
# standardizes everything

## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table,dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

prod<-data.table::fread("gee_4cast_data/prod_4cast_csvs/gee_RAP_data.csv")
prod <-as.data.frame(prod)
clim<-data.table::fread("gee_4cast_data/prod_4cast_csvs/gee_temporal_data.csv")
clim <-as.data.frame(clim)
spat<-data.table::fread("gee_4cast_data/prod_4cast_csvs/gee_spatial_data.csv")
spat <-as.data.frame(spat)

nLocs<-nrow(spat)

#memory.limit(10000000000)

## ----------------------------------- aboveground biomass (agb)  ------------------------------------------------
# prev year, and standardized

## pull out annual forbs & grasses, and perennials forbs & grasses, 
## and convert to long format
afg_data<-prod[(names(prod) %in% c(names(prod)[(grepl("afgAGB", names(prod)))]))]
pfg_data<-prod[(names(prod) %in% c(names(prod)[(grepl("pfgAGB", names(prod)))]))]

# get years from column names
tmp <- strsplit(names(afg_data),"_")
year <- as.numeric(sapply(tmp, "[", 2))

# convert to long format
agb_long <- data.frame(
  location = sort(rep(1:nLocs,ncol(afg_data))),
  year = rep(year,nLocs),
  afg = as.numeric(t(afg_data)),
  pfg = as.numeric(t(pfg_data))
)
rm(tmp,year,afg_data,pfg_data)

agb_long$agb <- agb_long$afg + agb_long$pfg # total herbaceous biomass

agb_long <- agb_long[,c("location","year","agb")] #select columns

# standardize agb
tmp <- agb_long %>% group_by(location) %>%
  summarise(mean_agb = mean(agb),sd_agb = sd(agb))
agb_long <- merge(agb_long,tmp,all.x=T)
agb_long$z_agb <- (agb_long$agb-agb_long$mean_agb)/agb_long$sd_agb
agb_data <- agb_long[,c("location","year","z_agb","mean_agb","sd_agb")]
rm(agb_long)

# add in missing data for 2021
tmp <- unique(agb_data[,c("location","mean_agb","sd_agb")],margin=2)
tmp$year <- 2021
tmp$z_agb <- NA
tmp <- tmp[,c("location","year","z_agb","mean_agb","sd_agb")] # reorder columns
agb_data <- rbind(agb_data,tmp)

# join previous year 
tmp <- agb_data
tmp <- tmp[,c("location","year","z_agb")] # drop mean column
names(tmp)[3] <- "prev_z_agb"
tmp$year <- tmp$year +1
agb_data <- merge(agb_data,tmp,all.x=T)


# ## ----------------------------------------------  fractional cover data ------------------------------------------------

cover_data<-prod[(names(prod) %in% c(names(prod)[(grepl("percent_pfg", names(prod)))]))]
cover_data<-cover_data[(names(cover_data) %in% c(names(cover_data)[!(grepl("shr", names(cover_data)))]))] 

# get years from column names
tmp <- strsplit(names(cover_data),"_")
year <- as.numeric(sapply(tmp, "[", 3))

# convert to long format
cover_long <- data.frame(
  location = sort(rep(1:nLocs,ncol(cover_data))),
  year = rep(year,nLocs),
  pfg_cover = as.numeric(t(cover_data))
)
rm(tmp,year,cover_data)

# add in missing data for 2021
tmp <- data.frame(location=unique(cover_long$location),
                  year=2021,
                  pfg_cover=NA
)                            
cover_long <- rbind(cover_long,tmp)

# add previous year
tmp <- cover_long
names(tmp)[3] <- "prev_pfg_cover"
tmp$year <- tmp$year +1
cover_data <- merge(cover_long,tmp,all.x=T)
rm(cover_long,tmp,prod)


## ---------------------------- precipitation ------------------------------------------------
pr_data<-clim
pr_data<-pr_data[(names(pr_data) %in% c(names(pr_data)[(grepl("pr", names(pr_data)))]))] #just pr

# get years and months from column names
tmp <- strsplit(names(pr_data),"_")
month <- as.numeric(sapply(tmp, "[", 2))
year <- as.numeric(sapply(tmp, "[", 3))

# convert to long format
pr_long <- data.frame(
    location = sort(rep(1:nLocs,ncol(pr_data))),
    year = rep(year,nLocs),
    month = rep(month,nLocs),
    pr = as.numeric(t(pr_data))
)
rm(tmp,month,year)

# define climate year
pr_long$clim_year <- ifelse(pr_long$month<=9,pr_long$year,pr_long$year+1)

# reformat
pr_long <- pr_long[,c("location","clim_year","month","pr")]

# aggregate early season months and standardize
pr_Oct2Mar <- pr_long %>% subset(month > 9 | month < 4) %>% 
                  group_by(location,clim_year) %>% 
                  summarise(pr_Oct2Mar = sum(pr))
# site means and standard deviations
tmp <- pr_Oct2Mar %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
            group_by(location) %>%
            summarise(mean_Oct2Mar = mean(pr_Oct2Mar),sd_Oct2Mar = sd(pr_Oct2Mar))
pr_Oct2Mar <- merge(pr_Oct2Mar,tmp,all.x=T)
pr_Oct2Mar$pr_frac <- pr_Oct2Mar$pr_Oct2Mar / pr_Oct2Mar$mean_Oct2Mar
pr_Oct2Mar$z_pr_Oct2Mar <- (pr_Oct2Mar$pr_Oct2Mar - pr_Oct2Mar$mean_Oct2Mar)/pr_Oct2Mar$sd_Oct2Mar

# aggregate late season months and standardize
pr_Apr2Sep <- pr_long %>% subset(month > 3 & month < 10) %>% 
  group_by(location,clim_year) %>% 
  summarise(pr_Apr2Sep = sum(pr))
# site means and standard deviations
tmp <- pr_Apr2Sep %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_Apr2Sep = mean(pr_Apr2Sep),sd_Apr2Sep = sd(pr_Apr2Sep))
pr_Apr2Sep <- merge(pr_Apr2Sep,tmp,all.x=T)
pr_Apr2Sep$z_pr_Apr2Sep <- (pr_Apr2Sep$pr_Apr2Sep - pr_Apr2Sep$mean_Apr2Sep)/pr_Apr2Sep$sd_Apr2Sep

# aggregate and standardize annual values
pr_annual <- pr_long %>% 
  group_by(location,clim_year) %>% 
  summarise(pr_annual = sum(pr))
# site means and standard deviations
tmp <- pr_annual %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_annual = mean(pr_annual),sd_annual = sd(pr_annual))
pr_annual <- merge(pr_annual,tmp,all.x=T)
pr_annual$z_pr_annual <- (pr_annual$pr_annual - pr_annual$mean_annual)/pr_annual$sd_annual
pr_annual <- pr_annual[,c("location","clim_year","z_pr_annual")]

# combine data sets
# cbind is faster than merge, but first confirm locations and years match
# identical(pr_annual$location, pr_Apr2Sep$location)
# identical(pr_annual$location, pr_Oct2Mar$location)
# identical(pr_annual$clim_year, pr_Apr2Sep$clim_year)
# identical(pr_annual$clim_year, pr_Oct2Mar$clim_year)
pr_data <- cbind(pr_annual,pr_Oct2Mar[,c("pr_Oct2Mar","z_pr_Oct2Mar","pr_frac")],
                 pr_Apr2Sep[c("pr_Apr2Sep","z_pr_Apr2Sep")])


#clean up
rm(pr_long,pr_Oct2Mar,pr_Apr2Sep,pr_annual,tmp)



## ---------------------------------------------- tmmx ------------------------------------------------
tmmx_data<-clim
tmmx_data<-tmmx_data[(names(tmmx_data) %in% c(names(tmmx_data)[(grepl("tmmx", names(tmmx_data)))]))] #just tmmx

# get years and months from column names
tmp <- strsplit(names(tmmx_data),"_")
month <- as.numeric(sapply(tmp, "[", 2))
year <- as.numeric(sapply(tmp, "[", 3))

# convert to long format
tmmx_long <- data.frame(
  location = sort(rep(1:nLocs,ncol(tmmx_data))),
  year = rep(year,nLocs),
  month = rep(month,nLocs),
  tmmx = as.numeric(t(tmmx_data))
)
rm(tmp,month,year,tmmx_data)

# define climate year
tmmx_long$clim_year <- ifelse(tmmx_long$month<=9,tmmx_long$year,tmmx_long$year+1)

# select and order columns
tmmx_long <- tmmx_long[,c("location","clim_year","month","tmmx")]

# aggregate early season months and standardize
tmmx_Oct2Mar <- tmmx_long %>% subset(month > 9 | month < 4) %>% 
  group_by(location,clim_year) %>% 
  summarise(tmmx_Oct2Mar = mean(tmmx))
# site means and standard deviations
tmp <- tmmx_Oct2Mar %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_Oct2Mar = mean(tmmx_Oct2Mar),sd_Oct2Mar = sd(tmmx_Oct2Mar))
tmmx_Oct2Mar <- merge(tmmx_Oct2Mar,tmp,all.x=T)
tmmx_Oct2Mar$z_tmmx_Oct2Mar <- (tmmx_Oct2Mar$tmmx_Oct2Mar - tmmx_Oct2Mar$mean_Oct2Mar)/tmmx_Oct2Mar$sd_Oct2Mar


# aggregate late season months and standardize
tmmx_Apr2Sep <- tmmx_long %>% subset(month > 3 & month < 10) %>% 
  group_by(location,clim_year) %>% 
  summarise(tmmx_Apr2Sep = mean(tmmx))
# site means and standard deviations
tmp <- tmmx_Apr2Sep %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_Apr2Sep = mean(tmmx_Apr2Sep),sd_Apr2Sep = sd(tmmx_Apr2Sep))
tmmx_Apr2Sep <- merge(tmmx_Apr2Sep,tmp,all.x=T)
tmmx_Apr2Sep$z_tmmx_Apr2Sep <- (tmmx_Apr2Sep$tmmx_Apr2Sep - tmmx_Apr2Sep$mean_Apr2Sep)/tmmx_Apr2Sep$sd_Apr2Sep

# aggregate and standardize annual values
tmmx_annual <- tmmx_long %>% 
  group_by(location,clim_year) %>% 
  summarise(tmmx_annual = mean(tmmx))
# site means and standard deviations
tmp <- tmmx_annual %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_annual = mean(tmmx_annual),sd_annual = sd(tmmx_annual))
tmmx_annual <- merge(tmmx_annual,tmp,all.x=T)
tmmx_annual$z_tmmx_annual <- (tmmx_annual$tmmx_annual - tmmx_annual$mean_annual)/tmmx_annual$sd_annual
tmmx_annual <- tmmx_annual[,c("location","clim_year","z_tmmx_annual")]

# combine data sets
# cbind is faster than merge, but first confirm locations and years match
# identical(tmmx_annual$location, tmmx_Apr2Sep$location)
# identical(tmmx_annual$location, tmmx_Oct2Mar$location)
# identical(tmmx_annual$clim_year, tmmx_Apr2Sep$clim_year)
# identical(tmmx_annual$clim_year, tmmx_Oct2Mar$clim_year)
tmmx_data <- cbind(tmmx_annual,tmmx_Oct2Mar[,c("tmmx_Oct2Mar","z_tmmx_Oct2Mar")],
                 tmmx_Apr2Sep[c("tmmx_Apr2Sep","z_tmmx_Apr2Sep")])


#clean up
rm(tmmx_long,tmmx_Oct2Mar,tmmx_Apr2Sep,tmmx_annual,tmp)


## ---------------------------------------------- vpd ------------------------------------------------
vpd_data<-clim
vpd_data<-vpd_data[(names(vpd_data) %in% c(names(vpd_data)[(grepl("vpd", names(vpd_data)))]))] #just vpd




## ---------------------------- precipitation ------------------------------------------------
pr_data<-clim
pr_data<-pr_data[(names(pr_data) %in% c(names(pr_data)[(grepl("pr", names(pr_data)))]))] #just pr

# get years and months from column names
tmp <- strsplit(names(pr_data),"_")
month <- as.numeric(sapply(tmp, "[", 2))
year <- as.numeric(sapply(tmp, "[", 3))

# convert to long format
pr_long <- data.frame(
  location = sort(rep(1:nLocs,ncol(pr_data))),
  year = rep(year,nLocs),
  month = rep(month,nLocs),
  pr = as.numeric(t(pr_data))
)
rm(tmp,month,year)

# define climate year
pr_long$clim_year <- ifelse(pr_long$month<=9,pr_long$year,pr_long$year+1)

# reformat
pr_long <- pr_long[,c("location","clim_year","month","pr")]

# aggregate early season months and standardize
pr_Oct2Mar <- pr_long %>% subset(month > 9 | month < 4) %>% 
  group_by(location,clim_year) %>% 
  summarise(pr_Oct2Mar = sum(pr))
# site means and standard deviations
tmp <- pr_Oct2Mar %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_Oct2Mar = mean(pr_Oct2Mar),sd_Oct2Mar = sd(pr_Oct2Mar))
pr_Oct2Mar <- merge(pr_Oct2Mar,tmp,all.x=T)
pr_Oct2Mar$pr_frac <- pr_Oct2Mar$pr_Oct2Mar / pr_Oct2Mar$mean_Oct2Mar
pr_Oct2Mar$z_pr_Oct2Mar <- (pr_Oct2Mar$pr_Oct2Mar - pr_Oct2Mar$mean_Oct2Mar)/pr_Oct2Mar$sd_Oct2Mar

# aggregate late season months and standardize
pr_Apr2Sep <- pr_long %>% subset(month > 3 & month < 10) %>% 
  group_by(location,clim_year) %>% 
  summarise(pr_Apr2Sep = sum(pr))
# site means and standard deviations
tmp <- pr_Apr2Sep %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_Apr2Sep = mean(pr_Apr2Sep),sd_Apr2Sep = sd(pr_Apr2Sep))
pr_Apr2Sep <- merge(pr_Apr2Sep,tmp,all.x=T)
pr_Apr2Sep$z_pr_Apr2Sep <- (pr_Apr2Sep$pr_Apr2Sep - pr_Apr2Sep$mean_Apr2Sep)/pr_Apr2Sep$sd_Apr2Sep

# aggregate and standardize annual values
pr_annual <- pr_long %>% 
  group_by(location,clim_year) %>% 
  summarise(pr_annual = sum(pr))
# site means and standard deviations
tmp <- pr_annual %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_annual = mean(pr_annual),sd_annual = sd(pr_annual))
pr_annual <- merge(pr_annual,tmp,all.x=T)
pr_annual$z_pr_annual <- (pr_annual$pr_annual - pr_annual$mean_annual)/pr_annual$sd_annual
pr_annual <- pr_annual[,c("location","clim_year","z_pr_annual")]

# combine data sets
# cbind is faster than merge, but first confirm locations and years match
# identical(pr_annual$location, pr_Apr2Sep$location)
# identical(pr_annual$location, pr_Oct2Mar$location)
# identical(pr_annual$clim_year, pr_Apr2Sep$clim_year)
# identical(pr_annual$clim_year, pr_Oct2Mar$clim_year)
pr_data <- cbind(pr_annual,pr_Oct2Mar[,c("pr_Oct2Mar","z_pr_Oct2Mar","pr_frac")],
                 pr_Apr2Sep[c("pr_Apr2Sep","z_pr_Apr2Sep")])


#clean up
rm(pr_long,pr_Oct2Mar,pr_Apr2Sep,pr_annual,tmp)



## ---------------------------------------------- vpd ------------------------------------------------
vpd_data<-clim
vpd_data<-vpd_data[(names(vpd_data) %in% c(names(vpd_data)[(grepl("vpd", names(vpd_data)))]))] #just tmmx

# get years and months from column names
tmp <- strsplit(names(vpd_data),"_")
month <- as.numeric(sapply(tmp, "[", 2))
year <- as.numeric(sapply(tmp, "[", 3))

# convert to long format
vpd_long <- data.frame(
  location = sort(rep(1:nLocs,ncol(vpd_data))),
  year = rep(year,nLocs),
  month = rep(month,nLocs),
  vpd = as.numeric(t(vpd_data))
)
rm(tmp,month,year,vpd_data)

# define climate year
vpd_long$clim_year <- ifelse(vpd_long$month<=9,vpd_long$year,vpd_long$year+1)

# select and order columns
vpd_long <- vpd_long[,c("location","clim_year","month","vpd")]

# aggregate early season months and standardize
vpd_Oct2Mar <- vpd_long %>% subset(month > 9 | month < 4) %>% 
  group_by(location,clim_year) %>% 
  summarise(vpd_Oct2Mar = mean(vpd))
# site means and standard deviations
tmp <- vpd_Oct2Mar %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_Oct2Mar = mean(vpd_Oct2Mar),sd_Oct2Mar = sd(vpd_Oct2Mar))
vpd_Oct2Mar <- merge(vpd_Oct2Mar,tmp,all.x=T)
vpd_Oct2Mar$z_vpd_Oct2Mar <- (vpd_Oct2Mar$vpd_Oct2Mar - vpd_Oct2Mar$mean_Oct2Mar)/vpd_Oct2Mar$sd_Oct2Mar

# aggregate late season months and standardize
vpd_Apr2Sep <- vpd_long %>% subset(month > 3 & month < 10) %>% 
  group_by(location,clim_year) %>% 
  summarise(vpd_Apr2Sep = mean(vpd))
# site means and standard deviations
tmp <- vpd_Apr2Sep %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_Apr2Sep = mean(vpd_Apr2Sep),sd_Apr2Sep = sd(vpd_Apr2Sep))
vpd_Apr2Sep <- merge(vpd_Apr2Sep,tmp,all.x=T)
vpd_Apr2Sep$z_vpd_Apr2Sep <- (vpd_Apr2Sep$vpd_Apr2Sep - vpd_Apr2Sep$mean_Apr2Sep)/vpd_Apr2Sep$sd_Apr2Sep

# aggregate and standardize annual values
vpd_annual <- vpd_long %>% 
  group_by(location,clim_year) %>% 
  summarise(vpd_annual = mean(vpd))
# site means and standard deviations
tmp <- vpd_annual %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_annual = mean(vpd_annual),sd_annual = sd(vpd_annual))
vpd_annual <- merge(vpd_annual,tmp,all.x=T)
vpd_annual$z_vpd_annual <- (vpd_annual$vpd_annual - vpd_annual$mean_annual)/vpd_annual$sd_annual
vpd_annual <- vpd_annual[,c("location","clim_year","z_vpd_annual")]

# combine data sets
# cbind is faster than merge, but first confirm locations and years match
# identical(vpd_annual$location, vpd_Apr2Sep$location)
# identical(vpd_annual$location, vpd_Oct2Mar$location)
# identical(vpd_annual$clim_year, vpd_Apr2Sep$clim_year)
# identical(vpd_annual$clim_year, vpd_Oct2Mar$clim_year)
vpd_data <- cbind(vpd_annual,vpd_Oct2Mar[,c("vpd_Oct2Mar","z_vpd_Oct2Mar")],
                   vpd_Apr2Sep[c("vpd_Apr2Sep","z_vpd_Apr2Sep")])


#clean up
rm(vpd_long,vpd_Oct2Mar,vpd_Apr2Sep,vpd_annual,tmp)

## ---------------------------------------------- ndvi ------------------------------------------------

ndvi_data<-clim
ndvi_data<-ndvi_data[(names(ndvi_data) %in% c(names(ndvi_data)[(grepl("ndvi", names(ndvi_data)))]))] #just vpd

# get years and months from column names
tmp <- strsplit(names(ndvi_data),"_")
month <- as.numeric(sapply(tmp, "[", 2))
year <- as.numeric(sapply(tmp, "[", 3))

# convert to long format
ndvi_long <- data.frame(
  location = sort(rep(1:nLocs,ncol(ndvi_data))),
  year = rep(year,nLocs),
  month = rep(month,nLocs),
  ndvi = as.numeric(t(ndvi_data))
)
rm(tmp,month,year,ndvi_data)

# define climate year
ndvi_long$clim_year <- ifelse(ndvi_long$month<=9,ndvi_long$year,ndvi_long$year+1)

# select and order columns
ndvi_long <- ndvi_long[,c("location","clim_year","month","ndvi")]

# aggregate early spring months and standardize
ndvi_Feb <- ndvi_long %>% subset(month == 2) 
# site means and standard deviations
tmp <- ndvi_Feb %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_Feb = mean(ndvi,na.rm=TRUE),sd_Feb = sd(ndvi,na.rm=TRUE))
ndvi_Feb <- merge(ndvi_Feb,tmp,all.x=T)
ndvi_Feb$z_ndvi_Feb <- (ndvi_Feb$ndvi - ndvi_Feb$mean_Feb)/ndvi_Feb$sd_Feb
names(ndvi_Feb)[4] <- "ndvi_Feb"

# aggregate late season months and standardize
ndvi_Apr2Sep <- ndvi_long %>% subset(month > 3 & month < 10) %>% 
  group_by(location,clim_year) %>% 
  summarise(ndvi_Apr2Sep = mean(ndvi,na.rm=TRUE))
# site means and standard deviations
tmp <- ndvi_Apr2Sep %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_Apr2Sep = mean(ndvi_Apr2Sep),sd_Apr2Sep = sd(ndvi_Apr2Sep))
ndvi_Apr2Sep <- merge(ndvi_Apr2Sep,tmp,all.x=T)
ndvi_Apr2Sep$z_ndvi_Apr2Sep <- (ndvi_Apr2Sep$ndvi_Apr2Sep - ndvi_Apr2Sep$mean_Apr2Sep)/ndvi_Apr2Sep$sd_Apr2Sep

# aggregate and standardize annual values
ndvi_annual <- ndvi_long %>% subset(month > 1 & month < 9) %>% 
  group_by(location,clim_year) %>% 
  summarise(ndvi_annual = mean(ndvi,na.rm=TRUE))
# site means and standard deviations
tmp <- ndvi_annual %>% subset(clim_year > 1979 & clim_year < 2021) %>% # drop incomplete years
  group_by(location) %>%
  summarise(mean_annual = mean(ndvi_annual),sd_annual = sd(ndvi_annual))
ndvi_annual <- merge(ndvi_annual,tmp,all.x=T)
ndvi_annual$z_ndvi_annual <- (ndvi_annual$ndvi_annual - ndvi_annual$mean_annual)/ndvi_annual$sd_annual
ndvi_annual <- ndvi_annual[,c("location","clim_year","z_ndvi_annual")]

# combine data sets
# cbind is faster than merge, but first confirm locations and years match
# identical(ndvi_annual$location, ndvi_Apr2Sep$location)
# identical(ndvi_annual$location, ndvi_Feb$location)
# identical(ndvi_annual$clim_year, ndvi_Apr2Sep$clim_year)
# identical(ndvi_annual$clim_year, ndvi_Feb$clim_year)
ndvi_data <- cbind(ndvi_annual,ndvi_Feb[,c("ndvi_Feb","z_ndvi_Feb")],
                  ndvi_Apr2Sep[c("ndvi_Apr2Sep","z_ndvi_Apr2Sep")])


#clean up
rm(ndvi_long,ndvi_Feb,ndvi_Apr2Sep,ndvi_annual,tmp,clim)


## ---------------------------------------------- Spatial data ------------------------------------------------

#coords, elev, soil
spat$location <- 1:nrow(spat)



############################################# merge into one 'model_df' #########################

# combine spatial data and productivity data 
model_df <- merge(spat, agb_data,all=T)

# add in cover data       
model_df <- merge(model_df,cover_data,all=T)

# add in precipitation data
names(pr_data)[2] <- "year" # rename for consistency
model_df <- merge(model_df,pr_data,all=T)

# add in temperature data
names(tmmx_data)[2] <- "year" # rename for consistency
model_df <- merge(model_df,tmmx_data,all=T)

# add in vpd data
names(vpd_data)[2] <- "year" # rename for consistency
model_df <- merge(model_df,vpd_data,all=T)

# add in ndvi data
names(ndvi_data)[2] <- "year" # rename for consistency
model_df <- merge(model_df,ndvi_data,all=T)

#clean up
rm(agb_data,cover_data,ndvi_data,pr_data,tmmx_data,vpd_data)

# # check for weird and missing values
# model_df_key<-data.frame()
# for (i in 1:ncol(model_df)){
#   model_df_key[i,1]<-names(model_df)[i]
#   model_df_key[i,2]<-round(min(model_df[,i], na.rm=TRUE),2) 
#   model_df_key[i,3]<-round(max(model_df[,i], na.rm=TRUE),2)
#   model_df_key[i,4]<-round(mean(model_df[,i], na.rm=TRUE) ,2)
#   model_df_key[i,5]<-round(sd(model_df[,i], na.rm=TRUE) ,2)
#   model_df_key[i,6]<-sum(is.na(model_df[,i]))
# }
# names(model_df_key)<-c("name", "min", "max", "mean", "sd", "count_NA")


## --------------------- cleaning and subsetting ----------------------------------

# remove NAs for all years except 2021
model_df2021<-subset(model_df, model_df$year==2021)
model_df <- subset(model_df, model_df$year<2021)
model_df<-na.omit(model_df)

# remove very low productivity locations
model_df<-subset(model_df, model_df$mean_agb>log(100))

# add 2021 back in
model_df<-rbind(model_df, model_df2021)
rm(model_df2021)

# subsetting coordinates with no missing years (missing due to NAs)
tmp <- table(model_df$location)
keeps<-which(tmp==35)
model_df<-subset(model_df, model_df$location %in% as.vector(keeps))


# (nLocs<-nrow(model_df2)/35)

#coords
coord_df<-as.data.frame(model_df[,c("long","lat")])
coord_df<-unique(coord_df,margin=2)

## old stuff...
# loc_keeps<-as.data.frame(cbind(model_df2$long,model_df2$lat,model_df2$yr, model_df2$row_keeps ))
# names(loc_keeps)<-c("long", "lat", "yr", "row_keeps")
# write.csv(loc_keeps,  "prod_model_outputs/loc_keeps.csv")

# names(loc_keeps)<-c("long", "lat", "yr", "row_keeps")
# loc_keeps<-subset(loc_keeps, loc_keeps$yr==1987)
# View(loc_keeps)
# nrow(loc_keeps)

model_df2020<-subset(model_df, model_df$year!=2021)
model_df2021<-subset(model_df, model_df$year==2021)

data.table::fwrite(model_df2020,"gee_4cast_data/model_csvs/march_all_model_csv.csv", row.names = F)
data.table::fwrite(model_df2021,"gee_4cast_data/model_csvs/march_forecast_2021_csv.csv", row.names = F)

rm(model_df2020,model_df2021,spat,tmp)
