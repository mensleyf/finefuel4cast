## ----------------------------------------------  data + packages ------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(corrplot,dplyr,tidyr,rstudioapi,stringr, ggplot2,tiff,raster,rgdal,spatialEco,glmnet,mvtnorm, plotrix, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

model_df <- fread(file="gee_4cast_data/model_csvs/march_all_model_csv.csv",header=T)
model_df <- as.data.frame(model_df)

# add an interaction term
model_df$z_prOctMar_z_ssm <- model_df$z_pr_Oct2Mar*model_df$z_ssm

#mod1: linear forecast
mod1_data<-subset(model_df, select=c(z_agb,prev_z_agb,year, z_pr_Oct2Mar,z_tmmx_Oct2Mar,z_vpd_Oct2Mar,z_ndvi_Feb,pr_frac,
                                     prev_pfg_cover,z_ssm,z_bulk_dens,z_prOctMar_z_ssm))
                          
#look at correlation amongst covariates
cor_data<-subset(mod1_data,select=c(z_agb,prev_z_agb, z_pr_Oct2Mar,z_tmmx_Oct2Mar,z_vpd_Oct2Mar,z_ndvi_Feb,pr_frac))

cor_m<-cor(cor_data)
plot.new()
par(mfrow=c(1,1))

# png("figures/covariate_correlation.png")
corrplot(cor_m, main="Correlation of weather covariates")
# dev.off()

# this version of the fit uses all years (no leave one year out here)
lm<-lm(z_agb~prev_z_agb+ z_pr_Oct2Mar+z_tmmx_Oct2Mar+z_vpd_Oct2Mar+z_ndvi_Feb+pr_frac
     +prev_pfg_cover + z_ssm+z_bulk_dens+z_prOctMar_z_ssm, data=mod1_data)

coefficients(lm)
#coeffs
names_coefs<-c("int",  names(mod1_data))
names_coefs<-names_coefs[!(names_coefs=="z_agb" | names_coefs=="year" )]
lm_coeffs<-as.data.frame(cbind(names_coefs,round(as.vector(coef(lm)),3 )))

# View(lm_coeffs)


write.csv(lm_coeffs,paste0("prod_model_outputs/lm_coeffs.csv"), row.names = FALSE)

