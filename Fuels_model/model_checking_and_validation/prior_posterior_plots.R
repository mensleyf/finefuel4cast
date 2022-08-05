##--------------- data, packages --------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rstan,dplyr,tidyr,shinystan,here,rstudioapi,readtext,  install = TRUE, update = getOption("pac_update"), character.only = FALSE)

fit1<-readRDS( "model_outputs/fuels_model.rds")

#these are the priors
# beta~normal(.25,.25);
# alpha~normal (.25,.25);
# sig_o~normal(.5,.1)T[0,];
# sig_p~normal(0, .25)T[0,];

##--------------- extract posteriors from model --------------------------------


#extract
alphaout<-rstan::extract(fit1, 'alpha', permuted=F);alphaout<-apply(alphaout, 3,c)
betaout<-rstan::extract(fit1, 'beta', permuted=F);beta<-apply(betaout, 3,c)
sig_oout<-rstan::extract(fit1, 'sig_o', permuted=F);sig_oout<-apply(sig_oout, 3,c)
sig_pout<-rstan::extract(fit1, 'sig_p', permuted=F);sig_pout<-apply(sig_pout, 3,c)


#save

png("Figures/posterior_distributions.png",res=400,height=4,width=5,units="in")
nIters<-nrow(sig_pout)
par(mfrow=c(2,2),tcl=-0.2,mar=c(3,4,1,1),mgp=c(2,0.5,0))

alpha_prior<-rnorm(nIters,.25,.25);
plot(density(alphaout), xlim=c(-1,1), main=expression(alpha), xlab="", cex.axis=1,cex.main=1.3,cex.lab=1.1)
polygon(density(alpha_prior), col=rgb(1,0,0,.2))
polygon(density(alphaout), col=rgb(0,0,1,.2), border=rgb(0,0,1))
legend("topleft", pch=15, legend =c("prior", "posterior"), 
       col=c( rgb(1,0,0,.4), rgb(0,0,1,.4)), cex=1.1, bty="n")

beta_prior<-rnorm(nIters,.25,.25);
plot(density(betaout), xlim=c(-1,1), main=expression(beta), xlab="",ylab="",cex.axis=1,cex.main=1.3,cex.lab=1.1)
polygon(density(beta_prior), col=rgb(1,0,0,.2))
polygon(density(alpha_prior), col=rgb(1,0,0,.2))

polygon(density(betaout), col=rgb(0,0,1,.2), border=rgb(0,0,1))

sig_o_prior<-abs(rnorm(nIters,.5,.1));
plot(density(sig_oout), xlim=c(-1,1), main=expression(sigma[o]), xlab="", cex.axis=1,cex.main=1.3,cex.lab=1.1)
polygon(density(sig_o_prior), col=rgb(1,0,0,.2))
polygon(density(sig_oout), col=rgb(0,0,1,.2), border=rgb(0,0,1))

sig_p_prior<-abs(rnorm(nIters,0,.25));
plot(density(sig_pout), xlim=c(-1,1), main=expression(sigma[p]),xlab="",ylab="", cex.axis=1,cex.main=1.3,cex.lab=1.1)
polygon(density(sig_p_prior), col=rgb(1,0,0,.2))
polygon(density(sig_pout), col=rgb(0,0,1,.2), border=rgb(0,0,1))


dev.off()
