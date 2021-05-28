#Creating a TMB grid for loglik with each param
#Change to which TMB you want to grid over anyways

#Will start with the simplest stepbystepmodel
#Doing grid for priors might be kind of awkward.

d <- seq(5, 115, 10)
theta <- seq(0.4,3,.2)
offset <- seq(0.01, 1, 0.11)
rho <- seq(0.1,1,0.1)
#What do I do about sd of the random effects :/

params <- expand.grid(d, theta, offset, rho)

library(TMB)
compile("scripts/stepbystepTMBmodel.cpp")
dyn.load(dynlib("scripts/stepbystepTMBmodel"))

countyeffect = rep(0,548)
yeareffect = rep(0,13)
dparam = 50
thetaparam = 1.4
rhoparam = 0.6  
a = 0.001

dd <- list(dist = orderedmat, dim = 548, SM = bigsharedusers, numberofyears = 13,
           fullcountyincidence = countylist)
pp <- list(log_d = log(dparam), theta = thetaparam, logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
           logsd_County = 0, logsd_Year = 0,
           YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

obj <- MakeADFun(data=dd, parameters=pp, DLL="stepbystepTMBmodel",
                 silent=TRUE,
                 random=c("YearRandomEffect","CountyRandomEffect"))

dseq <- seq(10,160,25)
thetaseq <- seq(0.5, 2.5, .25)
offsetseq <- seq(0.01, 1.01, 1)
rhoseq <- seq(0,1,.25)
logsdcountyseq <- seq(-1, 1, 1)
logsdyearseq <- seq(-4, 1, 1.5)
params <- expand.grid(dseq, thetaseq, rhoseq, offsetseq, logsdcountyseq, logsdyearseq)

gridgradient <- apply(X = params, MARGIN = 1 ,FUN = obj$gr)

likelihoodbyparam <- cbind(params, gridlikelihood2)
#write.csv(likelihoodbyparam, "data/TMBlikelihoodprofile.csv")

modellikelihood <- likelihoodbyparam %>% rename(c(d = Var1, theta = Var2, rho = Var3,
                                                  offsets = Var4, logsdcounty = Var5,
                                                  logsdyears = Var6))
likesurface <- ggplot(data = modellikelihood) +
  geom_line(aes(x = rho, y = gridlikelihood)) + #yikes im so confused as to what I am geom tiling
  facet_grid(logsdcounty ~ logsdyears) 
likesurface  

#Now doing the gradient of each object
dseq <- seq(10,160,25)
thetaseq <- seq(0.5, 2.5, .25)
offsetseq <- seq(0.01, 1.01, 1)
rhoseq <- seq(0,1,.25)
logsdcountyseq <- 0
logsdyearseq <- 0
params <- expand.grid(dseq, thetaseq, rhoseq, offsetseq, logsdcountyseq, logsdyearseq)

gridgradient2 <- apply(X = params, MARGIN = 1 ,FUN = obj2$gr)
#write.csv(gridgradient, "data/sbsgridgradient.csv")

transgridgradient2 <- t(gridgradient2)
transgridgradient2 <- as.data.frame(transgridgradient2)

sbsobjgradient2 <- cbind(params, transgridgradient2)

sbsgradient2 <- sbsobjgradient2 %>% rename(c(d = Var1, theta = Var2, rho = Var3,
                                                  offsets = Var4, logsdcounty = Var5,
                                                  logsdyears = Var6, dgr = V1, thetagr = V2, rhogr = V3,
                                           offsetsgr = V4, logsdcountygr = V5,
                                           logsdyearsgr = V6 ))
#write.csv(sbsgradient2, "data/bmbtmbgridgradient.csv")
