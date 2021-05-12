#TMB cut off model
library(TMB)
source("scripts/creatingfunctiondf.R")
compile("scripts/stepbystepwithcutoff.cpp")
dyn.load(dynlib("scripts/stepbystepwithcutoff"))

countyeffect = rep(0,548)
yeareffect = rep(0,13)
dparam = 50
thetaparam = 1.4
rhoparam = 0.6  
cutoffpoint = 900

a <- 0.001
dd <- list(dist = orderedmat, dim = 548, SM = bigsharedusers, numberofyears = 13,
           fullcountyincidence = countylist)
pp <- list(log_d = log(dparam), theta = thetaparam, logcutoff = log(cutoffpoint), 
           logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
           logsd_County = 0, logsd_Year = 0,
           YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

objective <- MakeADFun(data=dd, parameters=pp, DLL="stepbystepwithcutoff",
                 silent=TRUE,
                 random=c("YearRandomEffect","CountyRandomEffect"))

optimalcutoff <- with(objective, nlminb(start = par, obj = fn, gr=gr,
                         control=list(trace=10)))
optimalcutoff2 <- optim(par = objective$par, fn = objective$fn, gr = objective$gr,
                        method = "BFGS", control=list(trace=TRUE))

#I dont even know what to think anymore