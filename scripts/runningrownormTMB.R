#Script testing/ensuring that the rowmeanTMBmodel works

library(TMB)
library(tmbstan)
compile("scripts/rowmeanTMBmodel.cpp")
dyn.load(dynlib("scripts/rowmeanTMBmodel"))

source("scripts/creatingwinterfunctiondf.R") 

countyeffect = rep(0,548)
yeareffect = rep(0,13)
dparam = 500
thetaparam = 1.4
rhoparam = 0.4  
a = 0.1

dd <- list(dist = orderedmat, dim = 548, SM = bigSUmeanmatrix, numberofwinterdays = winterizedmatrix, numberofyears = 13,
           fullcountyincidence = countylist, dpriormean = log(505), dpriorscalingparam = 1.25, thetapriorpower = 4,
           rhomean = plogis(0), rhostd = plogis(3), offsetmean = log(1.1), offsetsd = log(1.01), thetamean = log(1.5), 
           thetapriorscale = 0.5, thetapowerscale = 3,
           logsdyear_mean = log(1.5), logsdyear_sd = log(1.07), logsdcounty_mean = log(1.5), 
           logsdcounty_sd = log(1.07))
#dpriorscalingparam is for d
#thetapriorpower is for d
#thetapower is for theta
#dscalingparam is for theta 

pp <- list(log_d = log(dparam), logtheta = log(thetaparam), logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
           logsd_County = 0, logsd_Year = 0,
           YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

modelobj <- MakeADFun(data=dd, parameters=pp, DLL="rowmeanTMBmodel",
                      silent=TRUE, 
                      random=c("YearRandomEffect","CountyRandomEffect"))

modelobjoptima <- with(modelobj, nlminb(start = par, obj = fn, gr=gr,
                                        control=list(trace=10)))

##Going to remove the priors for now.
compile("scripts/winterTMBnopriors.cpp")
dyn.load(dynlib("scripts/winterTMBnopriors"))

dd1 <- list(dist = orderedmat, dim = 548, SM = bigSUmeanmatrix, numberofwinterdays = winterizedmatrix, numberofyears = 13,
           fullcountyincidence = countylist)

pp <- list(log_d = log(dparam), logtheta = log(thetaparam), logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
           logsd_County = 0, logsd_Year = 0,
           YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

modelobj2 <- MakeADFun(data=dd1, parameters=pp, DLL="winterTMBnopriors",
                      silent=TRUE, 
                      random=c("YearRandomEffect","CountyRandomEffect"))

modelobj2$fn(modelobj2$par)

modelobjoptima2 <- with(modelobj2, nlminb(start = par, obj = fn, gr=gr,
                                        control=list(trace=10)))
