source("scripts/creatingfunctiondf.R")
library(TMB)
compile("scripts/stepbystepTMBmodel.cpp")
dyn.load(dynlib("scripts/stepbystepTMBmodel"))
countyeffect = rep(0,548)
yeareffect = rep(0,13)
dparam = 50
thetaparam = 1.4
rhoparam = 0.6  

a <- 0.001
dd <- list(dist = orderedmat, dim = 548, SM = bigsharedusers, numberofyears = 13,
           fullcountyincidence = countylist)
pp <- list(log_d = log(dparam), theta = thetaparam, logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
           logsd_County = 0, logsd_Year = 0,
           YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

obj <- MakeADFun(data=dd, parameters=pp, DLL="stepbystepTMBmodel",
                 silent=TRUE,
                 random=c("YearRandomEffect","CountyRandomEffect"))


opt2 <- with(obj, nlminb(start = par, obj = fn, gr=gr,
                         control=list(trace=10)))
#LL of 967.19

#Post Monday Meeting  with BMB
#Runnning his adjustments to see if they get the same results

compile("scripts/BMBTMB.cpp")
dyn.load(dynlib("scripts/BMBTMB"))



obj2 <- MakeADFun(data=dd, parameters=pp, DLL="BMBTMB",
                 silent=TRUE,
                 random=c("YearRandomEffect","CountyRandomEffect"))

optBMB <- with(obj2, nlminb(start = par, obj = fn, gr=gr,
                         control=list(trace=10)))

opt2BMB <- optim(par = optBMB$par, fn = obj2$fn, gr = obj2$gr, method = "BFGS", control=list(trace=TRUE))
#Oh.... ??? LL = 978.865

opt3BMB <- with(obj2, nlminb(start = opt2BMB$par, obj = fn, gr=gr,
                             control=list(trace=10)))

optBMB$par 
##LL of 1219.97
#log_d           theta            logit_rho    log_offsetparam    logsd_Year    logsd_County
#-11.2104551       0.2622527      -1.2418091      -3.8789451      -1.0126715  -354.8913564 

obj2$fn(optBMB$par)
#Works but seems vastly different from the original step by step TMB


compile("scripts/stepbystepwithpriors.cpp")
dyn.load(dynlib("scripts/stepbystepwithpriors"))

dd1 <- list(dist = orderedmat, dim = 548, SM = bigsharedusers, numberofyears = 13,
            fullcountyincidence = countylist, dmean = 80, dsd = 23.33, thetamean = 1.5, thetasd = 0.4166, offsetmean = 0.5, offsetsd = 0.15)
pp1 <- list(log_d = log(dparam), theta = thetaparam, logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
            logsd_County = 0, logsd_Year = 0,
            YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

obj3 <- MakeADFun(data=dd1, parameters=pp1, DLL="stepbystepwithpriors",
                  silent=TRUE,
                  random=c("YearRandomEffect","CountyRandomEffect"))

optprior <- with(obj3, nlminb(start = par, obj = fn, gr=gr,
                            control=list(trace=10)))

 opt2BMB <- optim(par = optBMB$par, fn = obj2$fn, gr = obj2$gr, method = "BFGS", control=list(trace=TRUE))


#Looking at Azzalini with optim params
distmat <- read.csv("data/distancematrix.csv")
distmat2 <- distmat[,-1]
rownames(distmat2) <- colnames(distmat2)
d1 <- as.matrix(distmat2)
diag(d1) <- 0


cutoffpoint <- exp(-(900/exp(-10.75))^0.26)
azzelinifun <- exp(-((d1)/exp(-10.75))^0.262)
diag(azzelinifun) <- 0
azzelinifun[(azzelinifun < cutoffpoint)] <- 0

#Now to weight matrix
localcountymat.w <- mat2listw(azzelinifun, style = "W")
localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
localcountymat <- as.matrix(localcountymat.m)

