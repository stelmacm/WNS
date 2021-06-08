#posterior simulations of the stepbystepwithpriors.cpp

source("scripts/creatingfunctiondf.R")
library(TMB)
library(rstan)
library(rstanarm)
library(shinystan)
compile("scripts/stepbystepwithpriors.cpp")
dyn.load(dynlib("scripts/stepbystepwithpriors"))

countyeffect = rep(0,548)
yeareffect = rep(0,13)
dparam = 50
thetaparam = 1.4
rhoparam = 0.6  
a = 0.001

#It does not accept the values from the function into the list? Get NaN for obj3$fn(obj3$par)
dd1 <- list(dist = orderedmat, dim = 548, SM = bigsharedusers, numberofyears = 13,
            fullcountyincidence = countylist, dmean = log(505), dsd = log(165), thetamean = 1.25, 
            thetasd = .4583)#, offsetmean = 0.5, offsetsd = 0.15)
#Hard coded offset priors bc logitnormal looks like something I dont want to play with too much

pp1 <- list(log_d = log(dparam), theta = thetaparam, logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
            logsd_County = 0, logsd_Year = 0,
            YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

obj3 <- MakeADFun(data=dd1, parameters=pp1, DLL="stepbystepwithpriors",
                  silent=TRUE, 
                  random=c("YearRandomEffect","CountyRandomEffect"))

nlminboptprior <- with(obj3, nlminb(start = par, obj = fn, gr=gr,
                              control=list(trace=10)))

nlminboptprior$par #Yoinks
#Why did I say yoinks? these are literally perfect

optimoptprior <- optim(par = obj3$par, fn = obj3$fn, gr = obj3$gr, 
                        method = "BFGS", control=list(trace=TRUE))

optimoptprior$par
library(tmbstan)
set.seed(1313)
cores <- parallel::detectCores()
options(mc.cores = cores)
stepbysteppriorsstan <- tmbstan(obj3) #should I be setting init??? 
#It doesnt exist?????

saveRDS(stepbysteppriorsstan, "data/stepbysteppriorstan.RDS")



