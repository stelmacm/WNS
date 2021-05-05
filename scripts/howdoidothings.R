#BMB meeting May 5

library(TMB)
source("modeltesting/creatingfunctiondf.R")
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
pp <- list(d = dparam, theta = thetaparam, rho = rhoparam,  offsetparam = a,YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

obj <- MakeADFun(dd, pp, DLL="stepbystepTMBmodel") 
#Problem occurs when
opt2 <- with(obj, nlminb(start = par, obj = fn, gr=gr))
#Fine lets switch to this 
opt3 <- optim( par = obj$par,fn = obj$fn, gr = obj$gr, method = "BFGS")
#Still no bueno 
#I know the problem is
#https://stackoverflow.com/questions/35757048/initial-value-in-vmmin-is-not-finite-even-when-changing-the-starting-value
#but this works if I were to use logit scale instead of cloglog so like why is there not enough param rooms

#Alternatively, when I do get answers using logit, mostly the random effects vectors change and the scaling parameters dont change by much
#in the logit model

#Next step is to go pseudo Bayesian and kinda play around with priors ect ect
#is tmbstan the way to go? Is tmbstan just stan that accepts tmb objects 
#(do I have to change the object? unclear from examples if its a model build or nll optimizer)
#https://cran.r-project.org/web/packages/glmmTMB/vignettes/mcmc.html
#Am I just going to be using NUTS?

#Are simulations in the TMB model worth doing or are the ones in R fine?
