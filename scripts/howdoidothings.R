library(TMB)
source("scripts/creatingfunctiondf.R")
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

## try with BFGS instead
opt3 <- optim( par = obj$par,fn = obj$fn, gr = obj$gr, method = "BFGS", control=list(trace=TRUE))
## start from nlminb optimum
opt3B <- optim( par = opt2$par, fn = obj$fn, gr = obj$gr, method = "BFGS", control=list(trace=TRUE))

## ... and vice versa (start nlminb from BFGS optimum)
opt2B <- with(obj, nlminb(start=opt3$par, objective=fn, gr=gr, control=list(trace=10)))

ee <- obj$env
obj$fn(ee$last.par.best)
with(ee,last.par.best[-random])
## this *should* generate a nice table of coefficients and standard deviations
library(broom.mixed)
class(obj) <- "TMB"
tidy(obj, effect="fixed", conf.int=TRUE) ## Wald confidence intervals
tidy(obj, effect="fixed", conf.int=TRUE, conf.method="uniroot")

head(obj$env$last.par.best)

lpb <- obj$env$last.par.best
lpb2 <- lpb
delta_d_vec <- seq(-5,5,by=0.1)
llvals <- sapply(delta_d_vec, function(x) { lpb2[1] <- lpb[1]+x; obj$fn(lpb2) })

## compute likelihood profile rather than Wald confidence intervals
tmbprofile(obj,name=1)

#Still no bueno
#I know the problem is
#https://stackoverflow.com/questions/35757048/initial-value-in-vmmin-is-not-finite-even-when-changing-the-starting-value
#but this works if I were to use logit scale instead of cloglog so like why is there not enough param rooms

#Alternatively, when I do get answers using logit, mostly the random effects vectors change and the scaling parameters dont change by much
#in the logit model

library(tmbstan)
t1 <- tmbstan(obj, init="last.par.best")
#Next step is to go pseudo Bayesian and kinda play around with priors ect ect
#is tmbstan the way to go? Is tmbstan just stan that accepts tmb objects
#(do I have to change the object? unclear from examples if its a model build or nll optimizer)
#https://cran.r-project.org/web/packages/glmmTMB/vignettes/mcmc.html
#Am I just going to be using NUTS?

#Are simulations in the TMB model worth doing or are the ones in R fine?
