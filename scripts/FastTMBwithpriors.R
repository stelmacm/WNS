#Running the new TMBmodelwithpriors.cpp
#This is combining the BMBmodel with with step by step model

library(TMB)
compile("scripts/TMBmodelwithpriors.cpp")
dyn.load(dynlib("scripts/TMBmodelwithpriors"))
source("scripts/creatingfunctiondf.R")

#Model has power exponential. Need to have fancy code from BMB to see what that bubble looks like

get_gnorm <- function(lwr=-1, upr=1, tail_prob=2*pnorm(lwr),
                      ctr_prob=abs(diff(pnorm(c(-1,1)*lwr/2)))) {
  require("gnorm")
  ## default tail_prob/ctr_prob assume lwr/upr symmetric around 0 ...
  ## start from Gaussian
  ## desired alpha
  sd <- abs(upr-lwr)/(-2*qnorm(tail_prob/2))
  ## convert to sd (?pgnorm)
  ## conversion factor: sqrt(1/(gamma(3/2)/(gamma(1/2))))
  alpha <- sd*sqrt(2)
  mu <- (lwr+upr)/2 ## symmetric, we don't have to estimate this
  start <- c(alpha=alpha, beta=2)
  tfun <- function(x) {
    ## compute probability within range
    pfun <- function(r) abs(diff(vapply(r,
                                        function(z) do.call("pgnorm",c(list(z, mu=mu), as.list(x))),
                                        FUN.VALUE=numeric(1))))
    tail_obs <- 1-pfun(c(upr,lwr))
    ctr_range <- c((mu+lwr)/2, (mu+upr)/2)
    ctr_obs <- pfun(ctr_range)
    return((tail_prob-tail_obs)^2 + (ctr_prob-ctr_obs)^2)
  }
  return(c(mu=mu,optim(par=start,fn=tfun)$par))
}

val <- get_gnorm(lwr = -1, upr = 3)

xs <- seq(-2, 2, length.out = 100)
plot(xs, dgnorm(xs, mu = val[1], alpha = val[2], beta = val[3]), type = "l", 
     xlab = "x", ylab = expression(p(x)))

plot(xs, dgnorm(xs, mu = 0, alpha = 1.25, beta = 4), type = "l", 
     xlab = "x", ylab = expression(p(x)))
#So I need to include a scaling param and a power param to the data
#And that comes from the function
#power param = thetaprior -> beta
#expscaleparam = dpriorscalingparam -> alpha
#How to determine shape??
#I kinda like 1 and 4 idek why

countyeffect = rep(0,548)
yeareffect = rep(0,13)
dparam = 50
thetaparam = 1.4
rhoparam = 0.6  
a = 0.001

#It does not accept the values from the function into the list? Get NaN for obj3$fn(obj3$par)
dd0 <- list(dist = orderedmat, dim = 548, SM = bigsharedusers, numberofyears = 13,
            fullcountyincidence = countylist, dpriormean = log(505), dpriorscalingparam = 1.25, thetapriorpower = 4,
            rhomean = plogis(0), rhostd = plogis(3), offsetmean = log(1.1), offsetsd = log(1.01), thetamean = 1.25, 
            thetapriorscale = 5.656854, thetapowerscale = 2)
#dpriorscalingparam is for d
#thetapriorpower is for d
#thetapower is for theta
#dscalingparam is for theta 

pp0 <- list(log_d = log(dparam), theta = thetaparam, logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
            logsd_County = 0, logsd_Year = 0,
            YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

expriorobj <- MakeADFun(data=dd0, parameters=pp0, DLL="TMBmodelwithpriors",
                  silent=TRUE, 
                  random=c("YearRandomEffect","CountyRandomEffect"))
expriorobj$fn(expriorobj$par)
expriorobj$gr(expriorobj$par)

nlminboptpriorexp <- with(expriorobj, nlminb(start = par, obj = fn, gr=gr,
                                    control=list(trace=10)))

#library(tmbstan)
#expstanobj <- tmbstan(expriorobj)
#saveRDS(object = expstanobj, file =  "data/TMBmodelwithpriorsstan.RDS")

#Is this was perfection is?
#theta always goes to whatever the thetapower is so idk

#Now we try the second verion
compile("scripts/altTMBmodelwithpriors.cpp")
dyn.load(dynlib("scripts/altTMBmodelwithpriors"))

dd1 <- list(dist = orderedmat, dim = 548, SM = bigsharedusers, numberofyears = 13,
            fullcountyincidence = countylist, dpriormean = log(505), dpriorscalingparam = 1.25, thetapriorpower = 4,
            rhomean = plogis(0), rhostd = plogis(3), offsetmean = log(1.1), offsetsd = log(1.01))

expriorobj2 <- MakeADFun(data=dd1, parameters=pp0, DLL="altTMBmodelwithpriors",
                        silent=TRUE, 
                        random=c("YearRandomEffect","CountyRandomEffect"))
expriorobj2$fn(expriorobj2$par)
expriorobj2$gr(expriorobj2$par)

nlminboptpriorexp2 <- with(expriorobj2, nlminb(start = par, obj = fn, gr=gr,
                                             control=list(trace=10)))
#Also goes to theta = 4....
#hmmmm

exp(- (500/505)^4)
exp(- (1000/505)^4)
exp(- (50/505)^4)

#Now I am thinking about adding priors to the random effects
#The random effects look really really Gaussian
#I dont know what we want from this and what we think this will do for us
plnorm(3, meanlog = 0, sdlog = 1)
