#June 11th meeting with BMB
#Lets start by going over the MCMC of the normal stan model.
#It had run twice. Once on mine once on BMB machine.
#One had 1 divergent the other had 101 divergent

source("scripts/stansamplesvsdistributions.R")
#Plots of the first objects... (1 divergent)
pairs1
plot(firststan)
#PLots of the 2nd objects...(101 divergent)
pairs2
plot(secondstan) #I dont know why rho doesnt plot over itself

ggplot() +
  geom_density(data = as.data.frame(list_of_draws2$logit_rho), aes(list_of_draws2$logit_rho), fill = "blue", alpha = 0.2)+
  ggtitle("Rho Parameter")


#Quick side note:
#I could not even create the 0 kernel azzalini matrix. lme4 and glmmtmb would say its a really bad answer
#But they dont give the "cliff answer" and I have played around to try and find where that cliff answer comes from
#and so far no luck. I am unsure as to where it is. I kind of want to push past it into the next big problem that I am having

source("scripts/creatingfunctiondf.R")
library(TMB)
compile("scripts/TMBmodelwithpriors.cpp")
dyn.load(dynlib("scripts/TMBmodelwithpriors"))

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
            fullcountyincidence = countylist, dpriormean = log(50), dpriorscalingparam = 1.25, thetapriorpower = 4)
#Hard coded offset priors bc logitnormal looks like something I dont want to play with too much

pp0 <- list(log_d = log(dparam), theta = thetaparam, logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
            logsd_County = 0, logsd_Year = 0,
            YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

expriorobj <- MakeADFun(data=dd0, parameters=pp0, DLL="TMBmodelwithpriors",
                        silent=TRUE, 
                        random=c("YearRandomEffect","CountyRandomEffect"))
expriorobj$fn(expriorobj$par)

#NaN but I have no idea why
#OK so NaN occurs when we have too large a value for the prior mean. Gotcha. I feel like now I am more confused about the prior
#I dont understand what to make it since my range doesnt work...
#So @ dpriormean = log(50) changing the shape of the prior doesnt change the LL
#dpriormean = log(40) makes LL worse...
#dpriomean = log(60) makes LL NaN
