#Testing out different TMB objects and basically going to compare the objects based on their priors
#And doing spatial simulations based on the optim results

source("scripts/creatingfunctiondf.R")
library(TMB)

#When I adjust my priors and run stuff 
compile("scripts/stepbystepwithpriors.cpp")
dyn.load(dynlib("scripts/stepbystepwithpriors"))

distmean <- function(lower, upper){
  distmiddle <- mean((lower:upper))
  distsd <- (upper - lower)/6
  return(list(distmiddle = distmiddle, distsd = distsd))
}

distpriors <- distmean(10, 1000)

countyeffect = rep(0,548)
yeareffect = rep(0,13)
dparam = 50
thetaparam = 1.4
rhoparam = 0.6  
a = 0.001

distcenter <- distpriors$distmiddle
distdev <- distpriors$distsd
#It does not accept the values from the function into the list? Get NaN for obj3$fn(obj3$par)
dd1 <- list(dist = orderedmat, dim = 548, SM = bigsharedusers, numberofyears = 13,
            fullcountyincidence = countylist, dmean = log(distcenter), dsd = log(distdev),
            thetamean = 1.25, 
            thetasd = .4583)#, offsetmean = 0.5, offsetsd = 0.15)
#Hard coded offset priors bc logitnormal looks like something I dont want to play with too much

pp1 <- list(log_d = log(dparam), theta = thetaparam, logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
            logsd_County = 0, logsd_Year = 0,
            YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

obj <- MakeADFun(data=dd1, parameters=pp1, DLL="stepbystepwithpriors",
                  silent=TRUE,
                  random=c("YearRandomEffect","CountyRandomEffect"))

#optprior <- with(obj, nlminb(start = par, obj = fn, gr=gr,control=list(trace=10)))
#6.22720, 1.25011, -0.640884, -3.67401 with LL 1320.9936
exp(6.22720)
distcenter

distpriors2 <- distmean(10, 1200)

distcenter2 <- distpriors2$distmiddle
distdev2 <- distpriors2$distsd
#It does not accept the values from the function into the list? Get NaN for obj3$fn(obj3$par)
dd2 <- list(dist = orderedmat, dim = 548, SM = bigsharedusers, numberofyears = 13,
            fullcountyincidence = countylist, dmean = log(distcenter2), dsd = log(distdev2),
            thetamean = 1.25, 
            thetasd = .4583)

obj2 <- MakeADFun(data=dd2, parameters=pp1, DLL="stepbystepwithpriors",
                  silent=TRUE,
                  random=c("YearRandomEffect","CountyRandomEffect"))

#optprior2 <- with(obj2, nlminb(start = par, obj = fn, gr=gr,
#                              control=list(trace=10)))
#6.40464, 1.25, -.640933, -3.67403 with LL 1321.029
exp(6.40464)
distcenter2

x <- seq(50, 1500, length=1000)
y <- dnorm(x, mean=625, sd=191.66)
plot(x, y, type="l", lwd=1)


x1 <- seq(0, 3, length=1000)
y1 <- dnorm(x1, mean=1.25, sd=.466)
plot(x1, y1, type="l", lwd=1)

source("scripts/spatialstatsforsims.R")

#obj sim stats
ff$maxdist
ff$meandist
ff$infayear

#obj2 sim stats 
gg$maxdist
gg$meandist
gg$infayear



#Questions are still in terms of priors.
#Like we see each parameter just going to the mean of each distribution
#I would like to think that although it seems kind of fishy the answer is reasonable
#I really can't figure out what to do about it
#Would like to go over stan model as well
library(shinystan)
obj3stan <- readRDS("data/stepbysteppriorSTAN.RDS")
shinystan::launch_shinystan(obj3stan)
