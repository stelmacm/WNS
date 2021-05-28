#Comparing speeds of BMBTMB and stepbystepTMB and comparing their likelihoods and gradients ect
source("scripts/creatingfunctiondf.R")
library(TMB)
compile("scripts/stepbystepTMBmodel.cpp")
dyn.load(dynlib("scripts/stepbystepTMBmodel"))

compile("scripts/BMBTMB.cpp")
dyn.load(dynlib("scripts/BMBTMB"))

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


obj2 <- MakeADFun(data=dd, parameters=pp, DLL="BMBTMB",
                  silent=TRUE,
                  random=c("YearRandomEffect","CountyRandomEffect"))

#Retry this on empty last.best.params
library(rbenchmark)
#Trying step by step
rbenchmark::benchmark(obj$fn(obj$par), replications = 100)
rbenchmark::benchmark(obj$gr(obj$par), replications = 10) #not that it matter

rbenchmark::benchmark(obj2$fn(obj2$par), replications = 100)
rbenchmark::benchmark(obj$gr(obj$par), replications = 10) #not that it matter
#oof. Alot faster. Should go forward with this one

sbsmodellikelihood <- read.csv("data/TMBnllprofile.csv")

likesurface <- ggplot(data = sbsmodellikelihood) +
  geom_tile(aes(x = d, y = theta, fill = gridlikelihood - min(gridlikelihood))) + #yikes im so confused as to what I am geom tiling
  facet_grid(logsdcounty + logsdyears ~ rho + offsets) +
  scale_fill_viridis_c(trans = "log10")
likesurface  

bmbmodellikelihood <- read.csv("data/BMBTMBllgrid.csv")

likesurface2 <- ggplot(data = bmbmodellikelihood) +
  geom_tile(aes(x = d, y = theta, fill = gridlikelihood2 - min(gridlikelihood2))) + #yikes im so confused as to what I am geom tiling
  facet_grid(logsdcounty + logsdyears ~ rho + offsets) +
  scale_fill_viridis_c(trans = "log10")
likesurface2  

library(gridExtra)
grid.arrange(likesurface, likesurface2, ncol=2)
#Likelihood plots look exactly the same beside one another

#Now going to compare the gradient of the two
bmbgradients <- read.csv("data/bmbtmbgridgradient.csv")
sbsgradients <- read.csv("data/sbsgridgradient.csv")
#goal is to compare the two together to see if they give the same answers 
dgrdiff <- bmbgradients$dgr - sbsgradients$dgr
thetadiff <- bmbgradients$thetagr - sbsgradients$thetagr
rhogrdiff <- bmbgradients$rhogr - sbsgradients$rhogr
offsetgrdiff <- bmbgradients$offsetsgr - sbsgradients$offsetsgr
logsdcountydiff <- bmbgradients$logsdcountygr - sbsgradients$logsdcountygr
logsdyeardiff <- bmbgradients$logsdyearsgr - sbsgradients$logsdyearsgr

gradientdifferences <- data.frame(dgrdiff, thetadiff, rhogrdiff, offsetgrdiff,logsdcountydiff, logsdyeardiff)
#Remove 0's
gradientdifferences[!(apply(gradientdifferences, 1, function(y) any(y == 0))),]

#K screw it
dgraddiff <- ggplot(data = gradientdifferences) +
  geom_point(aes(x = 1:nrow(gradientdifferences), y = abs(dgrdiff)))
dgraddiff

thetagraddiff <- ggplot(data = gradientdifferences) +
  geom_point(aes(x = 1:nrow(gradientdifferences), y = abs(thetadiff)))

rhograddiff <- ggplot(data = gradientdifferences) +
  geom_point(aes(x = 1:nrow(gradientdifferences), y = abs(rhogrdiff)))

offsetgraddiff <- ggplot(data = gradientdifferences) +
  geom_point(aes(x = 1:nrow(gradientdifferences), y = abs(offsetgrdiff)))

logsdcountygraddiff <- ggplot(data = gradientdifferences) +
  geom_point(aes(x = 1:nrow(gradientdifferences), y = abs(logsdcountydiff)))

logsdyeargraddiff <- ggplot(data = gradientdifferences) +
  geom_point(aes(x = 1:nrow(gradientdifferences), y = abs(logsdyeardiff)))

allthedifferences <- grid.arrange(dgraddiff, thetagraddiff, rhograddiff, offsetgraddiff, logsdcountygraddiff, logsdyeargraddiff,
                                  ncol = 2, nrow = 3)

#OR should I be comparing the param that I tested to begin with aka the param in quesions gradient difference
