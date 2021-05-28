source("scripts/creatingfunctiondf.R")
library(TMB)
#I am curious about priors
#How do I make shape of dist matrix have to do with anything
distmat <- read.csv("data/distancematrix.csv")
#looking at a melted distmat
meltdist <- melt(distmat)
#It will account for distances twice but who cares
hist((meltdist$value)) #might be useful when thinking of priors

#When I adjust my priors and run stuff 
compile("scripts/stepbystepwithpriors.cpp")
dyn.load(dynlib("scripts/stepbystepwithpriors"))

distmean <- function(lower, upper){
  distmiddle <- mean((lower:upper))
  distsd <- (upper - lower)/6
  return(c(distmiddle, distsd))
}

distpriors <- distmean(10, 1000)

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

optprior <- with(obj3, nlminb(start = par, obj = fn, gr=gr,
                              control=list(trace=10)))

#The answer always seems fine in the sense that it is what I expected it to
#only shift is in rho but I am unsure if it has it's own story if its story is dependant on azzalini
#The "story of the parameter" makes sense..As the d param grows, the less shared users matters
#This is consistent with what we have been doing. 
#I am worried I am forcing it too much and overfitting? Is that possible

#Now with the expand grid I had some troubles.
#Facetted along log sd county and log sd year because those were the 2 params I didnt care about
#but I would have rather this been rho and offset
#I think I want to do tile with d and theta and fill likelihood
#Just confused about what to do here in general
modellikelihood <- read.csv("data/TMBnllprofile.csv")

likesurface <- ggplot(data = modellikelihood) +
  geom_tile(aes(x = d, y = theta, fill = gridlikelihood - min(gridlikelihood))) + #yikes im so confused as to what I am geom tiling
  facet_grid(logsdcounty + logsdyears ~ rho + offsets) +
  scale_fill_viridis_c(trans = "log10")
likesurface  

#facet.grid(a+b~c+d)
#(1) labeller=label_both in facet_grid
#scale_x_continuous(expand=c(0,0))
#and the same for scale_y_continuous
#+ theme(panel.spacing=grid::unit(0,"lines")
#(getting rid of spaces between facets)
#3) filter to the 'best' panel and draw that geom_tile without faceting
#Did not get to simulations
#make sure that we are covering both 'modes'
library(tmbstan)
mcmcobj3 <- tmbstan(obj3)
#saveRDS(mcmcobj3, file = "data/stepbysteppriorSTAN.RDS")

dprof <- tmbprofile(obj3, "log_d")
plot(dprof) #This is it

rhoprof <- tmbprofile(obj3, "logit_rho")
plot(rhoprof)
plot(plogis(rhoprof$logit_rho), rhoprof$value)
