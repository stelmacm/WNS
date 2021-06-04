#Testing the ADfun with different mappings and comparing
#On the stepbystepTMBmodel.cpp version of the model 

source("scripts/creatingfunctiondf.R")
library(TMB)
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
pp <- list(log_d = log(dparam), theta = thetaparam, logit_rho = qlogis(rhoparam), 
           log_offsetparam = log(a),
           logsd_County = 0, logsd_Year = 0,
           YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

#Trying out maps for the Random effects
objmaprandeffects <- MakeADFun(data=dd, parameters=pp, DLL="stepbystepTMBmodel",
                  silent=TRUE, map = list(YearRandomEffect = as.factor(rep(0, 13)),
                                          CountyRandomEffect = as.factor(rep(0, 548))),
                  random=c("YearRandomEffect","CountyRandomEffect"))

nlminboptmapran <- with(objmaprandeffects, nlminb(start = par, obj = fn, gr=gr,
                                     control=list(trace=10)))
#Breaks optim YIKES. Mot good

#Trying the map for the rho param (only shared users verion)
#This means it takes whatever the default parameter is
rhoparam = 0
pp <- list(log_d = log(dparam), theta = thetaparam, logit_rho = qlogis(rhoparam), 
           log_offsetparam = log(a),
           logsd_County = 0, logsd_Year = 0,
           YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)


objmaprho <- MakeADFun(data=dd, parameters=pp, DLL="stepbystepTMBmodel",
                               silent=TRUE, map = list(logit_rho = factor(NA)),
                               random=c("YearRandomEffect","CountyRandomEffect"))

nlminboptmaprho <- with(objmaprho, nlminb(start = par, obj = fn, gr=gr,
                                                  control=list(trace=10)))
#Looks very reasonable in the sense that it is 
#Exactly what we would expect it to do
#Fall off a cliff and went to its default

rhoparam = 1
pp <- list(log_d = log(dparam), theta = thetaparam, logit_rho = qlogis(rhoparam), 
           log_offsetparam = log(a),
           logsd_County = 0, logsd_Year = 0,
           YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

nlminboptmaprho2 <- with(objmaprho, nlminb(start = par, obj = fn, gr=gr,
                                          control=list(trace=10)))

#Also jumps off a cliff but a little different....
#Still exactly what I thought it would do..ish

#Now going to run only the sharedusersonlyTMB.cpp file
compile("scripts/sharedusersonlyTMB.cpp")
dyn.load(dynlib("scripts/sharedusersonlyTMB"))
dd2 <- list(dim = 548, SM = bigsharedusers, numberofyears = 13,
           fullcountyincidence = countylist)
pp2 <- list(log_offsetparam = log(a),
           logsd_County = 0, logsd_Year = 0,
           YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

objSUonly <- MakeADFun(data=dd2, parameters=pp2, DLL="sharedusersonlyTMB",
                       silent=TRUE,
                       random=c("YearRandomEffect","CountyRandomEffect"))

nlminboptSUonly <- with(objSUonly, nlminb(start = par, obj = fn, gr=gr,
                                          control=list(trace=10)))
#Looks very reasonable in the sense that it is
#Curious what would happen if we ran 100 optims on this.
#Is there a bimodal surface on this? 

#That was a dumb question because there is only one param
#Removed it out of the code but they all go to the same place

compile("scripts/nosharedusersTMB.cpp")
dyn.load(dynlib("scripts/nosharedusersTMB"))

dd3 <- list(dist = orderedmat, dim = 548, numberofyears = 13,
           fullcountyincidence = countylist)
pp3 <- list(log_d = log(dparam), theta = thetaparam,
           log_offsetparam = log(a),
           logsd_County = 0, logsd_Year = 0,
           YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

objnoSU <- MakeADFun(data=dd3, parameters=pp3, DLL="nosharedusersTMB",
                       silent=TRUE,
                       random=c("YearRandomEffect","CountyRandomEffect"))

nlminboptnoSU <- with(objnoSU, nlminb(start = par, obj = fn, gr=gr,
                                          control=list(trace=10)))
#Exactly what we thought would happen
#Maybe going to the 100 optims on this as well

dseq <- seq(10,160,50)
thetaseq <- seq(0.5, 2.5, 1)
offsetseq <- seq(0.01, 1.01, 1)
logsdcountyseq <- 0
logsdyearseq <- 0
params <- expand.grid(dseq, thetaseq, offsetseq, logsdcountyseq, logsdyearseq)

paralledoptim <- function(i){
  results <- optim(par = c(log(params[i,1]),params[i,2],
                           log(params[i,3]), params[i,4], params[i,5]),
                   fn = objnoSU$fn, objnoSU$gr, method = "BFGS", control=list(trace=TRUE))
  if (inherits(results,"try-error")) return(NA_real_)
  return(results$par)
}

mcparoptim <- mclapply((1:nrow(params)), paralledoptim, mc.cores = numCores)
#So this is the bimodal surface, which is exactly what we thought
optimsdf <- data.frame(matrix(unlist(mcparoptim), nrow=length(mcparoptim), byrow=TRUE))
#I am very satisfied with the answers I get here
#This is what regular (R version of this code) gives
#I do see the bimodality here so thats nice