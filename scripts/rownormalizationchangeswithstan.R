#Script with the new different versions of row normalization
#The goal is to have something reasonable-ish looking

#This is the current distance matrix with exp dist applied to it
distmat <- read.csv("data/distancematrix.csv")
distmat2 <- distmat[,-1]
rownames(distmat2) <- colnames(distmat2)
d1 <- as.matrix(distmat2)
diag(d1) <- 0 
orderedmat <- d1[sort(rownames(d1)),sort(colnames(d1))]

azzelinifun <- exp(-((d1)/505)^1.5)
diag(azzelinifun) <- 0
orderedmat2 <- azzelinifun[sort(rownames(azzelinifun)),sort(colnames(azzelinifun))]
#view(orderedmat2)
#What it would look like row normalized
localcountymat.w <- mat2listw(azzelinifun, style = "W")
localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
localcountymat <- as.matrix(localcountymat.m)
#view(localcountymat)

#Both seem very reasonable realistic
rowsumvec <- vector()
for (i in 1:nrow(orderedmat)) {
  rowsumvec[i] <- sum(orderedmat[i,])
}
min(rowsumvec)
max(rowsumvec)
mean(rowsumvec) #Roughly 100

#Shared user matrix from
#source("scripts/creatingfunctiondf.R")
#I want to keep the shared user row normalized because if rho = 1 I want these to still be hazards
#rather than values greater than 1 (this makes sense to be but could be bogus)

#The script that has no row normalization is called fixedrownormmodel.cpp

library(TMB)
#library(tmbstan)
compile("scripts/fixedrownormmodel.cpp")
dyn.load(dynlib("scripts/fixedrownormmodel"))
set.seed(1313)
cores <- parallel::detectCores()
options(mc.cores = cores)
source("scripts/creatingfunctiondf.R")
#User ordered mat from above
distmat <- read.csv("data/distancematrix.csv")
distmat2 <- distmat[,-1]
rownames(distmat2) <- colnames(distmat2)
d1 <- as.matrix(distmat2)
diag(d1) <- 0 
orderedmat <- d1[sort(rownames(d1)),sort(colnames(d1))]

countyeffect = rep(0,548)
yeareffect = rep(0,13)
dparam = 50
thetaparam = log(1.4)
rhoparam = 0.6  
a = 0.001

dd <- list(dist = orderedmat, dim = 548, SM = bigshareduserssparse, numberofyears = 13,
           fullcountyincidence = countylist, dpriormean = log(505), dpriorscalingparam = 1.25, thetapriorpower = 4,
           rhomean = plogis(0), rhostd = plogis(3), offsetmean = log(1.1), offsetsd = log(1.01), thetamean = log(1.5), 
           thetapriorscale = 0.5, thetapowerscale = 3,
           logsdyear_mean = log(1.5), logsdyear_sd = log(1.07), logsdcounty_mean = log(1.5), 
           logsdcounty_sd = log(1.07))

pp <- list(log_d = log(dparam), logtheta = log(thetaparam), logit_rho = qlogis(rhoparam),  log_offsetparam = log(a),
           logsd_County = 0, logsd_Year = 0,
           YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

#This model still contains the shared users that are normalized but not the distance matrix.
#Upon multiplication of rho and addition of each matrix
modelobj <- MakeADFun(data=dd, parameters=pp, DLL="fixedrownormmodel",
                      silent=TRUE, 
                      random=c("YearRandomEffect","CountyRandomEffect"))

modelobjoptima <- with(modelobj, nlminb(start = par, obj = fn, gr=gr,
                                        control=list(trace=10)))

library(tmbstan)
modelobjstan <- tmbstan(modelobj)

saveRDS(object = modelobjstan,file = "data/stanrownormSU.RDS")

#Now I will create a version where shared users is not row normalized and distance is not normalized
#However get normalized after they are added to one another with the rho constant
dd2 <- list(dist = orderedmat, dim = 548, SM = notrownormbigshareduserssparse, numberofyears = 13,
           fullcountyincidence = countylist, dpriormean = log(505), dpriorscalingparam = 1.25, thetapriorpower = 4,
           rhomean = plogis(0), rhostd = plogis(3), offsetmean = log(1.1), offsetsd = log(1.01), thetamean = log(1.5), 
           thetapriorscale = 0.5, thetapowerscale = 3,
           logsdyear_mean = log(1.5), logsdyear_sd = log(1.07), logsdcounty_mean = log(1.5), 
           logsdcounty_sd = log(1.07))

modelobj2 <- MakeADFun(data=dd2, parameters=pp, DLL="fixedrownormmodel",
                      silent=TRUE, 
                      random=c("YearRandomEffect","CountyRandomEffect"))

modelobjoptima2 <- with(modelobj2, nlminb(start = par, obj = fn, gr=gr,
                                        control=list(trace=10)))

modelobjstan2 <- tmbstan(modelobj2)

saveRDS(object = modelobjstan,file = "data/stanrownormatend.RDS")
