#This will be the R portion of the model TMB
#Staring off with individual level because I dont know how to carry lists into TMB
source("modeltesting/creatingfunctiondf.R")
#This is what would be needed to run in
compile("scripts/fullmodelTMB2.cpp")
dyn.load(dynlib("scripts/fullmodelTMB"))
set.seed(123)
#SM = list of matrices that are shared users per year
#dist = matrix distances between 2 counties (counties in alphabetical order)
#countyincidence= list of matrices(technically vectors) that contain county incidence per year
#Dim = number of counties in matrices
#number of years = years including 2006 which gets removed in program
#unifectedcounty = uninfected counties (2006 removed)
#yearindicator = vector of factored years for the mixed model(2006 and years a county is infected are removed)
#countyindicator = vector of factored counties for the mixed model

data <- list(SM = bigsharedusers, dist = orderedmat, countyincidence = countylist, dim = 548, numberofyears = 13, 
             uninfectedcounty = uninfectedcountylist, yearindicator = years, countyindicator = counties, incidence = incidence) 
#d = scaling param
#theta = azzalini power
#rho = proportion of shared users compared to distance matrix 
#a = offset parameter (so that log 0 doesn't happen)
parameters <- list(d = 10, theta = 1.5, rho = .5, a = 0.001, Random_vectorone= random_vec, Random_vectortwo = random_vec2)
obj <- MakeADFun(data, parameters, DLL="individualmodelTMB")
obj$hessian <- TRUE
opt <- do.call("optim", obj)
opt
opt$hessian ## <-- FD hessian from optim
obj$he()    ## <-- Analytical hessian
sdreport(obj)
