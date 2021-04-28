library(TMB)
source("scrips/creatingfunctiondf.R")
compile("scripts/stepbystepTMBmodel.cpp")
dyn.load(dynlib("scripts/stepbystepTMBmodel"))

dd <- list(dist = orderedmat, dim = 548, SM = bigsharedusers, numberofyears = 13, 
           fullcountyincidence = countylist)
pp <- list(d = 50, theta = 2, rho = 0.5)

obj <- MakeADFun(dd, pp, DLL="stepbystepTMBmodel") 

