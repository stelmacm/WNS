#QUESTIONS:
#-Starting in sandbox.cpp (R side is called sandboxRside.R), how do I get a list of matrices into C++. This is the example from the TMB website but it keeps crashing
#-Is this even the right approach? Do I wanted to have a list of matrices and then build the model as I normally would or should I do other things
#-How does one actually make a 'mixed model' in TMB. It is a bit unclear from their tutorials and examples.
#-I at one point need to 'melt' and 'left_join'. Melt I can probably do with a for loop by I became stuck with left_join
#-Any further tips or resources for TMB?


#This will be the R portion of the model TMB
#Staring off with individual level because I dont know how to carry lists into TMB
source("scripts/creatingfunctiondf.R")
#This is what would be needed to run in

#THIS DOES NOT WORK INTENTIONALLY
compile("scripts/individualmodelTMB.cpp")
dyn.load(dynlib("scripts/individualmodelTMB"))
set.seed(123)
#SM = list of matrices that are shared users per year
#dist = matrix distances between 2 counties (counties in alphabetical order)
#countyincidence= list of matrices(technically vectors) that contain county incidence per year
data <- list(SM = sharedusers, dist = orderedmat, countyincidence = countylist) 
parameters <- list(d = 10, theta = 1.5, rho = .5, a = 0.001)
obj <- MakeADFun(data, parameters, DLL="individualmodelTMB")
obj$hessian <- TRUE
opt <- do.call("optim", obj)
opt
opt$hessian ## <-- FD hessian from optim
obj$he()    ## <-- Analytical hessian
sdreport(obj)
