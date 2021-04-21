#Sandbox for TMB
#Going to try to bring in a list of a list
library(TMB)
library(Matrix)


data <- list()
data$object <- list(a=1:3, b=matrix(1:9,3))
obj <- MakeADFun(data,........) 

parameters <- list(pp = rnorm(1))
#This is what would be needed to run in
compile("scripts/sandbox.cpp")
dyn.load(dynlib("scripts/sandbox"))
obj <- MakeADFun(data = data, parameters, DLL="sandbox")

