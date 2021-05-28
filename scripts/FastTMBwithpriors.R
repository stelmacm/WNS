#Running the new TMBmodelwithpriors.cpp
#This is combining the BMBmodel with with step by step model

source("scripts/creatingfunctiondf.R")
library(TMB)
compile("scripts/TMBmodelwithpriors.cpp")
dyn.load(dynlib("scripts/TMBmodelwithpriors"))