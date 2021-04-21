#Sandbox for TMB
#Going to try to bring in a list of a list
library(TMB)
## library(Matrix)


data <- list()
data$object <- list(a=1:3, b=matrix(1:9,3))
obj <- MakeADFun(data,........) 

parameters <- list(pp = rnorm(1))
parameters <- list(q=2)
#This is what would be needed to run in
compile("scripts/sandbox.cpp")
dyn.load(dynlib("scripts/sandbox"))
obj <- MakeADFun(data = data, parameters=parameters, DLL="sandbox")

## create a list of 
lapply(
    yearvec,
    function(y) {
        sparse.model.matrix(~county-1, data=subset(all_data,year==y))
    })
## then within TMB:
## extract Z[i] from the list
Z * county_effect
## if you have one obs per county per year, always in the same order,
## county order == obs order, then Z is diagonal
## or you could say
loghazard[i] += county_effect[i]
## or
loghazard[j] += county_effect[county[j]]
