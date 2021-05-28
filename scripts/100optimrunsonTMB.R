#Going to look for all the different optims to confirm bimodality of LL surface
#Need to think about what TMB model I want to be checking
source("scripts/creatingfunctiondf.R")
library(TMB)
library(parallel)
numCores <- detectCores()
library(foreach)
library(doParallel)
registerDoParallel(numCores)

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

obj <- MakeADFun(data=dd, parameters=pp, DLL="stepbystepTMBmodel",
                 silent=TRUE,
                 random=c("YearRandomEffect","CountyRandomEffect"))

dseq <- seq(10,160,50)
thetaseq <- seq(0.5, 2.5, 1)
offsetseq <- seq(0.01, 1.01, 1)
rhoseq <- seq(0.01,1,.49)
logsdcountyseq <- 0
logsdyearseq <- 0
params <- expand.grid(dseq, thetaseq, rhoseq, offsetseq, logsdcountyseq, logsdyearseq)

#IS this a parallel???
optimdf <- foreach(i = 1:nrow(params), .combine = rbind) %dopar% {
    results <- try(optim(par = c(log(params[i,1]),params[i,2],qlogis(params[i,3]),
                             log(params[i,4]), params[i,5], params[i,6]),
                 fn = obj$fn, obj$gr, method = "BFGS", control=list(trace=TRUE)),
                 silent = TRUE,options(try.outFile = stdout()))
    if(is.na(results)) bestresults <- 0
    else bestresults <- results$par
} #Am I using cores?


paralledoptim <- function(i){
  results <- optim(par = c(log(params[i,1]),params[i,2],qlogis(params[i,3]),
                               log(params[i,4]), params[i,5], params[i,6]),
                       fn = obj2$fn, obj2$gr, method = "BFGS", control=list(trace=TRUE))
  if (inherits(results,"try-error")) return(NA_real_)
  return(results$par)
}

mcparoptim <- mclapply((1:nrow(params)), paralledoptim, mc.cores = numCores) #Run overnight
#I think problems are solved with this one
#Surprisingly quick

optimsdf <- data.frame(matrix(unlist(mcparoptim), nrow=length(mcparoptim), byrow=TRUE))

#Oops I did it wrong...Needs to have all params transformed as well...
optimparamsdf <- as.data.frame(optimsdf) %>% rename(c(d = X1, theta = X2, rho = X3,
                                                offsets = X4, logsdcounty = X5,
                                                logsdyears = X6))
#write.csv(optimoutputs, file = "data/optimparamsdf.csv")
#Should calculate LL of each 


optimll <- apply(X = optimparamsdf, MARGIN = 1 ,FUN = obj$fn)

optimoutputs <- cbind(optimparamsdf, optimll)

optimoutputs <- read.csv("data/optimparamsdf.csv")
paramsandoutputs <- cbind(params, optimoutputs) %>% rename(c(dseq = Var1, thetaseq = Var2, rhoseq = Var3,
                                                             offsetseq = Var4, logsdcountyseq = Var5,
                                                             logsdyearsseq = Var6))
#write.csv(paramsandoutputs, "data/optimgridtesting.csv")
optimresults <- ggplot(data = optimoutputs) +
  geom_point(aes(x = d, y = (optimll - min(optimll))))
optimresults
#And so we have determined bimodality on our likelihood surface

ggplot(data = paramsandoutputs) +
  geom_tile(aes(x = dseq, y = thetaseq, fill = optimll)) +
  facet_grid(rhoseq ~ offsetseq) + 
  scale_fill_viridis_c(trans = "log10")
  
