#Finally a real mixed model script
library(lme4)
library(TMB)

#Okay so we make a model
#fixed 
fixedform <- incidence ~ year
randomform <- ~ (1|county)

#Use the csv from full-county-year-incidence.R
mixedmodeldf <- read.csv("data/mixedmodeldata.csv")
mixedmodeldf <- data.frame(mixedmodeldf)
mixedmodelx <- model.matrix(fixedform, mixedmodeldf)

yobserved <- mixedmodeldf$incidence

fullmodel <- reformulate(c(all.vars(fixedform),
                           all.vars(randomform)))

fr <- model.frame(fullmodel, mixedmodeldf)
reTrms <- mkReTrms(findbars(randomform), fr)
Z <- t(reTrms$Zt)

#Now we are going to fit a Bernoulli GLMM
compile("scripts/countyTMBmixedmodel.cpp")
dyn.load("scripts/countyTMBmixedmodel.so")

##   eta = X %*% beta + Z %*% b
##   response ~ Bernoulli(invlink(eta))

itemone <- MakeADFun(data=list(mixedmodelx=mixedmodelx,Z=Z,yobserved=yobserved),
                     parameters=list(beta=rep(0,2), ## slope and int
                                     b=rep(0,ncol(Z)),
                                     re_logsd=0),
                     ## specify that b is a random-effect vector (Laplace approx)
                     #questions about implications of random effect vectors and what TMB cran says
                     random="b"
                     ## What does silent = true do?
                     ## It just says "disable all tracing info?" like huh
)


#NOTE(from cran): do not use obj$fn or obj$gr use obj$fn(obj$par) or obj$gr(obj$par)

## the default parameter of the likelihood function (I think)
itemone$fn(itemone$par)
## the default parameter of the gradient function (I think)
itemone$gr(itemone$par)


#How do maps (ie map arguement) work in TMB for collecting and fixing parameters?

nlminb(start=itemone$par, objective=itemone$fn, gradient=itemone$gr)
#Doesn't converge. Oops
#Actually it does. 0 means converges, error messages are fine? i guess
#What is the difference between L-BFGS and BFGS
optim(fn = itemone$fn, gr = itemone$gr , par = itemone$par, method = "BFGS")
#0 means converges
#Need to read more into interpretation of both
