#Finally a real mixed model script
library(lme4)
library(TMB)

#Okay so we make a model
#fixed 
fixedform <- incidence ~ year
randomform <- ~ (1|county)

#Use the csv from full-county-year-incidence.R
#mixedmodeldf <- read.csv("data/mixedmodeldata.csv")
mixedmodeldf <- data.frame(mixedmodeldf)
mixedmodelx <- model.matrix(fixedform, mixedmodeldf)

yobserved <- mixedmodeldf$incidence

fullmodel <- reformulate(c(all.vars(fixedform),
                           all.vars(randomform)))

fr <- model.frame(fullmodel, mixedmodeldf)
reTrms <- mkReTrms(findbars(randomform), fr)
Z <- t(reTrms$Zt)

#Now we are going to fit a Bernoulli GLMM
#No C code yet

##   eta = X %*% beta + Z %*% b
##   response ~ Bernoulli(invlink(eta))

itemone <- MakeADFun(data=list(X=mixedmodelx,Z=Z,yobs=yobserved),
                     parameters=list(beta=rep(0,2), ## slope and int
                                     b=rep(0,ncol(Z)),
                                     re_logsd=0),
                     ## specify that b is a random-effect vector (Laplace approx)
                     random="b"
                     ## could add silent=TRUE here
)

## we end up with an object f
names(itemone)
## evaluate obj fn at original parameters
itemone$fn(itemone$par)
## evaluate gradient at original parameters
itemone$gr(itemone$par)

## the TMB folks seem to prefer nlminb.  You can use any gradient-based
## optimizer (e.g. optim(fn=f$fn, gr=f$gr, par=f$par, method="BFGS"))
nlminb(start=itemone$par, objective=itemone$fn, gradient=itemone$gr)

