#Finally a real mixed model script
library(lme4)
library(TMB)
library(glmmTMB)
library(tidyverse)


#Use the csv from full-county-year-incidence.R
mixedmodeldf <- (read_csv("data/mixedmodeldata.csv")
    %>% group_by(county)
    ## only track up to first infection
    %>% filter(cumsum(incidence)<=1)
    %>% ungroup()
    %>% mutate(year_c=year-min(year,na.rm=TRUE),  ## 'centered' (i.e. min=0)
               year_f=factor(year))               ## categorical
)

#Okay so we make a model
#fixed 
fixedform <- incidence ~ year_f
randomform <- ~ (1|county)


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

itemone <- MakeADFun(data=list(mixedmodelx=mixedmodelx,Z=Z,yobserved=yobserved,
                               offset=rep(0,length(yobserved)),
                     parameters=list(beta=rep(0,ncol(mixedmodelx)), ## slope and int
                                     b=rep(0,ncol(Z)),
                                     re_logsd=0),
                     ## specify that b is a random-effect vector (Laplace approx)
                     #questions about implications of random effect vectors and what TMB cran says
                     random="b",
                     ## map = list(beta=c(0,NA)),  ## hold the second element of beta fixed
                     ## at the value specified in 'parameters' (i.e. 0) while optimizing
                     silent = TRUE
                     ## What does silent = true do?
                     ## It just says "disable all tracing info?" like huh
)


#NOTE(from cran): do not use obj$fn or obj$gr use obj$fn(obj$par) or obj$gr(obj$par)

## the default parameter of the likelihood function (I think)
itemone$fn(itemone$par)

itemone$fn() ## uses last internally modified version of the parameters: UNSAFE

## the default parameter of the gradient function (I think)
itemone$gr(itemone$par)


#How do maps (ie map argument) work in TMB for collecting and fixing parameters?

system.time(m1 <- nlminb(start=itemone$par, objective=itemone$fn, gradient=itemone$gr))
#Doesn't converge. Oops
#Actually it does. 0 means converges, error messages are fine? i guess
#What is the difference between L-BFGS and BFGS
optim(fn = itemone$fn, gr = itemone$gr , par = itemone$par, method = "BFGS")
#0 means converges
#Need to read more into interpretation of both

system.time(m2 <- glmer(incidence ~ year_f + (1|county), family=binomial, data=mixedmodeldf))

system.time(m3 <- glmmTMB(incidence ~ year_f + (1|county), family=binomial, data=mixedmodeldf))

## TO DO ...
## * start making this into a real epidemiological model
## * should we worry about recovery?
## * did prob(inf) depend on previous incidence in earlier versions?
## * what about cloglog link/offset? implement that in this context
## * add a 'background infection' term (nonlinear!) and see how much difference that makes
##  glmer/glmmTMB formula
##    we had:  incidence[i,y] ~ 1 + offset(log(incidence[y-1])))
##    or:      incidence[i,y] ~ 1 + log(incidence[y-1])

##  in TMB with no intercept, X is a column of ones (only) (there are no other terms
##   to be estimated in the model)
##  then you have to add the offset
##
##  "0+" means no intercept, so
##   translating from R formula language
##   log-hazard(i,y)? = eta(i,y) = log(incidence[y-1])
##   Prob(i,y) = (1-(1-beta0)^(incidence[y-1]))
##   Prob(i,y) = (1-(1-beta0)^(incidence[y-1])*(1-beta1))
##  beta1 represents a background infection probability that doesn't depend on incidence
 
## also: think about implementing different connectivity matrices.
## right now we have only *global mean incidence*
## we could say that effective force of infection = M*incidence(prev time) where M
## is some appropriate adjacency matrix
