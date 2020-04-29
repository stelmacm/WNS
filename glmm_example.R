## Bernoulli GLM from scratch in R/TMB
library(lme4)
library(TMB)
library(Matrix)

## currently fitting linear trends (year is numeric)
fixed_form <- inf ~ year
random_form <- ~ (1 |cave) ## could be ~ (year|cave)

## model would be:
##  eta(i) = beta0 + beta_y*year(i) + b(cave(i))
##  mu(i) = invlink(eta(i))
##  b(j) ~ Normal()
##
## if we wanted to have different trends by cave
##  ~ (year|cave)
## eta(i) = beta0 + beta_y*year(i) + b_0(cave(i)) + b_1(cave(i))*year(i)
## 
## simulate data
dd <- expand.grid(year=1:10,cave=factor(letters[1:10]))
set.seed(101)
dd$inf <- rbinom(nrow(dd),size=1,prob=0.3)

## fixed-effect model matrix
X <- model.matrix(fixed_form, dd)
## find all variables in both formulas, turn
##  them into a formula
yobs <- dd$inf

## more complicated, using lme4 machinery so that we could take e.g.
## ~ (year|cave) + (1|county)  and automatically convert it to the correct Z matrix
full_form <- reformulate(c(all.vars(fixed_form),
                          all.vars(random_form)))
fr <- model.frame(full_form, dd)
reTrms <- mkReTrms(findbars(random_form), fr)
Z <- t(reTrms$Zt)

## if we don't need anything fancy in the way of random effect terms
## (i.e. if all we have is variation in the intercept across groups as in this example)
## Z is just an indicator matrix for 'cave'
Z <- t(Matrix::fac2sparse(dd$cave))

## now: use X, Z, response to fit a Bernoulli GLMM
##   eta = X %*% beta + Z %*% b
##   response ~ Bernoulli(invlink(eta))

compile("glmm_example.cpp")
dyn.load("glmm_example.so")
f <- MakeADFun(data=list(X=X,Z=Z,yobs=yobs),
          parameters=list(beta=rep(0,2), ## slope and int
                          b=rep(0,ncol(Z)),
                          re_logsd=0),
          ## specify that b is a random-effect vector (Laplace approx)
          random="b",
          ## could add silent=TRUE here
          silent=TRUE
          )
## we end up with an object f
names(f)
## evaluate obj fn at original parameters
f$fn(f$par)
## evaluate gradient at original parameters
f$gr(f$par)

## the TMB folks seem to prefer nlminb.  You can use any gradient-based
## optimizer (e.g. optim(fn=f$fn, gr=f$gr, par=f$par, method="BFGS"))
nlminb(start=f$par, objective=f$fn, gradient=f$gr)
