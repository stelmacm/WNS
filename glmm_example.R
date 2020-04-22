## Bernoulli GLM from scratch in R/TMB
library(lme4)
library(TMB)

## currently fitting linear trends (year is numeric)
fixed_form <- inf ~ year
random_form <- ~ (1 |cave) ## could be ~ (year|cave)
## simulate data
data <- expand.grid(year=1:10,cave=factor(letters[1:10]))
set.seed(101)
data$inf <- rbinom(nrow(data),size=1,prob=0.3)

## fixed-effect model matrix
X <- model.matrix(fixed_form, data)
## find all variables in both formulas, turn
##  them into a formula
yobs <- data$inf
full_form <- reformulate(c(all.vars(fixed_form),
                          all.vars(random_form)))
fr <- model.frame(full_form, data)
reTrms <- mkReTrms(findbars(random_form), fr)
Z <- t(reTrms$Zt)

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
          random="b"
          ## could add silent=TRUE here
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
