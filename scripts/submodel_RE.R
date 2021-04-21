library(TMB)
library(bbmle)
compile("./submodel_RE.cpp")
dyn.load(dynlib("./submodel_RE"))

set.seed(101)
rfp <- transform(emdbook::ReedfrogPred,
                 nsize=as.numeric(size),
                 random=rnorm(48),
                 block=rep(1:6,each=8))

rfpsim <- expand.grid(density=1:20,block=factor(1:20))
true_logit_a <- -1
true_log_h <- -1
true_logit_a_sd <- 0.3  ## log(0.3) = -1.20
set.seed(101)
## pick logit_a values for each block
logit_a_blk <- rnorm(20, mean=true_logit_a, sd=true_logit_a_sd)
a <- plogis(logit_a_blk[rfpsim$block])
prob <- a/(1 + a*exp(true_log_h)*rfpsim$density)
rfpsim$surv <- rbinom(nrow(rfpsim),
                      size=rfpsim$density,
                      prob=prob)

## generalized: non-Gaussian response
## mixed: >= 1 random effect
## nonlinear: response (location) is nonlinear function of parameters
form <- surv ~ dbinom(size = density,
                      prob = plogis(logit_a)/(1 + plogis(logit_a)*
                                              exp(log_h)*density))

parms <- log_a ~ 1 + (1|block)

##bbmle
mle1 <- bbmle::mle2(form,start=list(logit_a=c(2,0), log_h=log(4)),
                    parameters=list(logit_a~poly(random)),data=rfp)


## qzmle
## mle(form,start=list(h=4,log_a=2),parameters=list(log_a~poly(random)),data=rfp)

## X_logit_a <- model.matrix(~poly(random), data=rfp)
X_logit_a <- model.matrix(~1, data=rfpsim)
rt <- lme4::mkReTrms(list(quote(1|block)), fr=rfpsim)

library(Matrix) ## need this for t() of sparse matrix
dd <- with(rfpsim,
           lme4:::namedList(density, surv,
                           X_logit_a,
                           Z_logit_a=t(rt$Zt)))

obj <- MakeADFun(
  data = dd,
  parameters = list(logit_a_fixed_param=c(0),
                    logit_a_rand_param=rep(0,20),
                    logit_a_logsd=0,
                    log_h=0),
  silent=TRUE,
  DLL = "submodel_RE",
  random="logit_a_rand_param"
)

## likelihood = integral( L(data|beta,b)*L(b|sigma) db)
## ridge (i.e. no 'random=' argument): L(data|beta,b)*L(b|sigma)

## sdreport(obj)

opt2 <- with(obj, nlminb(start = par, obj = fn, gr=gr))
ss <- sdreport(obj)
plotrix::plotCI(1:nrow(rfpsim), ss$value,2*ss$sd, sfrac=0)

## compute probabilities with 'best' (ending) parameters
opt2$par
with(obj$env, last.par.best[-random]) ## fixed effects only
with(obj$env, last.par.best[random])  ## random effects only

obj$report(obj$env$last.par.best)$prob

class(obj) <- "TMB"
broom.mixed::tidy(obj)
library(broom.mixed)
dotwhisker::dwplot(obj)
