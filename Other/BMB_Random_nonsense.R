## SIMULATION protocol

## SI model with spatially weighted contacts; annual time steps

## 1. Get  a list of contact matrices, one for each year,
##       from the data (caves or counties, your choice)
##   Could be sparse, but probably doesn't matter
##   choices:
##    1. nearest-neighbour (null hypothesis)
##    2. global (null hypothesis)
##    3. based on number of shared geocache visits
##
## Q: how should these be normalized?  Probably want row sums equal to 1?
## W[[year]] is the contact matrix for a given year

## pick a value for beta
## make up some starting values (i.e. pick some infected caves in a sensible way): ivec is the state vector (=0 if uninfected, 1 if infected)

## For each year

uninf <- ivec==0
foi <- beta* (W[[year]] %*% ivec)
hazard <- 1-exp(-foi)
ivec[uninf] <- rbinom(sum(uninf), prob=hazard)


##
set.seed(101)
dd <- data.frame(x = rnorm(100))
dd$y <- rpois(100, lambda=exp(1+abs(dd$x)^0.2))
dd <- dd[order(dd$x),]
plot(y~x,data=dd)

## fit a glm
glm1 <- glm(y~poly(x,2,raw=TRUE),data=dd, family=poisson)

mm1 <- model.matrix(~poly(x,2),data=dd)
mm2 <- model.matrix(~poly(x,2,raw=TRUE), data=dd)
op <- par(mfrow=c(1,2),las=1)
matplot(dd$x,mm1,type="l",lty=1,main="ortho")
matplot(dd$x,mm2,type="l",lty=1,main="raw")
par(op)

## doing the same thing the hard way
nllfun <- function(b0,b1,b2,data=dd) {
    pred <- with(data, {
        lambda <- exp(b0 + b1*x + b2*x^2)
    })
    -sum(dpois(data$y,lambda=pred,log=TRUE))
}
nllfun(1,2,3)
opt <- optim(fn=function(p) nllfun(p[1],p[2],p[3]),par=c(1,2,3),
             method="BFGS")
opt$par
coef(glm1)
-logLik(glm1)
opt$value
library(bbmle)
mm <- mle2(y ~ dpois(exp(b0+b1*x+b2*x^2)),
     data=dd,
     start=list(b0=1,b1=2,b2=3))
rbind(mle2=coef(mm),glm=coef(glm1),optim=opt$par)

mm2 <- mle2(y ~ dpois(exp(b0+abs(x)^b1)),
     data=dd,
     start=list(b0=1,b1=1))

## try to use this machinery to fit the baseline-hazard model
##  that you proved can't be fitted with cloglog
##
## you fitted   glm(inf ~ 1+offset(log(n)), family=poisson(link="cloglog"))
##   but you can't fit the model where p = (1-(1-p_c)^n*(1-p_b))

mle2(inf ~ dbinom(prob=(1-(1-p_c)^n*(1-p_b)),
                  start=list(p_c=0.1,p_b=0.1),
                  data = ...)

## you might want to      
     ## plogis() is the CDF of the logistic distrib -> logistic function
     ## -> inverse-logit link (maps (-inf,inf) -> (0,1))
     mle2(inf ~ dbinom(prob=(1-(1-plogis(logit_p_c))^n*(1-plogis(logit_p_b))),
                  start=list(logit_p_c=-1,logit_p_b=-1),
                  data = ...)

## you could read Bolker's "Ecological Models and Data" book if you wanted ...
## that talks about the bbmle package a lot ...
