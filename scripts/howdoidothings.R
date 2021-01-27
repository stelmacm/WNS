#January 27th Meeting notes

library(tidyverse)

#The data frame with the best parameters
hessian <- read.csv("data/paramhessian.csv")
hessian <- hessian[,-1]
hessian.inv <- solve(hessian)
param.se <- sqrt(diag(hessian.inv))
param.se

#Create Confidence interval matrix 
CI.matrix <- as.data.frame(matrix(NA, nrow = 3, ncol = 3))
#Parameters from optim function 
bestparams <- c(9.501744, -1.066294, -6.065743)

CI.matrix[1,] <- bestparams
CI.matrix[2,] <- bestparams - 1.96 * param.se
CI.matrix[3,] <- bestparams + 1.96 * param.se
names(CI.matrix) <- c("scaling parameter", "theta", "a")
rownames(CI.matrix) <- c("ML", "95% Lower bound", "95% Upper bound")

CI.matrix$theta <- plogis(CI.matrix$theta)*2
CI.matrix$a <- exp(CI.matrix$a)

CI.matrix
#And I hope this is the CI of each param

#What I actually wanted to do

newdf <- read.csv("data/bestparammixeddf.csv")
library(bbmle)
#The construction of thinge needed to run the function
source("scripts/packages.R")
source("scripts/countylistanddistmatrix.R") 
source("scripts/parametrizefunction.R") #Just the function

#Now to use mle2 wrapper of optim instead of optim I need to choose appropriate
#Values from the mixed model df which is what newdf

#when using bbmle I need to know what I have
#p = 0.50
#N = 546 (Number of unique counties)
#k = 12 (number of years to infect a county??)
#But once a county is infected, the data for the remaining years is cut

#Now I want to use newdf to create the mle2 thing
subsetdf <- newdf %>% filter(incidence == 1)

#Tried lots of things and this was the one that made me feel I
#was going in the right direction

#p = prob, k = success, N= number of trials 
binomNLL1 = function(p, k, N) {
  -sum(dbinom(k, prob = p, size = N, log = TRUE)) 
}
unique(newdf$county)
nrow(subsetdf)
#I realize this looks like a bad attempt but there were 100 others that were much worse

attempt3 <- mle2(minuslogl = binomNLL1, start = list(p = .5),
                 data = list(N = nrow(546), k = 12))


#Also realized that with the current parameters, creating the weight matrix 
#all the counties are connected

source("scripts/fullmodeltorun.R")
Matrix::image(localcountymat.m)



#####
set.seed(101)
N <- 100
x <- rnorm(N)
eta <- -3 + 2*x
dd <- data.frame(x,y=rbinom(N, prob=plogis(eta), size=1))

m1 <- glm(y~1+x, data=dd, family=binomial)
stats::confint.default(m1)  ## WALD confidence intervals
## Waiting for profiling to be done...
## loading/calling MASS::confint.glm
confint(m1)

## function parameterized with a vector
## (this is what optim likes)
binomNLL <- function(p) {
    eta <- p[1] + p[2]*dd$x
    return(-sum(dbinom(dd$y, prob=plogis(eta), size=1, log=TRUE)))
}
op1 <- optim(fn=binomNLL, par=c(0,0), hessian=TRUE)
op1$par
op1$par + qnorm(0.975)*sqrt(diag(solve(op1$hessian))) ## Wald CIs

## returns a FUNCTION that calculates the likelihood for (fixedp1, *)
## i.e. we're going to use it to find the best p2 for a given fixed p1
binomwrap <- function(fixedp1) {
    function(p) binomNLL(c(fixedp1,p))
}
p1vec <- seq(-4, 0, length=51)
nllvec <- numeric(length(p1vec))
for (i in seq_along(p1vec)) {
    nllvec[i] <- optim(par=0, fn= binomwrap(p1vec[i]),method="BFGS")$val
}
plot(p1vec,nllvec,ylim=c(30,35))
abline(v=op1$par[1])
abline(h=op1$val+1.92)

## parameterized using a list of arguments
## (this is what mle2 likes)
binomNLL2 <- function(b0, b1) {
    eta <- b0 + b1*x
    return(-sum(dbinom(y, prob=plogis(eta), size=1, log=TRUE)))
}

m2 <- mle2(minuslogl=binomNLL2, start=list(b0=0,b1=0), data=dd)
## stats:::confint.default(m1)  ### ???
confint(m2, method="quad")
confint(m2)
confint(m1) ## compare with GLM profile CIs

## setting up an optim-style objective function for use with mle2
parnames(binomNLL) <- c("b0","b1")  ## assign the parameter names
m2B <- mle2(minuslogl=binomNLL,
            vecpar=TRUE, ## tells mle2 that the obj function uses a vector
            start=list(b0=0,b1=0),
            data=dd)
all(coef(m2B)==coef(m2))
