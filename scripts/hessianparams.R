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

attempt1 <- mle2(minuslogl = binomNLL1, start = list(p = .5),
                 data = list(N = nrow(546), k = 12))

