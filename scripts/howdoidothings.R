#My notes on exploration of shiny stan
#Read Gelman and Hill 2007 a tad
#Started reading Gelman and Carlin 
#Alot of things that I already have look nice naturally

#Main Questions:
#1. Priors on covariates? (very vague question) How to basically approach them?
#2. Regularization of priors? When is it appropriate and when is it too much? (also vague)
#3. Scaling priors. Wide scaling is bad because it goes to the middle? 
#4. Where should my threshold be for effective number of draws?
#5. Should I worry about prior intercept everytime I change prior?


#Problems:
#1.
#Initially all the Rhat's look lovely and all
#A pretty normal distribution of n_eff except there are
#a few that are 500ish and others that are 7000 which seems
#like a large range to me.
#Tried tightening up the priors and prior_intercepts via scale 
#to solve this
#It made the range smaller but gave much higher Rhats

#2.
#My next attempt which I realize now was wrong was to try to make a 
#gamm using mgcv and also gamm4
#https://stat.ethz.ch/pipermail/r-sig-ecology/2011-May/002148.html
#You discuss here how I'm pushing my data too hard and GAMM might be overfitting

#3.
#Is it crazy to penalize my priors? 
#What is horse shoe prior? (probably wont use it but didn't realy understand it)
#Is it just shrinkage/fake penalization? 
#My next goal is to work with priors of covariates
#My next thought process is to try laplace or student t for priors

#Just incase 
library(tidyverse)
library(lme4)
library(ggplot2)
library(brglm2)
library(glmmLasso)
library(glmmTMB)
library(brms) 
library(rstan)
library(rstanarm)
library(glmnet)
library(caret)
library(mgcv)
library(gamm4)
set.seed(101)
realdf <- (read.csv("data/mixedmodeldf.csv")  ## switch to read.csv (encodings)
           %>% as_tibble()
           %>% filter(year>2006)
           %>% mutate(previousyear=lag(incidence))
           %>% select(id,year,county, yc, incidence, previousyear)
           %>% mutate_at("year", factor)
)

#Remeber to uncomment for BB
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

formula <- incidence ~ (1|year) + (1|county) +
  offset(log(previousyear + 1))

scaled2nopriorint <- stan_glmer(formula ,
                 family=binomial(link = "cloglog"), data=realdf,
                 prior = normal(location = 0, scale = 2, autoscale = TRUE))

launch_shinystan(scaled2nopriorint)
