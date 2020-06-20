library(tidyverse)
library(lme4)
library(cowplot)
library(lattice)
library(ggplot2)
library(brglm2)
library(glmmLasso)
library(glmmTMB)
library(brms)
library(rstan)
library(rstanarm)
library(MCMCglmm)
#Where we finished last time

realdf <- (read_csv("data/mixedmodeldf.csv")
           %>% filter(year>2006)
           %>% mutate(previousyear=lag(incidence))
           %>% select(id,year,county, yc, incidence, previousyear)
           %>% mutate_at("year", factor)
)

#Look into random effects and shrinkage
#I liked stan but had trouble with how long it took
#Even ran it in parallel but many issues with running 
#parallel on ios for stan in R 4.0

#Brm worked nicely but for some models broke on the 4th chain
#breaks down bc it "breaks vectorization and slows down the sampling too much"?

#Settled on having year as fixed effect with prev year.
#Without prev year year as random effect isnt as nice
formula <- incidence ~ (1|year) + (1|county) +
  offset(log(previousyear + 1))


m1 <- glmmTMB(formula ,
              family=binomial(link = "cloglog"), data=realdf)

m2 <- stan_glmer(formula ,
                 family=binomial(link = "cloglog"), data=realdf,
                 cores=4)

options(encoding = "native.enc")
library(shinystan)
## https://github.com/stan-dev/shinystan/issues/171
launch_shinystan(m2)
#Still not the nicest log likihood...

#Had some success with just trying simulations but ultimately 
#couldn't compare goodness of fit of models based on simulations alone

sim <- simulate(formula[-2],  ## RHS only #<- nifty trick via BB notes
                newdata= realdf,
                newparams= list(beta=-3,  #most common output from optims
                               theta=c(1,1)), 
                family=binomial(link = "cloglog"))[[1]] #forget why the 1st element

#Read more into bayesian packages
#https://bmcbiol.biomedcentral.com/articles/10.1186/s12915-017-0357-7
#Read the above for what is meta analysis/PRISMA

#MCMCglmm does zero inflated binom. Am tempted to look more into this
#Don't know much about zero inflated binom models

#Is there a difference between best linear unbiased predictors and 
#posterior mean of random effects? I am not understanding many differnces in
#readings I have found

#TO DO(start of week):
#1. Find appropriate Beta coefficient for simulation (from optimizer)
#2. Figure out difference between brms and stan and such
#3. Is TMB the solution to the slow calculation
#4. Attempt better shrinkage?? (seems fine)
#5. Go more bayesian (but what does that entail...)

#TO DO(post meeting):
#1. Go more bayesian and attempt meta analysis?
#2. "play around with priors" (what does that entail)?
#3. Think about shared users as part of the priors???

