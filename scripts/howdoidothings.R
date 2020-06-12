library(tidyverse)
library(lme4)
library(glmmTMB)
library(cowplot)
library(lattice)
library(ggplot2)
library(brglm2)
library(detectseparation)
library(glmmLasso)
library(rstanarm)
library(brms)
library(tmbstan)

#What I ended up settling for

realdf <- (read_csv("data/mixedmodeldf.csv")
)

realdf$previousyear <- lag(readldf$incidence)

#I decided to remove 2006 bc there was only 1 infection
#I think I will remove 2018 because everyone is infected in 2018

realdf <- realdf[which(realdf$year > 2006), names(realdf) %in% 
                   c("id","year","county","yc","incidence","previousyear")]

realdf$year <- factor(realdf$year) #I want to do this when I import
                                        #But then cant subset factors.



realdf <- (read_csv("data/mixedmodeldf.csv")
    %>% filter(year>2006)
    %>% mutate(previousyear=lag(incidence))
    %>% select(id,year,county, yc, incidence, previousyear)
    %>% mutate_at("year", factor)
)

m1 <- glmer(incidence ~ year + (1|county),
            family=binomial(link = "cloglog"), data=realdf,
            verbose=100)

## try random effects/shrinkage
m1C <- glmer(incidence ~ (1|year) + (1|county),
            family=binomial(link = "cloglog"), data=realdf)

m1B <- glmmTMB(incidence ~ year + (1|county),
            family=binomial(link = "cloglog"), data=realdf)

m1D <- glmmTMB(incidence ~ (1|year) + (1|county),
            family=binomial(link = "cloglog"), data=realdf)

(hazard <- exp(fixef(m1B)$cond))
with(realdf,table(incidence,year))

## Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GHrule(0L), compDev = compDev,  : 
##   pwrssUpdate did not converge in (maxit) iterations

##Error PIRLS step halving failed to reduce deviance
#Can't rescale data (which is what I would do to solve this)
#So my second thought would be to add a control but not sure
#if that makes sense here
#What does clamping do?
#And is this basically just a nudge to use TMB for this problem

m2 <- glm(incidence ~ year, data = realdf,
          family = binomial(link = "cloglog"), method = "brglmFit")
#I found that I have complete separation via brglm2?
#I think I'm suppose to use "detect_separation"?
##But don't really know what penalization to do

update(m2, method=detect_separation)


#I found glmmLasso and started reading it
#Am I looking for packages to fix all my problems too much?
#The penalization should be done only on the fixed effects?
#What I want to do this week is compare a penalized model
#with years as fixed effects or years as random effects

m3 <- glm(incidence ~ offset(log(previousyear + 1)), data = realdf,
            family = binomial(link = "cloglog"))

m4 <- glmer(incidence ~ (1|year) + (1|county) +
                offset(log(previousyear + 1)),
            data = realdf, family = binomial(link = "cloglog"),
            control = glmerControl(tolPwrss=1e-3))

## m4_AGQ <- update(m4, nAGQ=25)

m4a <- allFit(m4)
#Fails to converge
ss <- summary(m4a)
names(ss)
ss$fixef
ss$sdcor

refit_glmmTMB <- function(model) {
    cc <- getCall(model)
    cc[[1]] <- quote(glmmTMB) ## replace the head with glmmTMB
    cc$control <- NULL        ## drop the control argument
    eval.parent(cc)           ## evaluate cc in the 'parent environment'
}

m4B <- refit_glmmTMB(m4)

## runs Stan with the internal TMB object
tmbstan(m4B$obj)
## slow!

m5 <- glmer(incidence ~ year + (1|county) + offset(log(previousyear + 1)),
            data = realdf, family = binomial(link = "cloglog"),
            control=glmerControl(tol))

## table of the number of
(realdf
    %>% group_by(county)
    %>% summarise(tot=sum(incidence))
    %>% ungroup()
    %>% pull(tot)
    %>% table()
)

#this is what I want my final model to be
#Very large eigenvalues and it wants me to rescale (dont think is possible)
#fails to converge hence my thought to use glmmLasso on year


formula <- incidence ~ (1|year) + (1|county) +
    offset(log(previousyear + 1))

mrstan <- rstanarm::stan_glmer(formula, data = realdf,
                family = binomial(link = "cloglog"))

mbrms <- brm(formula, data = realdf,
             family = bernoulli(link = "cloglog"))

## could also use brms, or 

## you can do the simulations with
s1 <- simulate(formula[-2],  ## RHS only
         newdata=realdf,
         newparams=list(beta=-4,   ## intercept (cloglog)
                        theta=c(1,1)), ## SDs
         family=binomial(link = "cloglog"))[[1]]
