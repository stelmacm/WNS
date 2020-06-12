library(tidyverse)
library(lme4)
library(cowplot)
library(lattice)
library(ggplot2)
library(brglm2)
library(glmmLasso)
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


m1 <- glmer(incidence ~ year + (1|county),
            family=binomial(link = "cloglog"), data=realdf)

#Error PIRLS step halving failed to reduce deviance
#Can't rescale data (which is what I would do to solve this)
#So my second thought would be to add a control but not sure
#if that makes sense here
#What does clamping do?
#And is this basically just a nudge to use TMB for this problem

m2 <- glm(incidence ~ year, data = realdf, family = binomial(link = "cloglog"), method = "brglmFit")
#I found that I have complete seperation via brglm2?
#I think I'm suppose to use "detect_seperation"?
#But don't really know what penalization to do

#I found glmmLasso and started reading it
#Am I looking for packages to fix all my problems too much?
#The penalization should be done only on the fixed effects?
#What I want to do this week is compare a penalized model
#with years as fixed effects or years as random effects

m3 <- glm(incidence ~ offset(log(previousyear + 1)), data = realdf,
            family = binomial(link = "cloglog"))

m4 <- glmer(incidence ~ (1|year) + (1|county) + offset(log(previousyear + 1)),
            data = realdf, family = binomial(link = "cloglog") )
#Fails to converge

m5 <- glmer(incidence ~ year + (1|county) + offset(log(previousyear + 1)),
            data = realdf, family = binomial(link = "cloglog") )
#this is what I want my final model to be
#Very large eigenvalues and it wants me to rescale (dont think is possible)
#fails to converge hence my thought to use glmmLasso on year
