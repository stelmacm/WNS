#The real DHARMa SIMS code block
library(tidyverse)
library(lme4)
library(DHARMa)
library(glmmTMB)
library(brms)
library(glmmADMB)
library(lattice)
library(brms)

mixedmodeldf <- read.csv("data/mixedmodeldf.csv")
mixedmodeldf$year <- factor(mixedmodeldf$year)
mixedmodeldf$previnf <- (lag(mixedmodeldf$incidence))

#So problem with mixed modeldf is that the years continue once incidence has 
#occered. So it is redundant.

#Changing to disappear after incidence occures
newdf <- mixedmodeldf %>% subset((incidence == 0) | (incidence == 1 & previnf == 0))

#just glm model
foimodel <- glm(incidence ~ offset(log(previousyear + 1)),
                data = newdf, family = binomial(link = "cloglog"))
summary(foimodel)
#So glm looks good.
glmsims <- simulateResiduals(foimodel)
plot(glmsims) #I dont think I underestand res vs pred

plotResiduals(foimodel, newdf$foi) #this makes sense I think

formula <- incidence ~ (1|year) + (1|county) + offset(log(previousyear + 1))

foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))
summary(foimm) #Year is a ~tad~ high

glmmsims <- simulateResiduals(foimm)
plot(glmmsims) #Not that nice...

plotResiduals(foimm)

#Trying to cheat...Maybe wrong approach to large eigenvalue problem
foimm2 <- glmer(formula, data = newdf, family = binomial(link = "cloglog"),
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#Trying glmmTMB
foiTMB <- glmmTMB(formula, data = newdf, family = binomial(link = "cloglog"))
summary(foiTMB) #Its actually a bit worse?
#Still cant figure it out...
tmbsims <- simulateResiduals(foiTMB) #oof

rr <- ranef(foiTMB)$cond
class(rr) <- "ranef.mer"

dotplot(rr)$year #Good
dotplot(rr)$county #Oh my....

#going to lightly try going bayesian
foibrms <- brm(formula, data = newdf, family = binomial(link = "cloglog"))
summary(foibrms) #Terrible, Uglt

foibrmpairs <- pairs(foibrms) #See WNS/figures/brmspairs.png

#Questions:
#I think it is unintuitive to me how to play around with formula
#ie mixedmodeldf$c_year <- as.numeric(as.character(mixedmodeldf$year))-2007
#formula <- incidence ~ 1 + c_year + (1|year) + (1|county) + offset(log(previousyear + 1))
#Unsure if this 
