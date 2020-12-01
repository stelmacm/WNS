#The real DHARMa SIMS code block
library(tidyverse)
library(lme4)
library(DHARMa)
library(glmmTMB)
library(brms)
library(glmmADMB)

mixedmodeldf <- read.csv("data/mixedmodeldf.csv")
mixedmodeldf$year <- factor(mixedmodeldf$year)

hist(mixedmodeldf$previousyear) #Ok...

#just glm model
foimodel <- glm(incidence ~ offset(log(previousyear + 1)),
                data = mixedmodeldf, family = binomial(link = "cloglog"))
summary(foimodel)
#So glm looks good.

glmsims <- simulateResiduals(foimodel)
plot(glmsims)
#deviation significant...

plotResiduals(foimodel, mixedmodeldf$foi) #oof

library(brglm2)
library(detectseparation)
septest <- glm(incidence ~ offset(log(previousyear + 1)),
    data = mixedmodeldf, family = binomial(link = "cloglog"), method="detect_separation")
#Really not understanding !fit2$converged: invalid arguement type

#Cant even cheat it...
cheekyseptest <- update(foimodel, method = "detect_separation")

#maybe I'm crazy....
infcheck <- check_infinite_estimates(foimodel)
plot(infcheck)
#So MLE doesnt go to infinity...

#This was dumb
detect_separation_control(
  implementation =  "lpSolveAPI", #ROI or lpSolveAPI
  solver = "lpsolve",
  linear_program = "dual", #Primal or dual... 
  #*me acting like I know why I'm picking dual...*
  purpose =  "test", #find or test
  tolerance = 1e-04,
  solver_control = list()
)

#maybe if I get rid of my ugly data all my problems will go away
newdata <- subset(mixedmodeldf,
                  abs(resid(foimodel, "pearson"))<10) #Nothing goes away....


#This is when I tried with glmm but nothing worked
#Mixed model formula
formula <- incidence ~ (1|year) + (1|county) + offset(log(previousyear + 1))

foimm <- glmer(formula, data = mixedmodeldf, family = binomial(link = "cloglog"), nAGQ = 1)

#Trying glmmTMB
foiTMB <- glmmTMB(formula, data = mixedmodeldf, family = binomial(link = "cloglog"))
summary(foiTMB)
#Really bad random effects... Complete seperation has occured 

residuals(foiTMB,"pearson") #UGH

newdata2 <- subset(mixedmodeldf,
                   abs(residuals(foiTMB,"pearson"))<10)

#https://github.com/glmmTMB/glmmTMB/issues/625 

#going to try dharma sims regardless
#Oof DHARMa hates me... I guess bayesian is a must 
foiTMBressims <- simulateResiduals(foiTMB)

#glmmADMB crashes my computer....
#foiadmb <- glmmadmb(incidence ~ (1|year) + county + offset(log(previousyear + 1)), data = mixedmodeldf, family = "binomial", link = "cloglog")
#all random effects must be factors?


#This takes so long but a quick way to check my sanity
foibrms <- brm(formula, data = mixedmodeldf, family = binomial(link = "cloglog"))
summary(foibrms)
#brms tells me this is garbage with r-hat of 4.22
#Chains aren't mixing
#ESS is too low?
#Probably need thining and like proper priors..

vignette("mcmc", package="glmmTMB")
vignette("model_evaluation",
         package="glmmTMB")
