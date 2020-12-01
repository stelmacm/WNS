#The real DHARMa SIMS code block
library(tidyverse)
library(lme4)
library(DHARMa)
library(glmmTMB)
library(brms)
## library(glmmADMB)
library(ggplot2); theme_set(theme_bw())

mixedmodeldf <- read.csv("data/mixedmodeldf.csv")
mixedmodeldf$year <- factor(mixedmodeldf$year)

hist(mixedmodeldf$previousyear) #Ok...

#just glm model
foimodel <- glm(incidence ~ offset(log(previousyear + 1)),
                data = mixedmodeldf, family = binomial(link = "cloglog"))
summary(foimodel)
coef(foimodel)
##So glm looks good.

foimodel2 <- glm(incidence ~ log(previousyear+1),
                 data = mixedmodeldf, family = binomial(link = "cloglog"))
coef(summary(foimodel2))

pframe <- data.frame(previousyear=seq(0,1,length=101))
pframe$pred1 <- predict(foimodel, newdata=pframe, type="response")
pframe$pred2 <- predict(foimodel2, newdata=pframe, type="response")

ggplot(mixedmodeldf,aes(previousyear, incidence)) +
    geom_point() +
    scale_x_continuous(trans="log1p") +
    geom_smooth(method="glm",
                method.args=list(family=binomial(link="cloglog"))) +
    geom_line(data=pframe,aes(y=pred1), colour="red") +
    geom_line(data=pframe,aes(y=pred2), colour="green")

with(mixedmodeldf,table(previousyear==0,incidence))

hist(subset(mixedmodeldf,incidence==0)$previousyear,
     col="gray",breaks=100,
     main="uninfected counties/years only")

hist(subset(mixedmodeldf,incidence==1)$previousyear,
     col="gray",breaks=100,
     main="infected counties/years only")

glmsims <- simulateResiduals(foimodel)
plot(glmsims)
#deviation significant...

plotResiduals(foimodel, mixedmodeldf$foi) #oof

#https://github.com/glmmTMB/glmmTMB/issues/625 

library(brglm2)
library(detectseparation)
septest <- glm(incidence ~ offset(log(previousyear + 1)),
               data = mixedmodeldf, family = binomial(link = "cloglog"), method="detect_separation")

## https://github.com/ikosmidis/detectseparation/issues
septest <- glm((mpg>19) ~ hp+offset(log(cyl)), data=mtcars, family=binomial(link="cloglog"))
update(septest, method="detect_separation")

#Really not understanding !fit2$converged: invalid argument type

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
##Mixed model formula
mixedmodeldf$c_year <- as.numeric(as.character(mixedmodeldf$year))-2007
formula <- incidence ~ 1 + c_year + (1|year) + (1|county) + offset(log(previousyear + 1))

## foimm <- glmer(formula, data = mixedmodeldf, family = binomial(link = "cloglog"), nAGQ = 1)

##Trying glmmTMB
## remotes::install_github("glmmTMB/glmmTMB/glmmTMB")
form0 <- incidence ~ (1 | year) + (1 | county) + offset(log(previousyear + 1))
foiTMB <- glmmTMB(form0, data = mixedmodeldf, family = binomial(link = "cloglog"))
rr1 <- residuals(foiTMB)
summary(foiTMB)
rr <- ranef(foiTMB)$cond
class(rr) <- "ranef.mer"

lattice::dotplot(rr)$year
lattice::dotplot(rr)$county
#Really bad random effects... Complete seperation has occured 

residuals(foiTMB,"pearson") #UGH

newdata2 <- subset(mixedmodeldf,
                   abs(residuals(foiTMB,"pearson"))<10)



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
