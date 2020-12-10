#The real DHARMa SIMS code block
library(tidyverse)
library(lme4)
library(DHARMa)
library(glmmTMB)
library(brms)
library(glmmADMB)
library(lattice)
library(broom.mixed)
library(viridisLite)

mixedmodeldf <- read.csv("data/mixedmodeldf.csv")
mixedmodeldf$year <- factor(mixedmodeldf$year)
mixedmodeldf$previnf <- (lag(mixedmodeldf$incidence))

# So problem with mixed modeldf is that the years continue once incidence has 
# occurred. So it is redundant.

#Changing to disappear after incidence occurs
newdf <- mixedmodeldf %>% filter((incidence == 0) | (incidence == 1 & previnf == 0))

#just glm model
foimodel <- glm(incidence ~ offset(log(previousyear + 1)),
                data = newdf, family = binomial(link = "cloglog"))
summary(foimodel)
op <- par(mfrow=c(2,2)); plot(foimodel); par(op)
#So glm looks good.
glmsims <- simulateResiduals(foimodel)
plot(glmsims) #I dont think I underestand res vs pred

with(newdf,hist(log(previousyear+1)))
with(newdf,plot(table(log(previousyear+1))))
with(newdf, table(previousyear==0,incidence))
## ?? why do half the values in the data set have FOI=0?
## this is OK for now

plotResiduals(foimodel, newdf$foi) #this makes sense I think

formula <- incidence ~ (1|year) + (1|county) + offset(log(previousyear + 1))

foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))
af <- allFit(foimm)  ## try everything
ss <- summary(af)
nll <- -1*ss$llik
nlls <- nll - min(nll)
cc <- cbind(ss$fixef,ss$sdcor,nlls)
pairs(cc,cex=2,gap=0)

summary(foimm) #Year is a ~tad~ high

glmmsims <- simulateResiduals(foimm)
plot(glmmsims) #Not that nice...

plotResiduals(foimm)

foimm3 <- update(foimm, control=glmerControl(optimizer="Nelder_Mead"))
## could look at predictions etc etc with this fit and see that they're
## really not all that different ...
                 
#Trying to cheat...Maybe wrong approach to large eigenvalue problem
foimm2 <- glmer(formula, data = newdf, family = binomial(link = "cloglog"),
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#Trying glmmTMB
foiTMB <- glmmTMB(formula, data = newdf, family = binomial(link = "cloglog"))
summary(foiTMB) #Its actually a bit worse?
#Still cant figure it out...
tmbsims <- simulateResiduals(foiTMB) #oof
plot(tmbsims)

rr <- ranef(foiTMB)$cond
class(rr) <- "ranef.mer"

dotplot(rr)$year #Good
dotplot(rr)$county #Oh my....
aa <- as.data.frame(rr)

## could take this data frame and join it with county-level
## info on 'first year infected'
first_inf <- (newdf
    %>% filter(incidence==1)
    %>% group_by(county)
    %>% arrange(year)
    %>% slice_min(1)
    %>% dplyr::select(county,year)
    %>% rename(first_yr=year)
)

aa2 <- (aa
    %>% as_tibble()
    %>% filter(grpvar=="county")
    %>% rename(county=grp)
    %>% dplyr::select(county,condval,condsd)
    %>% full_join(first_inf,by="county")
    %>% mutate(county=reorder(factor(county),condval))
)

gg0 <- ggplot(aa, aes(x=condval, xmin=condval-2*condsd, xmax=condval+2*condsd,
               y=grp)) +
    geom_pointrange()

gg0 + facet_wrap(~grpvar,ncol=2,scale="free")

ggplot(aa2, aes(x=condval, xmin=condval-2*condsd, xmax=condval+2*condsd,
               y=county, colour=first_yr)) +
    geom_pointrange() +
    scale_colour_viridis_d()



#going to lightly try going bayesian
foibrms <- brm(formula, data = newdf, family = binomial(link = "cloglog"))
summary(foibrms) #Terrible, Uglt

foibrmpairs <- pairs(foibrms) #See WNS/figures/brmspairs.png

## only two things that might be worth playing with on the GLMM end:
## * add a linear trend of year (as below)
## * see what happens if FOI is a predictor rather than an offset

## Y = exp(O)*exp(a+bx) = exp(O+a*bx)
## this assumes that Y is _strictly proportional_ to exp(O)
## what if we allow O to have a coefficient?
##  Y = exp(a+b*x+c*O)
## if O != 1 is a better fit ...
## Y = exp(c*O)*exp(a+b*x) = exp(O)^c*exp(a+b*x)
## so it means that whatever our offset (FOI, area, time observed, effort, ...)
##  the outcome is a POWER LAW of that offset rather than strictly proportional
##  e.g. diminishing returns
## if you say Y ~ offset(O) + O + X
##  it says Y=exp(O)*exp(c*O)*exp(a+b*X)
## the null value is proportionality, c measures difference from proportional
##  (c==0 we get back to the old offset model, c>0 accelerating, c<0 ..)
## I DON'T KNOW WHAT hazard not strictly proportional to FOI means in an epidemiological model!
##  FOI = beta*I
##  (beta*I)*S
##  (beta*I)^c * S  ... ???


#Questions:
#I think it is unintuitive to me how to play around with formula
#ie mixedmodeldf$c_year <- as.numeric(as.character(mixedmodeldf$year))-2007
#formula <- incidence ~ 1 + c_year + (1|year) + (1|county) + offset(log(previousyear + 1))
#Unsure if this 
