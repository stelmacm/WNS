library(tidyverse)
#The problem with the previous scripts was that one was dealing with
#counties that had white nose and the other was dealing with 
#counties that had white nose AND were tagged geocache locations

#I continued simply with the counties that have confirmed white nose

wnscounty <- read.csv("data/counties-containing-wns.csv")
names(wnscounty)[names(wnscounty) == "value"] <- "distancevalue"

#Binomial distance based model 
#Model of infected or not infected with respect to distance parameter. 
#(perhaps distance should be a factor rather than a value)
#Tried doing as factor and it crashed my computer
glm1 <- glm(incidence ~  distancevalue, data = wnscounty,
            family = binomial(link = "cloglog"))
summary(glm1)
#Gross. Not very pretty at all. This is such a bad Log likelihood

#Next step is I want to fit a mixed model?
#I am unsure if this is the direction to go in since out last mixed model did not 
#have a spatial element to it. 
#The previous for was
#formula <- incidence ~ (1|year) + (1|county) +
#offset(log(previousyear + 1))
#Now I am thinking to do it as 
#incidence ~ year + (1|distancevalue) + offset(log(previousyear + 1))
#Creating df of this currently and just want to know that my steps are logical
#Trying to incorporate the spatial aspect but just unsure if this even makes sense

#https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.0006-341X.2002.00129.x
#Interesting reading I found. I have to read again but seems like a cool approach for
#sampling from obsereved points in a continuous area
#Doesn't help us in this problem but none the less found it curious.

op <- par(mfrow=c(2,2))
plot(glm1)
par(op) ## restore graphics parameters

library(DHARMa)
ss <- simulateResiduals(glm1)
plot(ss)
plotResiduals(ss,na.omit(wnscounty$distancevalue))
vignette("DHARMa")

## summary of predictive accuracy?  (ROC)
table(wnscounty$incidence)

## what other information do we have that we can use in the model?
## year
## (receiving) county

## function(distance) {
##   calculate weight matrix, generate distancevalue
##   fit the model (quick)
##   return the log-likelihood
## }
##
## sapply() / for loop / optimize()
## log-spaced vector from 1/5  to 1/10000  (km?)
##
## double-check previous formula: what was 'previousyear' ?
#
## consider incidence ~ (1|year) + (1|county) + offset(log(distancevalue))
##   or               ~ (1|year) + (1|county) + log(distancevalue)

