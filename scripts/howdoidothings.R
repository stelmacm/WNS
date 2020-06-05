library(tidyverse)
library(lme4)
#What I have tried

#Tried to change df such that it looks like binomial version of mixedmodel df
#so it looks like this 

practicedf <- read_csv("data/mixedmodelpractice.csv")

#and it has no recovery stage (as per Adrian) 
#I tried to fit this into first just a mixed model and got singularity which makes sense
#Fit of incidence ~ year_f + (1|county), family=binomial, data=mixedmodeldf worked from last time

#Now if I tried the same line but with the binomial df it shouldnt work 

m1 <- glmer(incidence ~ year_f + (1|county), family=binomial(link = "cloglog"), data=practicedf)
#model fails to coverge which is clearly just a bad model
#So I went back to the incidence ~ 1 + offset(log(incidence[y-1]))
#but this doesnt work in glmer since its not a mixed model (right?)
#I tried this with a glm and realized I had to change up the data a bit and realized I ended up with the same
#glm as I did before
#I didnt think of different connectivity matricies since I wanted to get the original version of this going
#Tried to implement background infection term but I think I need to simply create a new vector for that?
#General uncertainties with what the data frame should look like and contain
#But know what to do with it once I get that

#Does looking into if incidence ~ (year|cave) make sense to do?
#How do I organize my data such that the only thing remaining is playing around with the model and not the data
