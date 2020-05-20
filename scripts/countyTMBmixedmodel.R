#Finally a real mixed model script
library(lme4)
library(TMB)

#Okay so we make a model
#fixed 
fixedform <- incidence ~ year
randomform <- ~ (1|county)

#Use the csv from full-county-year-incidence.R
#mixedmodeldf <- read.csv("data/mixedmodeldata.csv")
mixedmodeldf <- data.frame(mixedmodeldf)
mixedmodelframe <- model.matrix(fixedform, mixedmodeldf)

yobserved <- mixedmodeldf$incidence

fullmodel <- reformulate(c(all.vars(fixedform),
                           all.vars(randomform)))

fr <- model.frame(fullmodel, mixedmodeldf)
reTrms <- mkReTrms(findbars(randomform), fr)
Z <- t(reTrms$Zt)


