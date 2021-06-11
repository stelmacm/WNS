#An empty GLMM??? Lets discuss and look into it
library(tidyverse)
library(lme4)
pp <- matrix(rep(0,548), nrow = 548, ncol = 548)
#Maybe it just goes to 0 by default? whatever who cares
#Matrix full of 0's 

#Build the model right out of the TMB stats stuff
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))

#Pulling this out of for loop
incidencedata <- (read.csv("data/incidencepercounty.csv"))%>%
  dplyr::select(county, year, incidence) %>%
  mutate(year = factor(year))

county.incidence.by.year <- mixedmodeldf %>% filter(county != "Lewis-Washington" &
                                                      county != "King-Washington" &
                                                      county != "Plumas-California") %>%
  dplyr::select(-c(yc,id)) %>% arrange(county)

countylist <- list()
#Can't have county incidence by year bc that makes no sense
for(i in levels(mixedmodeldf$year)){
  countylist[[i]] <- county.incidence.by.year %>%
    filter(year == i) %>% dplyr::select(-c(county,year))
}

foidf <- county.incidence.by.year %>% filter(year == 2010) %>%
  dplyr::select(county)

#Multiply the matrices
for (i in levels(mixedmodeldf$year)){
  #multiply W_ij %*% I_t
  foivector <- pp %*% as.matrix(countylist[[i]])
  #reattach
  foidf[,i] <- foivector
}

#The melt looks something like this
forceofinfectiondf <- reshape2::melt(foidf, id = "county") %>%
  dplyr::rename(year = 'variable', foi = 'value') %>%
  arrange(county) %>%
  mutate(year = factor(year))
#Never arranged alphabetically before. odd 

modeldataframe <- left_join(forceofinfectiondf, incidencedata, by = c("county","year")) %>%
  mutate(previousyear=lag(incidence)) %>%
  filter(year != "2006")

modeldataframe$year <- factor(modeldataframe$year)
modeldataframe$previnf <- (lag(modeldataframe$foi))

#So problem with mixed modeldf is that the years continue once incidence has 
#occered. So it is redundant.

#Changing to disappear after incidence occures
newdf <- modeldataframe %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0))

#Mixed model formula
a <- 0.001
formula <- incidence ~ (1|year) + (1|county) + offset(log(previnf + a))

foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))
#Actually maybe I should glmmTMB this..
library(glmmTMB)

m1 <- glmmTMB(formula = formula, data = newdf, family = binomial(link = "cloglog"))

#OK so I mean I guess this sucks as an answer...
#I am now running 100 optims to try and see what the heck is happening and where it breaks
#I will then test its breaking point
source("scripts/creatingfunctiondf.R")
library(TMB)
compile("scripts/nosharedusersTMB.cpp")
dyn.load(dynlib("scripts/nosharedusersTMB"))
countyeffect = rep(0,548)
yeareffect = rep(0,13)
dparam = 50
thetaparam = 1.4
rhoparam = 0.6  
a = 0.001
dd3 <- list(dist = orderedmat, dim = 548, numberofyears = 13,
            fullcountyincidence = countylist)
pp3 <- list(log_d = log(dparam), theta = thetaparam,
            log_offsetparam = log(a),
            logsd_County = 0, logsd_Year = 0,
            YearRandomEffect = yeareffect, CountyRandomEffect = countyeffect)

objnoSU <- MakeADFun(data=dd3, parameters=pp3, DLL="nosharedusersTMB",
                     silent=TRUE,
                     random=c("YearRandomEffect","CountyRandomEffect"))

nlminboptnoSU <- with(objnoSU, nlminb(start = par, obj = fn, gr=gr,
                                      control=list(trace=10)))
#Exactly what we thought would happen
#Maybe going to the 100 optims on this as well

dseq <- seq(10,160,50)
thetaseq <- seq(0.5, 2.5, 1)
offsetseq <- seq(0.01, 1.01, 1)
logsdcountyseq <- 0
logsdyearseq <- 0
params <- expand.grid(dseq, thetaseq, offsetseq, logsdcountyseq, logsdyearseq) %>% rename(c(dseq = Var1, thetaseq = Var2,
                                                                                            offsetsseq = Var3, logsdcountyseq = Var4,
                                                                                            logsdyearsseq = Var5))

library(parallel)
numCores <- detectCores()
library(foreach)
library(doParallel)
registerDoParallel(numCores)

paralledoptim <- function(i){
  results <- optim(par = c(log(params[i,1]),params[i,2],
                           log(params[i,3]), params[i,4], params[i,5]),
                   fn = objnoSU$fn, objnoSU$gr, method = "BFGS", control=list(trace=TRUE))
  if (inherits(results,"try-error")) return(NA_real_)
  return(results$par)
}

mcparoptim <- mclapply((1:nrow(params)), paralledoptim, mc.cores = numCores)
#So this is the bimodal surface, which is exactly what we thought
optimsdf <- data.frame(matrix(unlist(mcparoptim), nrow=length(mcparoptim), byrow=TRUE)) %>% rename(c(d = X1, theta = X2,
                                                                                                     offsets = X3, logsdcounty = X4,
                                                                                                     logsdyears = X5))

distonlymatparams <- cbind(params, optimsdf)
#write.csv(distonlymatparams, "data/nosharedusersoptimgrid.csv")

llresults <- apply(optimsdf, 1 , objnoSU$fn)
alltheanswers <- cbind(distonlymatparams, llresults)
#I dont even know what to say... Im not getting the same answers
#I am all over the place. Frusterating
