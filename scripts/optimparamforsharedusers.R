#optimizing for new model creation with shared users
## OPTIM PROBLEM
## Testing different types of coord to km types
source("scripts/packages.R")
#source("scripts/wns-presence.R") #Something went wrong. Will fix oneday

library(tidyverse)
library(sf)
library(sp)

presence.scrape <- read.csv("data/relevant-records.csv")

distmat <- read.csv("data/distancematrix.csv")
distmat2 <- distmat[,-1]
rownames(distmat2) <- colnames(distmat2)
d1 <- as.matrix(distmat2)
diag(d1) <- 0 
#convert from m to km
d1 <- d1/1000
#Take known data
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))
#Remove things that don't work for me later on

numbersharedcountiesusers <- read.csv("data/numbersharedcountiesusers.csv")

#Country incidence by year needs to have the following counties removed
#King Washington
#Lewis Washington
#Plumas California

county.incidence.by.year <- mixedmodeldf %>% filter(county != "Lewis-Washington" &
                                                      county != "King-Washington" &
                                                      county != "Plumas-California") %>%
  dplyr::select(-yc)
#This dataframe is OK. 

#Only way I could figure it out is via
countylist <- county.incidence.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  dplyr::select(county)

#This is just county name list

parametrize <- function(p) {
  ## unpack parameter vector, via link functions
  d <- p[1]   ## identity (no constraint, we'll hope we don't hit 1.0 exactly)
  theta <- plogis(p[2])*2  ## plogis -> [0,1], then double it for [0,2]
  a <- exp(p[3])           ## must be positive
  #cutoffpoint <- (1/exp(p[4]))
  #cutoffpoint <- (1e-300)
  #Maybe just have to do it by hand
  azzelinifun <- exp(-((d1)/d)^theta)
  diag(azzelinifun) <- 0
  #azzelinifun[(azzelinifun < cutoffpoint)] <- 0
  
  #Now to weight matrix
  localcountymat.w <- mat2listw(azzelinifun, style = "W")
  localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
  localcountymat <- as.matrix(localcountymat.m)
  #Order the matrix so multiplication makes sense
  orderedmat <- localcountymat[sort(rownames(localcountymat)),sort(colnames(localcountymat))]
  
  #Create forloop to create dataframe
  #Create dataframe of county by year filled with incidence
  #Retake first year english so that sentence makes sense
  for(i in levels(mixedmodeldf$year)){
    countylist[,i] <- county.incidence.by.year %>%
      arrange(year) %>%
      filter(year == i) %>%
      dplyr::select(incidence)
  }
  #view(countylist)
  
  
  countylist <- as.data.frame(countylist[,-1])
  
  #Now we need to multiply W_ij by every I_t 
  #Another for loop
  
  #Create a base layer to begin with
  foidf <- county.incidence.by.year %>%
    arrange(year) %>%
    filter(year == 2006) %>%
    dplyr::select(county)
  
  
  #For loop that creates force of infection datafrome
  for (i in levels(mixedmodeldf$year)) {
    
    sharedusersperyear <- numbersharedcountiesusers %>% filter(year == i) %>%
      dplyr::select(-year)
    
    #perfect
    sharedmatrix <- dcast(sharedusersperyear, county ~ county2)
    #Works because 1st column is names is row 1
    usermatrix <- sharedmatrix[,-1]
    pp <- as.matrix(usermatrix)
    #Set diag = 0
    diag(pp) <- 0
    
    #Now to weight matrix
    sharedusers.w <- mat2listw(pp, style = "W")
    sharedusers.m <- as(sharedusers.w, "CsparseMatrix") 
    sharedusers <- as.matrix(sharedusers.m)
    
    #need to detach
    infectionvector <- countylist[,i]
    infectionvector <- as.matrix(infectionvector)
    
    userstimeslocation <- sharedusers %*% pp
    #multiply W_ij %*% I_t
    foivector <- userstimeslocation %*% infectionvector
    #reattach
    foidf[,i] <- foivector
  }
  
  
  #The melt looks something like this
  forceofinfectiondf <- reshape2::melt(foidf, id = "county") %>%
    dplyr::rename(year = 'variable', foi = 'value') %>%
    arrange(county) %>%
    mutate(year = factor(year))
  #Never arranged alphabetically before. odd 
  
  incidencedata <- (read.csv("data/incidencepercounty.csv"))%>%
    dplyr::select(county, year, incidence) %>%
    mutate(year = factor(year))
  
  modeldataframe <- left_join(forceofinfectiondf, incidencedata, by = c("county","year")) %>%
    mutate(previousyear=lag(incidence)) %>%
    filter(year != "2006") #Maybe there are nicer ways to do this. oh well
  
  modeldataframe$year <- factor(modeldataframe$year)
  modeldataframe$previnf <- (lag(modeldataframe$foi))
  
  #So problem with mixed modeldf is that the years continue once incidence has 
  #occered. So it is redundant.
  
  #Changing to disappear after incidence occures
  newdf <- modeldataframe %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0))
  
  #Mixed model formula
  formula <- incidence ~ (1|year) + (1|county) + offset(log(previnf + a))
  
  foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))
  
  optimalloglik <- logLik(foimm)
  #return(foimm)
  return(-1*c(optimalloglik))
}

##From my understanding you pick a "range" for the parameters to be tested at
##They are going downward because I tried upwards and maybe that is making a difference???
## inverse-link functions: identity, 2*plogis(x), exp()
## link functions: identity, qlogis(x/2), log()

linkfun <- function(p) c(p[1], qlogis(p[2]/2), log(p[3]))
test <- parametrize(linkfun(c(60,2,1)))
test2 <- parametrize(linkfun(c(40,0.9,1)))
test3 <- parametrize(linkfun(c(150,1.8,0.8)))
test4 <- parametrize(linkfun(c(100,1,0.1)))
# d
d <- seq(100,5,-5)
# theta
theta <- seq(2,0.9,-.1)
#a
a <- seq(0.01,1,0.01)
#I have tried lots of different methods for doing things. More so confused throughout
bestparams <- optim(par = linkfun(c(60,1,0.9)), fn = parametrize, control=list(trace=1000),
                    hessian = TRUE)
#I think I got what I wanted???

listem <- function(q){
  scalingparam <- q[1]   ## identity (no constraint, we'll hope we don't hit 1.0 exactly)
  tita <- plogis(q[2])*2  ## plogis -> [0,1], then double it for [0,2]
  aa <- exp(q[3])  
  return(c(scalingparam, tita, aa))
}
