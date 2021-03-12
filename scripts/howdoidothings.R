#Script for Bolker to run
source("scripts/packages.R")
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

#import already weighted matrix

numbersharedcountiesusers <- read_csv("data/weightedshareduserdf.csv") %>%
  mutate(year = factor(year)) %>% dplyr::select(-X1)

#test <- vroom("data/weightedsharedusers.csv") %>% dplyr:: select(-1) %>% mutate(year = factor(year))

buildthemodel <- function(p) {
  ## unpack parameter vector, via link functions
  d <- p[1]   ## identity (no constraint, we'll hope we don't hit 1.0 exactly)
  theta <- plogis(p[2])*2  ## plogis -> [0,1], then double it for [0,2]
  a <- exp(p[3])           ## must be positive
  rho <- plogis(p[4])
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
    sharedmatrix <- dcast(sharedusersperyear, county ~ county2, value.var = "num.shared")
    #Works because 1st column is names is row 1
    usermatrix <- sharedmatrix[,-1]
    sharedusers <- as.matrix(usermatrix)
    
    #need to detach
    infectionvector <- countylist[,i]
    infectionvector <- as.matrix(infectionvector)
    
    userstimeslocation <- rho*sharedusers + (1-rho)*orderedmat
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
  return(optimalloglik)
  
}


#D, theta, a, rho
linkfun <- function(p) c(p[1], qlogis(p[2]/2), log(p[3]), qlogis(p[4]))

#d <- p[1]   ## identity (no constraint, we'll hope we don't hit 1.0 exactly)
#theta <- p[2]  ## plogis -> [0,1], then double it for [0,2]
#a <- p[3]           ## must be positive
#rho <- p[4]

bestparams <- optim(par = linkfun(c(100, 1.5, .7, 0.5)), fn = buildthemodel, control=list(trace=1000),
                    hessian = TRUE)

#This one start to work and runs the optim better than the above which means starting values matter alot? 
#Eventually it still crashes @ c(9.06057739257812, -4.33280418319937, -7.99249410438807, 10.6374206542969)
bestparams <- optim(par = linkfun(c(10,.5,.5,0.5)), fn = buildthemodel, control=list(trace=1000),
                       hessian = TRUE)

listem <- function(q){
  scalingparam <- q[1]   ## identity (no constraint, we'll hope we don't hit 1.0 exactly)
  tita <- plogis(q[2])*2  ## plogis -> [0,1], then double it for [0,2]
  aa <- exp(q[3])
  row <- plogis(q[4])
  return(c(scalingparam, tita, aa, row))
}

