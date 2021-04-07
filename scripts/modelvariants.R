#Creating functions for different models and random effects
#This program requires the previous build info before calling the function
#All models are tested on a for loop tha compares models briefly
#Lowest d = 5, lowest theta = .25, lowest a = .35, lowest rho = 0.05

#Model 1
originalmodel <- function(p) {
  ## unpack parameter vector, via link functions
  cat(p,"\n")
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
  
  #Multiply the matrices
  for (i in levels(mixedmodeldf$year)){
    #Maths
    userstimeslocation <- rho*as.matrix(sharedusers[[i]]) + (1-rho)*orderedmat
    #multiply W_ij %*% I_t
    foivector <- userstimeslocation %*% as.matrix(countylist[[i]])
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
    filter(year != "2006") #Maybe there are nicer ways to do this. oh well
  
  modeldataframe$year <- factor(modeldataframe$year)
  modeldataframe$previnf <- (lag(modeldataframe$foi))
  
  #So problem with mixed modeldf is that the years continue once incidence has 
  #occered. So it is redundant.
  
  #Changing to disappear after incidence occures
  newdf <- modeldataframe %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0))
  
  #Mixed model formula
  formula <- incidence ~ (1|year) + (1|county) + offset(log(previnf + a))
  
  foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog")),
               silent=TRUE)
  
  if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(optimalloglik)
}

#Model 2
modelwithnoyear <- function(p) {
  ## unpack parameter vector, via link functions
  cat(p,"\n")
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
  
  #Multiply the matrices
  for (i in levels(mixedmodeldf$year)){
    #Maths
    userstimeslocation <- rho*as.matrix(sharedusers[[i]]) + (1-rho)*orderedmat
    #multiply W_ij %*% I_t
    foivector <- userstimeslocation %*% as.matrix(countylist[[i]])
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
    filter(year != "2006") #Maybe there are nicer ways to do this. oh well
  
  modeldataframe$year <- factor(modeldataframe$year)
  modeldataframe$previnf <- (lag(modeldataframe$foi))
  
  #So problem with mixed modeldf is that the years continue once incidence has 
  #occered. So it is redundant.
  
  #Changing to disappear after incidence occures
  newdf <- modeldataframe %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0))
  
  #Mixed model formula
  formula <- incidence ~ (1|county) + offset(log(previnf + a))
  
  foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog")),
               silent=TRUE)
  
  if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  #return(optimalloglik)
  return(foimm)
}
#Good starting params to optim are d = 5, theta = 1, offset = 0.35, rho = 0.05
#Side note: Not that big a difference for rho = .35. County std dev is 0 for both rho

#Model 3
modelwithnocounty <- function(p) {
  ## unpack parameter vector, via link functions
  cat(p,"\n")
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
  
  #Multiply the matrices
  for (i in levels(mixedmodeldf$year)){
    #Maths
    userstimeslocation <- rho*as.matrix(sharedusers[[i]]) + (1-rho)*orderedmat
    #multiply W_ij %*% I_t
    foivector <- userstimeslocation %*% as.matrix(countylist[[i]])
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
    filter(year != "2006") #Maybe there are nicer ways to do this. oh well
  
  modeldataframe$year <- factor(modeldataframe$year)
  modeldataframe$previnf <- (lag(modeldataframe$foi))
  
  #So problem with mixed modeldf is that the years continue once incidence has 
  #occered. So it is redundant.
  
  #Changing to disappear after incidence occures
  newdf <- modeldataframe %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0))
  
  #Mixed model formula
  formula <- incidence ~ (1|year) + offset(log(previnf + a))
  
  foimm <- try(glmer(formula, data = newdf, family = binomial(link = "cloglog")),
               silent=TRUE)
  
  if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(optimalloglik)
  #return(foimm)
}
#Good starting params to optim are d = 5, theta = 1, offset = 0.05, rho = 0.35
#Side note: Best LL is around those params and theta is the only one that "doesnt matter"

#Model 4
modelonlyoffsetFOI <- function(p) {
  ## unpack parameter vector, via link functions
  cat(p,"\n")
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
  
  #Multiply the matrices
  for (i in levels(mixedmodeldf$year)){
    #Maths
    userstimeslocation <- rho*as.matrix(sharedusers[[i]]) + (1-rho)*orderedmat
    #multiply W_ij %*% I_t
    foivector <- userstimeslocation %*% as.matrix(countylist[[i]])
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
    filter(year != "2006") #Maybe there are nicer ways to do this. oh well
  
  modeldataframe$year <- factor(modeldataframe$year)
  modeldataframe$previnf <- (lag(modeldataframe$foi))
  
  #So problem with mixed modeldf is that the years continue once incidence has 
  #occered. So it is redundant.
  
  #Changing to disappear after incidence occures
  newdf <- modeldataframe %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0))
  
  #Mixed model formula
  formula <- incidence ~ offset(log(previnf + a))
  
  foimm <- try(glm(formula, data = newdf, family = binomial(link = "cloglog")),
               silent=TRUE)
  
  if (inherits(foimm,"try-error")) return(NA_real_)
  optimalloglik <- logLik(foimm)
  return(optimalloglik)
  #return(foimm)
}
#Good starting params to optim are d = 5, theta = 1, offset = 0.05, rho = 0.35
#Definetly an odd model because this is a definite best LL. Silly enough its not a mixed model

#My question is basically does it even make sense to run a model without an a? (offset param)
#I wonder if it would even run. I don't know what I want

linkfun <- function(p) c(p[1], qlogis(p[2]/2), log(p[3]), qlogis(p[4]))
#dist, theta, a (offset param), rho



#Small test for each

testmodel2pt1 <- modelwithnoyear(linkfun(c(10,.5,.1,.3)))
testmodel2pt2 <- modelwithnoyear(linkfun(c(60,1.3,.01,.5))) #Looks very good. Worth looking into more
testmodel2pt3 <- modelwithnoyear(linkfun(c(15,1.9,.01,.5)))

#These models probably just don't make sense
#Values are meh. high std var for year
testmodel3pt1 <- modelwithnocounty(linkfun(c(10,.5,.1,.3)))
testmodel3pt2 <- modelwithnocounty(linkfun(c(60,1.3,.01,.5)))

#Its a glm so it guess I need to look at it different
#Looks hella nice tho
testmodel4pt1 <- modelonlyoffsetFOI(linkfun(c(10,.5,.1,.3)))
testmodel4pt2 <- modelonlyoffsetFOI(linkfun(c(60,1.3,.01,.5)))





#Playing around looking for starting valuess for optim

k <- 1
resultsmodel4 <- data.frame(dis = as.numeric(),
                            tita = as.numeric(),
                            offsetpara = as.numeric(),
                            ro = as.numeric(),
                            loglike = as.numeric()
                            )
for (a in seq(.05,.95,.3)) {
  for (d in seq(5, 105, 25)) {
    for (rho in seq(.05,.95,.3)) {
      for (theta in seq(.25,1.75,.75)) {
        resultsmodel4[k,1] <- d
        resultsmodel4[k,2] <- theta
        resultsmodel4[k,3] <- a
        resultsmodel4[k,4] <- rho
        resultsmodel4[k,5] <- as.numeric(modelwithnocounty(linkfun(c(d,theta,a,rho))))
        k <- k+1
      }
    }
  }
}
resultsmodel4$model <- 4
#write.csv( resultsmodel2, "data/noyearmodelforloop.csv")
#write.csv(resultsmodel3, "data/nocountymodelforloop.csv")
#write.csv(resultsmodel4, "data/onlyoffsetmodelforloop.csv")


bestparams <- optim(par = linkfun(c(9, .6, 0.3, 0.7)), fn = buildthemodel, control=list(trace=1000),
                    hessian = TRUE)


#Playing with parallel
microbenchmark(
for (i in 1:1000) {
  print(sqrt(i))
}
)
library(foreach)
foreach (i=1:10) %do% {
  sqrt(i)
}
library(doParallel)
numCores <- detectCores()
registerDoParallel(numCores)  # use multicore, set to the number of our cores
microbenchmark(
foreach (i=1:1000) %dopar% {
  sqrt(i)
}
)
