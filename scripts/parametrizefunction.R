#Just the parametrize function
#In order to run a second time need to build a new county list
#ie optimizedhyperparam.R
#Or in countylist and dist mattrix

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
    #need to detach
    infectionvector <- countylist[,i]
    infectionvector <- as.matrix(infectionvector)
    #multiply W_ij %*% I_t
    foivector <- localcountymat %*% infectionvector
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
  return(-1*c(optimalloglik))
}

