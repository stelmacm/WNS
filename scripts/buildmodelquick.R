
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

numbersharedcountiesusers <- read_csv("data/weightedshareduserdf.csv") %>%
  mutate(year = factor(year)) %>% dplyr::select(-X1)

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

buildthemodel <- function(p) {
  ## unpack parameter vector, via link functions
  d <- p[1]   ## identity (no constraint, we'll hope we don't hit 1.0 exactly)
  theta <- p[2]  ## plogis -> [0,1], then double it for [0,2]
  a <- p[3]           ## must be positive
  rho <- p[4]
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
    
    #need to detach
    infectionvector <- countylist[,i]
    infectionvector <- as.matrix(infectionvector)
    
    userstimeslocation <- rho*pp + (1-rho)*orderedmat
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
  return(foimm)
  
}

#D, theta, a, rho
#model1 <- buildthemodel(c(100, 2, 1, 0.3))
#model2 <- buildthemodel(c(50,1.5,0.1, 0.5))
#model3 <- buildthemodel(c(80,1.9,0.4, 0.5))
#model4 <- buildthemodel(c(100,1.7,0.9, 0.5))
#model5 <- buildthemodel(c(100, 2, 0.5 , 0.5))

bestmodel <- buildthemodel((c(9.4570985, 0.8363515, 0.4154285, 1.4099289)))
bestmodel
