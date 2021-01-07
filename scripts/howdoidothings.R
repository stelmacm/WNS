#Questions:
#1. Trouble with optim a bit?
#2. Trying to plto predictions vs actual on same graph. Doesn't look nice
#3. Joining residuals of first year of inf. Does that mean the residual of any year at
# the occurance of just the first year that the county recieved the infection. 
#I think I just did 3 wrong and as I write it out I get it now oops

#OPTIM PROBLEM

#Take known data
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))
#Remove things that don't work for me later on
county.incidencee.by.year <- mixedmodeldf[-c(27,28,29,30,31,32,33,
                                             34,35,36,37,38,39,
                                             5201, 5202,5203,5204,
                                             5205, 5206,5207,5208,5209,
                                             5210,5211,5212,5213,
                                             7151,7152,7153,7154,7155,7156
                                             ,7157,7158,7159,7160,7161,7162,7163),-4]
#Only way I could figure it out is via
countylist <- county.incidencee.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  dplyr::select(county)


parametrize <- function(d, theta, a){
  #Maybe just have to do it by hand
  azzelinifun <- exp(-((d1
                        
  )/d)^theta)
  diag(azzelinifun) <- 0
  azzelinifun[(azzelinifun < 1e-100)] <- 0
  
  #Now to weight matrix
  localcountymat.w <- mat2listw(azzelinifun, style = "W")
  localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
  localcountymat <- as.matrix(localcountymat.m)
  
  
  #Create forloop to create dataframe
  #Create dataframe of county by year filled with incidence
  #Retake first year english so that sentence makes sense
  for(i in levels(mixedmodeldf$year)){
    countylist[,i] <- county.incidencee.by.year %>%
      arrange(year) %>%
      filter(year == i) %>%
      dplyr::select(incidence)
  }
  #view(countylist)
  
  
  countylist <- as.data.frame(countylist[,-1])
  
  #Now we need to multiply W_ij by every I_t 
  #Another for loop
  
  #Create a base layer to begin with
  foidf <- county.incidencee.by.year %>%
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
  return(optimalloglik)
}

#From my understanding you pick a "range" for the parameters to be tested at
#They are going downward because I tried upwards and maybe that is making a difference???
test <- parametrize(60,2,1)
#d
d <- seq(100,5,-5)
#theta
theta <- seq(2,0.9,-.1)
#a
a <- seq(0.01,1,0.01)
#I have tried lots of different methods for doing things. More so confused throughout
bestparams <- optim(par = c(2,0.9,0.1), fn = parametrize,theta = theta, a = a, control = list(fnscale = -1))

#Now testing for optimal a value to add to log in order to make a simpler version

avalueparam <- function(a){
  #Maybe just have to do it by hand
  azzelinifun <- exp(-((d1)/35)^0.9)
  diag(azzelinifun) <- 0
  azzelinifun[(azzelinifun < 1e-100)] <- 0
  
  #Now to weight matrix
  localcountymat.w <- mat2listw(azzelinifun, style = "W")
  localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
  localcountymat <- as.matrix(localcountymat.m)
  
  
  #Create forloop to create dataframe
  #Create dataframe of county by year filled with incidence
  #Retake first year english so that sentence makes sense
  for(i in levels(mixedmodeldf$year)){
    countylist[,i] <- county.incidencee.by.year %>%
      arrange(year) %>%
      filter(year == i) %>%
      dplyr::select(incidence)
  }
  #view(countylist)
  
  
  countylist <- as.data.frame(countylist[,-1])
  
  #Now we need to multiply W_ij by every I_t 
  #Another for loop
  
  #Create a base layer to begin with
  foidf <- county.incidencee.by.year %>%
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
  return(optimalloglik)
}

bestaval <- optim(par = 0.01, fn = avalueparam, control = list(fnscale = -1))
#This one finds the best a given the best scaling param and theta from the nested for loop.
#Dont really want toput in a into the nested for loop because computatin time was alot

#A moment of chaos as several plots pop up (theyre all ugly)
#Oops in typical Martin fashion it works in my environment but not a reproducable thing
source("scripts/plotpredictions.R")
