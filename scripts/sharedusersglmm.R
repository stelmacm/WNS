#Full model to run with shared users matrix
#Problem with fullmodeltorun.R and this script end at 2017 not 2018
#source("scripts/sharedusersmatrix.R")
#numbersharedcountiesusers is the df to filter out
#write.csv(numbersharedcountiesusers, "data/numbersharedcountiesusers.csv")
#read in the file above ^

distmat <- read.csv("data/distancematrix.csv")
distmat2 <- distmat[,-1]
rownames(distmat2) <- colnames(distmat2)
d1 <- as.matrix(distmat2)

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

countylist <- county.incidence.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  dplyr::select(county)

azzelinifun <- exp(-((d1)/9.508066)^0.512292358)
diag(azzelinifun) <- 0

#Now to weight matrix
localcountymat.w <- mat2listw(azzelinifun, style = "W")
localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
localcountymat <- as.matrix(localcountymat.m)

#Create incidence matrix
for(i in levels(mixedmodeldf$year)){
  countylist[,i] <- county.incidence.by.year %>%
    arrange(year) %>%
    filter(year == i) %>%
    dplyr::select(incidence)
}
#Remove county names
countylist <- as.data.frame(countylist[,-1])

#Create a base layer to begin with
foidf <- county.incidence.by.year %>%
  arrange(year) %>%
  filter(year == 2006) %>%
  dplyr::select(county)

#Create Matrix multiplication
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
  
  #name the cols
  colnames(sharedusers) <- colnames(usermatrix)
  rownames(sharedusers) <- colnames(usermatrix)
  
  orderedmat <- localcountymat[sort(rownames(localcountymat)),sort(colnames(localcountymat))]
  
  #need to detach
  infectionvector <- countylist[,i]
  infectionvector <- as.matrix(infectionvector)
  
  userstimeslocation <- orderedmat %*% sharedusers
  #multiply W_ij %*% I_t
  foivector <- userstimeslocation %*% infectionvector
  #reattach
  foidf[,i] <- foivector
}

#The melt looks something like this
forceofinfectiondf <- reshape2::melt(foidf, id = "county") %>%
  dplyr::rename(year = 'variable', shareduserfoi = 'value') %>%
  arrange(county) %>%
  mutate(year = factor(year))

incidencedata <- (read.csv("data/incidencepercounty.csv"))%>%
  dplyr::select(county, year, incidence) %>%
  mutate(year = factor(year))

modeldataframe <- left_join(forceofinfectiondf, incidencedata, by = c("county","year")) %>%
  mutate(previousyear=lag(incidence)) %>%
  filter(year != "2006") #Maybe there are nicer ways to do this. oh well

modeldataframe$year <- factor(modeldataframe$year)
modeldataframe$previnf <- (lag(modeldataframe$shareduserfoi))

#Changing to disappear after incidence occures
newdf <- modeldataframe %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0))

#Mixed model formula
formula <- incidence ~ (1|year) + (1|county) + offset(log(previnf + 0.00231367))

foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))


#write.csv(newdf, "data/shareusermodeldataframe.csv")
