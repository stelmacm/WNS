source("scripts/packages.R")
#Another model builder but with weighted shared users already created
#Take known data
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

uninfectedcounties <- county.incidence.by.year 
uninfectedcounties$uninfected <- 0

uninfectedcounties$uninfected[uninfectedcounties$incidence == 0] <- 1

uninfectedcounties <- uninfectedcounties %>% filter(year!= 2006)

uninfectedcountylist <- as.vector(uninfectedcounties$uninfected)

#TRYING SOMETHING OUT
countylist <- county.incidence.by.year %>%
  filter(year == 2006) %>%
  dplyr::select(county)

for(i in levels(mixedmodeldf$year)){
  countylist[,i] <- county.incidence.by.year %>%
    arrange(year) %>%
    filter(year == i) %>%
    dplyr::select(incidence)
}

countylist <- countylist %>% dplyr::select(-county)
countylist <- as.matrix(countylist) #This is the incidence I am taking with me to TMB

#Now creating shared users mat
#import already weighted matrix of shared users
numbersharedcountiesusers <- read_csv("data/weightedshareduserdf.csv") %>%
  mutate(year = factor(year)) %>% dplyr::select(-X1)

sharedusersperyear <- list()
for (i in levels(mixedmodeldf$year)) {
  #Could ultimately do this outside of for loop since its always constant and have a list of vectors inside
  sharedusersperyear[[i]] <- (numbersharedcountiesusers %>% filter(year == i) %>%
                                dplyr::select(-year))
}

sharedmatrix <- list()
sharedusers <- list()
for (i in levels(mixedmodeldf$year)){
  #perfect
  sharedmatrix[[i]] <- dcast(sharedusersperyear[[i]], county ~ county2, value.var = "num.shared")
  #Works because 1st column is names is row 1
  removingcounty <- sharedmatrix[[i]]
  sharedusers[[i]] <- removingcounty[,-1]
}

countysharedusers <- array(as.numeric(unlist(sharedusers)), dim = )

#13 levels
bigsharedusers <- cbind(sharedusers[[1]], sharedusers[[2]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[3]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[4]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[5]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[6]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[7]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[8]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[9]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[10]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[11]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[12]])
bigsharedusers <- cbind(bigsharedusers, sharedusers[[13]])
#write.csv(bigsharedusers, "data/bigsharedusers.csv")
bigsharedusers <- as.matrix(bigsharedusers) #This is the shared users I want for TMB

#Quickly creating the same thing but Sparse
bigshareduserssparse <- as(bigsharedusers, "sparseMatrix")

#Also taking the classic d1 mat
#import and create distance matrix
distmat <- read.csv("data/distancematrix.csv")
distmat2 <- distmat[,-1]
rownames(distmat2) <- colnames(distmat2)
d1 <- as.matrix(distmat2)
diag(d1) <- 0 
#Removing cut off for now
#Going to order d1 matrix so I dont have in C code.
orderedmat <- d1[sort(rownames(d1)),sort(colnames(d1))]


localcountymat.w <- mat2listw(d1, style = "W")
localcountymat.m <- as(localcountymat.w, "CsparseMatrix") 
localcountymat <- as.matrix(localcountymat.m)
#Order the matrix so multiplication makes sense
orderedmat <- localcountymat[sort(rownames(localcountymat)),sort(colnames(localcountymat))]

rho <- .5

countyinalist <- list()
#Can't have county incidence by year bc that makes no sense
for(i in levels(mixedmodeldf$year)){
  countyinalist[[i]] <- county.incidence.by.year %>%
    filter(year == i) %>% dplyr::select(-c(county,year))
}

foidf <- county.incidence.by.year %>% filter(year == 2010) %>%
  dplyr::select(county)

#Multiply the matrices
for (i in levels(mixedmodeldf$year)){
  #Maths
  userstimeslocation <- rho*as.matrix(sharedusers[[i]]) + (1-rho)*orderedmat
  #multiply W_ij %*% I_t
  foivector <- userstimeslocation %*% as.matrix(countyinalist[[i]])
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
  mutate(previousyear=lag(incidence))
#Maybe there are nicer ways to do this. oh well

modeldataframe$year <- factor(modeldataframe$year)
modeldataframe$previnf <- (lag(modeldataframe$foi))

#So problem with mixed modeldf is that the years continue once incidence has 
#occered. So it is redundant.

#Changing to disappear after incidence occures
newdf <- modeldataframe %>% subset((incidence == 0) | (incidence == 1 & previousyear == 0)) %>%
  filter(year != 2006)

#These are my variables into the TMB world
counties <- as.factor(newdf$county) #I dont think I want this as a factor but I'm unsure what else to do with it
years <- as.factor(newdf$year)
fixed_vec <- rep(0, length(newdf))
random_vec <- rep(0,length(newdf))
random_vec2 <- rep(0,length(newdf))
incidence <- as.vector(newdf$incidence)

#Non-row normalized shared users matrix
numbersharedcountiesusers2 <- read_csv("data/numbersharedcountiesusers.csv") %>%
  dplyr::select(-X1) %>% mutate(year = factor(year))

sharedusersperyear2 <- list()
for (i in levels(mixedmodeldf$year)) {
  #Could ultimately do this outside of for loop since its always constant and have a list of vectors inside
  sharedusersperyear2[[i]] <- (numbersharedcountiesusers2 %>% filter(year == i) %>%
                                dplyr::select(-year))
}

sharedmatrix2 <- list()
sharedusers2 <- list()
for (i in levels(mixedmodeldf$year)){
  #perfect
  sharedmatrix2[[i]] <- dcast(sharedusersperyear2[[i]], county ~ county2, value.var = "num.shared")
  #Works because 1st column is names is row 1
  removingcounty <- sharedmatrix2[[i]]
  sharedusers2[[i]] <- removingcounty[,-1]
}

countysharedusers2 <- array(as.numeric(unlist(sharedusers2)))

#13 levels
notrownormbigsharedusers <- cbind(sharedusers[[1]], sharedusers2[[2]])
notrownormbigsharedusers <- cbind(notrownormbigsharedusers, sharedusers2[[3]])
notrownormbigsharedusers <- cbind(notrownormbigsharedusers, sharedusers2[[4]])
notrownormbigsharedusers <- cbind(notrownormbigsharedusers, sharedusers2[[5]])
notrownormbigsharedusers <- cbind(notrownormbigsharedusers, sharedusers2[[6]])
notrownormbigsharedusers <- cbind(notrownormbigsharedusers, sharedusers2[[7]])
notrownormbigsharedusers <- cbind(notrownormbigsharedusers, sharedusers2[[8]])
notrownormbigsharedusers <- cbind(notrownormbigsharedusers, sharedusers2[[9]])
notrownormbigsharedusers <- cbind(notrownormbigsharedusers, sharedusers2[[10]])
notrownormbigsharedusers <- cbind(notrownormbigsharedusers, sharedusers2[[11]])
notrownormbigsharedusers <- cbind(notrownormbigsharedusers, sharedusers2[[12]])
notrownormbigsharedusers <- cbind(notrownormbigsharedusers, sharedusers2[[13]])
#write.csv(bigsharedusers, "data/bigsharedusers.csv")
notrownormbigsharedusers <- as.matrix(notrownormbigsharedusers) #This is the shared users I want for TMB

#Quickly creating the same thing but Sparse
notrownormbigshareduserssparse <- as(notrownormbigsharedusers, "sparseMatrix")
