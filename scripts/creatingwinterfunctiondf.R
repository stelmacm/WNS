source("scripts/packages.R")

#Now creating the distance decay matrix
distmat <- read.csv("data/distancematrix.csv")
distmat2 <- distmat[,-1]
rownames(distmat2) <- colnames(distmat2)
d1 <- as.matrix(distmat2)
diag(d1) <- 0 

orderedmat <- d1[sort(rownames(d1)),sort(colnames(d1))]

mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))

numbersharedcountiesusers <- read_csv("data/numbersharedcountiesusers.csv") %>%
  dplyr::select(-X1) %>% mutate(year = factor(year))

#Taking this from creatingfunctiondf.R
sharedusersperyear <- list()
for (i in levels(mixedmodeldf$year)) {
  sharedusersperyear[[i]] <- (numbersharedcountiesusers %>% filter(year == i) %>%
                                dplyr::select(-year))
}

sharedmatrix <- list()
sharedusers <- list()
for (i in levels(mixedmodeldf$year)){
  sharedmatrix[[i]] <- dcast(sharedusersperyear[[i]], county ~ county2, value.var = "num.shared")
  #Works because 1st column is names is row 1
  removingcounty <- sharedmatrix[[i]]
  sharedusers[[i]] <- removingcounty[,-1]
}

#Doing this to find the mean for row normalization purposes
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
bigsharedusers <- as.matrix(bigsharedusers)
#Extracting the row means that we want in question
SUmean <- mean(rowMeans(bigsharedusers)) #0.0009559544
#Dividing the shared users matrices by the mean
SUmatrixmeaned <- list()
for (i in levels(mixedmodeldf$year)) {
  SUmatrixmeaned[[i]] <- sharedusers[[i]]/SUmean
}

bigSUmean <- bigsharedusers / SUmean
bigSUmeanmatrix <- as(bigSUmean, "sparseMatrix")

#Pulling out SUmatrixmeaned

#Pulling this out of for loop
incidencedata <- (read.csv("data/incidencepercounty.csv"))%>%
  dplyr::select(county, year, incidence) %>%
  mutate(year = factor(year))

county.incidence.by.year <- mixedmodeldf %>% filter(county != "Lewis-Washington" &
                                                      county != "King-Washington" &
                                                      county != "Plumas-California") %>%
  dplyr::select(-c(yc,id)) %>% arrange(county)


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


frenchify <- function(countydf){
  countydf[countydf == "La C\x99te-De-Gasp\x8e-Quebec"] <- "La Côte-De-Gaspé-Quebec"
  countydf[countydf == "L'\x83rable-Quebec"] <- "L'Érable-Quebec"
  countydf[countydf == "Lotbini̬re-Quebec"] <- 'Lotbinière-Quebec'
  countydf[countydf == "Le Haut-Saint-Laurent-Qu̩bec"] <- 'Le Haut-Saint-Laurent-Québec'
  countydf[countydf == "Memphr̩magog-Quebec"] <- 'Memphrémagog-Quebec'
  countydf[countydf == "Le Haut-Saint-Fran\x8dois-Quebec"] <- 'Le Haut-Saint-François-Quebec'
  countydf[countydf == "Antoine-Labelle-Qu̩bec"] <- 'Antoine-Labelle-Québec'
  countydf[countydf == "La C̫te-de-Beaupr̩-Qu̩bec"] <- 'La Côte-de-Beaupré-Québec'
  return(countydf)
}#OK

#Now need to create winterization into the equation
#created from newdf
#winterization <- newdf %>% arrange(year, county) %>% dplyr::select(winterdays)
#write.csv(winterization, "data/numberofwinterdaysforTMB.csv")
winterization <- read.csv("data/numberofwinterdaysforTMB.csv")
winterizationvector <- as.vector(winterization) #Actually this is wrong
#NEed to make nxn matrix to create the winterdays per year
#and then that gets filtered along with the current set up

#winterstuff <- newmodeldf %>% dplyr::select(c(county, year, winterdays)) %>% filter(year!= 2006)
#wintermartix <- dcast(winterstuff, county ~ year)
#view(finalwintermatrix)
#rownames(wintermartix) <- wintermartix$county
#finalwintermatrix <- wintermartix[,-1]
#write.csv(finalwintermatrix, "data/winterdaysperyearmatrixTMB.csv")
winterizedmatrix <- read.csv("data/winterdaysperyearmatrixTMB.csv")
winterizedmatrix <- winterizedmatrix[,-1]
winterizedmatrix <- as.matrix(winterizedmatrix)
