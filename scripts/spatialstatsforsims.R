#Spatial statistics of sims
#Maybe create it as a function??
#It can return multiple things

#Lets start by assuming one runs buildmodelquick.R
#The funciton there should return the model but we need the DF from that
#So we will have df of the for
#County, year, FOI, incidence, prev FOI, prev inf
#From there we create a subsetted version?
#Goals: To try and see max/min distance disease has travelled per year, infections by year, ect

#Start by finding every distance between county i,j
distmat <- read.csv("data/distancematrix.csv")
colnames(distmat) <- c("X", distmat$X)
distlong <- melt(distmat) %>% rename(c(county = X, county2 = variable, distance = value))

greatestdisttravelled <- list()
meandisttravelled <- list()
infectionsinayear <- list()

source("scripts/bernoullivizfunction.R")
#The function that builds the viz figure is called simviz
source("scripts/buildmodelbasedonTMB.R")
#The function to build the model df is called buildthemodel
#The order of the args for build the model is d, theta, a, rho
source("scripts/bernoullisimsasfunction.R")
#Function that sims the df. Function is called berniesims and takes the model df as arg
#The data frame only have infected ones

model1 <- buildthemodel(c(5.55, 1.25, -6.25, .58))

firstsim <- berniesims(model1) %>% dplyr::select(-c(foi, incidence, previousyear))


simrez <- left_join(model1, firstsim, by = c("county", "year")) 
simrez[is.na(simrez)] <- 0

spatialstats <- function(simresults){
  simresults[simresults == "La C\x99te-De-Gasp\x8e-Quebec"] <- 'La Côte-De-Gaspé-Quebec'
  simresults[simresults == "L'\x83rable-Quebec"] <- "L'Érable-Quebec"
  simresults[simresults == "Lotbini̬re-Quebec"] <- 'Lotbinière-Quebec'
  simresults[simresults == "Le Haut-Saint-Laurent-Qu̩bec"] <- 'Le Haut-Saint-Laurent-Québec'
  simresults[simresults == "Memphr̩magog-Quebec"] <- 'Memphrémagog-Quebec'
  simresults[simresults == "Le Haut-Saint-Fran\x8dois-Quebec"] <- 'Le Haut-Saint-François-Quebec'
  simresults[simresults == "Antoine-Labelle-Qu̩bec"] <- 'Antoine-Labelle-Québec'
  simresults[simresults == "La C̫te-de-Beaupr̩-Qu̩bec"] <- 'La Côte-de-Beaupré-Québec'
  #So we have fixed the df. Now we need to seperate by each year and compare on a year to year basis
  
  for (i in 1:10) {
  yearone <- simresults %>% filter(year == (2006 + i)) %>% filter(bernie == 1)
  yeartwo <- simresults %>% filter(year == (2007 + i)) %>% filter(bernie == 1)
  
  #Maybe just filter out what counties are in county distances and you can see what the max and min are
  whoshere <- distlong %>% filter(county %in% yearone$county) %>% filter(county2 %in% yeartwo$county)
  greatestdisttravelled[i] <- max(whoshere$distance) #Make these info storages a list 
  meandisttravelled[i] <- mean(whoshere$distance)
  infectionsinayear[i] <- nrow(yeartwo)
  }
  
  #Wonder if its worth comparing each years simulation vs the actual and comparing the difference in years
  return(list(maxdist = greatestdisttravelled, meandist = meandisttravelled, infayear = infectionsinayear))
}
ff <- spatialstats(simrez)

#No idea why this doesnt work....
#vizs <- simviz(simresults = anothersim)

#I think this makes it work
simresults <- berniesims(model1)
source("scripts/bernieviz.R")
infplot
