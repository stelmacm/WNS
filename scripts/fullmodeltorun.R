#Whole model with proper parameters

source("scripts/packages.R")
source("scripts/wns-presence.R")

remotes::install_github("yonghah/esri2sf")

library(tidyverse)
library(sf)
library(sp)

presence.scrape <- read.csv("data/relevant-records.csv")

#Remove Cali and Wash for now
uniq.df <- (presence.df
            %>% dplyr::filter(!STATEPROV %in% c("California","Washington"),
                              !duplicated(county)) #so we only have unique counties
)

wnslat <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[1]])
wnslon <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[2]])

wns.center.coords <- cbind(wnslat, wnslon)

d1 <- distm(wns.center.coords, fun = distGeo)
dimnames(d1) <- list(uniq.df$county,uniq.df$county)

diag(d1) <- 0 
#convert from m to km
d1 <- d1/1000

#Take known data
mixedmodeldf <- read.csv("data/incidencepercounty.csv") %>%
  as_tibble() %>%
  mutate(year = factor(year))
#Remove things that don't work for me later on

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

azzelinifun <- exp(-((d1)/9.508066)^0.512292358)
diag(azzelinifun) <- 0
#azzelinifun[(azzelinifun < cutoffpoint)] <- 0

#d = 9.508066
#theta = 0.512292358
#a = 0.00231367

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
formula <- incidence ~ (1|year) + (1|county) + offset(log(previnf + 0.00231367))

foimm <- glmer(formula, data = newdf, family = binomial(link = "cloglog"))

optimalloglik <- logLik(foimm)