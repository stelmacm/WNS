library(tidyverse)
#Testing different types of coord to km types
source("scripts/wns-presence.R")

presence.scrape <- read.csv("data/relevant-records.csv")

#Remove Cali and Wash for now
uniq.df<-presence.df %>% dplyr::filter(.,STATEPROV != c("California","Washington"))
uniq.df<-uniq.df[!duplicated(uniq.df$county),] #so we only have unique counties

wnslat <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[1]])
wnslon <- map_dbl(uniq.df$geoms, ~st_centroid(.x)[[2]])

wns.center.coords <- cbind(wnslat, wnslon)

#method 2 (probably the one I will keep)
library(geosphere)
#Many cool functions to remember for later
#distm is the fun but has different methods. DistGeo and distHaversine both seem reasonable
#distGeo takes shortest dist between 2 points on an ellipoid (WGS84) (called geodesic distance)
#I think distGeo is Vincenty (Vinsanity lol) and is more accurate
#https://stackoverflow.com/questions/38248046/is-the-haversine-formula-or-the-vincentys-formula-better-for-calculating-distan
#SEA LEVELS MATTER??? ugh ridinkidinkulous
#option 1
d1<- distm(wns.center.coords, fun = distGeo)
dimnames(d1) <- list(uniq.df$county,uniq.df$county)
view(d1)
d2<- distm(wns.center.coords, fun = distHaversine)
dimnames(d2) <- list(uniq.df$county,uniq.df$county)
view(d2)
identical(d1,d2) #False. mean relative difference is 0.00102. Probably fine
#I am a CLOWN. I spent so much time double checking only to realize im double checking cities not counties
#Which is why everything was wrong

#convert from m to km
d1 <- d1/1000
#Havent decided yet but both are *reasonable*
#Is it cheating to simply pick which one has the nicer likelihood?

xfun2 <- function(x) {
  exp(-pmax(x,30))
}

decay.mat <- xfun2(d1)
#Now set cut off

decay.mat[(decay.mat < 1e-300)] <- 0 #Chose 300 but maybe I should do more?
#Logic for choosing 300 was going through several the matrix and seeing that many
#of the incredibly "small" points were in the 300's
#Although there are some in the upper 200 there is not many and they are usually within state
#which seemed reasonable

diag(decay.mat) <- 0 #Cant see matrix for some reason 

#Now to weight matrix
localcountymat.w <- mat2listw(decay.mat, style = "W")
localcountymat.m <- as(localcountymat.w, "CsparseMatrix") #I hope this makes it sparse
dimnames(localcountymat.m) <- list(uniq.df$county,uniq.df$county)
#A *little* upset I can't file_out this in drake so everything can be up to date but its fine
Matrix::image(localcountymat.m)

#Now organize so we can have a glm format
shareduser <- read.csv("data.gc-shared-users.csv")
#A bit confused on how the left_join this together

countyneighborincidence <- left_join(shareduser, localcountymat.m, by= c("county1","county2")) #This is wrong
#Need to rename dim of localcountymat in order for this to go
